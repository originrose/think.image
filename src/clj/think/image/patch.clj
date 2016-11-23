(ns think.image.patch
  (:require [think.image.core]
            [think.image.image :as image]
            [mikera.image.core :as mi]
            [mikera.image.protocols :as protos]
            [think.image.pixel :as pixel]
            [think.image.image-util :as image-util]
            [clojure.core.matrix :as mat]
            [clojure.core.matrix.macros :refer [c-for]]
            [think.datatype.core :as dtype]
            [clojure.pprint :as pp])
  (:import [java.awt.image BufferedImage]
           [java.awt Rectangle]
           [java.util Arrays]
           [think.image ImageOperations]))


(defn image->content-rect
  [img]
  (-> (image/ensure-buffered-image img)
      (mi/filter-image (com.jhlabs.image.EdgeFilter.))
      image/gray-image->bounding-rect))


(defn content-rect->gray-image
  "Without a specific method to get a precise content mask we can create one from the
  content rect returned from image->content-rect."
  [src-img ^Rectangle content-rect]
  (let [retval (image/new-image-from-prototype src-img :gray)]
    (image/fill retval content-rect (pixel/pack-pixel 255 255 255 255))
    retval))

(defn image->edge-content-mask
  [img]
  (content-rect->gray-image img (image->content-rect img)))


(defn double-to-long
  ^long [^double item]
  (long (Math/round item)))


(defn random-sub-rect
  "Create a square sub-rect inside the outer-rect"
  [^Rectangle outer-rect ^long output-dim]
  (let [left-rand (double (rand))
        top-rand (double (rand))
        output-left (double-to-long (+ (.x outer-rect) (* (- (.width outer-rect) output-dim) left-rand)))
        output-top (double-to-long (+ (.y outer-rect) (* (- (.height outer-rect) output-dim) top-rand)))]
    (Rectangle. output-left output-top output-dim output-dim)))


(defn pixel-byte->double
  ^double [byte-item]
  (pixel/norm-color (bit-and 0xFF (unchecked-int byte-item))))


;;Cutoff for if something is considered in the mask or not.
(def ^:dynamic *content-threshold-cutoff* 0.98)


(defn is-rect-completely-within-mask?
  "Is this rect contained by the mask"
  ([mask-img ^Rectangle rect ^bytes byte-data]
   (image/->array mask-img rect byte-data)
   (let [num-bytes (alength byte-data)
         byte-sum (loop [idx 0
                         sum 0]
                    (if (< idx num-bytes)
                      (recur (inc idx)
                             (+ sum (bit-and 0xFF (unchecked-int
                                                   (aget byte-data idx)))))
                      sum))]
     (>= (double (/ byte-sum
                    (* (.width rect) (.height rect) 255.0)))
         *content-threshold-cutoff*)))
  ([mask-img ^Rectangle rect]
   (is-rect-completely-within-mask? mask-img rect
                                    (byte-array (* (.width rect)
                                                   (.height rect))))))


(defn generate-n-filtered-rects
  "Generate max-src-rect-count random sub-rects, filter by a given
filter function.  Return rect-count of those."
  [rect-filter-fn content-rect rect-count rect-dim
   & {:keys [max-src-rect-count]
      :or {max-src-rect-count 500}}]
  (->> (take max-src-rect-count (repeatedly #(random-sub-rect content-rect rect-dim)))
       (filter rect-filter-fn)
       (take rect-count)))



(defmacro image->patch-impl
  [retval-r retval-g retval-b data-array num-pixels cast-fn]
  `(c-for
    [idx# 0 (< idx# ~num-pixels) (inc idx#)]
    (pixel/with-unpacked-pixel (aget ~data-array idx#)
      (aset ~retval-r idx# (~cast-fn (- (/ (int ~'r) 255.0) 0.5)))
      (aset ~retval-g idx# (~cast-fn (- (/ (int ~'g) 255.0) 0.5)))
      (aset ~retval-b idx# (~cast-fn (- (/ (int ~'b) 255.0) 0.5))))))


(defn image->patch
  ([^BufferedImage img ^Rectangle rect datatype ^ints data-array]
   (image/->array img rect data-array)
   (let [retval-num-pixels (*(.width rect) (.height rect))]
    (condp = datatype
      :double
      (let [retval-r (double-array retval-num-pixels)
            retval-g (double-array retval-num-pixels)
            retval-b (double-array retval-num-pixels)]
        (image->patch-impl retval-r retval-g retval-b data-array retval-num-pixels double)
        [retval-r retval-g retval-b])
      :float
      (let [retval-r (float-array retval-num-pixels)
            retval-g (float-array retval-num-pixels)
            retval-b (float-array retval-num-pixels)]
        (image->patch-impl retval-r retval-g retval-b data-array retval-num-pixels float)
        [retval-r retval-g retval-b]))))
  ([img ^Rectangle rect datatype]
   (image->patch img rect datatype(int-array (* (.width rect)
                                                (.height rect)))))
  ([img ^Rectangle rect]
   (image->patch img rect :double))
  ([^BufferedImage img]
   (image->patch img (image-util/image->rect img))))


(defn patch->image
  ^BufferedImage [data ^long img-width]
  (let [[n-rows n-cols] (mat/shape data)
        img-height (quot (long n-cols) img-width)
        retval (image/new-image image/*default-image-impl* img-width img-height :rgb)
        byte-data (byte-array (mat/ecount data))
        n-pixels (long n-cols)
        [r-data g-data b-data] data]
    (assert (= 3 n-rows))
    (c-for [idx 0 (< idx n-pixels) (inc idx)]
           (aset byte-data (+ (* idx 3) 0)
                 (unchecked-byte (* 255.0 (+ (double (dtype/get-value r-data idx)) 0.5))))
           (aset byte-data (+ (* idx 3) 1)
                 (unchecked-byte (* 255.0 (+ (double (dtype/get-value g-data idx)) 0.5))))
           (aset byte-data (+ (* idx 3) 2)
                 (unchecked-byte (* 255.0 (+ (double (dtype/get-value b-data idx)) 0.5)))))
    (image/array-> retval byte-data)
    retval))



(defn masked-image->patches
  ([img mask-img patch-count patch-dim content-rect datatype
    & {:keys [image-augmentation-fn]}]
   (let [patch-rects (vec
                      (generate-n-filtered-rects
                       (partial is-rect-completely-within-mask? mask-img)
                       content-rect
                       patch-count patch-dim))
         patch-data-array (int-array (* (long patch-dim) (long patch-dim)))
         image-augmentation-fn (or image-augmentation-fn identity)]
     (mapv (fn [rect]
             (let [aug-image (-> (image/sub-image img rect)
                                 image-augmentation-fn)]
               (image->patch aug-image (image-util/image->rect aug-image) datatype patch-data-array)))
           patch-rects))))



(defn buffered-image-has-alpha-channel?
  "Return true of this image has alpha channel"
  [^BufferedImage img]
  (let [buf-img-type (.getType img)]
    (condp = buf-img-type
      BufferedImage/TYPE_INT_ARGB true
      BufferedImage/TYPE_4BYTE_ABGR true
      false)))

(defn image->patches
  "If an image has an alpha channel, then we assume it has a mask.  Else we assume
it is safe to create patches out of the entire image."
  [img patches-per-image patch-dim datatype & {:keys [image-augmentation-fn]}]
  (try
    (let [has-transparency? (buffered-image-has-alpha-channel? img)
          img (mi/ensure-default-image-type img)
          width (image/width img)
          height (image/height img)
          mask (byte-array (* width height))]
      (if-not has-transparency?
        (Arrays/fill mask (unchecked-byte -1))
        (let [^ints data (image/->array img)
              data-len (alength data)]
          (c-for
           [idx 0 (< idx data-len) (inc idx)]
           (aset mask idx
                 (unchecked-byte (pixel/color-int-to-unpacked
                                  (aget data idx) pixel/a-shift))))))
      (let [content-rect (if-not has-transparency?
                           (image-util/image->rect img)
                           (ImageOperations/byteMaskToRectangle mask width height))
            mask-image (image/array-> (image/new-image img width height :gray) mask)]

        (masked-image->patches img mask-image patches-per-image
                               patch-dim content-rect datatype
                               :image-augmentation-fn image-augmentation-fn)
        ))

    (catch Throwable e
      (println "Failed to process image" img)
      (clojure.pprint/pprint e)
      [])))


(defn image-src->patches
  "Given a filename produce a set of random rgb patches of a given datatype potentially
augmented."
  [img-src patches-per-image patch-dim datatype & {:keys [image-augmentation-fn]}]
  (image->patches (protos/as-image img-src) patches-per-image patch-dim datatype :image-augmentation-fn image-augmentation-fn))
