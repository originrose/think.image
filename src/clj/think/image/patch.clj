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


(set! *warn-on-reflection* true)
(set! *unchecked-math* true)


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
  [data-array num-pixels cast-fn array-fn normalize?]
  `(let [retval-r# (~array-fn ~num-pixels)
         retval-g# (~array-fn ~num-pixels)
         retval-b# (~array-fn ~num-pixels)]
     (c-for
       [idx# 0 (< idx# ~num-pixels) (inc idx#)]
       (pixel/with-unpacked-pixel (aget ~data-array idx#)
         (if ~normalize?
           (do
             (aset retval-r# idx# (~cast-fn (- (/ (int ~'r) 255.0) 0.5)))
             (aset retval-g# idx# (~cast-fn (- (/ (int ~'g) 255.0) 0.5)))
             (aset retval-b# idx# (~cast-fn (- (/ (int ~'b) 255.0) 0.5))))
           (do
             (aset retval-r# idx# (~cast-fn ~'r))
             (aset retval-g# idx# (~cast-fn ~'g))
             (aset retval-b# idx# (~cast-fn ~'b))))))
     [retval-r# retval-g# retval-b#]))


(defn image->patch
  "By default, turns BufferedImage img into an array suitable for cortex input.
  If :normalize set to false, returns RGB array of pixel values."
  [^BufferedImage img & {:keys [rect datatype colorspace data-array normalize]
                         :or {datatype :double colorspace :rgb normalize true}}]
  (let [^Rectangle rect (or rect (image-util/image->rect img))
        retval-num-pixels (* (.width rect) (.height rect))
        ^ints data-array (or data-array (int-array retval-num-pixels))
        _ (image/->array img rect data-array)
        retval (condp = datatype
                 :double (image->patch-impl data-array retval-num-pixels double double-array normalize)
                 :float (image->patch-impl data-array retval-num-pixels float float-array normalize))]
    (condp = colorspace
      :rgb retval
      :gray [(first retval)])))


(defn patch-mean-subtract
  "Subtracts means of global images from each channel and optionally reorders RGB->BGR.
  Use for ResNet image preprocessing."
  [patch r-mean g-mean b-mean & {:keys [datatype bgr-reorder]
                                 :or {datatype :double bgr-reorder true}}]
  (let [patch-pairs (map vector patch [r-mean g-mean b-mean])
        centered-patch (map (fn [channel]
                              (map #(- % (second channel)) (first channel))) patch-pairs)
        array-fn (condp = datatype :double double-array :float float-array)]
    (if bgr-reorder
      (->> (reverse centered-patch)
           (mapv #(array-fn %)))
      (mapv #(array-fn %) centered-patch))))


(defn patch->image
  ^BufferedImage [data ^long img-width]
  (let [[n-rows n-cols] (mat/shape data)
        n-rows (long n-rows)
        img-height (quot (long n-cols) img-width)
        retval (image/new-image image/*default-image-impl* img-width img-height :rgb)
        byte-data (byte-array (mat/ecount data))
        n-pixels (long n-cols)
        r-data (first data)
        [g-data b-data] (if-not (= 1 n-rows)
                          [(nth data 1) (nth data 2)]
                          [nil nil])]
    (assert (#{1 3} n-rows))
    (c-for [idx 0 (< idx n-pixels) (inc idx)]
           (aset byte-data (+ (* idx n-rows) 0)
                 (unchecked-byte (* 255.0 (+ (double (dtype/get-value r-data idx)) 0.5))))
           (when (= 3 n-rows)
             (aset byte-data (+ (* idx n-rows) 1)
                   (unchecked-byte (* 255.0 (+ (double (dtype/get-value g-data idx)) 0.5))))
             (aset byte-data (+ (* idx n-rows) 2)
                   (unchecked-byte (* 255.0 (+ (double (dtype/get-value b-data idx)) 0.5))))))
    (image/array-> retval byte-data)
    retval))



(defn masked-image->patches
  [img mask-img patch-count patch-dim content-rect datatype
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
              (image->patch aug-image :data-array patch-data-array)))
          patch-rects)))


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
