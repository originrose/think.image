(ns think.image.patch
  (:require [think.image.core]
            [think.image.image :as image]
            [mikera.image.core :as mi]
            [think.image.pixel :as pixel]
            [clojure.core.matrix :as mat]
            [clojure.core.matrix.macros :refer [c-for]])
  (:import [java.awt.image BufferedImage]
           [java.awt Rectangle]))


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
        output-left (double-to-long (* (- (.width outer-rect) output-dim) left-rand))
        output-top (double-to-long (* (- (.height outer-rect) output-dim) top-rand))]
    (Rectangle. output-left output-top output-dim output-dim)))


(defn pixel-byte->double
  ^double [byte-item]
  (pixel/norm-color (bit-and 0xFF (unchecked-int byte-item))))


;;Cutoff for if something is considered in the mask or not.
(def ^:dynamic *content-threshold-cutoff* 0.98)


(defn is-rect-completely-within-mask?
  "Is this rect contained by the mask"
  ([mask-img ^Rectangle rect byte-data]
   (image/->array mask-img rect byte-data)
   (>= (double (/ (mat/esum (map pixel-byte->double byte-data))
                  (* (.width rect) (.height rect))))
       *content-threshold-cutoff*))
  ([mask-img ^Rectangle rect]
   (is-rect-completely-within-mask? mask-img rect (byte-array (* (.width rect) (.height rect))))))


(defn generate-n-good-rects
  [mask-image content-rect box-count patch-dim]
  (->> (take 500 (repeatedly #(random-sub-rect content-rect patch-dim)))
       (filter #(is-rect-completely-within-mask? mask-image %))
       (take box-count)))


(defn create-patch
  "Create a planar rgb core.matrix patch from a an image and bounding box."
  [image ^Rectangle patch-rect]
  (let [byte-pixels (byte-array (* 3 (.width patch-rect) (.height patch-rect)))]
    (image/->array image patch-rect byte-pixels)
    (-> (mat/array :vectorz (partition 3 (map pixel-byte->double byte-pixels)))
        (mat/sub! 0.5)
        (mat/transpose))))


(defn patch->image
  ^BufferedImage [patch patch-dim]
  (let [patch (if (= 1 (count (mat/shape patch)))
                (mat/reshape patch [3 (* patch-dim patch-dim)])
                patch)
        row-count 3
        col-count (* patch-dim patch-dim)
        img-height (long patch-dim)
        ;;because they are square the transpose operation does not copy
        ^doubles corrected-patch (-> patch
                                     (mat/transpose)
                                     (mat/add 0.5)
                                     (mat/mul! 255.0)
                                     (mat/to-double-array))
        num-bytes (* row-count col-count)
        num-pixels (quot num-bytes 3)
        pixel-data (int-array num-pixels)
        retval (mi/new-image patch-dim img-height false)]
    (c-for [idx 0 (< idx num-pixels) (inc idx)]
           (let [get-offset (* idx 3)]
            (aset pixel-data idx (pixel/pack-pixel
                                  (unchecked-int (aget corrected-patch get-offset))
                                  (unchecked-int (aget corrected-patch (+ 1 get-offset)))
                                  (unchecked-int (aget corrected-patch (+ 2 get-offset)))
                                  255))))
    (mi/set-pixels retval pixel-data)
    retval))


(defn image->patch
  [^BufferedImage img]
  (let [^ints pixels (mi/get-pixels img)
        num-pixels (alength pixels)
        ^doubles r-data (double-array num-pixels)
        ^doubles g-data (double-array num-pixels)
        ^doubles b-data (double-array num-pixels)]
    (c-for [pixel 0 (< pixel num-pixels) (inc pixel)]
           (pixel/with-unpacked-pixel (aget pixels pixel)
             (aset r-data pixel (double r))
             (aset g-data pixel (double g))
             (aset b-data pixel (double b))))
    (-> (mat/array :vectorz [r-data g-data b-data])
        (mat/div! 255.0)
        (mat/sub! 0.5))))


(defn image->patches
  ([img mask-op patch-count patch-dim]
   (let [mask-img (mask-op img)
         content-rect (image/gray-image->bounding-rect mask-img)
         patch-rects (generate-n-good-rects mask-img content-rect patch-count patch-dim)]
     (mapv #(create-patch img %) patch-rects)))
  ([img patch-count patch-dim]
   (image->patches img image->edge-content-mask patch-count patch-dim)))


(defn generate-patches-per-scale
  ([img mask-op patch-dim scale num-patches]
   (-> (if (= 1.0 scale)
         img
         (image/resize img (double-to-long (* (image/width img) scale))))
       (image->patches mask-op num-patches patch-dim)))
  ([img patch-dim scale num-patches]
   (generate-patches-per-scale img image->edge-content-mask patch-dim scale num-patches)))
