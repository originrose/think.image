(ns think.image.histogram
  (:require [think.image.pixel :refer :all]
            [think.image.quantize :as quantize]
            [mikera.image.core :as image]
            [clojure.core.matrix :as mat])
  (:import [java.awt.image BufferedImage]))


(defn sparse-histogram-from-pixels
  "Create a sparse histogram map of color->pixel-count from integer pixel data"
  [^ints pixels width height]
  (let [n-pixels (* width height)]
    [(persistent! (reduce (fn [retval idx]
                            (let [px (get-masked-pixel (aget pixels idx))
                                  px-count (inc (get retval px 0))]
                              (assoc! retval px px-count)))
                          (transient {})
                          (range (count pixels))))
     n-pixels]))


(defn sparse-histogram-64
  "Create a sparse histogram of colors in an image"
  [^BufferedImage img]
  (let [width (.getWidth img)
        height (.getHeight img)
        ^ints pixels (quantize/quantize-range-pixels (image/get-pixels img) 3.0)]
    (sparse-histogram-from-pixels pixels width height)))

(defn sparse-histogram-64-to-dense-histogram-64
  "sparse-unnormalized to dense normalized"
  [sparse-histogram-retval]
  (let [[histo npix] sparse-histogram-retval]
    (map (fn [bin-color]
           (let [px-count (get histo bin-color 0)]
             [bin-color (/ px-count npix)]))
         quantize/BINS)))

(defn dense-histogram-to-vector
  "Create a vector of the histogram of colors"
  [histo]
  (mat/array (map second histo)))
