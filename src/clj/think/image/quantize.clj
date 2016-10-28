(ns think.image.quantize
  (:require [clojure.core.matrix :as m]
            [mikera.image.core :as image]
            [mikera.image.filters :as filters]
            [mikera.image.colours :as c]
            [think.image.pixel :refer :all])
  (:import [java.awt.image BufferedImage]))

(defn quantize
  "Quantize an image to n colors.  The underlying filter builds an octree to determine
  a set of colors that will produce minimal distortion, but these will be different
  colors for each image."
  [img n-colors]
  (image/filter-image img (filters/quantize n-colors)))

(def BINS
  (into [] (sort (for [r (range 4)
                       g (range 4)
                       b (range 4)]
                   (pack-pixel
                    (denorm-color (/ r 3.0))
                    (denorm-color (/ g 3.0))
                    (denorm-color (/ b 3.0))
                    (int 0xFF))))))

(defn rough-quantize-color ^Integer [^Integer c ^Double range]
  (denorm-color (/ (Math/floor (+ (* (norm-color c) range) 0.5)) range)))

(defn quantize-px-range
  "Quantize 32-bit ARGB(8888) pixels to RGB(222) format.  On a little-endian machine
  this means we use the right most 6 bits "
  ^Integer [^Integer px ^Double range]
  (let [range range]
    (with-unpacked-pixel px
      (pack-pixel
       (rough-quantize-color r range)
       (rough-quantize-color g range)
       (rough-quantize-color b range)
       (rough-quantize-color a range)))))

(defn quantize-range-pixels
  [^ints pixels ^Double quant-range]
  (let [size (count pixels)]
    (loop [i 0]
      (when (< i size)
        (aset pixels i (quantize-px-range (aget pixels i) quant-range))
        (recur (inc i))))
    pixels))


(defn range-quantize
  "quantize the image so that each channel only has range integer precision.
So if range is 4, you are only using 2 bits per channel.
  If range is 8, you are using 3 bits per channel"
  [^BufferedImage img ^double range]
  (let [^ints pixels (image/get-pixels img)
        _ (quantize-range-pixels pixels range)
        _ (image/set-pixels img pixels)]
    img))

(defn median-cut
  "Takes a list and sorts it by the elements with the greatest range and splits it at the median."
  [l]
  (let [element-width (count (first l))
        max-spread (->> (for [i (range element-width)]
                          (let [li (map #(nth % i) l)]
                            [i (- (apply max li) (apply min li))]))
                        (sort-by second >)
                        (ffirst))]
    (->> l
         (sort-by (fn [x] (nth x max-spread)))
         (split-at (int (/ (count l) 2))))))

(defn quantized-palette
  "Takes an image and returns a pallette of colors that could be used to
  quantize the image using the median-cut algorithm. Note that n must be
  a power of 2."
  ([img n]
   (let [pixels (->> img image/get-pixels vec (map c/components-rgb))]
     (->> (loop [buckets [pixels]
                 iterations (dec (/ (Math/log n) (Math/log 2)))]
            (let [new-buckets (mapcat median-cut buckets)]
              (if (pos? iterations)
                (recur new-buckets (dec iterations))
                new-buckets)))
          (map (fn [bucket] (map int (m/div (apply m/add bucket) (count bucket))))))))
  ([img] (quantized-palette img 256)))
