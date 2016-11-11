(ns think.image.pixel
  (:require [mikera.image.core :as image]
            [clojure.core.matrix.macros :refer [c-for]])
  (:import [java.awt.image BufferedImage]
           [think.image ByteColor]))

;; Warn on potentially poor performance
(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

;; Colors coming in from images are in ARGB format
(defonce a-shift (int 24))
(defonce r-shift (int 16))
(defonce g-shift (int 8))
(defonce b-shift (int 0))

(defn color-int-to-unpacked ^long [^Integer px ^Integer shift-amount]
  (int (bit-and 0xFF (bit-shift-right px shift-amount))))

(defn color-unpacked-to-int ^Integer [^Integer bt ^Integer shift-amount]
  (bit-shift-left (bit-and 0xFF bt) shift-amount))

(defn unpack-pixel
  "Returns [R G B A].  Note that java does not have unsigned types
  so some of these may be negative"
  [^Integer px]
  [(color-int-to-unpacked px r-shift)
   (color-int-to-unpacked px g-shift)
   (color-int-to-unpacked px b-shift)
   (color-int-to-unpacked px a-shift)])

(defn pack-pixel
  "Converts RGBA values to a single integer representing the color."
  (^Integer [^Integer r ^Integer g ^Integer b ^Integer a]
   (unchecked-int (bit-or
                   (color-unpacked-to-int a a-shift)
                   (color-unpacked-to-int r r-shift)
                   (color-unpacked-to-int g g-shift)
                   (color-unpacked-to-int b b-shift))))
  (^Integer [data]
   (pack-pixel (data 0) (data 1) (data 2) (data 3))))

(defmacro with-unpacked-pixel
  "Unpack a color integer into RGBA values and bind them
  to r, g, b, and a variables locally."
  [px & body]
  `(let [px# ~px
         ~'r (color-int-to-unpacked px# r-shift)
         ~'g (color-int-to-unpacked px# g-shift)
         ~'b (color-int-to-unpacked px# b-shift)
         ~'a (color-int-to-unpacked px# a-shift)]
     ~@body))

(defn norm-color
  "Transform a color intensity from [0, 255] to [0, 1]."
  ^Double [^Integer c]
  (/ (double c) 255.0))

(defn clamp
  "Makes sure item is between min-val and max-val, clamping it to
  that range if necessary."
  ^Double [^Double item ^Double min-val ^Double max-val]
  (min (max item min-val) max-val))

(defn double-to-int
  ^Integer [dval]
  ;;Add .5 so that we correctly round e.g numbers >= 1.5 to 2.
  (unchecked-int (+ dval 0.5)))

(defn double-round
  ^double [^double number]
  (double (Math/round number)))

(defn denorm-color
  "Inverse function for norm-color, transforms colors from [0, 1]
  to [0, 255]."
  ^Integer [^Double c]
  (double-to-int (clamp (* c 255.0) 0.0 255.0)))

(defn is-valid-pixel-location? [^Integer x ^Integer y ^Integer width ^Integer height]
  (and (>= x 0)
       (< x width)
       (>= y 0)
       (< y height)))

(defn pixel-location-to-index[^Integer x ^Integer y ^Integer width]
  (+ x (* y width)))

(defn get-masked-pixel
  "Make sure alpha is always 1.  This allows writing out the image and checking
  the result but still ensures we are ignoring alpha which should have been multiplied
  through by this stage of the pipeline"
  [^Integer color ]
  color)
;  (with-unpacked-pixel color
                                        ;    (pack-pixel r g b a)))


(defn image-requires-alpha-channel
  [^BufferedImage img]
  (let [^ints pixels (image/get-pixels img)
        pix-count (alength pixels)]
    (loop [idx 0]
      (if (< idx pix-count)
        (if (not= (int 255) (color-int-to-unpacked (aget pixels idx) a-shift))
         true
         (recur (inc idx)))
        false))))


(defmacro blend
  [alpha val one-minus-alpha dest]
  `(+ (* ~alpha ~val) (* ~one-minus-alpha ~dest)))


(defn mode-pixel
  ;; Given a sequence of integer pixel values, returns the most common color
  [s]
  (key (apply max-key val (frequencies s))))
