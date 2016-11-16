(ns think.image.data-augmentation
  (:require
    [mikera.image.core :as imagez]
    [clojure.core.matrix.macros :refer [c-for]]
    [think.image.pixel :as think-pixel]
    [think.image.core :as image]
    [think.image.image :as think-image]
    [clojure.core.matrix :as m])
  (:import
    [java.awt.image BufferedImage AffineTransformOp]
    [java.awt.geom AffineTransform]))


(defn- generate-random-int-pixel
  []
  (think-pixel/pack-pixel (rand-int 255) (rand-int 255) (rand-int 255) 255))


(defn- shift-color-component
  [color shift]
  (let [new-color (+ color shift)]
    (if (> new-color 255)
      (- new-color 256)
      new-color)))


(defn- shift-channels
  [color-vec shift]
  (let [size (+ shift (count color-vec))]
    (subvec (vec (flatten (repeat size color-vec)))
            shift size)))

;; clips image
(defn rotate
  "Rotates an image (degrees) and clips parts of image that are outside the bounds"
  [^BufferedImage img degrees random?]
  (let [width (.getWidth img)
        height (.getHeight img)
        degrees (if random? (rand-int 360) degrees)
        out-image (imagez/new-image width height)
        tx (AffineTransform.)
        radians (Math/toRadians degrees)
        _ (.rotate tx radians (quot width 2) (quot height 2))
        op (AffineTransformOp. tx AffineTransformOp/TYPE_BICUBIC)]
    (.filter op img out-image)
    out-image))


(defn mirror
  "Mirrors an image using keys :horizontal :vertical :random"
  [^BufferedImage img & {:keys [direction]
                         :or {direction :random}}]
  (cond
    (= direction :horizontal) (imagez/flip img :horizontal)
    (= direction :vertical) (imagez/flip img :vertical)
    (= direction :random) (imagez/flip img (first (shuffle [:horizontal :vertical])))))


(defn normalize-image
  "Normalizes the image (0-255 to 0-1). Returns a core.matrix vector"
  [^BufferedImage img]
  (let [^ints pixels (imagez/get-pixels img)
        num-pixels (alength pixels)
        ^doubles r-data (double-array num-pixels)
        ^doubles g-data (double-array num-pixels)
        ^doubles b-data (double-array num-pixels)]
    (c-for [pixel 0 (< pixel num-pixels) (inc pixel)]
           (think-pixel/with-unpacked-pixel (aget pixels pixel)
             (aset r-data pixel (double r))
             (aset g-data pixel (double g))
             (aset b-data pixel (double b))))
    (-> (m/array :vectorz [r-data g-data b-data])
      (m/div! 255.0)
      (m/sub! 0.5))))


(defn color-shift
  "Shifts pixel values of the image by channel. Shift amount can be different for each channel"
  [^BufferedImage img shift-vec random?]
  (let [^ints pixels (imagez/get-pixels img)
        num-pixels (alength pixels)
        [red blue green] (if random? [(rand-int 255) (rand-int 255) (rand-int 255)] shift-vec)]
    (c-for [idx 0 (< idx num-pixels) (inc idx)]
           (think-pixel/with-unpacked-pixel (aget pixels idx)
             (aset pixels idx (think-pixel/pack-pixel (shift-color-component r red)
                                                      (shift-color-component b blue)
                                                      (shift-color-component g green) 255))))
    (imagez/set-pixels img pixels)
    img))


(defn channel-shift
  "Rearranges the channels in the image by the shift amount (RGB order)"
  [^BufferedImage img shift]
  (let [^ints pixels (imagez/get-pixels img)
        num-pixels (alength pixels)]
    (c-for [idx 0 (< idx num-pixels) (inc idx)]
           (think-pixel/with-unpacked-pixel (aget pixels idx)
             (aset pixels idx (think-pixel/pack-pixel
                                (conj (shift-channels [r g b] shift) 255)))))
    (imagez/set-pixels img pixels)
    img))


(defn inject-noise
  "Injects noise (pixel-wise) based on the percentage of noise desired in the output image"
  [^BufferedImage img noise-ratio]
  (let [width (.getWidth img)
        height (.getHeight img)
        ^int pixels (imagez/get-pixels img)
        num-pixels (alength pixels)
        num-noisy-pixels (* noise-ratio num-pixels)
        locations-to-edit (take num-noisy-pixels (shuffle (range num-pixels)))
        out-image (imagez/new-image width height)]
    (doseq [location locations-to-edit]
      (aset pixels location (generate-random-int-pixel)))
    (imagez/set-pixels out-image pixels)
    out-image))

(defn- rotate-img-emb-impl [img r x y]
  (let [w (.getWidth img)
        h (.getHeight img)
        at (new java.awt.geom.AffineTransform)
        _ (.translate at x y)
        _ (.rotate at r)
        source-corners [0 0 w 0 w h 0 h]
        dest-corners (double-array [0.0 0.0 1.0 1.0 2.0 2.0 3.0 3.0])
        _ (.transform at (float-array source-corners) 0 dest-corners 0 4)
        xs (take-nth 2 dest-corners)
        ys (take-nth 2 (rest dest-corners))
        minp [(apply min xs) (apply min ys)]
        maxp [(apply max xs) (apply max ys)]
        nw (+ 1 (- (first maxp) (first minp)))
        nh (+ 1 (- (second maxp) (second minp)))
        at2 (new java.awt.geom.AffineTransform)
        _ (.translate at2 x y)
        _ (.translate at2 (- (first minp)) (- (second minp)))
        _ (.rotate at2 r)
        ato (new java.awt.image.AffineTransformOp at2 java.awt.image.AffineTransformOp/TYPE_BICUBIC)
        out (imagez/new-image nw nh)
        _ (.filter ato img out)
        ]
    out))

;; increases size of the image
(defn rotate-img-embiggen
  "rotate the image by a given angle, increasing the size of the result
  image to ensure that it is fully contained."
  ([img r] (rotate-img-emb-impl img r (* 0.5 (.getWidth img)) (* 0.5 (.getHeight img))))
  ([img r x y] (rotate-img-emb-impl img r x y)))

(defn make-noise-image [img]
  (let [height (.getHeight img)
        width (.getWidth img)
        new-img (imagez/new-image width height)
        pxls (imagez/get-pixels img)]
    (dotimes [i (* width height)]
      (aset pxls i (generate-random-int-pixel)))
    (imagez/set-pixels new-img pxls)
    new-img))
