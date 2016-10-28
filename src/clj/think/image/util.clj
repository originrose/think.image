(ns think.image.util
  (:require [clojure.core.matrix :as m]
            [mikera.image.core :as i]
            [mikera.image.colours :as colour]
            [think.image.color :as color]))

(defn hue-wheel-image
  "Takes a saturation and a value and produces an image that walks the hue
  wheel whith that saturation and value."
  ([s v]
   (let [img (i/new-image 256 256)]
     (doseq [x (range 0 256)
             y (range 0 256)]
       (let [i (+ (* 256 x) y)
             h (mod i 360)
             [r g b] (color/hsv->rgb [h s v])
             px-color (colour/rgb-from-components r g b)]
         (i/set-pixel img x y px-color)))
     img))
  ([] (hue-wheel-image 1.0 1.0)))

(defn square-image
  "Generates an image with a square in the middle of it"
  []
  (let [img (i/new-image 256 256)]
    (doseq [x (range 64 192)
            y (range 64 192)]
      (let [px-color (colour/rgb-from-components 255 0 0)]
        (i/set-pixel img x y px-color)))
    img))

(defn- dist [x1 y1 x2 y2]
  (let [dx (- x2 x1)
        dy (- y2 y1)]
    (Math/sqrt (+ (* dx dx) (* dy dy)))))

(defn circular-image
  "Generates an image with a circle in the middle of it"
  []
  (let [img (i/new-image 256 256)
        r 100
        c 128]
    (doseq [x (range 0 256)
            y (range 0 256)]
      (let [px-color (if (< (dist c c x y) r)
                       (colour/rgb-from-components 255 0 0)
                       (colour/rgb-from-components 0 0 0))]
        (i/set-pixel img x y px-color)))
    img))

(defn black-image
  "Generates black image"
  []
  (let [img (i/new-image 256 256)]
    (doseq [x (range 0 256)
            y (range 0 256)]
      (i/set-pixel img x y (colour/rgb-from-components 0 0 0)))
    img))

(defn rand-image
  "Generates a random (noise) image"
  []
  (let [img (i/new-image 256 256)]
    (doseq [x (range 0 256)
            y (range 0 256)]
      (i/set-pixel img x y (colour/rgb-from-components (rand-int 255) (rand-int 255) (rand-int 255))))
    img))

(defn rand-colored-blocks-image
  "Generates a bunch of blocks"
  []
  (let [image-size 256
        block-size 16
        img (i/new-image image-size image-size)]
    (doseq [x (range 0 (int (/ image-size block-size)))
            y (range 0 (int (/ image-size block-size)))]
      (let [[r g b] [(rand-int 255) (rand-int 255) (rand-int 255)]]
        (doseq [xi (range (* x block-size) (+ (* x block-size) block-size))
                yi (range (* y block-size) (+ (* y block-size) block-size))]
          (i/set-pixel img xi yi (colour/rgb-from-components r g b)))))
    img))

(defn hue-wheel-colored-blocks-image
  "Generates a bunch of blocks"
  ([s v]
  (let [image-size 256
        block-size 16
        img (i/new-image image-size image-size)]
    (doseq [x (range 0 (int (/ image-size block-size)))
            y (range 0 (int (/ image-size block-size)))]
      (let [i (+ (* (int (/ image-size block-size)) y 2) (* x 11))
             h (mod i 360)
             [r g b] (color/hsv->rgb [h s v])]
        (doseq [xi (range (* x block-size) (+ (* x block-size) block-size))
                yi (range (* y block-size) (+ (* y block-size) block-size))]
          (i/set-pixel img xi yi (colour/rgb-from-components r g b)))))
    img))
  ([] (hue-wheel-colored-blocks-image 1.0 1.0)))

(defn many-colored-blocks-image
  "Generates a bunch of blocks"
  []
  (let [image-size 256
        block-size (/ image-size 16)
        img (i/new-image image-size image-size)]
    (doseq [x (range 0 (int (/ image-size block-size)))
            y (range 0 (int (/ image-size block-size)))]
      (let [bi (+ (* block-size y) x)
            h (* 360.0 (/ bi image-size))]
        (doseq [xi (range (* x block-size) (+ (* x block-size) block-size))
                yi (range (* y block-size) (+ (* y block-size) block-size))]
          (let [s (/ (mod xi block-size) block-size)
                v (/ (mod yi block-size) block-size)
                [r g b] (color/hsv->rgb [h s v])]
            (i/set-pixel img xi yi (colour/rgb-from-components r g b))))))
    img))
