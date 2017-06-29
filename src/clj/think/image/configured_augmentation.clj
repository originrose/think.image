(ns think.image.configured-augmentation
  "A configuration-driven set of image augmentations.
  See also data_augmenation for alternative implementation."
  (:require [mikera.image.core :as i]
            [clojure.java.io :as io])
  (:import (java.awt.geom AffineTransform)
           (java.awt.image AffineTransformOp)))

;;;;;;;;;;;; CONFIGURATION ;;;;;;;;;;;;;
;; These are all available configuration options.
;; The intended way to use this is to set your own values using set-config,
;; then run (preprocess) on your images.
;;;;;;;;;;;; CONFIGURATION ;;;;;;;;;;;;;


(def default-config ^{:private true}
{:horizontal-flip true      ; result: image will, with 50% probability, be flipped left-to-right
 :vertical-flip   false     ; result: image will, with 50% probability, be flipped upside-down
 :crop-location :centered   ; result: image crop occurs at center.  Other option is cropping at a random location.
 :crop-x 0.85               ; result: image will be cropped to 85% of its width. Some cropping is good because a 20% border is added to images to avoid cutoff of rotated portions of image.
 :crop-y 0.85               ; result: image will be cropped to 85% of its height. Some cropping is good because a 20% border is added to images to avoid cutoff of rotated portions of image.
 :max-zoom 1.2              ; result: image will be scaled up randomly, at most to 120% of original size
 :min-zoom 0.9              ; result: image will be scaled down randomly, at most to 90% of original size
 :rotation-range 15         ; result: image will be randomly rotated up to 15 degrees left or right
 :shear-x 0.15              ; result: image will be randomly sheared x-wise in the range [-0.15, 0.15]
 :shear-y 0.15              ; result: image will be randomly sheared x-wise in the range [-0.15, 0.15]
 :warp-range 0.15           ; result: see the (warp) function below
 :output-width 44           ; result: final image width will be 44
 :output-height 44          ; result: final image height will be 44
 })

(def config*
  "This is the global source of truth for all configuration."
  (atom default-config))

(defn set-config
  "Update the configuration either by passing in a config map or a filename."
  [new-config]
  (let [config-update (if (string? new-config)
                        (-> new-config (slurp) (read-string))
                        new-config)]
    (swap! config* merge config-update)))

(defn reset-config [] 
  (set-config default-config))


;;;;;;;;;;;; AUGMENTATIONS ;;;;;;;;;;;;;;;;;
; Image augmentations are built off of java affineTransform, or imagez (which uses affineTransform as well)
; The paradigm is that after setting config*, you use only one function from this file: (preprocess).
; (preprocess) will first upsize an image so that subsequent transforms are more granular, then perform the transforms,
; then add in a border so that no part of a transoformed image is chopped, then crop down
; (so that you can choose how much border to keep) and finally size-down to :output-width and :output-height,
; which are presumeably the size that will be fed into a neural network.
;;;;;;;;;;;; AUGMENTATIONS ;;;;;;;;;;;;;;;;;

(defn- size-up
  "Upsize an image so that transforms are less pixelated."
  [img]
  (i/resize img (* 2 (i/width img)) (* 2 (i/height img))))

(defn- crop
  [img]
  "Crop an image either at the center or at a random location."
  (let [{:keys [crop-location crop-x crop-y]} @config*
        crop-width (* (i/width img) crop-x)
        crop-height (* (i/height img) crop-y)]
    (condp = crop-location
      :random (i/sub-image img (rand (- (i/width img) crop-width)) (rand (- (i/height img) crop-height)) crop-width crop-height)
      :centered (i/sub-image img (/ (- (i/width img) crop-width) 2) (/ (- (i/height img) crop-height) 2) crop-width crop-height))))

(defn- size-down
  "Size the image appropriately for input into the network."
  [img]
  (i/resize img
            (->> @config* :output-width)
            (->> @config* :output-height)))

;;;;;;;;;; Basics of affine transorms ;;;;;;;;;;;;;;
; AffineTransform(float m00, float m10, float m01, float m11, float m02, float m12)
; Constructs a new AffineTransform from 6 floating point values representing the 6 specifiable entries of the 3x3 transformation matrix.
; [ x']   [  m00  m01  m02  ] [ x ]   [ m00x + m01y + m02 ]
; [ y'] = [  m10  m11  m12  ] [ y ] = [ m10x + m11y + m12 ]
; [ 1 ]   [   0    0    1   ] [ 1 ]   [         1         ]
; Note that if tr1 and tr2 are transforms viewed as matrices,
; then (.concatenate tr1 tr2) mutates tr1 to tr1*tr2,
; while (.preConcatenate tr1 tr2) mutates tr1 to tr2*tr1
;;;;;;;;;; Basics of affine transorms ;;;;;;;;;;;;;;


(defn- transform-matrix
  "Interop wrapper for the affineTransform constructor."
  ([m00 m10 m01 m11 m02 m12]
   (AffineTransform. (float m00) (float m10) (float m01) (float m11) (float m02) (float m12)))
  ([m00 m10 m01 m11]
   (transform-matrix m00 m10 m01 m11 0 0))
  ([m02 m12]
   (transform-matrix 1 0 0 1 m02 m12))
  ([]
   (transform-matrix 1 0 0 1 0 0)))

(defn- h-flip
  "Return the horizontal flip transform with 50% probability
  (horizontal means across the y-axis, so that e.g. an h-flipped person is still standing upright)."
  []
  (let [m00 (if (and (:horizontal-flip @config*) (odd? (rand-int 2))) -1 1)]
    (transform-matrix m00 0 0 1)))

(defn- v-flip
  "Return the vertical flip transform with 50% probability."
  []
  (let [m11 (if (and (:vertical-flip @config*) (odd? (rand-int 2))) -1 1)]
    (transform-matrix 1 0 0 m11)))

(defn- rand-range
  "Utility: select a random double from the uniform distribution in range [a b] or [-b b]."
  ([a b]
   (+ a (rand (- b a))))
  ([b] (rand-range (- b) b)))

(defn- zoom
  "Returns a transform which scales an image randomly within the range [:min-zoom, :max-zoom]."
  []
  (let [z (rand-range (:min-zoom @config*) (:max-zoom @config*))]
    (transform-matrix z 0 0 z)))

(defn- rotate
  "Returns a transform which rotates an image (degrees) randomly within the degree range [-:rotation-range, :rotation-range]"
  []
  (let [tx (AffineTransform.)]
    (.rotate tx (Math/toRadians (rand-range (:rotation-range @config*))))
    tx))

(defn- shear
  "Returns a transform which shears an image (equivalent to '(transform-matrix 1 sx sy 1)') sx and sy are in a random range."
  []
  (let [tx (AffineTransform.)]
    (.shear tx (rand-range (:shear-x @config*)) (rand-range (:shear-y @config*)))
    tx))

(defn- warp
  "Returns an area-preserving (determinant=1) linear transformation which is
  (approximately) within the range [-:warp-range, :warp-range] from the identity transformation in each entry."
  []
  (let [w (:warp-range @config*)
        [m00 m01] [(+ 1 (rand-range w)) (rand-range w)]
        [m10 m11] [(rand-range w) (+ 1 (rand-range w))]
        det (- (* m00 m11) (* m10 m11))
        [m00 m01 m10 m11] (map #(/ % (Math/sqrt det)) [m00 m01 m10 m11])]
    (transform-matrix m00 m10 m01 m11)))

(defn- apply-transforms
  "Center the image, then apply the first transform in txs, then the next, to the last.
  Then uncenter the image and embed it into the center of an image with 20% border so that warped images are not cut.
  User may use the crop paramter to remove some or all of the border."
  [img txs]
  (let [tx        (or (first txs) (AffineTransform.))
        _         (doseq [t (rest txs)] (.preConcatenate tx t))
        tx-before (AffineTransform.)
        _         (.translate tx-before (- (quot (i/width img) 2)) (- (quot (i/height img) 2)))
        border    (int (* 0.2 (max (i/width img) (i/height img))))
        tx-after  (.createInverse tx-before)
        _         (.translate tx-after border border)
        _         (.concatenate tx tx-before)
        _         (.preConcatenate tx tx-after)
        op        (AffineTransformOp. tx AffineTransformOp/TYPE_BICUBIC)
        out-image (i/new-image (+ (* 2 border) (i/width img)) (+ (* 2 border) (i/height img)))]
    (.filter op img out-image)
    out-image))

(defn preprocess
  "Randomly apply flips, zooms, and crops to images according to config.
  Output properly sized training and test images for consumption by network."
  [file augment?]
  (let [img        (-> file (io/file) (i/load-image) (size-up))
        transforms (if augment?
                     [(h-flip) (v-flip) (warp) (rotate) (shear) (zoom)]
                     [])]
    (-> (apply-transforms img transforms) (crop) (size-down))))
