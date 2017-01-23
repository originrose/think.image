(ns think.image.data-augmentation
  (:require
    [mikera.image.core :as i]
    [mikera.image.colours :as c]
    [clojure.core.matrix.macros :refer [c-for]]
    [think.image.pixel :as think-pixel]
    [think.image.core :as image]
    [think.image.image :as think-image]
    [clojure.core.matrix :as m])
  (:import
    [java.awt.image BufferedImage AffineTransformOp]
    [java.awt Color Graphics2D Rectangle]
    [java.awt.geom AffineTransform Point2D Point2D$Float]))


(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defn- generate-random-int-pixel
  ^long
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
        degrees (if random? (rand-int degrees) degrees)
        out-image (i/new-image width height)
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
    (= direction :horizontal) (i/flip img :horizontal)
    (= direction :vertical) (i/flip img :vertical)
    (= direction :random) (i/flip img (first (shuffle [:horizontal :vertical])))))


(defn normalize-image
  "Normalizes the image (0-255 to 0-1). Returns a core.matrix vector"
  [^BufferedImage img]
  (let [^ints pixels (i/get-pixels img)
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
  (let [^ints pixels (i/get-pixels img)
        num-pixels (alength pixels)
        [red blue green] (if random? [(rand-int 255) (rand-int 255) (rand-int 255)] shift-vec)]
    (c-for [idx 0 (< idx num-pixels) (inc idx)]
           (think-pixel/with-unpacked-pixel (aget pixels idx)
             (aset pixels idx (think-pixel/pack-pixel (shift-color-component r red)
                                                      (shift-color-component b blue)
                                                      (shift-color-component g green) 255))))
    (i/set-pixels img pixels)
    img))


(defn channel-shift
  "Rearranges the channels in the image by the shift amount (RGB order)"
  [^BufferedImage img shift]
  (let [^ints pixels (i/get-pixels img)
        num-pixels (alength pixels)]
    (c-for [idx 0 (< idx num-pixels) (inc idx)]
           (think-pixel/with-unpacked-pixel (aget pixels idx)
             (aset pixels idx (think-pixel/pack-pixel
                                (conj (shift-channels [r g b] shift) 255)))))
    (i/set-pixels img pixels)
    img))


(defn inject-noise
  "Injects noise (pixel-wise) based on the percentage of noise desired in the output image"
  [^BufferedImage img noise-ratio]
  (let [width (.getWidth img)
        height (.getHeight img)
        ^ints pixels (i/get-pixels img)
        num-pixels (alength pixels)
        num-noisy-pixels (* noise-ratio num-pixels)
        locations-to-edit (take num-noisy-pixels (shuffle (range num-pixels)))
        out-image (i/new-image width height)]
    (doseq [location locations-to-edit]
      (aset pixels location (generate-random-int-pixel)))
    (i/set-pixels out-image pixels)
    out-image))

(defn- rotate-img-emb-impl [^BufferedImage img r x y]
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
        out (i/new-image nw nh)
        _ (.filter ato img out)
        ]
    out))

;; increases size of the image
(defn rotate-img-embiggen
  "rotate the image by a given angle, increasing the size of the result
  image to ensure that it is fully contained."
  ([^BufferedImage img r] (rotate-img-emb-impl img r (* 0.5 (.getWidth img)) (* 0.5 (.getHeight img))))
  ([img r x y] (rotate-img-emb-impl img r x y)))

(defn make-noise-image [^BufferedImage img]
  (let [height (.getHeight img)
        width (.getWidth img)
        new-img (i/new-image width height)
        ^ints pxls (i/get-pixels img)]
    (dotimes [i (* width height)]
      (aset pxls i (generate-random-int-pixel)))
    (i/set-pixels new-img pxls)
    new-img))

(defn- get-shifted-image
  [^BufferedImage img fetch-locations write-locations]
  (let [out-image (i/new-image (.getWidth img) (.getHeight img))
        [x1 y1 width1 height1] fetch-locations
        [x2 y2 width2 height2] write-locations
        shifted-pixels (.getDataElements (.getRaster img) x1 y1 width1 height1 nil)]
    (.setDataElements (.getRaster out-image) x2 y2 width2 height2 shifted-pixels)
    out-image))

(defn shift
  "Shifts the image by a given amount. Returns a vector of 2 images for each shift direction pair (left/right or top/bottom) based on the direction (:horizontal or :vertical)"
  [^BufferedImage img shift direction & {:keys [random?]
                               :or {random? false}}]
  (let [width (.getWidth img)
        height (.getHeight img)
        shift (if random? (rand-int shift) shift)]
    (cond
      (= direction :horizontal)
      [(get-shifted-image img [shift 0 (- width shift) height] [0 0 (- width shift) height])
       (get-shifted-image img [0 0 (- width shift) height] [shift 0 (- width shift) height])]
      (= direction :vertical)
      [(get-shifted-image img [0 shift width (- height shift)] [0 0 width (- height shift)])
       (get-shifted-image img [0 0 width (- height shift)] [0 shift width (- height shift)])]
      )))

(defn random
  "random float between a and b"
  [a b]
  (+ a (* (rand) (- b a))))

(def ^{:private true} pi 3.141592653589793)
(def ^{:private true} tau (* 2.0 pi))

(defn- rot 
  "Produce a rotation matrix to rotate a 2D vector by a given angle, 
  where the angle is in 'turns', i.e. 180 degrees = 0.5)"
  ([turns]
    (let [a (* tau turns)]
      [[  (m/cos a)  (m/sin a)]
       [(-(m/sin a)) (m/cos a)]])))

(defn assoc-if
  "Same as assoc, but skip the assoc if v is nil"
  [m & kvs]
  (->> kvs
    (partition 2)
    (filter second)
    (map vec)
    (into m)))

; ----------------------------------------------------------------------------------------------
; random rectangle transforms
; ----------------------------------------------------------------------------------------------

(defn rect-tx-recipe [& {:keys [source-filename
                                source-dims
                                target-dims
                                flip-x
                                flip-y
                                lower-scaling-limit
                                max-angle]
                         :or { source-filename nil
                               source-dims nil }}]
  "build a rect-tx-recipe for use with random-rect-tx.
  source-dims are required, but its handy to build a recipe without these
  if you won't know the source resolution/filename until later."
  { :source-dims source-dims  ; [width height] 
    :source-filename source-filename 
    :target-dims target-dims  ; [width height]
    :flip-x flip-x
    :flip-y flip-y
    :lower-scaling-limit lower-scaling-limit    
    :max-angle max-angle })

(defn add-rect-tx-source-info [rect-tx
                               [source-width source-height]              
                               & {:keys [source-filename] :or { :source-filename nil }}]
  "add the source image info - resolution and optional filename."
  (assoc-if rect-tx :source-dims [source-width source-height]              
                    :source-filename source-filename))



(defn random-rect-tx [{:keys [source-filename
                              source-dims 
                              target-dims 
                              flip-x 
                              flip-y 
                              lower-scaling-limit 
                              max-angle]}]
  "random rectangle transformation, aka random-rect-tx.
  Generate a specification for creating a target image from a rectangle within a source image.  
  The rectangle in the source is positioned by a random rotation, translation, scaling, and 
  x/y flip.  
  Parameters:
  source-dims: [source-width source-height] - dimensions of the source image.
  target-dims: [target-width target-height] - dimensions of the target image.
  flip-x : if true, 50% chance of an x-flip.
  flip-y : if true, 50% chance of a y-flip.
  lower-scaling-limit : This is a ratio of source rect size to target rect size. 
                        The randomly generated scale factor will be between this and the largest
                        possible value. 
                        for instance 0.5 means allow 128x128 source rect for 256x256 target.
                        2.0 means only allow larger than 512x512 for source rect for 256x256 target.
      if lower-scaling-limit is too large, for instance 5.0 for a 100x100 target and 400x400 source, 
      then the largest possible scale factor for a given rotation is used.
  max-angle: maximum rotation (in 'turns'!) allowed in constructing source rect.
             for example:  0.0 means no rotation allowed.
                           1.0 means any angle is allowed.
                           0.25 means up to +-90 degrees of rotation."
  (let [[source-width source-height] source-dims
        [target-width target-height] target-dims
        angle (* 2.0 (- (rand) 0.5) max-angle)    ;  in 'turns'
        ; construct corner points of the rotated rectangle.
        rm (rot angle)
        rx (m/mmul rm [target-width 0])
        ry (m/mmul rm [0 target-height])
        xes [0 (first rx) (+ (first rx) (first ry)) (first ry)]
        yes [0 (second rx) (+ (second rx) (second ry)) (second ry)]
        ; min and max of those points.
        minx (reduce min xes) 
        maxx (reduce max xes) 
        miny (reduce min yes) 
        maxy (reduce max yes)
        xw (- maxx minx)
        yh (- maxy miny)
        xes (map #(- % minx) xes)
        yes (map #(- % miny) yes)
        ; largest scale factor possible with our current rotation. 
        upper-scale (min (/ source-width xw) (/ source-height yh))
        ; actual scale factor we'll use.
        scale-factor (if (< lower-scaling-limit upper-scale)
                        (random lower-scaling-limit upper-scale)
                        upper-scale)
        ; scale our test points.
        xes (map (partial * scale-factor) xes)
        yes (map (partial * scale-factor) yes)
        ; calc random translation.
        xrange (- source-width (* xw scale-factor)) 
        yrange (- source-height (* yh scale-factor))
        xt (random 0 xrange)
        yt (random 0 yrange) 
        ; apply random translation to test points.
        xes (map (partial + xt) xes)
        yes (map (partial + yt) yes)
        ; apply xflip if needed.
        xflip (if flip-x (if (> (rand) 0.5) true false) false)
        [xes yes] (if xflip
                    (let [[x0 x1 x2 x3] xes  
                          [y0 y1 y2 y3] yes] 
                      ; swap 0,1 and 2,3
                      [[x1 x0 x3 x2] [y1 y0 y3 y2]])
                    [xes yes])
        ; yflip if needed.
        yflip (if flip-y (if (> (rand) 0.5) true false) false)
        [xes yes] (if yflip
                    (let [[x0 x1 x2 x3] xes 
                          [y0 y1 y2 y3] yes] 
                      ; swap 0,3 and 1,2
                      [[x3 x2 x1 x0] [y3 y2 y1 y0]])
                    [xes yes])
        ]
    (assoc-if { :translate1 [minx miny]       ; pre-scaling translation
                :translate2 [(- xt) (- yt)]   ; post-scaling translation
                :angle angle                  ; rotation in turns
                :scale scale-factor           ; ratio of (source rect size) over (target rect size)
                :source-dims [source-width source-height]
                :target-dims [target-width target-height]
                :xflip xflip                  ; whether to flip the image in x (map c+x to c-x, where c is the image center)
                :yflip yflip                  ; whether to flip the image in y 
                :corners (map list xes yes) } ; corners of the source rectangle, for sanity checking.
          :source-filename source-filename))) ; add in the source-filename if its not nil.

(defn rect-tx->affinetransform ^AffineTransform [{:keys [translate1 translate2 xflip yflip angle scale target-dims]}]
  (let [scale (/ 1.0 scale)
        [width height] target-dims
        [xt1 yt1] translate1 
        [xt2 yt2] translate2 
        [xc yc] [(quot width 2) (quot height 2)]
        tx (AffineTransform.)]
    (if xflip
      (do (.translate tx (* width 0.5) 0)
          (.scale tx -1.0 1.0) 
          (.translate tx (- (* width 0.5)) 0)))
    (if yflip
      (do 
        (.translate tx 0 (* height 0.5))
        (.scale tx 1.0 -1.0) 
        (.translate tx 0 (- (* height 0.5)))
        ))
    (.rotate tx (* angle tau) 0 0)
    (.translate tx xt1 yt1)
    (.scale tx scale scale)
    (.translate tx xt2 yt2)
    tx)) 

(defn xform-point [^AffineTransform tx [x y]]
  "transform an [x y] point with the AffineTransform and return the result."
  (let [from (Point2D$Float. (float x) (float y))
        to (Point2D$Float. )]
    (.transform tx from to)
    [(.getX to) (.getY to)]))

(defn rect-tx-image  
  "given a source image and a 'rect-tx', return a new image according to the 
  rect-tx parameters, with width and height as in :target-dims.
  optional is the affinetransform interpolation type.  Use TYPE_NEAREST_NEIGHBOR 
  for masks, where you don't want interpolated colors."
  (^BufferedImage [^BufferedImage img 
                  {:keys [target-dims] :as tx}
                  ato]   ; AffineTransformOp/TYPE_BICUBIC, TYPE_BILINEAR, or TYPE_NEAREST_NEIGHBOR
    (let [[width height] target-dims
          tx (rect-tx->affinetransform tx)
          out-image (i/new-image width height)
          op (AffineTransformOp. tx AffineTransformOp/TYPE_BICUBIC)]
      (.filter op img out-image)
      out-image))
 (^BufferedImage [^BufferedImage img 
                 tx]
   (rect-tx-image img tx AffineTransformOp/TYPE_BICUBIC)))

(defn draw-point-lines [^BufferedImage img 
                         points
                         color]
  "draw lines through the sequence of points, and from the last point back to the first."
  (let [g2d (.createGraphics img)
        bg (.getBackground g2d)
        p+1 (concat (rest points) [(first points)])]
    (.setColor g2d color)
    (doall
      (map (fn [[x1 y1] [x2 y2]]
             (.drawLine g2d (int x1) (int y1) (int x2) (int y2)))
           points
           p+1))))

; ----------------------------------------------------------------------------------
; some handy functions for verifying visually that the rect-tx is working, or if 
; not, how it isn't working.
; ----------------------------------------------------------------------------------

(defn draw-tx-box [^BufferedImage img 
                        {:keys [corners] :as tx}
                        color]
  (draw-point-lines img corners color))

(defn test-image [filename [target-width target-height]]
  (let [img (i/load-image filename)
        mp (random-rect-tx [(.getWidth img) (.getHeight img)] [target-width target-height] true true 1.0 0.25)
        tx (rect-tx->affinetransform mp)
        out (rect-tx-image img mp)
        orig-pts [[0 0] [0 target-height] [target-width target-height] [target-width 0]]
        inv-tx (.createInverse tx)
        inv-pts (map (partial xform-point inv-tx) orig-pts)
        ]
    (i/write out (str filename ".out.png") "png")
    (draw-tx-box img mp Color/BLACK)
    (draw-point-lines img inv-pts Color/RED)
    (i/write img (str filename ".box.png") "png")
    mp))

(defn test-image-nx [filename [target-width target-height] n] 
  (let [img (i/load-image filename)
        mpis (map (fn [i] [(random-rect-tx [(.getWidth img) (.getHeight img)] [target-width target-height] true true 0.3 0.1)
                          i])
                 (take n (iterate inc 0)))]
    (doall
      (map (fn [[mp i]]
             (i/write (rect-tx-image img mp) (str filename i ".out.png") "png"))
           mpis)) 
    mpis
    ))

; ----------------------------------------------------------------------------------
; random image-and-mask rects.   
; given a directory of images (and masks), generate a series of rect-tx structs, each associated 
; with a specific image.  
; the images may be of varying resolutions, so source resolution is left out of the 
; rect-tx-recipe.  
; ----------------------------------------------------------------------------------

(defn load-image-w-helpful-exception ^BufferedImage [file]
  (try
    (i/load-image file)
    (catch Exception e 
      (println "exception opening file: " file)
      (println "msg: " (.getMessage e)))))

(defn rect-tx-w-mask [^BufferedImage image 
                      ^BufferedImage mask 
                      mask-color->category-index
                      {:keys [target-dims] :as rect-tx }]
  (let [image-patch (rect-tx-image image rect-tx)
        [target-width target-height] target-dims
        ; nearest neighbor interpolation so that we don't get invalid mask colors. 
        mask-patch (rect-tx-image mask rect-tx AffineTransformOp/TYPE_NEAREST_NEIGHBOR)
        [mask-width mask-height] [(.getWidth mask-patch) (.getHeight mask-patch)]
        labels (m/new-matrix :vectorz mask-height mask-width)]
    (doall 
      (for [x (range 0 mask-width) 
            y (range 0 mask-height)]
        (m/mset! labels y x (mask-color->category-index (c/components-rgb (.getRGB mask-patch x y))))))
    {:labels labels 
     :final-img image-patch
     :rect-tx rect-tx }))


(defn make-rect-tx-observation-ftn [image-dir mask-dir mask-color->category-index rect-tx-target-recipe]
  "Return a function that, when called, will produce a randomized image, mask, and rect-tx
  from the directories.  The rect-tx can be used to reproduce the image and mask at a later point.
  image-dir: the directory containing image files.
  mask-dir:  the directory containing mask files with names <imagefilename>-mask.png
  color-categories: a map from colors to category numbers.  every pixel in the mask images should have a color in this map.
  rect-tx-recipe: struct of args for random-rect-tx ftn. "
  (let [image-files (vec (rest (file-seq (clojure.java.io/file image-dir))))  ; skip the first file, which is the directory name.
        image-count (count image-files)]
    (fn []
      (let [^java.io.File image-file (get image-files (int (* (rand) (- image-count 1))))
            ^java.io.File mask-file (clojure.java.io/file mask-dir (str (.getName image-file) "-mask.png"))
            image (load-image-w-helpful-exception image-file)
            mask (load-image-w-helpful-exception mask-file)
            [source-width source-height] [(.getWidth image) (.getHeight image)]
            rect-tx-recipe (add-rect-tx-source-info rect-tx-target-recipe [source-width source-height] :source-filename (.getName image-file))
            rect-tx (random-rect-tx rect-tx-recipe)
            ]
        (rect-tx-w-mask image mask mask-color->category-index rect-tx)))))


(defn replay-rtx-w-mask [image-dir mask-dir mask-color->category-index rect-tx]
  "rebuild a training image and mask, given a rect-tx and the appropriate 
  image directories (and a color->index function)"
  (let [{:keys [source-filename
                source-dims 
                target-dims 
                flip-x 
                flip-y 
                lower-scaling-limit 
                max-angle]} rect-tx
        ^java.io.File image-file (clojure.java.io/file image-dir source-filename)
        ^java.io.File mask-file (clojure.java.io/file mask-dir (str source-filename "-mask.png"))
        image (load-image-w-helpful-exception image-file)
        mask (load-image-w-helpful-exception mask-file)
        image-dims [(.getWidth image) (.getHeight image)]]
    (if (not= source-dims image-dims)
      { :fail "image dimensions don't match!" } 
      (rect-tx-w-mask image mask mask-color->category-index rect-tx))))

                                         


