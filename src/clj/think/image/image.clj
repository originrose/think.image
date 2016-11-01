(ns think.image.image
  (:require [think.image.protocols :as ip]
            [mikera.image.core :as imagez]
            [think.image.pixel :as pixel]
            [think.image.image-util :refer :all]
            [clojure.core.matrix.macros :refer [c-for]])
  (:import [java.awt.image BufferedImage]
           [java.util Arrays]
           [java.awt Rectangle]
           [think.image ByteColor]
           [think.image ImageOperations]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(def ^:dynamic *default-image-type* :rgb)
(def ^:dynamic *default-image-impl* (BufferedImage. 2 2 BufferedImage/TYPE_INT_RGB))

;;Wrappers around the protocol to simplify usage to clients and to
;;provide type hints

(defn image-type
  "Return one of [:rgba :rgb :gray]"
  [img]
  (ip/image-type img))

(defn width
  ^long [img]
  (long (ip/width img)))

(defn height
  ^long [img]
  (long (ip/height img)))

(defn allocate-array-for-image
  ([img ^Rectangle rect]
   (ip/allocate-array-for-image img rect))
  ([img]
   (allocate-array-for-image img (image->rect img))))

(defn array-matches-image?
  ([img ^Rectangle rect array-data]
   (ip/array-matches-image? img rect array-data))
  ([img array-data]
   (array-matches-image? img array-data)))

(defn ->array
  "Copy a rect from the image into an array."
  ([img ^Rectangle rect dest-array]
   (if-not (array-matches-image? img rect dest-array)
     (let [temp-array (->array img rect)]
       (ByteColor/convert temp-array (buffered-image-type img)
                          dest-array (array->buffered-image-type dest-array rect)
                          ByteColor/Black))
     (do (ip/->array img rect dest-array)
         dest-array)))
  ([img ^Rectangle rect]
   (->array img rect (allocate-array-for-image img rect)))
  ([img]
   (->array img (image->rect img))))

(defn array->
  "Copy an array of data into a rect in the image."
  ([img ^Rectangle rect source-array]
   (if-not (array-matches-image? img rect source-array)
     (array-> img rect
              (ByteColor/convert source-array (array->buffered-image-type source-array rect)
                                 (allocate-array-for-image img rect) (buffered-image-type img)
                                 ByteColor/Black))
     (ip/array-> img rect source-array))
   img)
  ([img source-array]
   (array-> img (image->rect img) source-array)))

(defn resize
  "Resize (scale) the image return a new image of the same type."
  ([img ^long new-width ^long new-height]
   (let [new-img (ip/resize img new-width new-height)]
     (if-not (= (image-type new-img)
                (image-type img))
       (ip/convert new-img (image-type img))
       new-img)))
  ([img ^long new-width]
   (let [ratio (/ (double new-width)
                  (width img))
         new-height (Math/round (* ratio (height img)))]
     (resize img new-width new-height))))

(defn convert
  "Convert an image to a different type."
  [img image-type]
  (ip/convert img image-type))

(defn new-image
  "Create a new image potentially using the default image implementation
and the default image type."
  ([img new-width new-height image-type]
   (ip/new-image img new-width new-height image-type))
  ([img new-width new-height]
   (new-image img new-width new-height *default-image-type*))
  ([new-width new-height]
   (new-image *default-image-impl* new-width new-height)))

;;;Optional accelerated image ops implementations with defaults
(defn ->buffered-image
  [img]
  (ip/->buffered-image img))

(defn as-buffered-image
  [img]
  (when (instance? BufferedImage img)
    img))

(defn ensure-buffered-image
  "As cheaply as possible, convert to buffered image."
  [img]
  (or (as-buffered-image img)
      (->buffered-image img)))

(defn buffered-image->
    "Convert a buffered image into a new image.  If no implementation image is provided
then one is created using the default image implementation."
  ([img ^BufferedImage buffered-image]
   (ip/buffered-image-> img buffered-image))
  ([^BufferedImage buffered-image]
   (buffered-image->
    (new-image *default-image-impl* (width buffered-image) (height buffered-image))
    buffered-image)))


(defn sub-image
  "Create a new sub-image from an existing image"
  ([img ^Rectangle rect]
   (ip/sub-image img rect))
  ([img]
   (ip/sub-image img (image->rect img))))


(defn clone
  "Copy the image producing a new image of the same type."
  [source-image]
  (sub-image source-image))

(defn fill
  "Fill a rect of the image with a solid color"
  ([img ^Rectangle rect int-color]
   (ip/fill img rect int-color))
  ([img int-color]
   (fill img (image->rect img) int-color)))

(defn new-image-from-prototype
  "Create a new image using the height, width, and possibly the image type
of the prototype image."
  ([prototype image-type]
   (ip/new-image prototype (width prototype) (height prototype) image-type))
  ([prototype]
   (new-image-from-prototype prototype (ip/image-type prototype))))

;;Copy to a sub-rect of dest from a sub-rect of source.
;;returns dest-img
(defn copy-to
  "Copy a sub-rect of the source image to a sub-rect of the destination image.  This function
does not coerce types so the types of the images must match exactly."
  ([source-img source-rect dest-img dest-x dest-y]
   (let [^Rectangle source-rect source-rect]
    (assert (= (image-type source-img)
               (image-type dest-img)))
    (check-copy-extents source-img source-rect)
    (check-copy-extents dest-img (Rectangle. dest-x dest-y (.width source-rect) (.height source-rect)))
    (ip/copy-to source-img source-rect
                dest-img dest-x dest-y)))
  ([source-img dest-img]
   (assert (and (= (width source-img) (width dest-img))
                (= (height source-img) (height dest-img))))
   (copy-to source-img (image->rect source-img) dest-img 0 0)))


(defn convert
  "Convert a source image to the desired image type.  Always returns a new image even in the
case where a new image is not required."
  [source-img dst-image-type]
  (if (= dst-image-type (image-type source-img))
    (clone source-img)
    (ip/convert source-img dst-image-type)))

(defn release
  "Release a given image.  Optional operations and systems that require this should use the
resource system (thinktopic/resource)."
  [source-img]
  (ip/release source-img)
  nil)


(defn gray-image->bounding-rect
  ^Rectangle [img]
  (let [num-pixels (* (width img) (height img))
        byte-pixels (byte-array num-pixels)]
    (->array img (image->rect img) byte-pixels)
    (ImageOperations/byteMaskToRectangle byte-pixels (width img) (height img))))

(defn bounding-rect->bounding-box
  [^Rectangle rect]
  [(.x rect) (.y rect) (+ (.x rect) (.width rect)) (+ (.y rect) (.height rect))])

(defn bounding-box->bounding-rect
  ^Rectangle [[^Integer x-min ^Integer y-min ^Integer x-max ^Integer y-max]]
  (Rectangle. x-min y-min (- x-max x-min) (- y-max y-min)))


(defn offset-rect
  ^Rectangle [^Rectangle rect x y]
  (Rectangle. (+ (.x rect) x) (+ (.y rect) y)
              (.width rect) (.height rect)))

(defn scale-rect
  ^Rectangle [^Rectangle rect, ^double ratio]
  (Rectangle. (Math/round (* ratio (.x rect)))
              (Math/round (* ratio (.y rect)))
              (Math/round (* ratio (.width rect)))
              (Math/round (* ratio (.height rect)))))

(defn shrink-rect
  ^Rectangle [^Rectangle source]
  (Rectangle. (inc (.x source))
              (inc (.y source))
              (dec (.width source))
              (dec (.height source))))

(defn clip-extent
  ^long [^long coord ^long clip-coord ^long clip-coord-width]
  (min (max coord clip-coord) (+ clip-coord clip-coord-width)))

(defn clip-rect
  "clip item rect such that it fits within clip-rect"
  [^Rectangle item-rect ^Rectangle clip-rect]
  (let [new-x (clip-extent (.x item-rect) (.x clip-rect) (.width clip-rect))
        new-y (clip-extent (.y item-rect) (.y clip-rect) (.height clip-rect))
        new-max-x (clip-extent (+ (.x item-rect) (.width item-rect)) (.x clip-rect) (.width clip-rect))
        new-max-y (clip-extent (+ (.y item-rect) (.height item-rect)) (.y clip-rect) (.height clip-rect))]
    (Rectangle. new-x new-y (- new-max-x new-x) (- new-max-y new-y))))


(defn widen-rect
  ^Rectangle [^Rectangle bbox ^long img-width ^long img-height ^double multiplier]
  (let [box-width (.width bbox)
        box-height (.height bbox)
        ideal-width (* box-width multiplier)
        ideal-height (* box-height multiplier)
        width-add (/ (- ideal-width box-width) 2)
        height-add (/ (- ideal-height box-height) 2)]
    (-> (Rectangle. (- (.x bbox) width-add)
                    (- (.y bbox) height-add)
                    ideal-width
                    ideal-height)
        (clip-rect (Rectangle. 0 0 img-width img-height)))))


(defn same-dimensions?
  [img ^Rectangle rect]
  (and (= (width img) (.width rect))
       (= (height img) (.height rect))))

(defn shrink-if-same-dimensions
  "Shrink the bounding rect if it is the same dimensions as the image."
  ^Rectangle [img ^Rectangle bounding-rect]
  (if (same-dimensions? img bounding-rect)
    (shrink-rect bounding-rect)
    bounding-rect))

(defn pixel-count
  [img]
  (* (width img) (height img)))

(defn pad-or-resize
  "Either pad or resize an image using background color when padding.  When resize by a small enough amount,
it is better quality-wise to pad.  The function also optionally takes a sequence of
bounding boxes that pertain to the image and adjusts them so they are in the same relative location
on the new image.  Returns either the image or a tuple of the image and the resized bounding boxes."
  ([img ^long dest-width ^long background-color bounding-rect-seq]
   (let [resize-ratio (double (/ dest-width (width img)))
         dest-height (int (+ (* (height img) resize-ratio) 0.5))]
     ;;If we are resizing up which is inherently going to cause visual corruption
     ;;then if the amount is small enough we just copy into a larger matrix centered around
     ;;the same place.  Else we do a scale operation.
     (cond
       (> 1.1 resize-ratio 1.0)
       (let [dest-mat (-> (new-image img dest-width dest-height (image-type img))
                          (fill background-color))

             blit-x-offset (Math/floor (+ (/ (- dest-width (width img)) 2.0 ) 0.5))
             blit-y-offset (Math/floor (+ (/ (- dest-height (height img)) 2.0 ) 0.5))

             final-bbox-seq (mapv #(offset-rect % blit-x-offset blit-y-offset) bounding-rect-seq)]
         (copy-to img (image->rect img) dest-mat blit-x-offset blit-y-offset)
         [dest-mat final-bbox-seq])
       (> 1.0 resize-ratio 0.9)
       (let [dest-mat (-> (new-image img dest-width dest-height (image-type img))
                          (fill background-color))
             blit-x-offset (Math/floor (+ (/ (- (width img) dest-width) 2.0 ) 0.5))
             blit-y-offset (Math/floor (+ (/ (- (height img) dest-height) 2.0 ) 0.5))
             source-rect (Rectangle. blit-x-offset
                                     blit-y-offset
                                     dest-width
                                     dest-height)
             final-bbox-seq (mapv #(offset-rect % (- blit-x-offset) (- blit-y-offset)) bounding-rect-seq)]
         (copy-to img source-rect dest-mat 0 0)
         [dest-mat final-bbox-seq])
       :else
       (let [retval (resize img dest-width dest-height)
             final-bbox-seq (mapv #(scale-rect %(double (/ dest-width (width img)))) bounding-rect-seq)]
         [retval final-bbox-seq]))))
  ([img ^long dest-width ^long background-color]
   (first (pad-or-resize img dest-width background-color []))))


(defn get-pixels-outside-bbox
  ;; Produces a sequence of pixels that lie outside the bounding box
  ;; so we can generate stats on color distribution of the background
  [original-img ^Rectangle bounding-rect]
  (let [^ints pixels (->array original-img (image->rect original-img) (int-array (pixel-count original-img)))
        width (width original-img)
        height (height original-img)
        bounding-rect (shrink-if-same-dimensions original-img bounding-rect)
        out-buffer (transient [])
        x1 (.x bounding-rect)
        y1 (.y bounding-rect)
        x2 (+ (.x bounding-rect) (.width bounding-rect))
        y2 (+ (.y bounding-rect) (.height bounding-rect))]
    (loop [x 0]
      (when (< x width)
        (loop [y 0]
          (when (< y height)
            (if-not (and (and (>= x x1) (< x x2)) (and (>= y y1) (< y y2)))
              (conj! out-buffer (aget pixels (+ (* x height) y))))
            (recur (inc y))))
        (recur (inc x))))
    (persistent! out-buffer)))


(defn grayscale
  [img]
  (convert img :gray))

;;Building a patch dictionary

(defn grayscale-pixels
  "Return a byte array of grayscale pixels for the image."
  [img]
  (let [num-pixels (* (width img) (height img))]
    (->array img (image->rect img) (byte-array num-pixels))))

(defn alpha-blend-image
  [img blend-color]
  (if (or (= (image-type img) :gray)
          (= (image-type img) :rgb))
    img
    (convert img :rgb)))

(defn get-height-from-aspect-width
  "aspect defined as width/height"
  [aspect width]
  (/ width aspect))

(defn get-width-from-aspect-height
  "aspect defined as width/height"
  [aspect height]
  (* aspect height))

(defn get-aspect-from-width-height
  [width height]
  (/ width height))

(defn get-larger-target-dims
  "Get the target width,height if you want a new image with the
  destination aspect ratio by enlarging (no cropping)"
  [src-width src-height
   dest-aspect]
  (let [src-aspect (get-aspect-from-width-height src-width src-height)]
    (if (> src-aspect dest-aspect) ;;if src is wider than we need
      [src-width (get-height-from-aspect-width dest-aspect src-width)]
      [(get-width-from-aspect-height dest-aspect src-height) src-height])))


(defn fixed-sized-matrix
  ;;Create an image of exactly these dimensions preserving the
  ;;entire bounding box of content and cropping/scaling where necessary
  ;;Style.com additions - 1) Detection of images with colored
  ;;backgrounds and no cropping for such images. 2) Vertical
  ;;padding gets added to the top of the generated mat so that all images are aligned to the
  ;;bottom edge
  [source-matrix
   ^Rectangle content-bbox
   inner-bbox-seq
   ^Integer background-color
   dest-width
   dest-height
   & {:keys [widen-ratio
             ignore-background]}]
  (let [colored-bg (when-not ignore-background
                     (not= (pixel/mode-pixel (get-pixels-outside-bbox source-matrix content-bbox)) -1))
        ^Rectangle widen-bbox (cond
                                colored-bg
                                (Rectangle. 0 0 (width source-matrix) (height source-matrix))
                                widen-ratio
                                (widen-rect content-bbox (width source-matrix) (height source-matrix)
                                            widen-ratio)
                                :else content-bbox)
        ;;Create an image of the desired aspect ratio keeping the entire box
        ;;without any cropping/scaling.
        bbox-width (.width widen-bbox)
        bbox-height (.height widen-bbox)
        min-x (.x widen-bbox)
        min-y (.y widen-bbox)
        dest-aspect (get-aspect-from-width-height dest-width dest-height)
        [blit-width blit-height] (get-larger-target-dims bbox-width bbox-height dest-aspect)
        blit-x-offset (Math/floor (+ (/ (- blit-width bbox-width) 2) 0.5))
        ;;Moving vertical padding to the top; thereby aligning all images to the bottom edge
        blit-y-offset (Math/floor (- blit-height bbox-height))
        blit-mat (-> (new-image source-matrix blit-height blit-width (image-type source-matrix))
                     (fill background-color))
        source-rect (Rectangle. min-x
                                min-y
                                bbox-width
                                bbox-height)
        _ (comment (println source-rect blit-x-offset blit-y-offset))
        _ (copy-to source-matrix source-rect blit-mat blit-x-offset blit-y-offset)
        blit-bbox (offset-rect content-bbox (- blit-x-offset min-x) (- blit-y-offset min-y))
        offset-bbox-seq (mapv #(offset-rect % (- blit-x-offset min-x) (- blit-y-offset min-y))
                              (concat [content-bbox] inner-bbox-seq))
        [retval all-bbox-seq] (pad-or-resize blit-mat dest-width background-color offset-bbox-seq)
        ;;*very* useful debugging
        ;;_ (image/show (draw-bounding-box (opencv-to-image retval)
        ;;                                 final-bbox (pack-pixel 0 0 0 255)))
        ]
    (release blit-mat)
    [retval (first all-bbox-seq) (rest all-bbox-seq)]))


(defn draw-vertical-line
  ^ints [^ints pixels width height x-pos start-y end-y color]
  (let [line-len (long (- end-y start-y))
        color (int color)]
    (c-for [idx 0 (< idx line-len) (inc idx)]
           (aset pixels (+ (* (+ idx start-y) width) x-pos) color)))
  pixels)

(defn draw-horizontal-line
  ^ints [^ints pixels width height y-pos start-x end-x color]
  (let [line-len (long (- end-x start-x))
        color (int color)]
    (c-for [idx 0 (< idx line-len) (inc idx)]
           (aset pixels (+ (* y-pos width) (+ idx start-x)) color)))
  pixels)

(defn draw-rect
  "Given an input image, draw a bounding box in the color
defined by a packed int color.  Return a new image"
  [img ^Rectangle bbox ^Integer int-color]
  (let [width (.width bbox)
        height (.height bbox)
        retval (clone img)
        rendered-pixels (-> (->array retval bbox (int-array (* (.width bbox) (.height bbox))))
                            (draw-vertical-line width height 0 0 (- height 1) int-color)
                            (draw-vertical-line width height (- width 1) 0 (- height 1) int-color)
                            (draw-horizontal-line width height 0 0 (- width 1) int-color)
                            (draw-horizontal-line width height (- height 1) 0 (- width 1) int-color))]
    (array-> retval bbox rendered-pixels)))

(defn draw-rects
  [img rect-seq ^Integer int-color]
  (reduce (fn [img ^Rectangle rect]
            (draw-rect img rect int-color))
          img
          rect-seq))


(defn set-alpha-mask
  "Given a source image set it's alpha mask to be these mask bytes.  Produces a new rgba image."
  [source-img ^bytes alpha-bytes]
  (let [num-pixels (pixel-count source-img)
        ^bytes img-bytes (->array source-img (image->rect source-img) (byte-array (* 4 num-pixels)))]
    (c-for [idx 0 (< idx num-pixels) (inc idx)]
           (aset img-bytes (+ (* 4 idx) 3) (aget alpha-bytes idx)))
    (array-> (new-image-from-prototype source-img :rgba) img-bytes)))


(defn bad-bounding-rect?
  [^Rectangle bbox ^long width ^long height]
  (let [bbox-width (.width bbox)
        bbox-height (.height bbox)]
    (or (< bbox-width (* width 0.10))
        (< bbox-height (* height 0.10)))))

(defn ensure-valid-bounding-rect
  "Ensure this rect contains enough of an image to mean something
and make sure it is within the bounds of the image."
  ^Rectangle [^Rectangle bbox ^long width ^long height]
  (if (bad-bounding-rect? bbox width height)
    (Rectangle. 0 0 width height)
    (clip-rect bbox (Rectangle. 0 0 width height))))


(defn image->mask-image
  "Given an image, take pixels that have any non-black color at all
and make them a mask."
  [img]
  (let [image-mask (new-image-from-prototype img :gray)
        ^bytes gray-data (->array img (image->rect img)
                                  (byte-array (* (width img)
                                                 (height img))))]
    (c-for [idx 0 (< idx (alength gray-data)) (inc idx)]
           (aset gray-data idx
                 (unchecked-byte (if (not= 0 (aget gray-data idx))
                                   255
                                   0))))
    (array-> image-mask gray-data)
    image-mask))
