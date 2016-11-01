(ns think.image.image-util
  (:require [think.image.protocols :as ip]
            [mikera.image.core :as imagez]
            [think.image.pixel :as pixel])
  (:import [java.awt.image BufferedImage]
           [java.util Arrays]
           [java.awt Rectangle]
           [think.image ByteColor]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)


(def buffered-image-types
  [[:rgba BufferedImage/TYPE_INT_ARGB]
   [:rgb BufferedImage/TYPE_INT_RGB]
   [:gray BufferedImage/TYPE_BYTE_GRAY]])

(defn buffered-image-type->image-type
  [buf-img-type]
  (let [retval
        (ffirst (filter (comp #(= buf-img-type %) second) buffered-image-types))]
    (when-not retval
      (throw (Exception. (format "Unrecognized buffered image type: %s" buf-img-type))))
    retval))

(defn image-type->buffered-image-type
  [img-type]
  (let [retval
        (second (first (filter (comp #(= img-type %) first) buffered-image-types)))]
    (when-not retval
      (throw (Exception. (format "Unrecognized image type: %s" img-type))))
    retval))

(defn buffered-image-type
  "Get the buffered image type of a given image"
  [img]
  (image-type->buffered-image-type (ip/image-type img)))

(defn byte-array->image-type
  ([^bytes array-data ^long img-width ^long img-height]
   (let [num-pixels (* img-width img-height)
         num-elts (alength array-data)
         num-channels (quot num-elts num-pixels)]
     (case num-channels
       1 :gray
       3 :rgb
       4 :rgba
       (throw (Exception. "Failed to match num channels to image type")))))
  ([^bytes array-data ^Rectangle rect]
   (byte-array->image-type array-data (.width rect) (.height rect))))

(defn array->image-type
  ([array-data ^long img-width ^long img-height]
   (if (instance? (Class/forName "[B") array-data)
     (byte-array->image-type array-data img-width img-height)
     :rgba))
  ([array-data ^Rectangle rect]
   (array->image-type array-data (.width rect) (.height rect))))

(defn array->buffered-image-type
  [array-data ^Rectangle rect]
  (image-type->buffered-image-type
   (array->image-type array-data (.width rect) (.height rect))))

(defn ensure-array-matches
  "Ensure the array type matches the image.  If not perform conversion."
  [img ^Rectangle rect array-data]
  (if-not (ip/array-matches-image? img rect array-data)
    (let [new-dest (ip/allocate-array-for-image img rect)]
      (ByteColor/convert array-data (array->buffered-image-type array-data rect)
                         new-dest (buffered-image-type img)
                         ByteColor/Black))
    array-data))

(defn image->rect
  ^Rectangle [img]
  (Rectangle. 0 0 (ip/width img) (ip/height img)))


(defn check-extents
  ([value min-val max-val]
   (assert (and (<= value max-val)
                (>= value min-val))))
  ([value max-val] (check-extents value 0 max-val)))

(defn check-copy-extents
  [img ^Rectangle rect]
  (let [min-x (.x rect)
        min-y (.y rect)
        rect-width (.width rect)
        rect-height (.height rect)]
   (let [img-width (ip/width img)
         img-height (ip/height img)]
     (check-extents min-x img-width)
     (check-extents min-y img-height)
     (check-extents (+ min-x rect-width) img-width)
     (check-extents (+ min-y rect-height) img-height))))
