(ns think.image.buffered-image
  (:require [mikera.image.core :as imagez]
            [think.image.protocols :as ip]
            [think.image.image :as think-image]
            [think.image.image-util :as util]
            [clojure.core.matrix.macros :refer [c-for]])
  (:import [java.awt.image BufferedImage]
           [java.util Arrays]
           [java.awt Rectangle]
           [think.image ByteColor]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)


(defn- get-array-rect
  "Returns a array data that matches the type of the image."
  ([^BufferedImage image ^Rectangle rect ary-data]
   (.getDataElements (.getRaster image) (.x rect) (.y rect) (.width rect) (.height rect) ary-data)
   (when (= (think-image/image-type image) :rgb)
     ;;Set all the alpha to 255 because an integer contains rgba..
     (let [^ints ary-data ary-data
           len (alength ary-data)]
       (c-for [idx 0 (< idx len) (inc idx)]
              (let [existing (aget ary-data idx)]
                (aset ary-data idx (bit-or existing (bit-shift-left 0xFF 24)))))))
   ary-data)
  ([^BufferedImage image ^Rectangle rect]
   (get-array-rect image rect (think-image/allocate-array-for-image image rect))))

(defn- set-array-rect
  "The type of ary-data has to match exactly the type of the image.
[:rgb int-array]
[:rgba int-array]
[:gray byte-array]"
  ([^BufferedImage image ^Rectangle rect ary-data]
   (.setDataElements (.getRaster image) (.x rect) (.y rect) (.width rect) (.height rect) ary-data)
   image))


(extend-type BufferedImage
  ip/PImageImplementation
  (image-type [img] (util/buffered-image-type->image-type (.getType img)))
  (width [img] (.getWidth img))
  (height [img] (.getHeight img))
  (allocate-array-for-image [img ^Rectangle rect]
    (let [num-pixels (* (.width rect) (.height rect))]
      (if (= (.getType img) BufferedImage/TYPE_BYTE_GRAY)
        (byte-array num-pixels)
        (int-array num-pixels))))
  (array-matches-image? [img ^Rectangle rect array-data]
    (if (= (.getType img) BufferedImage/TYPE_BYTE_GRAY)
      (and (instance? (Class/forName "[B") array-data)
           (= :gray (util/byte-array->image-type array-data rect)))
      (instance? (Class/forName "[I") array-data)))
  ;;We expect the layers above this to make sure dest-array matches exactly
  ;;the type of the image.
  (->array [img ^Rectangle rect dest-array]
    (get-array-rect img rect dest-array))
  ;;We expect the layers above this layer to make sure source array matches exactly
  ;;the type of the image.
  (array-> [img ^Rectangle rect source-array]
    (set-array-rect img rect source-array))
  ;;Resize arbitrarily.  Returns new image preserving original
  (resize [img new-width new-height]
    ;;This may change image type but the image layer will take care of the conversion if that happens.
    (imagez/resize img new-width new-height))
  ;;Create a new image with this packed color.  Packed colors default to black with alpha of 255
  (new-image [img new-width new-height image-type]
    (BufferedImage. new-width new-height (util/image-type->buffered-image-type image-type))))
