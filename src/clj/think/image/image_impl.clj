(ns think.image.image-impl
  (:require [think.image.protocols :as ip]
            [mikera.image.core :as imagez]
            [think.image.pixel :as pixel]
            [think.image.image-util :refer :all]
            [think.image.image :as ti])
  (:import [java.awt.image BufferedImage]
           [java.util Arrays]
           [java.awt Rectangle]
           [think.image ByteColor]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)


(defn ->buffered-image-default
  [img]
  (let [retval (BufferedImage. (ip/width img) (ip/height img) (buffered-image-type img))
        temp-data (ip/allocate-array-for-image retval (image->rect retval))
        ary-data (ti/->array img (image->rect img) temp-data)]
    (ti/array-> retval ary-data)
    retval))

(defn buffered-image->-default
  [img ^BufferedImage buffered-image]
  (let [pix-data (ti/->array buffered-image)]
    (ti/array-> (ti/new-image img (.getWidth buffered-image) (.getHeight buffered-image)
                              (buffered-image-type->image-type (.getType buffered-image)))
                pix-data)))

(defn sub-image-default
  [img ^Rectangle rect]
  (let [retval (ti/new-image img (.width rect) (.height rect) (ti/image-type img))
        img-data (ti/allocate-array-for-image img rect)]
    (ti/->array img rect img-data)
    (ti/array-> retval (image->rect retval) img-data)))

(defn- allocate-array-for-image-type
  [^Rectangle rect image-type]
  (let [pix-count (* (.width rect) (.height rect))]
    (if (= image-type :gray)
      (byte-array pix-count)
      (int-array pix-count))))

(defn fill-default
  [img ^Rectangle rect int-color]
  (let [[^int r ^int b ^int g ^int a] (pixel/unpack-pixel int-color)
        img-ary (allocate-array-for-image-type rect (ti/image-type img))]
    (ti/->array img rect img-ary)
    (if (= (ti/image-type img) :gray)
      (Arrays/fill ^bytes img-ary (unchecked-byte r))
      (Arrays/fill ^ints img-ary (int int-color)))
    (ti/array-> img rect img-ary)))

(defn copy-to-default
  [source-img source-rect
   dest-img dest-x dest-y]
  (let [transfer-ary (ti/allocate-array-for-image source-img source-rect)
        ^Rectangle source-rect source-rect]
    (ti/->array source-img source-rect transfer-ary)
    (ti/array-> dest-img (Rectangle. dest-x dest-y (.width source-rect) (.height source-rect)) transfer-ary)))

(defn convert-default
  [img dst-image-type]
  (let [array-source (ti/->array img)
        retval (ti/new-image-from-prototype img dst-image-type)
        array-dest (ti/allocate-array-for-image retval)
        _ (ByteColor/convert array-source (image-type->buffered-image-type (ti/image-type img))
                             array-dest (image-type->buffered-image-type dst-image-type)
                             ByteColor/Black)]
    (ti/array-> retval array-dest)))


(extend-type Object
  ip/PImageAccel
  ;;Create a buffered image from an image
  (->buffered-image
    [img]
    (->buffered-image-default img))
  ;;Returns a new image from a buffered image
  (buffered-image->
    [img buffered-image]
    (buffered-image->-default img buffered-image))
  ;;create a new sub image that does not reference original
  (sub-image
    [img ^Rectangle rect]
    (sub-image-default img rect))
  ;;fill a rect with a solid color
  (fill
    [img ^Rectangle rect int-color]
    (fill-default img rect int-color))
  ;;Copy to a sub-rect of dest from a sub-rect of source.
  ;;returns dest-img.  source and dest must be same type
  (copy-to
    [source-img source-rect dest-img dest-x dest-y]
    (copy-to-default source-img source-rect dest-img dest-x dest-y))
  ;;Convert or clone image.  Always return a new image.
  (convert [img dst-image-type]
    (convert-default img dst-image-type))
  ;;nop
  (release [img]))
