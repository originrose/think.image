(ns think.image.protocols
  (:require [think.image.pixel :as pixel]))


(def supported-image-types
  [:rgba
   :rgb
   :gray])

(defn default-packed-color
  ^long [] (long (pixel/pack-pixel 0 0 0 255)))


;;If implementations require release type semantics they are expected to use
;;resource system.
(defprotocol PImageImplementation
  "Required base protocol for image implementations. Rects are instances of
java.awt.Rectangle."
  (image-type [img])
  (width [img])
  (height [img])
  ;;Allocate an array that is the most native match for the image and this implementation.
  ;;Note there are conversion functions available on ByteColor to change the
  ;;array to a different type.
  (allocate-array-for-image [img rect])
  ;;Does this array type match the image?  This must be true for either
  ;;->array or array-> to work.
  (array-matches-image? [img rect array-data])
  ;;Place image data into an array allocated with allocate array for image.
  (->array [img rect dest-array])
  ;;Blit an array allocated with allocate array for image into a sub-portion of the image
  (array-> [img rect source-array])
  ;;Resize arbitrarily.  Returns new image preserving original
  (resize [img new-width new-height])
  ;;Create a new image with default packed color.
  (new-image [img new-width new-height image-type]))


(defprotocol PImageAccel
  "Optional protocol if implementations want to accelerate specific operations.
Defaults are provided in terms of the base implementation
so some or none of these functions need to be implemented."
 ;;Convert to a buffered image
 (->buffered-image [img])
 ;;Returns a new image from a buffered image
 (buffered-image-> [img buffered-image])
 ;;create a new sub image that does not reference original
 (sub-image [img rect])
 ;;fill a rect with a solid color
 (fill [img rect int-color])
 ;;Copy to a sub-rect of dest from a sub-rect of source.
 ;;returns dest-img.  source and dest must be same image type.
 (copy-to [source-img source-rect
           dest-img dest-x dest-y])
 ;;convert to a different type.  Does not change original image
 ;;Returns same image if types match, else returns new image.
 (convert [img image-type])
 ;;Release memory associated with image.  If this is a requirement
 ;;the implementations need to use the resource system in general but some
 ;;algorithms will want to immediately release temporaries created within
 ;;themselves.
 (release [img]))
