(ns think.image.backend-util
  (:require [mikera.image.core :as imagez]
            [think.image.image :as ti]
            [clojure.java.io :as io]
            [clojure.test :refer :all]
            [think.image.buffered-image]
            [clojure.core.matrix :as mat]
            [think.image.image-util :as util])
  (:import [java.awt Rectangle]
           [java.awt.image BufferedImage]
           [think.image ByteColor]))


(defn backend-gray-test
  [img]
  (let [original-image (imagez/load-image (io/file "test/data/cityskyline.jpg"))
        test-img (ti/buffered-image-> img original-image)
        gray-rect (Rectangle. 400 250 400 400)
        gray-rect-data (ti/->array test-img gray-rect (byte-array (* 400 400)))
        _ (ti/array-> test-img gray-rect gray-rect-data)
        test-array (ti/->array test-img gray-rect (int-array (* 400 400)))
        test-bytes (ByteColor/convertAllocate test-array (util/image-type->buffered-image-type (ti/image-type test-img))
                                              Byte/TYPE BufferedImage/TYPE_BYTE_GRAY
                                              ByteColor/Black)]

    (comment (imagez/show (ti/->buffered-image test-img)))

    ;;Magic number found through testing.
    (is (= (mat/esum test-bytes)
           -10020134))
    (is (= (mat/esum test-array)
           -718491695398))
    (is (= (mat/esum gray-rect-data)
           (mat/esum test-bytes)))))


(defn backend-resize-test
  [img]
  (let [original-image (imagez/load-image (io/file "test/data/cityskyline.jpg"))
        test-img (ti/buffered-image-> img original-image)
        smaller-image (ti/resize test-img (long (* (ti/width test-img) 0.5)))
        image-rect (util/image->rect smaller-image)]
    (comment (imagez/show (ti/->buffered-image smaller-image)))))


(defn backend-fixed-size-matrix-test
  [img]
  (let [original-image (imagez/load-image (io/file "test/data/cityskyline.jpg"))
        test-img (ti/buffered-image-> img original-image)
        original-rect-seq [(Rectangle. 400 250 400 400) (Rectangle. 450 300 100 100)]
        [result-img content-bbox inner-bbox-seq] (ti/fixed-sized-matrix test-img (first original-rect-seq)
                                                                        [(second original-rect-seq)] -1 512 512
                                                                        :ignore-background true
                                                                        :widen-ratio 1.3)
        rect-seq (concat [content-bbox] inner-bbox-seq)
        result-gray-bytes (ti/->array result-img (util/image->rect result-img) (byte-array (ti/pixel-count result-img)))]
    ;;The algorithm does padding instead of resize here because the final result size is just a bit larger and it isn't worth scaling.
    ;;This means however that we should get pixel-exact results from each backend (because no scaling).
    (is (= content-bbox
           (Rectangle. 56 56 400 400)))
    (is (= (first inner-bbox-seq)
           (Rectangle. 106 106 100 100)))
    (is (= (mat/esum result-gray-bytes) -14512963))))


(defn alpha-mask-test
  [img]
  (let [original-image (imagez/load-image (io/file "test/data/cityskyline.jpg"))
        test-img (ti/buffered-image-> img original-image)
        alpha-gray (ti/new-image-from-prototype test-img :gray)
        alpha-rect (Rectangle. 400 250 400 400)
        _ (ti/fill alpha-gray alpha-rect -1)
        alpha-img (ti/set-alpha-mask test-img (ti/->array alpha-gray (util/image->rect alpha-gray)
                                                          (byte-array (ti/pixel-count alpha-gray))))]
    (comment (imagez/show (ti/->buffered-image alpha-img)))))
