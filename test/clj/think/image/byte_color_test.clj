(ns think.image.byte-color-test
  (:require [clojure.test :refer :all]
            [clojure.core.matrix.macros :refer [c-for]])
  (:import [think.image ByteColor]
           [java.awt.image BufferedImage]))




(deftest color-to-int
  (let [test-color (ByteColor. 255 0 64 255)
        test-int (ByteColor/toInt test-color)]
    (is (.equals test-color (ByteColor/toColor test-int)))))


(deftest alpha-blend
  (let [test-color (ByteColor. 255 128 64 128)
        blend-color (ByteColor. 0)
        result (ByteColor. 128 64 32 255)]
    (is (.equals result (ByteColor/alphaBlend test-color blend-color)))))


(deftest gray-scale
  (let [test-color (ByteColor. 255 128 64)
        gray-color (ByteColor/grayScale test-color)]
    (is (= gray-color (unchecked-byte
                       (+ (* 255.0 0.2989)
                          (* 128.0 0.5870)
                          (* 64.0 0.1140)
                          0.5))))))


(deftest convert-gray-scale
  (let [test-color (ByteColor. 255 128 64)
        color-array (into-array ByteColor (repeat 16 test-color))
        gray-color (ByteColor. (unchecked-byte
                                (+ (* 255.0 0.2989)
                                   (* 128.0 0.5870)
                                   (* 64.0 0.1140)
                                   0.5)))
        answer (into-array ByteColor (repeat 16 gray-color))
        byte-result
        (ByteColor/convertAllocate color-array BufferedImage/TYPE_INT_ARGB Byte/TYPE BufferedImage/TYPE_BYTE_GRAY ByteColor/Black)
        int-result
        (ByteColor/convertAllocate color-array BufferedImage/TYPE_INT_ARGB Integer/TYPE BufferedImage/TYPE_BYTE_GRAY ByteColor/Black)
        byte-color-result
        (ByteColor/convertAllocate color-array BufferedImage/TYPE_INT_ARGB ByteColor BufferedImage/TYPE_BYTE_GRAY ByteColor/Black)
        int-result-color
        (ByteColor/convertAllocate int-result BufferedImage/TYPE_BYTE_GRAY ByteColor BufferedImage/TYPE_BYTE_GRAY ByteColor/Black)
        byte-result-color
        (ByteColor/convertAllocate byte-result BufferedImage/TYPE_BYTE_GRAY ByteColor BufferedImage/TYPE_BYTE_GRAY ByteColor/Black)]

    (is (= (vec answer) (vec byte-color-result)))
    (is (= (vec answer) (vec int-result-color)))
    (is (= (vec answer) (vec byte-result-color)))))



(deftest convert-alpha-blend
  (let [test-color (ByteColor. 255 128 64 128)
        blend-color (ByteColor. 0)
        result (ByteColor. 128 64 32 255)
        color-array (into-array ByteColor (repeat 16 test-color))
        answer (into-array ByteColor (repeat 16 result))
        byte-result
        (ByteColor/convertAllocate color-array BufferedImage/TYPE_INT_ARGB Byte/TYPE BufferedImage/TYPE_INT_RGB ByteColor/Black)
        int-result
        (ByteColor/convertAllocate color-array BufferedImage/TYPE_INT_ARGB Integer/TYPE BufferedImage/TYPE_INT_RGB ByteColor/Black)
        byte-color-result
        (ByteColor/convertAllocate color-array BufferedImage/TYPE_INT_ARGB ByteColor BufferedImage/TYPE_INT_RGB ByteColor/Black)
        int-result-color
        (ByteColor/convertAllocate int-result BufferedImage/TYPE_INT_RGB ByteColor BufferedImage/TYPE_INT_RGB ByteColor/Black)
        byte-result-color
        (ByteColor/convertAllocate byte-result BufferedImage/TYPE_INT_RGB ByteColor BufferedImage/TYPE_INT_RGB ByteColor/Black)]
    (is (= (vec answer) (vec byte-color-result)))
    (is (= (vec answer) (vec int-result-color)))
    (is (= (vec answer) (vec byte-result-color)))))
