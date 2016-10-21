(ns think.image.image-backend-test
  (:require [mikera.image.core :as imagez]
            [think.image.core]
            [think.image.image :as ti]
            [clojure.java.io :as io]
            [clojure.test :refer :all]
            [clojure.core.matrix :as mat]
            [think.image.pixel :as pixel]
            [think.image.backend-util :refer :all])
  (:import [java.awt Rectangle]
           [java.awt.image BufferedImage]
           [think.image ByteColor]))



(deftest buffered-image-backend
  (let [buf-img (BufferedImage. 2 2 BufferedImage/TYPE_BYTE_GRAY)]
    (backend-gray-test buf-img)
    (backend-resize-test buf-img)
    (backend-fixed-size-matrix-test buf-img)))
