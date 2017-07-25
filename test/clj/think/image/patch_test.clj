(ns think.image.patch-test
  (:require [clojure.test :refer :all]
            [mikera.image.core :as i]
            [mikera.image.colours :as colour]
            [think.image.patch :as p]
            [think.image.util :refer [hue-wheel-image square-image circular-image rand-image]]
            [think.image.core]
            [think.image.image :as image]
            [think.image.pixel :as pixel]
            [think.image.image-util :as iu]
            [clojure.core.matrix.macros :refer [c-for]]
            [clojure.core.matrix :as m]
            [clojure.core.matrix.stats :as m-stats])
  (:import [java.awt.image BufferedImage]
           [java.awt Rectangle]))

(def patch-dim 64)

(deftest test-patches
  (testing "Exercise the patch generation code."
    (let [test-img (hue-wheel-image)
          mask-img (image/new-image image/*default-image-impl*
                    (image/width test-img) (image/height test-img) :gray)
          _ (image/array-> mask-img (byte-array (repeat
                                                 (* (image/width test-img)
                                                    (image/height test-img))
                                                 (byte -1))))
          patch-count 5
          patches (p/masked-image->patches test-img mask-img patch-count patch-dim
                                           (iu/image->rect test-img) :double)
          pi (map #(p/patch->image % patch-dim) patches)]
      (is (= patch-count (count patches)))
      (doseq [patch-image pi]
        (is (= (i/width patch-image) (int patch-dim)))
        (is (= (i/height patch-image) (int patch-dim)))))))

(deftest test-content-rect
  (testing "Ensure that the content rect code generates the correct bounding box."
    (let [bb (-> (circular-image) p/image->content-rect)]
      (is (= 28 (.x bb)))
      (is (= 28 (.y bb)))
      (is (= 200 (.width bb)))
      (is (= 200 (.height bb))))))

(deftest test-patches-within-content
  (testing "Exercise the patch generation code to ensure that patches exist within the content rect."
    (with-bindings {#'p/*content-threshold-cutoff* 1.0}
     (let [test-img (circular-image)
           patch-count 5
           patch-dim 32
           ;;Create an image mask that is exactly the red area of the circular image
           patches (p/masked-image->patches test-img (image/image->mask-image test-img)
                                            patch-count patch-dim (iu/image->rect test-img)
                                            :double)
           images (mapv #(p/patch->image % patch-dim) patches)]
       ;; TODO -- ensure that generated patches match the expected patch
       (doseq [img images]
         ;;Default serialization type of an image is
         (let [^ints int-data (image/->array img)
               num-pixels (alength int-data)
               r-sum (reduce (fn [^double r-sum int-pix]
                               (pixel/with-unpacked-pixel int-pix
                                 (+ r-sum r)))
                             0.0
                             (seq int-data))]
           (is (= 255 (long (Math/round (double (/ r-sum num-pixels))))))))))))



(deftest patches-colorspace
  (let [test-img (circular-image)
        obs-1 (p/image->patch test-img :colorspace :gray)
        obs-2 (p/image->patch test-img :colorspace :rgb)
        img-width (image/width test-img)]
    (is (= 1 (first (m/shape obs-1))))
    (is (= 3 (first (m/shape obs-2))))
    (let [img-1 (p/patch->image obs-1 img-width)
          img-2 (p/patch->image obs-2 img-width)]
      (is (= (image/height img-1)
             (image/height img-2)))
      ;;If nothing got thrown we are probably good.
      )))


(deftest patch-mean-subtract
  (let [test-img (circular-image)
        obs-2 (p/image->patch test-img :colorspace :rgb :normalize false)
        means (mapv m-stats/mean obs-2)
        obs-3 (apply p/patch-mean-subtract obs-2 means)
        sub-means (mapv m-stats/mean obs-3)]
    (is (not= 0.0 (double (first means))))
    (is (m/equals [0 0 0] sub-means))))
