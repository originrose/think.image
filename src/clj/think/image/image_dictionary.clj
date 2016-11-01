(ns think.image.image-dictionary
  (:require [clojure.core.matrix :as mat]
            [think.image.image :as ti]
            [thinktopic.bluejay.cluster :refer (k-means)])
  (:import [java.awt.image BufferedImage]))


(defn stats
  [ary]
  (let [n (apply + (mat/shape ary))
        mean (/ (mat/esum ary) (double n))
        variance (/ (reduce (fn [sum v]
                              (+ sum (Math/pow (float (- v mean)) 2)))
                            0
                            (mat/eseq ary))
                    n)
        stdev (Math/sqrt variance)]
    [mean variance stdev]))

(defn normalize-patch
  [patch]
  (let [[mean variance stdev] (stats patch)]
    (mat/div (mat/sub patch mean) stdev)))


(defn img->normalized-grayscale-array
  [^BufferedImage img]
  (let [width (.getWidth img)
        height (.getHeight img)
        n (* width height)
        pixels (mat/array (ti/grayscale-pixels img))
        pixels (normalize-patch pixels)
        pixels (mat/reshape pixels [height width])]
    pixels))


(defn sample-patches
  "Sample n patches of size w x h at random locations."
  [pixels n w h]
  (let [width (mat/column-count pixels)
        height (mat/row-count pixels)]
    (mat/array (map mat/eseq
                    (repeatedly n #(mat/submatrix pixels [[(rand-int (- height h)) h]
                                                          [(rand-int (- width w)) w]]))))))

(defn feature-patches
  "Return a matrix of w*h rows and n columns of wxh patches from within the image"
  [img n w h]
  (sample-patches (img->normalized-grayscale-array img) n w h))


(defn sampled-patch-seq
  "Returns a seq of n normalized, grayscale patches per image of size w x h."
  [image-seq n w h]
  (pmap #(feature-patches % n w h) image-seq))


(defn build-signature-dictionary
  "Run k-means using the rows of the matrix as input vectors
  used on result of sampled-patch-seq"
  ([patches k iters]
   (k-means patches :k k :iters iters))
  ([patches k]
   (build-signature-dictionary patches k 10)))


;;Using patch dictionary to label an image

(defn image-signature
  "Given an image and a signature dictionary, return a texture
  signature which is a set of dot products of random patches of the
  image with patches in the feature dictionary.  Each image patch is
  dotted with every dictionary feature vector and the absolute values
  are summed.  This final sequence is then normalized"
  [^BufferedImage img n-patches patch-width patch-height feature-dictionary]
  (let [patches (feature-patches img n-patches patch-width patch-height)
        responses (for [patch patches
                        feature feature-dictionary]
                    (Math/abs (double (mat/dot patch feature))))
        sums (mat/array (apply map + (partition (mat/row-count feature-dictionary) responses)))]
    (mat/normalise sums)))
