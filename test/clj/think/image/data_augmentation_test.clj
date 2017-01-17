(ns think.image.data-augmentation-test
  (:require [think.image.data-augmentation :refer [rect-mog->affinetransform 
                                                   random-rect-mog 
                                                   xform-point
                                                   random]]
            [clojure.test :refer :all]
            ))

(defn- corner-points [w h]
  [[0 0] [w 0] [w h] [0 h]])

(defn- dbleq [a b]
  (< (Math/abs (- a b)) 0.01))

(defn test-mog-pts [m]
  (let [tx (rect-mog->affinetransform m)
        itx (.createInverse tx)
        tpoints (apply corner-points (:target-dims m))
        rcorners (map (partial xform-point tx) (:corners m))
        invpoints (map (partial xform-point itx) tpoints)]
    ; check xforming the target image dims to the corner dims.
    ; also the corner points back to the target dims.  
    (and (every? identity (map dbleq (apply concat tpoints) (apply concat rcorners)))
         (every? identity (map dbleq (apply concat (:corners m)) (apply concat invpoints))))))

(defn test-many-mogs [n]
  (doall (map test-mog-pts 
              (map (fn [_] (random-rect-mog [(random 10 4000) (random 10 4000)]
                                            [(random 10 4000) (random 10 4000)]
                                            true
                                            true
                                            (random 0.1 100.0)
                                            1.0))
                   (repeat n 0)))))

(deftest rect-mog-sanity-check []
  (is (test-many-mogs 100)))


