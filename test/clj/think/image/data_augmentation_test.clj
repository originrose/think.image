(ns think.image.data-augmentation-test
  (:require [think.image.data-augmentation :refer [rect-mog->affinetransform 
                                                   random-rect-mog 
                                                   xform-point
                                                   random]]
            [clojure.pprint :as pp]
            [clojure.test :refer :all]
            ))

(defn- corner-points [w h]
  [[0 0] [w 0] [w h] [0 h]])

(defn- dbleq [a b]
  (< (Math/abs (- a b)) 0.01))

(defn- within-rect? [x1 y1 x2 y2 x y]
  (let [x (Math/round x)
        y (Math/round y)
        res (and (<= x1 x)
                 (<= x x2)
                 (<= y1 y)
                 (<= y y2))]
    (if res
      res
      (do 
        (println "within-rect fail: " [x1 y1 x2 y2 x y])
        res))))

(defn test-mog-pts [m]
  (let [tx (rect-mog->affinetransform m)
        itx (.createInverse tx)
        [sw sh] (:source-dims m)
        tpoints (apply corner-points (:target-dims m))
        rcorners (map (partial xform-point tx) (:corners m))
        invpoints (map (partial xform-point itx) tpoints)]
    (and ; all source rect points should be within the source image.
         ; (every? identity (map (fn [[x y]] (within-rect? 0 0 sw sh x y)) invpoints))
         (every? identity (map (fn [[x y]] (within-rect? 0 0 sw sh x y)) (:corners m)))
         ; check xforming the target image dims to the corner dims.
         (every? identity (map dbleq (apply concat tpoints) (apply concat rcorners)))
         ; also the corner points back to the target dims. 
         (every? identity (map dbleq (apply concat (:corners m)) (apply concat invpoints)))
         )))

(defn test-many-mogs [n]
  (let [mogs (map (fn [_] (random-rect-mog [(Math/floor (random 10 4000)) (Math/floor (random 10 4000))]
                                            [(Math/floor (random 10 4000)) (Math/floor (random 10 4000))]
                                            true
                                            true
                                            (random 0.1 100.0)
                                            1.0))
                   (repeat n 0))]
    (doall (map test-mog-pts mogs))))

(deftest rect-mog-sanity-check []
  (is (every? identity (test-many-mogs 100))))


