(ns think.image.color-test
  (:require [think.image.color :refer [ciedist94 hsv->rgb rgb->hsv]] 
            [clojure.test :refer :all]
            ))


(def cie94testdata '((53.0 0.65 0.15 33.0 -0.45 -0.1 20.03112)
    (42.0 -0.3 0.1 74.0 -0.2 -0.15 32.001118)
    (12.0 -1.0 -0.45 32.0 0.3 0.9 20.084782)
    (94.0 -0.1 -0.55 77.0 0.5 0.45 17.03928)
    (75.0 -0.8 0.35 46.0 -0.6 -0.85 29.02483)
    (83.0 -0.65 -0.7 67.0 0.75 0.0 16.074173)
    (70.0 -0.7 0.9 54.0 0.35 -0.95 16.13608)
    (81.0 0.45 -0.8 53.0 -0.35 0.05 28.023375)
    (40.0 -0.2 -0.65 25.0 -1.0 0.8 15.088856)
    (66.0 0.85 -0.7 93.0 0.55 0.15 27.014244)
    (44.0 -0.5 0.5 23.0 -0.9 0.5 21.00363)
    (67.0 0.4 0.25 42.0 -0.25 0.6 25.010727)
    (32.0 0.6 0.55 86.0 0.0 0.25 54.003925)
    (96.0 -0.15 -0.9 87.0 0.25 -0.3 9.027307)
    (100.0 -0.6 0.3 61.0 -0.25 -0.75 39.015385)
    (2.0 -0.2 -0.65 73.0 -0.3 0.65 71.01173)
    (74.0 0.1 -0.65 96.0 -0.5 0.8 22.05474)
    (22.0 -0.3 -0.85 64.0 -0.65 -0.95 42.0015)
    (73.0 -0.35 0.3 38.0 0.25 -1.0 35.02875)
    (91.0 0.6 0.45 82.0 -0.25 0.2 9.042115)
    (100 0.6 0.45 82 0.25 0.2 18.0048)
    (74 120 -45 28 128 -128 51.6137)
    (3 -60 75 100 -75 48 97.7888)
    (53 65 15 33.0 -45 1 58.1709)
    (88 44 4 55 5 55 50.4926)))

(defn colortest [distftn colordata]
  (let [r (distftn (vec (take 3 colordata)) (vec (take 3 (drop 3 colordata))))]
    [(last colordata) r (- r (last colordata))]))

(defn ciedist94test []
  (let [labresults (map (fn [x] (colortest ciedist94 x)) cie94testdata)
       goodnessarray (map (fn [r] (< (nth r 2) 0.0001)) labresults)]
   (every? identity goodnessarray)))

(deftest cie94test
  (testing "compare ciedist94 results to known good data"
    (is (ciedist94test))))

(deftest hsvtest 
  (let [testcolor (fn [[r g b]]
                    (is (= [r g b]
                           (map long (hsv->rgb (rgb->hsv [r g b]))))))
        testcolors [[0 0 0]
                    [1 1 1]
                    [127 127 127]
                    [255 255 255]
                    [0 255 0]
                    [255 0 255]
                    [0 127 0]
                    [127 0 127]]]
    (doseq [color testcolors]
      (testcolor color))))
                    
