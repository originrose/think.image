(ns think.image.color
  (:require [clojure.core.matrix :as m]
            #?(:cljs [goog.string :refer [format]])
            #?(:clj  [clojure.edn :as edn]
               :cljs [cljs.reader :as edn])))

;; -------------------------------------------------------------------------------------------
;; make a random RGB color 
;; -------------------------------------------------------------------------------------------

(defn random-color [] 
  #?(:clj (format "%06x" (rand-int 16rFFFFFF))
     :cljs 
        (let [r (rand-int 16rFFFFFF)
              ret (.toString r 16)
              pad (- 6 (.-length ret))]
          (str (apply str (repeat pad "0")) ret))))

;; -------------------------------------------------------------------------------------------
;; color format conversions 
;; -------------------------------------------------------------------------------------------

(defn rgb->int [[r g b]]
  (bit-or (bit-shift-left (int r) 16) (bit-shift-left (int g) 8) (int b)))

(defn hex-string-to-rgb [hs]
  (let [red   (edn/read-string (str "0x" (apply str (take 2 hs)))) 
        green (edn/read-string (str "0x" (apply str (take 2 (drop 2 hs)))))
        blue  (edn/read-string (str "0x" (apply str (take 2 (drop 4 hs)))))]
    [red green blue]))   

(defn rgb-to-hex-string [[r g b]]
  #?(:clj (format "%06x" (rgb->int [r g b]))
     :cljs (let [twopad (fn [s] (let [pad (- 2 (.-length s))]
                               (str (apply str (repeat pad "0")) s)))] 
          (str (twopad (.toString r 16)) (twopad (.toString g 16)) (twopad (.toString b 16))))))


(defn rgb->hsv [[r g b]]
  (let [r (/ r 255.0)
        g (/ g 255.0)
        b (/ b 255.0)
        c-max (max r g b)
        c-min (min r g b)
        d (- c-max c-min)
        v c-max]
    (if-not (= 0.0 d)
      (let [s (/ d c-max)
            h (* (condp = c-max
                r (/ (- g b) d)
                g (+ 2 (/ (- b r) d))
                b (+ 4 (/ (- r g) d))) 60)
            h (if (< h 0)
                (+ 360 h)
                h)]
        [h s v])
      [0 0 v])))


(defn hsv->rgb [[h s v]]
  (if (<= s 0.0)
    [0 0 0]
    (let [h (mod h 360.0)
          hue (/ h 60.0)
          i (int hue)
          ff (- hue i)
          p (* v (- 1.0 s))
          q (* v (- 1.0 (* s ff)))
          t (* v (- 1.0 (* s (- 1.0 ff))))]
      (->> (case i
             0 [v t p]
             1 [q v p]
             2 [p v t]
             3 [p q v]
             4 [t p v]
             5 [v p q])
           (map #(* 255.0 %))))))

; From here:
; http://www.f4.fhtw-berlin.de/~barthel/ImageJ/ColorInspector//HTMLHelp/farbraumJava.htm
; (originally from here: http://www.brucelindbloom.com)

(defn rgb->xyz [[r g b]]
  (let [r (/ r 255)  ; r 0..1
        g (/ g 255)  ; g 0..1
        b (/ b 255)  ; b 0..1

        r (if (<= r 0.04045)
            (/ r 12.92)
            (Math/pow (/ (+ r 0.055) 1.055) 2.4))

        g (if (<= g 0.04045)
            (/ g 12.92)
            (Math/pow (/ (+ g 0.055) 1.055) 2.4))

        b (if (<= b 0.04045)
            (/ b 12.92)
            (Math/pow (/ (+ b 0.055) 1.055) 2.4))

        ; RGB-linear --> XYZ(D50) from https://www.w3.org/Graphics/Color/srgb
        ; Fun fact : D65 corresponds roughly to the average midday light in Western Europe / Northern Europe
        ; D50 is cooler (coresponding to horizon light), and might match better to indoor.
        x (+ (* 0.436052025 r) (* 0.385081593 g) (* 0.143087414 b))
        y (+ (* 0.222491598 r) (* 0.71688606  g) (* 0.060621486 b))
        z (+ (* 0.013929122 r) (* 0.097097002 g) (* 0.71418547  b))]
    [x y z]))

(defn xyz->lab [[x y z]]
  (let [eps (/ 216 24389)
        k (/ 24389 27)
        ;; reference white D50 from https://www.w3.org/Graphics/Color/srgb
        Xr 0.964221
        Yr 1.0
        Zr 0.825211

        xr (/ x Xr)
        yr (/ y Yr)
        zr (/ z Zr)

        fx (if (> xr eps)
             (Math/pow xr (/ 1 3))
             (/ (+ (* k xr) 16) 116))

        fy (if (> yr eps)
             (Math/pow yr (/ 1 3))
             (/ (+ (* k yr) 16) 116))

        fz (if (> zr eps)
             (Math/pow zr (/ 1 3))
             (/ (+ (* k zr) 16) 116))

        Ls (- (* 116 fy) 16)
        as (* 500 (- fx fy))
        bs (* 200 (- fy fz))

        ;; This scaling is not needed.
        ;L (int (+ (* 2.55 Ls) 0.5))
        ;a (int (+ as 0.5))
        ;b (int (+ bs 0.5))
         ]
    [Ls as bs]))

(defn rgb->lab [rgb]
  (-> (rgb->xyz rgb)
      (xyz->lab)))

(defn rgb->hsl
  [[r g b]]
  (let [r (/ r 255.0)
        g (/ g 255.0)
        b (/ b 255.0)
        c-max (max r g b)
        c-min (min r g b)
        l (/ (+ c-max c-min) 2.0)]
    (if (= c-max c-min)
      [0 0 l]
      (let [d (- c-max c-min)
            s (if (> l 0.5)
                (/ d (- 2 c-max c-min))
                (/ d (- c-max c-min)))
            h (cond
                (= r c-max) (+ (/ (- g b) d)
                               (if (< g b) 6.0 0.0))
                (= g c-max) (+ (/ (- b r) d) 2)
                (= b c-max) (+ (/ (- r g) d) 4))
            h (/ h 6.0)]
        [h s l]))))

(defn lab->xyz [[l a b]]
  (let [y (/ (+ l 16.0) 116.0)
        x (+ (/ a 500.0) y)
        z  (- y (/ b 200.0))

        y (if (> (Math/pow y 3) 0.008856)
            (Math/pow y 3)
            (/ (- y (/ 16.0 116.0))  7.787))
        x (if (> (Math/pow x 3) 0.008856)
            (Math/pow x 3)
            (/ (- x (/ 16.0 116.0))  7.787))
        z (if (> (Math/pow z 3) 0.008856)
            (Math/pow z 3)
            (/ (- z (/ 16.0 116.0))  7.787))

        x  (* 95.047 x)      ;; ref_x =  95.047     Observer= 2°, Illuminant= D65
        y  (* 100.000 y)     ;; ref_y = 100.000
        z  (* 108.883 z)     ;; ref_z = 108.883

        x  (/ x 100.0)       ;; x from 0 to  95.047      (Observer = 2°, Illuminant = D65)
        y  (/ y 100.0)       ;; y from 0 to 100.000
        z  (/ z 100.0)       ;; z from 0 to 108.883
        ]
    [x y z]))

(defn xyz->rgb
  [[x y z]]
  (let [r (+ (* x  3.2406) (* y -1.5372) (* z -0.4986))
        g (+ (* x -0.9689) (* y  1.8758) (* z  0.0415))
        b (+ (* x  0.0557) (* y -0.2040) (* z  1.0570))

        r (* (if (> r 0.0031308)
               (- (* 1.055 (Math/pow r (/ 1 2.4))) 0.055)
               (* 12.92 r)) 255.0)

        g (* (if (> g 0.0031308)
               (- (* 1.055 (Math/pow g (/ 1 2.4))) 0.055)
               (* 12.92 g)) 255.0)

        b (* (if (> b 0.0031308)
               (- (* 1.055 (Math/pow b (/ 1 2.4))) 0.055)
               (* 12.92 b)) 255.0)]
    (mapv #(int (max (min % 255) 0)) [r g b])))

(defn lab->rgb
  "Warning, losses were found when converting from lab->rgb and back again."
  [lab]
  (-> lab
      lab->xyz
      xyz->rgb))

;; -------------------------------------------------------------------------------------------
;; color distance algorithms
;; -------------------------------------------------------------------------------------------

;; cie 76 color distance.  colors should be LAB format.
(defn ciedist76 [[l1 a1 b1] [l2 a2 b2]]
  ;; just the pythagorean distance.
  (m/distance [l1 a1 b1] [l2 a2 b2]))

;; "just noticable difference" threshold for ciedist76
(def ciedist76jnd 2.3)

;; cie 94 color distance.  colors should be LAB format.
(defn ciedist94 [[l1 a1 b1] [l2 a2 b2]]
  (let [deltaL  (- l1 l2)
        delta-a (- a1 a2)
        delta-b (- b1 b2)
        c1      (m/sqrt (+ (* a1 a1) (* b1 b1)))
        c2      (m/sqrt (+ (* a2 a2) (* b2 b2)))
        deltaCab (- c1 c2)
        kL      1.0     ;; use 2.0 for textiles, 1.0 for paint
        K1      0.045   ;; use 0.048 for textiles, 0.045 for paint
        K2      0.015   ;; use 0.014 for textiles, 0.015 for paint
        kC      1.0
        kH      1.0
        SL      1.0
        SC      (+ 1.0 (* K1 c1))
        SH      (+ 1.0 (* K2 c1))
        ;; only define the square of deltaHab as it can be negative
        deltaHab-squared (+ (* delta-a delta-a) (* delta-b delta-b) (* -1.0 deltaCab deltaCab))
        deltaE94 (m/sqrt (+ (m/pow (/ deltaL (* kL SL)) 2.0)
                          (m/pow (/ deltaCab (* kC SC)) 2.0)
                          (/ deltaHab-squared (* kH SH kH SH))))
      ]
  deltaE94))

;; "just noticable difference" for ciedist94
(def ciedist94jnd 2.3)

(defn rgbdist [[r1 g1 b1] [r2 g2 b2]]
  ;; just the pythggorean distance.
  (m/distance [r1 g1 b1] [r2 g2 b2]))
