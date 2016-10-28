(ns think.image.bounding-box
  (:require [think.image.pixel :refer :all]
            [mikera.image.core :as image])
  (:import [java.awt.image BufferedImage]))

(set! *warn-on-reflection* true)
(set! *unchecked-math* true)

(defn to-bounding-box-from-flattened
  "Create a bounding box from a list of 4 doubles (the result of
  flattening)"
  [data]
  (let [[x-min y-min x-max y-max] (into [] data)]
    [[(int x-min) (int y-min)][(int x-max) (int y-max)]]))

(defn min-vals
  [[x y] [xx yy]]
  [(min x xx) (min y yy)])

(defn max-vals
  [[x y] [xx yy]]
  [(max x xx) (max y yy)])


(defn int-round
  [dval]
  (int (+ dval 0.5)))

(defn widen-bounding-box
  [bbox width height multiplier]
  (let [[x-min y-min x-max y-max] bbox
        box-max  [x-max y-max]
        box-min [x-min y-min]
        box-width (- (box-max 0) (box-min 0))
        box-height (- (box-max 1) (box-min 1))
        ideal-width (* box-width multiplier)
        ideal-height (* box-height multiplier)
        width-add (/ (- ideal-width box-width) 2)
        height-add (/ (- ideal-height box-height) 2)
        box-min [(int-round (- (box-min 0) width-add)) (int-round (- (box-min 1) width-add))]
        box-max [(int-round (+ (box-max 0) width-add)) (int-round (+ (box-max 1) width-add))]]
    (into [] (concat (max-vals [0 0] box-min) (min-vals [width height] box-max)))))


(defn relative-bbox
  "Given two bounding boxes in an identical coordinate space
  , return a new box which is the second box
  described in the coordinate space defined by the first box"
  [coord-bbox bbox]
  (let [[c-min-x c-min-y c-max-x c-max-y] coord-bbox
        [min-x min-y max-x max-y] bbox]
    [(- min-x c-min-x)
      (- min-y c-min-y)
     (- max-x c-min-x)
     (- max-y c-min-y)]))

(defn scale-bbox
  "Scale a bounding box such that the scaled box
  will fit the same relative locations on the scaled image"
  [bbox scale]
  (let [[min-x min-y max-x max-y] bbox]
    [(double-to-int (* scale min-x))
     (double-to-int (* scale min-y))
     (double-to-int (* scale max-x))
     (double-to-int (* scale max-y))]))

(defn offset-bbox
  [bbox x-offset y-offset]
  (let [[x-min y-min x-max y-max] bbox]
    [(double-to-int (+ x-min x-offset)) (double-to-int (+ y-min y-offset))
     (double-to-int (+ x-max x-offset)) (double-to-int (+ y-max y-offset))]))

(defn is-same-size?
  [bbox width height]
  (= [0 0 width height] bbox))

(defn shrink-bbox
  [bbox]
  (let [[x1 y1 x2 y2] bbox]
    [(inc x1) (inc y1) (dec x2) (dec y2)]))


(defn draw-bounding-box
  "Given an input image, draw a bounding box in the color
defined by a packed int color.  Return a new image"
  [^BufferedImage img bbox ^Integer int-color]
  (let [width (.getWidth img)
        height (.getHeight img)
        ^ints pixels (image/get-pixels img)
        [min-x min-y max-x max-y] bbox
        result (image/new-image width height)
        box-width (int (- max-x min-x))
        box-height (int (- max-y min-y))]
    (loop [y 0]
      (when (< y box-height)
        (loop [x 0]
          (when (< x box-width)
            (let [draw-pixel (or (or (= 0 y)
                                     (= (- box-height 1) y))
                                 (or (= 0 x)
                                     (= (- box-width 1) x)))
                  idx (+ (* width (+ y min-y))
                         (+ x min-x))]
              (when draw-pixel
                (aset pixels idx int-color))
              (recur (inc x)))))
        (recur (inc y))))
    (image/set-pixels result pixels)
    result))


(defn bounding-box-width
  [[x-min y-min x-max y-max]]
  (- x-max x-min))

(defn bounding-box-height
  [[x-min y-min x-max y-max]]
  (- y-max y-min))

(defn int-subrect
  [^ints source width height bbox]
  (let [[x-min y-min x-max y-max] bbox
        out-width (- x-max x-min)
        out-height (- y-max y-min)
        out-px-count (* out-width out-height)
        ^ints out-pixels (int-array out-px-count)]
    (loop [idx 0]
      (when (< idx out-px-count)
        (let [row (rem idx out-width)
              column (int (/ idx out-width))
              input-row (+ row x-min)
              input-column (+ column y-min)
              input-idx (+ (* input-column width)
                           input-row)]
          ;;(println "row" row "column" column "input-row"
          ;;         input-row "input-column" input-column)
          (aset out-pixels idx
                (aget source input-idx))
          (recur (inc idx)))))
    out-pixels))



(defn bounding-box-seq
  [width height target-dim]
  (let [last-pos-x (- width target-dim)
        last-pos-y (- height target-dim)]
    (if (<= target-dim width)
      (repeatedly (fn []
                    (let [x-min (rand-int last-pos-x)
                          y-min (rand-int last-pos-y)
                          x-max (+ x-min target-dim)
                          y-max (+ y-min target-dim)]
                      [x-min y-min x-max y-max])))
      (repeat nil))))

(defn bad-bounding-box?
  [bbox width height]
  (let [bbox-width (bounding-box-width bbox)
        bbox-height (bounding-box-height bbox)]
    (or (< bbox-width (* width 0.10))
        (< bbox-height (* height 0.10)))))


(defn ensure-valid-bbox
  [bbox width height]
  (if (bad-bounding-box? bbox width height)
    [0 0 width height]
    (let [[x-min y-min x-max y-max] bbox
          x-max (min x-max width)
          y-max (min y-max height)]
      [x-min y-min x-max y-max])))
