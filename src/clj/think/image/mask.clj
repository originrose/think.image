(ns think.image.mask
  (:require [mikera.image.core :as image])
  (:import [java.awt.image BufferedImage]))


(defn maskbuf-to-image
  "Given a mask buffer create an image from it"
  [^"[B" pixbuf width height]
  (let [pixcount (* width height)
        ^BufferedImage img (BufferedImage. width height BufferedImage/TYPE_BYTE_GRAY)]
    (image/set-pixels img pixbuf)
    img))

(defmacro clamp-mask-val
  [mval]
  `(unchecked-byte (if (>= (bit-and ~mval 0x000000FF) 128)
                     255
                     0)))

(defn mask-to-int
  ^Integer [^Byte mask idx]
  (let [bit-pos (rem idx 32)
        bit-val (if (>= (bit-and mask 0x000000FF) 128)
                  1
                  0)]
    (unchecked-int (bit-shift-left bit-val bit-pos))))


(defn int-to-mask
  ^Byte [^Integer item idx]
  (let [bit-val (bit-and (bit-shift-left 1 (rem idx 32)) item)]
    (if (= bit-val 0)
      0
      (unchecked-byte 255))))



(defn maskbuf-to-packed-intbuf
  ^ints [^"[B" maskbuf]
  (let [pixcount (count maskbuf)
        intcount (/ pixcount 32)
        leftover (rem pixcount 32)
        intcount (if (> leftover 0)
                   (inc intcount)
                   intcount)
        retval (int-array intcount 0)]
    (loop [idx 0]
      (when (< idx pixcount)
        (let [maskval (aget maskbuf idx)
              intidx (/ idx 32)
              intval (aget retval intidx)
              intval (unchecked-int (bit-or intval (mask-to-int maskval idx)))]
          (aset retval intidx intval)
          (recur (inc idx)))))
    retval))


(defn packed-intbuf-to-maskbuf
  ^"[B" [^ints intbuf ]
  (let [intcount (count intbuf)
        pixcount (* intcount 32)
        retval (byte-array pixcount)]
    (loop [idx 0]
      (when (< idx pixcount)
        (let [intval (aget intbuf (/ idx 32))
              maskval (int-to-mask intval idx)]
          (aset retval idx maskval)
          (recur (inc idx)))))
    retval))


(defn mask-int-values
  [^ints int-vals ^ints mask]
  (let [num-ints (count int-vals)]
    (loop [idx 0]
      (when (< idx num-ints)
        (let [mask-index (/ idx 32)
              mask-int (aget mask mask-index)
              mask-val (int-to-mask mask-int idx)]
          (when (= 0 mask-val)
            (aset int-vals idx 0))
          (recur (inc idx))))))
  int-vals)
