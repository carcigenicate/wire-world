(ns wire-world.logic.drawing-helpers
  (:require [wire-world.logic.world :as w]))

(defn sign [n]
  (cond
    (pos? n) 1
    (neg? n) -1
    :else 0))

(defn dec-to-zero [n]
  (let [opp (* -1 (sign n))]
    (+ n opp)))

(defn draw-line [world start-pos offsets cell-state]
  (loop [cur-pos (map int start-pos)
         cur-offsets (map int offsets)
         acc-world world]
    (let [offset-signs (map sign cur-offsets)]
      (if (every? zero? offset-signs)
        acc-world

        (recur (map + cur-pos offset-signs)
               (map dec-to-zero cur-offsets)
               (w/set-cell acc-world cur-pos cell-state))))))

(defn draw-offsets [world start-pos cell-state offset-pairs]
  (loop [[cur-offsets & rest-offsets] offset-pairs
         cur-pos start-pos
         acc-world world]
    (if cur-offsets
      (recur rest-offsets
             (map + cur-pos cur-offsets)
             (draw-line acc-world cur-pos cur-offsets cell-state))

      acc-world)))

(defn draw-box [world tl-pos dimensions cell-state]
  (let [[dec-wid dec-hei :as d] (map dec dimensions)
        [n-wid n-hei] (map - d)]
    (-> world
        (draw-offsets tl-pos cell-state
                      [[dec-wid 0] [0 dec-hei]
                       [n-wid 0] [0 n-hei]]))))



