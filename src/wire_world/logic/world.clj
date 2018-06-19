(ns wire-world.logic.world
  (:require [wire-world.logic.grid :as gr]
            [wire-world.logic.change-recorder :as cr]
            [clojure.string :as s]))

(defrecord World [grid change-recorder])

; TODO: Make active-states and make ::empty just nil?
(def states #{::empty ::head ::tail ::conductor})

(defn new-world []
  (->World gr/new-grid cr/new-recorder))

(defn- ensure-valid-cell-state [cell-state]
  (when-not (states cell-state)
    (throw (IllegalArgumentException. (str "Illegal cell state: " cell-state)))))

(defn unrecorded-set-cell [world pos cell-state]
  (ensure-valid-cell-state cell-state)

  (update world :grid #(if (= cell-state ::empty)
                         (gr/remove-cell % pos)
                         (gr/set-cell % pos cell-state))))

(defn set-cell [world pos cell-state]
  (-> world
      (unrecorded-set-cell pos cell-state)
      (update :change-recorder cr/record pos)))

(defn get-cell [world pos]
  (or (gr/get-cell (:grid world) pos)
      ::empty))

(defn- positions-surrounding [pos]
  (let [distance 1
        [px py] pos
        d-range #(range (- % distance) (+ % distance 1))]
    (for [y (d-range py)
          x (d-range px)
          :let [new-pos [x y]]
          :when (not (= pos new-pos))]
      new-pos)))

; TODO: Generalize?
(defn heads-surrounding [world pos]
  (->> (positions-surrounding pos)
       (map #(get-cell world %))
       (filter #{::head})
       (count)))

(defn new-cell-state [world pos]
  (case (get-cell world pos)
    ::empty ::empty
    ::head ::tail
    ::tail ::conductor
    (if (#{1 2} (heads-surrounding world pos)) ; Conductor
      ::head
      ::conductor)))

(defn altered-cells [world]
  (cr/get-records (:change-recorder world)))

(defn advance-world [world]
  (let [to-be-updated (altered-cells world)
                      #_(get-modified-and-neighbors world)
        cleared (update world :change-recorder cr/clear-records)]
    (reduce (fn [w pos]
              (set-cell w pos
                        (new-cell-state world pos)))
            cleared
            to-be-updated)))

(defn stop-all-electrons [world]
  (update-in world [:grid :cell-map] ; FIXME: Fragile!?
    #(reduce-kv (fn [acc k v]
                  (if (= v ::head)
                    (assoc acc k ::conductor)
                    acc))
             %
             %)))


(defn view-world [world tl-pos view-dimensions]
  (let [sym #(case % ::empty \space, ::conductor \+,
                     ::head \H, ::tail \T)
        [tlx tly] tl-pos
        [vw vh] view-dimensions]
    (->> (for [y (range tly (+ tly vh))]
           (for [x (range tlx (+ tlx vw))]
             (sym (get-cell world [x y]))))

         (map #(s/join " " %))
         (s/join "\n"))))

(def propogation-test-world
  (let [c #(set-cell % %2 ::conductor)]
    (-> (new-world)
        (set-cell [0 0] ::head)
        (c [1 0]) ; Top
        (c [2 0])
        (c [3 0])
        (c [4 0])

        (c [0 5]) ; Bottom
        (c [1 5])
        (c [2 5])
        (c [3 5])
        (c [4 5])
        (c [5 5])
        (c [6 5])
        (c [7 5])
        (c [8 5])

        (c [0 1]) ; Left
        (c [0 2])
        (c [0 3])
        (c [0 4])

        (c [5 0]) ; Right
        (c [5 1])
        (c [4 2])  ; Diode
        (c [4 3])
        (c [6 2])
        (c [6 3])
        (c [5 3])
        (c [5 4]))))


