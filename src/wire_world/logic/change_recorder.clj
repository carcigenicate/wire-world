(ns wire-world.logic.change-recorder)

(def new-recorder #{})

(defn record [recorder record]
  (conj recorder record))

(defn get-records [recorder]
  (seq recorder))

(defn clear-records [recorder]
  new-recorder)