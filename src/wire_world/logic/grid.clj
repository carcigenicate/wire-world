(ns wire-world.logic.grid)

; TODO: Record which cells changed and update it and its cells' neighbors

(defrecord Grid [cell-map])

(def new-grid
  (->Grid {}))

(defn set-cell [grid pos cell-state]
  (assoc-in grid [:cell-map pos] cell-state))

(defn remove-cell [grid pos]
  (update grid :cell-map dissoc pos))

(defn get-cell [grid pos]
  (get-in grid [:cell-map pos]))