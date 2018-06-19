(ns wire-world.ui.type-test
  (:require [seesaw.core :as sc]
            [seesaw.graphics :as sg]
            [seesaw.dev :as sd]

            [clojure.string :as s])
  (:import (java.awt Event)
           (java.awt.event InputEvent MouseEvent KeyEvent)))

(defn paint [str-atom cvs g]
  (let [font-size 30
        text @str-atom]
    (sg/draw g
       (sg/string-shape font-size font-size text)
       (sg/style :font {:name "Arial", :size font-size}))))

(defn key-handler [str-atom, ^KeyEvent e]
  (let [chr (.getKeyChar e)
        code (.getKeyCode e)]
    (println chr code)
    (swap! str-atom
           #(cond
              (<= 16 code 18) %
              (= chr \backspace) (if (empty? %)
                                   %
                                   (subs % 0 (dec (count %))))
              :else (str % (.getKeyChar e))))))

(defn new-canvas [str-atom]
  (let [canvas (sc/canvas :paint (partial paint str-atom),
                          :id :canvas)]

    canvas))

(defn new-frame []
  (let [str-atom (atom "")
        f (sc/frame :content (new-canvas str-atom),
                    :size [1000 :by 1000])
        canvas (sc/select f [:#canvas])]

    (sc/listen f :key-pressed (partial key-handler str-atom))

    (add-watch str-atom :repainter
               (fn [_ _ _ n]
                  (sc/repaint! canvas)))

    f))
