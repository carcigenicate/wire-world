(ns wire-world.ui.first
  (:require [seesaw.core :as sc]
            [seesaw.dev :as sd]
            [seesaw.color :as s-col]
            [seesaw.graphics :as sg]

            [wire-world.logic.world :as w]
            [wire-world.logic.drawing-helpers :as dh :refer [draw-line draw-box draw-offsets]]

            [wire-world.ui.ui-helpers :as uh]

            [helpers.general-helpers :as g]
            [clojure.string :as s])

  (:import (javax.swing Timer SwingUtilities)
           (java.awt.event MouseEvent)
           (java.awt AWTEvent)))

; TODO: Make an atom so it can be changed by options later?
(def view-width 25)
(def view-height view-width)

(def world-update-delay 50)
(def canvas-update-delay 25)

(def empty-color :grey)

(def active-states (disj w/states ::w/empty))

(defn screen-size-mult [view-dimensions canvas]
  (let [[vw vh] view-dimensions
        [cw ch] (uh/comp-dimensions canvas)]
    [(/ cw vw)
     (/ ch vh)]))

(def cell-color
  {::w/empty :grey
   ::w/conductor :yellow
   ::w/head :blue
   ::w/tail :red})

(defn paint [world-atom cvs g]
  (let [world @world-atom
        altered-coords (w/altered-cells world)
        [x-mult y-mult :as mults] (screen-size-mult [view-width view-height] cvs)]
    (doseq [pos altered-coords
            :let [[sx sy] (map * pos mults)
                  cell-state (w/get-cell world pos)]]
      (sg/draw g
         (sg/rect sx sy x-mult y-mult)
         (sg/style :background (cell-color cell-state))
         #_((sg/string-shape sx (+ sy (/ y-mult 2)) (str (vec pos)))
            (sg/style :font {:name "Arial", :size 15}))))))

(defn canvas-mouse-handler [world-atom, tool-atom, view-dimensions, canvas,
                            ^MouseEvent e]
  (let [screen-pos (uh/mouse-pos e)
        screen-mults (screen-size-mult view-dimensions canvas)
        grid-pos (->> (map / screen-pos screen-mults)
                      (map int))]

    (swap! world-atom
      w/set-cell grid-pos (if (SwingUtilities/isLeftMouseButton e)
                            @tool-atom
                            ::w/empty))))

(defn new-canvas [world-atom]
  (let [canvas (sc/canvas :paint (partial paint world-atom)
                          :background empty-color
                          :id :canvas)]

    canvas))

(defn new-tool-option [label-text cell-state]
  (let [label (sc/label :text (str label-text))
        option (sc/radio :class :tool-option, :user-data cell-state)]
    (sc/horizontal-panel :items [label option])))

(defn new-tool-picker [tool-type-atom]
  (let [option-panels (map #(new-tool-option (s/capitalize (name %)) %)
                           active-states)

        tool-panel (sc/vertical-panel :items option-panels)

        option-boxes (sc/select tool-panel [:.tool-option])

        btn-group (sc/button-group :buttons option-boxes)]

    (sc/listen btn-group
       :selection (fn [^AWTEvent e] (reset! tool-type-atom
                                            (sc/user-data(.getSource e)))))

    tool-panel))

(defn new-action-panel [world-atom]
  (let [cut-pwr-btn (sc/button :text "Kill")
        action-panel (sc/vertical-panel :items [cut-pwr-btn])]

    (sc/listen cut-pwr-btn
       :action (fn [_] (swap! world-atom w/stop-all-electrons)))

    action-panel))

(defn new-main-panel [world-atom tool-type-atom]
  (let [main-panel (sc/border-panel :center (new-canvas world-atom)
                                    :west (new-tool-picker tool-type-atom),
                                    :east (new-action-panel world-atom))]
    main-panel))

(defn start-world-updater [world-atom]
  (sc/timer
    (fn [_] (swap! world-atom w/advance-world))
    :delay world-update-delay))

(defn start-canvas-updater [canvas]
  (sc/timer
    (fn [_] (sc/repaint! canvas))
    :delay canvas-update-delay))

(defn stop-timers [& timers]
  (doseq [t timers]
    (.stop ^Timer t)))

(defn new-frame [world]
  (let [world-atom (atom world)
        tool-atom (atom ::w/head)

        frame (sc/frame :content (new-main-panel world-atom tool-atom)
                        :size [1000 :by 1000])

        canvas (sc/select frame [:#canvas])

        world-t (start-world-updater world-atom)
        canvas-t (start-canvas-updater canvas)

        handler (partial canvas-mouse-handler world-atom tool-atom
                         [view-width view-height]
                         canvas)]

    (sc/listen canvas
       :mouse-pressed handler
       :mouse-dragged handler)

    (sc/listen frame
       :window-closing (fn [_]
                         (stop-timers world-t canvas-t)))

    frame))

(let [c ::w/conductor
      vw view-width, vh view-height]
  (def test-world
      (-> (w/new-world)
          (draw-box [0 0] [3 30] c)
          (draw-line [3 0] [10 0] c)
          (draw-box [11 0] [5 20] c)

          (w/set-cell [15 8] ::w/empty)
          (draw-line [14 8] [0 2] c)
          (draw-line [16 8] [0 2] c)

          (draw-line [0 0] [0 2] ::w/head)
          (w/set-cell [1 0] ::w/tail)))

  (def huge-test
    (let [start [(/ vw 2) 0]]

      (-> (w/new-world)
          (draw-line start [0 vh] c)
          (draw-line [0 (/ vh 2)] [view-width 0] c)
          (draw-box [0 0] [vw vh] c)
          (draw-line [(/ vw 4) 0] [0 (/ vh 2)] c)
          (draw-line [(* vw 3/4) (/ vh 2)] [0 (/ vh 2)] c)

          (draw-line start [0 2] ::w/head))))

  (def plate-test
    (-> (w/new-world)
        (draw-offsets [0 0] c
          (take (* vw 2)
            (cycle [[0 vh] [1 0]
                    [0 (- vh)] [1 0]])))

        (draw-line [0 (/ vh 2)] [vw 0] c)
        (draw-line [0 (* vh 1/3)] [vw 0] c)
        (draw-line [0 (* vh 2/3)] [vw 0] c)
        #_
        (w/set-cell [0 0] ::w/head)))

  (def click-test
    (-> (w/new-world)
        (draw-box [0 0] [vw vh] c))))