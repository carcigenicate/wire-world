(ns wire-world.ui.ui-helpers
  (:import (java.awt Component)
           (java.awt.event MouseEvent)))

(defmacro thread [& body]
  ~(doto (Thread. ^Runnable (fn [] ~@body))
         (.start)))

(defn comp-dimensions [^Component c]
  [(.getWidth c) (.getHeight c)])

(defn mouse-pos [^MouseEvent e]
  [(.getX e) (.getY e)])