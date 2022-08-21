(ns ray-tracer-challenge-clj.canvas
  (:require [clojure.math :as math]
            [clojure.string :as str]
            [ray-tracer-challenge-clj.color :refer [color hadamard-product pixels->str]]
            [ray-tracer-challenge-clj.string-formatting
             :refer
             [format-body multi-line-string]]))

(defn canvas ([width height pixel]
              (let [rows (into [] (for [i (range height)]
                                    (into [] (for [j (range width)]
                                               pixel))))]
                {:width  width
                 :height height
                 :rows   rows}))
  ([width height]
   (canvas width height (color 0 0 0))))

(defn pixel-at [canvas x y]
  (get-in canvas [:rows y x]))

(defn write-pixel [canvas x y pixel]
  {:pre [(< x (:width canvas))
         (< y (:height canvas))]}
  (assoc canvas :rows (assoc-in (canvas :rows) [y x] pixel)))

(defn ppm-header [canvas]
  (multi-line-string "P3"
                     (str (:width canvas) " " (:height canvas))
                     "255"))

(defn canvas->ppm [canvas]
  (let [header (ppm-header canvas)
        body (->> canvas
                  :rows
                  (map pixels->str)
                  (apply multi-line-string)
                  (format-body 70))]
    (str (multi-line-string header body) "\n")))
