(ns ray-tracer-challenge-clj.canvas
  (:require
   [ray-tracer-challenge-clj.color :refer [color pixels->str]]
   [ray-tracer-challenge-clj.matrix
    :refer [get-in-matrix matrix update-matrix]]
   [ray-tracer-challenge-clj.string-formatting
    :refer
    [format-body multi-line-string]]))

(defn canvas ([width height pixel]
              (matrix width height pixel))
  ([width height]
   (canvas width height (color 0 0 0))))

(defn pixel-at [canvas x y]
  (get-in-matrix canvas y x))

(defn write-pixel [canvas x y pixel]
  (update-matrix canvas y x pixel))

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
