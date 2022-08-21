(ns ray-tracer-challenge-clj.matrix)

(defn matrix [width height fill]
  (let [rows (into [] (for [i (range height)]
                        (into [] (for [j (range width)]
                                   fill))))]
    {:width  width
     :height height
     :rows   rows}))

(defn update-matrix [matrix row column item]
  {:pre [(< column (:width matrix))
         (< row (:height matrix))
         (> (:width matrix) 0)
         (> (:height matrix) 0)]}
  (assoc matrix :rows (assoc-in (matrix :rows) [row column] item)))

(defn get-in-matrix [matrix row column]
  (get-in matrix [:rows row column]))
