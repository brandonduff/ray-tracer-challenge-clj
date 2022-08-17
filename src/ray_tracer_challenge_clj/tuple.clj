(ns ray-tracer-challenge-clj.tuple
  (:require
   [clojure.math :as math]
   [clojure.string :as str]))

(defn multi-line-string [& strings]
  (str/join "\n" strings))

(def sum #(reduce + %))

(defn close-to? [a b]
  (< (abs (- a b)) 0.00001))

(defn tuple [x y z w]
  {:x x :y y :z z :w w})

(defn point-tuple? [tuple]
  (== (:w tuple) 1))

(defn vector-tuple? [tuple]
  (== (:w tuple) 0))

(defn point-tuple [x y z]
  (tuple x y z 1.0))

(defn vector-tuple [x y z]
  (tuple x y z 0))

(defn tuples-equal? [t1 t2]
  (every? (fn [[key, val]]
            (close-to? val (key t2)))
          t1))

(defn add-tuples [& tuples]
  (apply merge-with + tuples))

(defn subtract-tuples [& tuples]
  (apply merge-with - tuples))

(defn negate-tuple [t]
  (subtract-tuples (tuple 0 0 0 0) t))

(defn multiply-tuple [t scalar]
  (into {} (map (fn [[key val]] [key (* val scalar)])) t))

(defn divide-tuple [t scalar]
  (multiply-tuple t (/ 1 scalar)))

(defn magnitude [v]
  (->> v
       vals
       (map #(math/pow % 2))
       sum
       math/sqrt))

(defn normalize [v]
  (->> v
       vals
       (map #(/ % (magnitude v)))
       (apply tuple)))

(defn dot-product [t1 t2]
  (sum (map * (vals t1) (vals t2))))

(defn cross-product [t1 t2]
  (vector-tuple
   (- (* (:y t1) (:z t2)) (* (:z t1) (:y t2)))
   (- (* (:z t1) (:x t2)) (* (:x t1) (:z t2)))
   (- (* (:x t1) (:y t2)) (* (:y t1) (:x t2)))))

(defn color [r g b]
  {:red r :green g :blue b})

(defn hadamard-product [c1 c2]
  (apply color (map * (vals c1) (vals c2))))

(defn canvas ([width height pixel]
              (let [rows (into [] (for [i (range height)]
                                    (into [] (for [j (range width)]
                                               pixel))))]
                {:width  width
                 :height height
                 :rows   rows}))
  ([width height]
   (canvas width height (color 0 0 0))))

(defn write-pixel [canvas x y pixel]
  (assoc canvas :rows (assoc-in (canvas :rows) [y x] pixel)))

(defn clamp [lower upper n]
  (cond
    (< n lower) lower
    (> n upper) upper
    :else n))

(defn color->str [color]
  (->> (vals (multiply-tuple color 255))
       (map math/round)
       (map (partial clamp 0 255))
       (str/join " ")))

(defn pixel-at [canvas x y]
  (get-in canvas [:rows y x]))

(defn write-line [line]
  (str/join " " (map color->str line)))

(defn ppm-header [canvas]
  (multi-line-string "P3"
                     (str (:width canvas) " " (:height canvas))
                     "255"))

(defn should-include? [length current-result candidate]
  (<=
   (+ (count current-result) (count candidate) 1)
   length))

(defn merge-word [length lines candidate]
  (let [last-word (last lines)]
    (if (should-include? length last-word candidate)
      (conj (butlast lines) (str last-word " " candidate))
      (conj lines candidate))))

(defn format-line [length words]
  (reduce
     (fn [result current-word]
       (if (should-include? length (last result) current-word)
         (conj (vec (butlast result)) (str (last result) " " current-word))
         (conj result current-word)))
     [(first words)]
     (rest words)))

(defn format-body [length string]
  (apply multi-line-string
         (reduce (fn [result line]
             (if (<= (count line) length)
               (conj result line)
               (concat result (format-line length (str/split line #"\s")))))
           []
           (str/split-lines string))))

(defn canvas-to-ppm [canvas]
  (let [header (ppm-header canvas)
        body (->> canvas
                  :rows
                  (map write-line)
                  (apply multi-line-string))]
    (multi-line-string header (format-body 70 body))))
