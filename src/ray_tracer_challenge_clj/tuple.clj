(ns ray-tracer-challenge-clj.tuple
  (:require
   [clojure.math :as math]
   [clojure.string :as str]
   [ray-tracer-challenge-clj.string-formatting
    :refer
    [format-body multi-line-string]]))

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

(defn clamp [lower upper n]
  (cond
    (< n lower) lower
    (> n upper) upper
    :else n))
