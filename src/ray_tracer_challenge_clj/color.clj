(ns ray-tracer-challenge-clj.color
  (:require [clojure.math :as math]
            [clojure.string :as str]
            [ray-tracer-challenge-clj.string-formatting
             :refer
             [format-body multi-line-string]]
            [ray-tracer-challenge-clj.tuple :refer [clamp multiply-tuple]]))

(defn color [r g b]
  {:red r :green g :blue b})

(defn hadamard-product [c1 c2]
  (apply color (map * (vals c1) (vals c2))))

(defn color->str [color]
  (->> (vals (multiply-tuple color 255))
       (map math/round)
       (map (partial clamp 0 255))
       (str/join " ")))

(defn pixels->str [line]
  (str/join " " (map color->str line)))
