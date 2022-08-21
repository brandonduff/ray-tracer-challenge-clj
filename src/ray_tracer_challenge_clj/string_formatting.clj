(ns ray-tracer-challenge-clj.string-formatting
  (:require
   [clojure.string :as str]))

(defn multi-line-string [& strings]
  (str/join "\n" strings))

(defn should-include? [length current-result candidate]
  (<=
   (+ (count current-result) (count candidate) 1)
   length))

(defn merge-word [length line word]
  (let [last-word (last line)]
    (if (should-include? length last-word word)
      (conj (vec (butlast line)) (str last-word " " word))
      (conj line word))))

(defn format-line [length words]
  (reduce
   (partial merge-word length)
   [(first words)]
   (rest words)))

(defn format-body [length string]
  (->> string
       str/split-lines
       (reduce (fn [result line]
                 (if (<= (count line) length)
                   (conj result line)
                   (concat result (format-line length (str/split line #"\s")))))
               [])
       (apply multi-line-string)))
