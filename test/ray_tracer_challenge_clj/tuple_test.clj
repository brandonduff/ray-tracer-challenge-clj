(ns ray-tracer-challenge-clj.tuple-test
  (:require  [clojure.test :refer :all]
             [ray-tracer-challenge-clj.tuple :refer :all]
             [clojure.string :as str]))

(defn apply-to-n-lines [f n string]
  (str/join "\n" (f n (str/split-lines string))))

(deftest tuple-test
  (testing "tuple with w=1.0 is a point"
    (let [a (tuple 4.3 -4.2 3.1 1.0)]
      (is (= 4.3 (:x a)))
      (is (= -4.2 (:y a)))
      (is (= 3.1 (:z a)))
      (is (= 1.0 (:w a)))
      (is (point-tuple? a))
      (is (not (vector-tuple? a)))))

  (testing "tuple with w=0.0 is a vector"
    (let [a (tuple 4.3 -4.2 3.1 0.0)]
      (is (not (point-tuple? a)))
      (is (vector-tuple? a))))

  (testing "creating points and vectors"
    (is (point-tuple? (point-tuple 1 1 1)))
    (is (vector-tuple? (vector-tuple 1 1 1))))

  (testing "tuple equality"
    (is (tuples-equal?
         (point-tuple 1 2 3)
         (point-tuple 1 2 3)))
    (is (not (tuples-equal?
              (point-tuple 1 2 3)
              (vector-tuple 1 2 3))))
    (is (tuples-equal?
         (point-tuple 1 2 3.0)
         (point-tuple 1 2 2.9999999999))))

  (testing "adding tuples"
    (let [a (point-tuple 3 -2 5)
          b (vector-tuple -2 3 1)]
      (is (tuples-equal? (add-tuples a b) (point-tuple 1 1 6)))))

  (testing "subtracting points gives a vector"
    (let [a (point-tuple 3 2 1)
          b (point-tuple 5 6 7)]
      (is (tuples-equal?
           (subtract-tuples a b)
           (vector-tuple -2 -4 -6)))))

  (testing "subtracting a vector from a point gives a point"
    (let [p (point-tuple 3 2 1)
          v (vector-tuple 5 6 7)]
      (is (tuples-equal?
           (subtract-tuples p v)
           (point-tuple -2 -4 -6)))))

  (testing "subtracting two vectors gives a vector"
    (let [v1 (vector-tuple 3 2 1)
          v2 (vector-tuple 5 6 7)]
      (is (tuples-equal?
           (subtract-tuples v1 v2)
           (vector-tuple -2 -4 -6)))))
  (testing "negating a tuple"
    (let [a (tuple 1 -2 3 -4)]
      (is (tuples-equal? (tuple -1 2 -3 4) (negate-tuple a)))))

  (testing "multiplying and dividing tuple by a scalar"
    (let [a (tuple 1 -2 3 -4)]
      (is (tuples-equal?
           (tuple 3.5 -7 10.5 -14)
           (multiply-tuple a 3.5)))
      (is (tuples-equal?
           (tuple 0.5 -1 1.5 -2)
           (divide-tuple a 2)))))

  (testing "magnitude"
    (is (close-to? 1 (magnitude (vector-tuple 1 0 0))))
    (is (close-to? (Math/sqrt 14) (magnitude (vector-tuple -1 -2 -3))))
    (is (close-to? 1 (magnitude (vector-tuple 0 0 1))))))

(deftest normalize-test
  (is (tuples-equal?
       (vector-tuple 1 0 0)
       (normalize (vector-tuple 4 0 0))))
  (is (tuples-equal?
       (vector-tuple 0.26726 0.53452 0.80178)
       (normalize (vector-tuple 1 2 3)))))

(deftest dot-product-test
  (is (=
       20
       (dot-product
        (vector-tuple 1 2 3)
        (vector-tuple 2 3 4)))))

(deftest cross-product-test
  (let [a (vector-tuple 1 2 3)
        b (vector-tuple 2 3 4)]
    (is (tuples-equal?
         (vector-tuple -1 2 -1)
         (cross-product a b)))
    (is (tuples-equal?
         (vector-tuple 1 -2 1)
         (cross-product b a)))))

(deftest color-test
  (let [c (color -0.5 0.4 1.7)]
    (is (= -0.5 (:red c)))
    (is (= 0.4 (:green c)))
    (is (= 1.7 (:blue c)))))

(deftest hadamard-product-test
  (let [c1 (color 1 0.2 0.4)
        c2 (color 0.9 1 0.1)]
    (is (tuples-equal? (color 0.9 0.2 0.04) (hadamard-product c1 c2)))))

(deftest canvas-test
  (let [c (canvas 10 20)]
    (is (= 10 (:width c)))
    (is (= 20 (:height c)))
    (is (= 20 (count (:rows c))))
    (is (= 200 (count (flatten (:rows c)))))
    (every? (fn [p] (= (color 0 0 0) p)) (flatten (:rows c)))))

(deftest write-pixel-test
  (let [c (canvas 10 20)
        red (color 1 0 0)
        c' (write-pixel c 2 3 red)]
    (is (= red (pixel-at c' 2 3)))))

(defn first-n-lines [n string]
  (apply-to-n-lines take n string))

(defn last-n-lines [n string]
  (apply-to-n-lines take-last n string))

(deftest canvas-to-ppm-test
  (testing "header"
    (let [c (canvas 5 3)
          ppm (canvas-to-ppm c)]
      (is (= (multi-line-string "P3" "5 3" "255")
             (first-n-lines 3 ppm)))))

  (testing "constructing pixel data"
    (let [c (canvas 5 3)
          c1 (color 1 0 0)
          c2 (color 0 0.5 0)
          c3 (color 0 0 1)
          result (-> c
                     (write-pixel 0 0 c1)
                     (write-pixel 2 1 c2)
                     (write-pixel 4 2 c3)
                     canvas-to-ppm)]
      (is (= (multi-line-string
              "255 0 0 0 0 0 0 0 0 0 0 0 0 0 0"
              "0 0 0 0 0 0 0 128 0 0 0 0 0 0 0"
              "0 0 0 0 0 0 0 0 0 0 0 0 0 0 255")
             (last-n-lines 3 result)))))

  (testing "splitting long lines"
    (let [c (canvas 10, 2 (color 1 0.8 0.6))
          ppm (canvas-to-ppm c)]
      (is (= (multi-line-string
              "255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204"
              "153 255 204 153 255 204 153 255 204 153 255 204 153"
              "255 204 153 255 204 153 255 204 153 255 204 153 255 204 153 255 204"
              "153 255 204 153 255 204 153 255 204 153 255 204 153")
             (last-n-lines 4 ppm))))))

(deftest color->str-test
  (is (= "0 128 0" (color->str (color 0 0.5 0))))

  (testing "clamping to 0"
    (is (= "0 0 0" (color->str (color -0.5 0 0))))))

(deftest write-line-test
  (is (= "0 0 0 255 255 255"
         (write-line [(color 0 0 0) (color 1 1 1)]))))

(deftest should-include?-test
  (let [include? (partial should-include? 5)]
    (is (include? "123" "4"))
    (is (include? "12" "34"))
    (is (not (include? "1234" "5")))
    (is (not (include? "12345" "6")))
    (is (not (include? "123" "45")))))

(deftest format-line-test
  (let [subject (partial format-line 5)]
    (is (= ["ab cd"] (subject ["ab" "cd"])))
    (is (= ["abc" "de"] (subject ["abc" "de"])))
    (is (= ["abcde" "f"] (subject ["abcde" "f"])))
    (is (= ["ab cd" "ef"] (subject ["ab" "cd" "ef"])))))

(deftest format-body-test
  (let [subject (partial format-body 10)]
    (testing "leaves it alone if doesn't need formatting"
      (is (= (multi-line-string "ab cd df" "gh hi jk")
             (subject (multi-line-string "ab cd df" "gh hi jk")))))
    (testing "evens it out when it needs it"
      (is (= (multi-line-string "abcdefghi" "jkl")
             (subject "abcdefghi jkl")))
      (is (= (multi-line-string "123 456 78" "9 10")
             (subject "123 456 78 9 10"))))))

(defn tick [env proj]
  (let [position (add-tuples (:position proj) (:velocity proj))
        velocity (add-tuples (:velocity proj) (:gravity env) (:wind env))]
    (prn (:x position) (:y position))
    {:position position :velocity velocity}))

(def p {:position (point-tuple 0 1 0) :velocity (vector-tuple 1 3 0)})
(def e {:gravity (vector-tuple 0 -0.1 0) :wind (vector-tuple -0.01 0 0)})

(defn next-tick []
  (def p (tick e p)))
