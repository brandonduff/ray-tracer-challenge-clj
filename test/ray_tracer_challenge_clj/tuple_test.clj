(ns ray-tracer-challenge-clj.tuple-test
  (:require  [clojure.test :refer :all]
             [ray-tracer-challenge-clj.tuple :refer :all]))

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


(defn tick [env proj]
  (let [position (add-tuples (:position proj) (:velocity proj))
        velocity (add-tuples (:velocity proj) (:gravity env) (:wind env))]
    (prn (:x position) (:y position))
    { :position position :velocity velocity }))

(def p { :position (point-tuple 0 1 0) :velocity (vector-tuple 1 3 0)})
(def e { :gravity (vector-tuple 0 -0.1 0) :wind (vector-tuple -0.01 0 0)})

(defn next-tick []
  (def p (tick e p)))
