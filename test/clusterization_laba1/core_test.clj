(ns clusterization-laba1.core-test
  (:require [clojure.test :refer :all]
            [clusterization-laba1.core :refer :all])
  (import java.lang.Math))


(deftest parse-line-and-remove-last-test
  (is (= [1.0 2.0 3.0 4.0] (parse-line-and-remove-last "1,2,3,4,5"))))

(deftest parse-file-test
  (is (=[[0.0 3.0] [1.0 5.0] [2.0 4.0]] (parse-file "resources/test.txt"))))

(deftest get-square-of-difference-test
  (are [x1 x2] (= 36.0 (get-square-of-difference x1 x2))
       9 3
       3 9
       6 0
       0 6
       1 -5))

(deftest calculate-euclidean-distance-test
  (are [x1 x2] (= 5.0 (calculate-euclidean-distance x1 x2))
       [3 0] [0 4]
       [2 -2] [-2 1]))

(deftest calculate-hamming-distance-test
  (are [x1 x2] (= 2 (calculate-hamming-distance x1 x2))
       [1 2] [3 4]
       [-1 -2] [1 2]))

(deftest calculate-points-potential-test
  (let [x1 [3 0]
        x2 [0 4]
        alpha 1.5
        ]
  (is (= (Math/exp(-(* alpha (calculate-euclidean-distance x1 x2)))) (calculate-points-potential x1 x2 calculate-euclidean-distance alpha)))))

(deftest get-potential-test
  (let [x [1 1]
        points [[1 1] [2 2] [3 3]]
        alpha 1.5
        distance1 (Math/exp (- (* alpha (Math/sqrt 2))))
        distance2 (Math/exp (- (* alpha (Math/sqrt 8))))
        ;expected-result (list (+ 1 distance1 distance2) points)
        ]
  (is (= (+ 1 distance1 distance2) (first(get-potential points x calculate-euclidean-distance alpha))))))


(deftest update-potentials-test
  (let [potentials [[5 [5 4]] [3 [2 2]] [2 [1 4]]]
        core [5 [5 4]]
        beta 1.5
        new-potentials [[ 0.0 [5 4]]
                        [(- 3 (* (first core) (calculate-points-potential (last core) [2 2] calculate-euclidean-distance beta))) [2 2]]
                        [(- 2 (* (first core) (calculate-points-potential (last core) [1 4] calculate-euclidean-distance beta))) [1 4]]]]
    (is (= new-potentials (update-potentials potentials core calculate-euclidean-distance beta)))))

(run-all-tests)
