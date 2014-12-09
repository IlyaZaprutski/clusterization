(ns clusterization-laba1.core
  (:gen-class)
  (:require [clojure.tools.cli :refer [cli]])
  (:require [clojure.java.io :as io])
  (:require [clojure.string :as string])
  (import java.lang.Math)
 )


(defn parse-line-and-remove-last[line]
        (map #(Double/parseDouble %)
             ( drop-last(string/split line #","))))


(defn parse-file [file-name]
  (into []
        (with-open [rdr (io/reader file-name)]
           (doall (map parse-line-and-remove-last(line-seq rdr))))))


(defn get-square-of-difference [x1 x2]
  (Math/pow (- x1, x2) 2))


(defn calculate-euclidean-distance [p1 p2]
  (Math/sqrt (reduce +
                     (map get-square-of-difference p1 p2))))

(defn calculate-hamming-distance [x1 x2]
  (reduce
    +
    (map
      (fn [y1 y2]
        (if (= y1 y2) 0 1))
      x1 x2)))


(defn calculate-points-potential [x1 x2 distance-fn coef]
  (Math/exp(-(* coef (distance-fn x1 x2)))))


(defn get-potential [points x distance-fn alpha]
  (list (reduce + (map #(calculate-points-potential % x distance-fn alpha) points)) x))


(defn get-potentials [points distance-fn alpha]
  (map #(get-potential points % distance-fn alpha) points))


(defn update-potentials [potentials core distance-fn beta]
  (map #(vector (- (first %) (* (first core) (calculate-points-potential (last %) (last core) distance-fn beta))) (last %)) potentials))


(defn reject-core [potentials rejected-point]
  (map #(if (= rejected-point %) (list 0 (last rejected-point)) %) potentials))


(defn get-cluster-cores [points distance-fn]
  (let [radius-a 3
        radius-b (* radius-a 1.5)
        alpha (/ 4 (Math/pow radius-a 2))
        beta (/ 4 (Math/pow radius-b 2))
        upper-threshold 0.5
        lower-threshold 0.15
        potentials (get-potentials points distance-fn alpha)
        first-core (apply max-key first potentials)
        first-core-potential (first first-core)]

    (loop [potentials (update-potentials potentials first-core distance-fn beta)
           cores (list (last first-core))]
      (let [new-core (apply max-key first potentials)
            new-core-potential (first new-core)
            new-core-point (last new-core)]
        (cond
         (> new-core-potential (* upper-threshold first-core-potential))
           (recur (update-potentials potentials new-core distance-fn beta) (conj cores new-core-point))
         (< new-core-potential (* lower-threshold first-core-potential)) cores
         :else
           (let [dmin (apply min (map #(distance-fn new-core-point %) cores))]
             (if (>= (+ (/ dmin radius-a) (/ new-core-potential first-core-potential)) 1)
               (recur (update-potentials potentials new-core distance-fn beta) (conj cores new-core-point))
               (recur (reject-core potentials new-core) cores))))))))


(defn process-file [file-path distance-type]
  (let[
       file-data (parse-file file-path)
       cores (get-cluster-cores file-data (if (= distance-type "e") calculate-euclidean-distance calculate-hamming-distance))
       ]

    (println (string/join "\n" (map #(list (inc (.indexOf file-data %)) %) cores)))))


(defn -main [& args]
  (let [[opts args] (cli args ["-f" "--file"]
                              ["-d" "--distance"])]
  (process-file (:file opts) (:distance opts))))
