(ns life
  (:use clojure.test))

(defn alive [] :alive)
(defn dead [] :dead)
(defn alive? [cell] (= (alive) cell))
(defn dead? [cell] (= (dead) cell))

(declare neighbours)

(defn next-gen [grid cell]
  (let [num-alive (count (filter alive? (neighbours grid)))]
    (cond
        (< num-alive 2) (dead)
        (= num-alive 2) (if (alive? cell) (alive) (dead))
        (= num-alive 3) (alive)
        :else (dead))))

(deftest succession
  (let [alive-neighbours (fn [num] (concat (replicate num (alive))
                                           (replicate (- 8 num) (dead))))]
    (testing "no neighbours"
      (binding [neighbours (fn [_] (alive-neighbours 0))]
        (is (dead? (next-gen nil (dead))))
        (is (dead? (next-gen nil (alive))))))
    (testing "one neighbour"
      (binding [neighbours (fn [_] (alive-neighbours 1))]
        (is (dead? (next-gen nil (dead))))
        (is (dead? (next-gen nil (alive))))))
    (testing "two neighbours"
      (binding [neighbours (fn [_] (alive-neighbours 2))]
        (is (dead? (next-gen nil (dead))))
        (is (alive? (next-gen nil (alive))))))
    (testing "three neighbours"
      (binding [neighbours (fn [_] (alive-neighbours 3))]
        (is (alive? (next-gen nil (dead))))
        (is (alive? (next-gen nil (alive))))))
    (testing "four neighbours"
      (binding [neighbours (fn [_] (alive-neighbours 4))]
        (is (dead? (next-gen nil (dead))))
        (is (dead? (next-gen nil (alive))))))
    (testing "five neighbours"
      (binding [neighbours (fn [_] (alive-neighbours 5))]
        (is (dead? (next-gen nil (dead))))
        (is (dead? (next-gen nil (alive))))))))

(run-tests)
