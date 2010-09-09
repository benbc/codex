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

(defn alive-neighbours [num]
  (concat (replicate num (alive)) (replicate (- 8 num) (dead))))

(deftest dies-with-no-neighbours
  (binding [neighbours (fn [_] (alive-neighbours 0))]
    (is (dead? (next-gen nil (alive))))))

(deftest succession
  (let [make-test (fn [[num-alive start expected]]
                    (binding [neighbours (fn [_] (alive-neighbours num-alive))]
                      (is (expected (next-gen nil start)))))]
    (doseq [spec [[0 (dead)  dead?]
                  [0 (alive) dead?]
                  [1 (dead)  dead?]
                  [1 (alive) dead?]
                  [2 (dead)  dead?]
                  [2 (alive) alive?]
                  [3 (dead)  alive?]
                  [3 (alive) alive?]
                  [4 (dead)  dead?]
                  [4 (alive) dead?]
                  [5 (dead)  dead?]
                  [5 (alive) dead?]
                  [8 (dead)  dead?]
                  [8 (alive) dead?]]]
      (make-test spec))))

(run-tests)
