;; --- Day 3: Spiral Memory ---

;; You come across an experimental new kind of memory stored on an
;; infinite two-dimensional grid.

;; Each square on the grid is allocated in a spiral pattern starting
;; at a location marked 1 and then counting up while spiraling
;; outward. For example, the first few squares are allocated like
;; this:

;; 17  16  15  14  13
;; 18   5   4   3  12
;; 19   6   1   2  11
;; 20   7   8   9  10
;; 21  22  23---> ...

;; While this is very space-efficient (no squares are skipped),
;; requested data must be carried back to square 1 (the location of
;; the only access port for this memory system) by programs that can
;; only move up, down, left, or right. They always take the shortest
;; path: the Manhattan Distance between the location of the data and
;; square 1.

;; For example:

;; Data from square 1 is carried 0 steps, since it's at the access port.
;; Data from square 12 is carried 3 steps, such as: down, left, left.
;; Data from square 23 is carried only 2 steps: up twice.
;; Data from square 1024 must be carried 31 steps.

;; How many steps are required to carry the data from the square
;; identified in your puzzle input all the way to the access port?

(ns advent2017.day3
  (:require
   [clojure.test :refer [deftest is are run-all-tests]]
   [clojure.spec.alpha :as s]
   [medley.core :as medley]))

(s/def ::coords
  (s/cat :x int?, :y int?))

(defn manhattan-distance
  [from to]
  (->> (map - from to) (map medley/abs) (apply +)))

(def odds
  "Sequence of the positive odd numbers."
  (iterate (partial + 2) 1))

(defn square [x] (* x x))

(defn step
  [[x y] dir]
  (case dir
    :left  [(dec x) y]
    :right [(inc x) y]
    :up    [x (inc y)]
    :down  [x (dec y)]))

(defn backwards-square
  "Given a coordinate in the lower right corner, returns a sequence of
  the coordinates around the perimeter (decreasing, starting at coords)"
  [coords]
  (butlast
   (reductions (fn [c dir]
                 (step c dir))
               coords
               (mapcat (partial repeat (* 2 (first coords))) [:left :up :right :down]))))

(defn spiral-coord
  "Returns the grid coordinate which contains the nth memory address."
  [n]
  (let [[c num] (medley/find-first (fn [[i x]] (>= (square x) n))
                                   (medley/indexed odds))]
    (nth (backwards-square [c (- c)])
         (- (square num) n))))

(defn solve1 [in]
  (manhattan-distance [0 0] (spiral-coord in)))

(def input 368078)

(deftest one
  (are [n steps] (= (solve1 n) steps)
    1 0
    12 3
    23 2
    1024 31))

(comment

  (run-all-tests #"day3")

  (solve1 input) ;=> 371

  )

;; --- Part Two ---

;; As a stress test on the system, the programs here clear the grid
;; and then store the value 1 in square 1. Then, in the same
;; allocation order as shown above, they store the sum of the values
;; in all adjacent squares, including diagonals.

;; So, the first few squares' values are chosen as follows:

;; Square 1 starts with the value 1.
;; Square 2 has only one adjacent filled square (with value 1), so it also stores 1.
;; Square 3 has both of the above squares as neighbors and stores the sum of their values, 2.
;; Square 4 has all three of the aforementioned squares as neighbors and stores the sum of their values, 4.
;; Square 5 only has the first and fourth squares as neighbors, so it gets the value 5.
;; Once a square is written, its value does not change. Therefore, the first few squares would receive the following values:

;; 147  142  133  122   59
;; 304    5    4    2   57
;; 330   10    1    1   54
;; 351   11   23   25   26
;; 362  747  806--->   ...

;; What is the first value written that is larger than your puzzle input?

(defn neighbors [[x y]]
  #{[(dec x) y] [(inc x) y] [(inc x) (inc y)] [(inc x) (dec y)]
    [x (dec y)] [x (inc y)] [(dec x) (dec y)] [(dec x) (inc y)]})

(def spiral
  "A sequence of the coordinates in the spiral."
  (cons [0 0] (mapcat (fn [n]
                        (reverse (backwards-square [n (- n)])))
                      (rest (range)))))

(defn solve2 [in]
  (reduce (fn [memory coords]
            (let [v (apply + (keep memory (neighbors coords)))]
              (if (> v in)
                (reduced v)
                (assoc memory coords v))))
          {[0 0] 1}
          (rest spiral)))

(deftest two
  (is (= [1 2 4 5 10 11 23 25 26 54 57 59 122 133 142 147 304 330 351 362 747 806]
         (take 22 (iterate solve2 1)))))

(comment

  (solve2 input) ;=> 369601

  )
