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
