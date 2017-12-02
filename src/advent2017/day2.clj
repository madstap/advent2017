(ns advent2017.day2
  (:require
   [clojure.test :refer [deftest is are run-all-tests]]
   [clojure.string :as str]
   [madstap.comfy :as comfy]))

(defn parse [s]
  (keep (fn [row]
          (not-empty
           (keep comfy/str->int (str/split row #"\s"))))
        (str/split-lines s)))

(def input (parse (slurp "resources/day2.txt")))

(defn checksum [spreadsheet]
  (transduce (map #(- (apply max %) (apply min %))) + spreadsheet))

(def example ""
  "
5 1 9 5
7 5 3
2 4 6 8")

(deftest foo
  (is (= 18 (checksum (parse example)))))

(comment

  (checksum input) ;=> 47136

  (run-all-tests)

  )
