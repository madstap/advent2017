;; --- Day 4: High-Entropy Passphrases ---

;; A new system policy has been put in place that requires all
;; accounts to use a passphrase instead of simply a password. A
;; passphrase consists of a series of words (lowercase letters)
;; separated by spaces.

;; To ensure security, a valid passphrase must contain no duplicate words.

;; For example:

;; aa bb cc dd ee is valid.
;; aa bb cc dd aa is not valid - the word aa appears more than once.
;; aa bb cc dd aaa is valid - aa and aaa count as different words.

;; The system's full passphrase list is available as your puzzle input.
;; How many passphrases are valid?

(ns advent2017.day4
  (:require
   [clojure.string :as str]))

(defn parse [s]
  (map #(str/split % #"\s+")
       (str/split-lines s)))

(def input (parse (slurp "resources/day4.txt")))

(defn valid? [passphrase]
  (= #{1} (-> passphrase (frequencies) (vals) (set))))

(defn solve1 [in]
  (count (filter valid? in)))

(comment

  (solve1 input) ;=> 383

  )
