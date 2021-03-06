;; --- Day 9: Stream Processing ---

;; A large stream blocks your path. According to the locals, it's not
;; safe to cross the stream at the moment because it's full of
;; garbage. You look down at the stream; rather than water, you
;; discover that it's a stream of characters.

;; You sit for a while and record part of the stream (your puzzle
;; input). The characters represent groups - sequences that begin with
;; { and end with }. Within a group, there are zero or more other
;; things, separated by commas: either another group or garbage. Since
;; groups can contain other groups, a } only closes the
;; most-recently-opened unclosed group - that is, they are
;; nestable. Your puzzle input represents a single, large group which
;; itself contains many smaller ones.

;; Sometimes, instead of a group, you will find garbage. Garbage
;; begins with < and ends with >. Between those angle brackets, almost
;; any character can appear, including { and }. Within garbage, < has
;; no special meaning.

;; In a futile attempt to clean up the garbage, some program has
;; canceled some of the characters within it using !: inside garbage,
;; any character that comes after ! should be ignored, including <, >,
;; and even another !.

;; You don't see any characters that deviate from these rules. Outside
;; garbage, you only find well-formed groups, and garbage always
;; terminates according to the rules above.

;; Here are some self-contained pieces of garbage:

;; <>, empty garbage.
;; <random characters>, garbage containing random characters.
;; <<<<>, because the extra < are ignored.
;; <{!>}>, because the first > is canceled.
;; <!!>, because the second ! is canceled, allowing the > to terminate the garbage.
;; <!!!>>, because the second ! and the first > are canceled.
;; <{o"i!a,<{i<a>, which ends at the first >.
;; Here are some examples of whole streams and the number of groups they contain:

;; {}, 1 group.
;; {{{}}}, 3 groups.
;; {{},{}}, also 3 groups.
;; {{{},{},{{}}}}, 6 groups.
;; {<{},{},{{}}>}, 1 group (which itself contains garbage).
;; {<a>,<a>,<a>,<a>}, 1 group.
;; {{<a>},{<a>},{<a>},{<a>}}, 5 groups.
;; {{<!>},{<!>},{<!>},{<a>}}, 2 groups (since all but the last > are canceled).

;; Your goal is to find the total score for all groups in your
;; input. Each group is assigned a score which is one more than the
;; score of the group that immediately contains it. (The outermost
;; group gets a score of 1.)

;; What is the total score for all groups in your input?

(ns advent2017.day9
  (:require
   [clojure.test :refer [deftest is are]]))

(defn parse [s]
  (first
   (reduce (fn [[acc level garbage? cancelled?] x]
             (if garbage?
               (if cancelled?
                 [acc level garbage? false]
                 (case x
                   \! [acc level true true]
                   \> [acc level false false]
                   [acc level true false]))
               (case x
                 \{ [acc (inc level) false false]
                 \} [(conj acc level) (dec level) false false]
                 \< [acc level true false]
                 [acc level false false])))
           [[] 0 false false]
           s)))

(defn solve1 [in]
  (->> in (parse) (reduce +)))

(deftest one
  (are [s score] (= (solve1 s) score)
    "{}" 1
    "{{{}}}" 6
    "{{},{}}" 5
    "{{{},{},{{}}}}" 16
    "{<a>,<a>,<a>,<a>}" 1
    "{{<ab>},{<ab>},{<ab>},{<ab>}}" 9
    "{{<!!>},{<!!>},{<!!>},{<!!>}}" 9
    "{{<a!>},{<a!>},{<a!>},{<ab>}}" 3))

(def input (slurp "resources/day9.txt"))

(comment

  (solve1 input) ;=> 12505

  )

;; --- Part Two ---

;; Now, you're ready to remove the garbage.

;; To prove you've removed it, you need to count all of the characters
;; within the garbage. The leading and trailing < and > don't count,
;; nor do any canceled characters or the ! doing the canceling.

;; How many non-canceled characters are within the garbage in your puzzle input?

(defn solve2 [s]
  (first
   (reduce (fn [[acc garbage? cancelled?] x]
             (if garbage?
               (if cancelled?
                 [acc garbage? false]
                 (case x
                   \! [acc true true]
                   \> [acc false false]
                   [(inc acc) true false]))
               (if (= \< x)
                 [acc true false]
                 [acc false false])))
           [0 false false]
           s)))

(deftest two
  (are [s characters] (= (solve2 s) characters)
    "<>" 0
    "<random characters>" 17
    "<<<<>" 3
    "<{!>}>" 2
    "<!!>" 0
    "<!!!>>" 0
    "<{o\"i!a,<{i<a>" 10))

(comment

  (solve2 input) ;=> 6671

  )
