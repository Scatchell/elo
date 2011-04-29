(ns elo.core
  (:use [clojure.java.io :only [reader]]
        [clojure.string :only [split upper-case]]))

(def *k-factor* 32)

(def *starting-score* 1200)

(defn expected-result [a b]
  (/ 1 (+ 1 (Math/pow 10 (/ (- a b) 400)))))

(defn score-change [a b result]
  (* *k-factor* (- result (expected-result a b))))

(defn update-scores [scores {:keys [playerA playerB outcome]}]
  (let [a     (or (scores playerA) *starting-score*)
        b     (or (scores playerB) *starting-score*)
        delta (Math/round (score-change a b outcome))]
    (-> scores
        (assoc playerA (- a delta))
        (assoc playerB (+ b delta)))))

(defn to-result [str]
  (case (upper-case (or str "d"))
    "A" 0.0
    "B" 1.0
    0.5))

(defn game-seq [file]
  (let [lines (line-seq (reader file))
        keys  (map keyword (split (first lines) #"\s+"))]
    (for [line (rest lines)]
      (let [vals (split line #"\s+")]
        (update-in (zipmap keys vals) [:outcome] to-result)))))

(defn process-file [file k-factor starting-score]
  (binding [*k-factor*       k-factor
            *starting-score* starting-score]
    (reduce update-scores
            {}
            (game-seq file))))
