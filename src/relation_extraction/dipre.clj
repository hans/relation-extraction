(ns relation-extraction.dipre
  (:require [clojure.string :as s]))

(def test-corpus
  (str "<b>John Hemingway</b> wrote <b>Of Mice and Men</b>. "
       "before <b>John Hemingway</b> wrote <b>Of Mice and Men</b>. "
       "<b>Mark Twain</b> wrote <b>Huckleberry Finn</b>. "
       "when <b>Mark Twain</b> was writing <b>Huckleberry Finn</b>. "
       "<b>Huckleberry Finn</b>, written by <b>Mark Twain</b>, ."
       "when <b>Dan Brown</b> was writing <b>The Da Vinci Code</b>. "
       "<b>The Stranger</b>, written by <b>Albert Camus</b>, ."))

(def test-seeds
  '(["John Hemingway" "Of Mice and Men"]))

(def instance-regex
  ;; TODO might be better to anchor regex on the input string (the first
  ;; %s) and search backwards after an actual search is found
  "(?:[.!?]|^)([^.!?]*?)%s([^.!?]+?)%s([^.!?]*)")

(defn find-instances-for-seed
  "Find instances of the given seed in a corpus. Returns 3-tuples of the form

      (prefix, middle, suffix)"
  [corpus seed]

  (let [re-pattern (re-pattern (apply format instance-regex seed))]
    (map rest (re-seq re-pattern corpus))))

(defn find-instances
  "Find instances of the given seeds in a corpus. Returns 3-tuples
  of the form

      (prefix, middle, suffix)"
  [corpus seeds]

  (mapcat (partial find-instances-for-seed corpus) seeds))

(defn has-initial
  "Return true if the given string has the given prefix."
  [str initial]

  (and (>= (count str) (count initial))
     (= (.substring str 0 (count initial)) initial)))

(defn has-terminal
  "Return true if the given string has the given ending."
  [str terminal]

  (and (>= (count str) (count terminal))
     (= (.substring str (- (count str) (count terminal))) terminal)))

(defn find-longest-initial
  "Find the longest common string initial in a set of strings."
  [strs]

  (loop [str (first strs)
         strs-rest (rest strs)]
    (cond
     (empty? str) ""
     (every? #(has-initial % str) strs-rest) str
     :else (recur (.substring str 0 (dec (count str))) strs-rest))))

(defn find-longest-terminal
  ;; TODO: Support finding multiple terminators
  "Find the longest common string terminator in a set of strings."
  [strs]

  (loop [str (first strs)
         strs-rest (rest strs)]
    (cond
     (empty? str) ""
     (every? #(has-terminal % str) strs-rest) str
     :else (recur (.substring str 1) strs-rest))))

(defn find-patterns
  "Extract common patterns from the selected instances. Returns a list
  of 3-tuples of the form

      (prefix, middle, suffix)"
  [instances]

  (let [;; Group by middle content
        grouped-middle (group-by second instances)]
    (for [[middle tuples] grouped-middle

          :let [prefix (find-longest-terminal (map first tuples))
                suffix (find-longest-initial (map #(nth % 2) tuples))]

          :when (not (or (empty? prefix) (empty? suffix)))]
      [(s/triml prefix) middle (s/trimr suffix)])))

(def seed-regex
  "%s([^.!?]+)%s([^.!?]+)%s")

(defn generate-seeds-for-pattern
  "Draw seeds for the given pattern from a corpus. Returns a collection of
  pairs of the form ent1, ent2."
  [corpus pattern]

  (let [pattern-re (re-pattern (apply format seed-regex pattern))]
    (set (map rest (re-seq pattern-re corpus)))))

(defn generate-seeds
  "Generate new seeds from an improved pattern list and the given corpus."
  [corpus patterns]
  (mapcat (partial generate-seeds-for-pattern corpus) patterns))

(defn run-iteration
  "Run a single iteration of the DIPRE algorithm given seed relation tuples."
  ;; TODO: shouldn't accept the entire corpus as a string.. :)
  ;; TODO: uniquify
  [corpus seeds]

  (let [instances (find-instances corpus seeds)
        patterns (find-patterns instances)
        seeds (generate-seeds corpus patterns)]
    seeds))
