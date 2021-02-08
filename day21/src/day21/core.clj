(ns day21.core
  (:require [clojure.set])
  (:gen-class))

; ---------------------------------------
; common

(def input-file "resources\\input.txt")

(defn parse
  "Splits the input string by \n."
  [s]
  (clojure.string/split s #"\n"))

(def input (parse (slurp input-file)))

(defn parse-line
  "Parses an input line and returns a vector of two items.
  First item is a list of the known ingredients. Second item is a vector
  of the unknown ingredients."
  [line]
  (let [ingredients (clojure.string/split line #"[(|)]")
        unknown-ingredients (clojure.string/split (first ingredients) #" ")
        known-ingredients (->> (clojure.string/split (second ingredients) #"[,| ]")
                               (filter (comp boolean seq) )
                               rest)]
    [known-ingredients (set unknown-ingredients)]))

(def parsed-input (map parse-line input))

; ---------------------------------------
; problem 1

(defn update-possible-ingredients
  "First argument is a vector of two items: a list of known allergens and a set of
  all ingredients (for a food). Second argument is a map of {allergen -> all possible
  ingredients} for each known allergen.
  Returns a new map of {allergen -> all ingredients} for each known allergen."
  [[known-allergens unknown-allergens] allergen-map]
  (reduce (fn [result allergen]
            (let [current-unknown (get allergen-map allergen)]
              (if current-unknown
                (assoc result allergen (clojure.set/intersection current-unknown unknown-allergens))
                (assoc result allergen unknown-allergens))))
          {}
          known-allergens))

(defn update-all-possible-ingredients
  "Returns a map of {allergen -> possible ingredients} for every known allergen."
  []
  (reduce (fn [result [allergen ingredients]]
            (let [new-ingredients (update-possible-ingredients [allergen ingredients] result)]
              (into result new-ingredients)))
          {}
          parsed-input))

(def memoized-update-all-possible-ingredients (memoize update-all-possible-ingredients))

(defn collect-possible-allergen-ingredients
  "Returns a set of possible ingredients that could be allergens."
  []
  (let [allergens-map (memoized-update-all-possible-ingredients)]
    (reduce into (map #(second %) allergens-map))))

(defn count-non-allergens
  "Returns the total number of ingredients from the food that cannot be allergens."
  []
  (let [possible-allergens (collect-possible-allergen-ingredients)]
    (reduce (fn [result [_ unknown-allergens]]
              (let [non-allergens (clojure.set/difference unknown-allergens possible-allergens)]
                (+ result (count non-allergens))))
            0
            parsed-input)))

; ---------------------------------------
; problem 2

(defn figure-allergens
  "Returns a vector of vectors. Each vector contains the allergen name and the
  corresponding ingredient."
  [possible-ingredients]
  (loop [result []
         ingredients possible-ingredients]
    (if (empty? ingredients)
      result
      (let [identified (filter #(= 1 (count (second %))) ingredients)
            identified-ingredients (reduce into (map second identified))
            new-result (reduce (fn [result [allergen ingredient]]
                                      (assoc result allergen (first ingredient)))
                                    {}
                                    identified)
            unidentified (filter #(not= 1 (count (second %))) ingredients)
            clear-identified #(vector (first %) (clojure.set/difference (second %) identified-ingredients))
            new-ingredients (map clear-identified unidentified)]
        (recur (into result new-result) new-ingredients)))))

; ---------------------------------------
; results

(defn day21-1
  []
  (count-non-allergens))

(defn -main
  []
  (println (day21-1))                                       ;1958
  (println (figure-allergens (memoized-update-all-possible-ingredients))))
