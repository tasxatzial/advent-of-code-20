(ns day21.core
  (:gen-class))

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
    [known-ingredients unknown-ingredients]))

(def parsed-input (map parse-line input))

(defn -main
  []
  (println parsed-input))
