(ns day07.core
  (:gen-class))

; ---------------------------------------
; common

(def input-file "resources\\input.txt")

(defn str->int
  "Converts a string to integer."
  [^String s]
  (Integer. s))

(defn parse
  "Splits the input string by \n."
  [input]
  (clojure.string/split input #"\n"))

(def parsed-input (parse (slurp input-file)))

(defn create-bag-count
  "Creates a map from a string that has one of the two formats:
  '5 muted yellow bags' returns {:mutedyellow 5}
  'no other bags' returns {}."
  [s]
  (let [split-s (clojure.string/split s #" ")]
    (if (= (first split-s) "no")
      {}
      {(keyword (str (nth split-s 1) (nth split-s 2)))
       (str->int (nth split-s 0))})))

(defn inner-outer-split
  "Splits a rule into a list of two strings, first one describes
  the outer bag, second one describes the inner bags."
  [rule]
  (clojure.string/split rule #"contain "))

(defn create-outer-bag
  "Creates a keyword that describes the outer bag using the result of
  inner-outer-split()."
  [inner-outer-split]
  (let [s-outer (first inner-outer-split)
        s-outer-bag (clojure.string/split s-outer #" ")]
    (keyword (str (nth s-outer-bag 0) (nth s-outer-bag 1)))))

(defn create-inner-bags
  "Creates a map of {:bag_type count} that describes the inner bags using
  the result of inner-outer-split()."
  [inner-outer-split]
  (let [s-inner (second inner-outer-split)
        s-inner-bags (clojure.string/split s-inner #", ")]
    (reduce (fn [result s-inner-bag]
              (into result (create-bag-count s-inner-bag)))
            {}
            s-inner-bags)))

(defn parse-rule
  "Parses the result of inner-outer-split() into a map that describes both
  the inner and outer bags."
  [inner-outer-split]
  {(create-outer-bag inner-outer-split) (create-inner-bags inner-outer-split)})

; Creates a map of {:outer_bag_type {:inner_bag_type1 count1 :inner_bag_type2 count2 ...}}
(def parsed-rules
  (reduce (fn [result rule]
            (into result (parse-rule (inner-outer-split rule))))
          {}
          parsed-input))

; ---------------------------------------
; problem 1

; remove the shiny gold bag rules from map of final rules
(def rules-without-shinygold (dissoc parsed-rules :shinygold))

(declare inner-bags-contain-shinygold?)

(defn bag-contains-shinygold?
  "Returns true if a bag key can contain the shiny gold key."
  [bag-key]
  (let [inner-bags (bag-key rules-without-shinygold)]
    (if (empty? inner-bags)
      false
      (if (contains? inner-bags :shinygold)
        true
        (inner-bags-contain-shinygold? inner-bags)))))

(defn inner-bags-contain-shinygold?
  "Returns true if the value (inner bags) of a bag key can contain the
  shiny gold key."
  [inner-bags]
  (if (empty? inner-bags)
    false
    (let [first-bag (first inner-bags)
          bag-key (first first-bag)]
      (if (bag-contains-shinygold? bag-key)
        true
        (inner-bags-contain-shinygold? (rest inner-bags))))))

(defn -main
  []
  (println rules-without-shinygold))