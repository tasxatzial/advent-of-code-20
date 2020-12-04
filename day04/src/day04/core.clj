(ns day04.core
  (:gen-class))

; ---------------------------------------
; common

(def input-file "resources\\input.txt")

(def required-keys '(:byr :iyr :eyr :hgt :hcl :ecl :pid))

(defn parse
  "Splits the input string by \n\n and then splits again by space.
  Returns a list of vectors, each element in the vector is
  one field (key:value) of the passport."
  [s]
  (let [password-strings (clojure.string/split s #"\n\n")]
    (map #(clojure.string/split % #"[ \n]") password-strings)))

(defn get-passport-field
  "Creates a (key value) map from a passport field."
  [x]
  (let [[key val] (clojure.string/split x #":")]
    {(keyword key) val}))

(defn create-passport
  "Creates a passport map from a passport string."
  [passport]
  (reduce (fn [passport field]
            (into passport (get-passport-field field)))
          {}
          passport))

; put all passports in a list
(def passports (map create-passport (parse (slurp input-file))))

; ---------------------------------------
; problem 1

(defn hasRequiredKeys?
  [passport]
  (every? true? (map #(contains? passport %) required-keys)))

(defn isPassportValid1?
  [passport]
  (hasRequiredKeys? passport))

; ---------------------------------------
; problem 2

(def eye-colors #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})

(defn str->int
  "Converts a string to integer."
  [^String s]
  (Integer. s))

(defn isBirthYearValid?
  "Checks if the birth year is valid."
  [year-string]
  (let [int-year (str->int year-string)]
    (and (>= int-year 1920) (<= int-year 2002))))

(defn isIssueYearValid?
  "Checks if the issue year is valid."
  [year-string]
  (let [int-year (str->int year-string)]
    (and (>= int-year 2010) (<= int-year 2020))))

(defn isExpirationYearValid?
  "Checks if the expiration year is valid."
  [year-string]
  (let [int-year (str->int year-string)]
    (and (>= int-year 2020) (<= int-year 2030))))

(defn isHeightValid?
  "Checks if the height is valid."
  [height-string]
  (let [units (take-last 2 height-string)
        height (drop-last 2 height-string)]
    (if (and (seq units) (seq height))
      (let [parsed-units (apply str units)
            parsed-height (str->int (apply str height))]
        (or (and (= "cm" parsed-units) (>= parsed-height 150) (<= parsed-height 193))
            (and (= "in" parsed-units) (>= parsed-height 59) (<= parsed-height 76))))
      false)))

(defn isHairColorValid?
  "Checks if the hair color is valid."
  [color]
  (let [leading-hash (take 1 color)
        color-value (drop 1 color)]
    (and (boolean (seq leading-hash))
         (boolean (seq color-value))
         (= "#" (apply str leading-hash))
         (= 6 (count color-value))
         (every? true? (map #(or (and (>= (int %) (int '\0)) (<= (int %) (int '\9)))
                                 (and (>= (int %) (int '\a)) (<= (int %) (int '\f))))
                            (map char color-value))))))

(defn isEyeColorValid?
  "Checks if the eye color is valid."
  [color]
  (contains? eye-colors color))

; ---------------------------------------
; results

(def day04-1
  (count (filter true? (map isPassportValid1? passports))))

(defn -main
  []
  (println day04-1))                                        ;216
