(ns day04.core
  (:gen-class))

; ---------------------------------------
; common

(def input-file "resources\\input.txt")

(def required-keys '(:byr :iyr :eyr :hgt :hcl :ecl :pid))

(defn parse
  "Splits the input string by \n\n and then splits again by space.
  Returns a list of vectors, each element in the vector is
  one string field key:value of the passport."
  [s]
  (let [password-strings (clojure.string/split s #"\n\n")]
    (map #(clojure.string/split % #"[ \n]") password-strings)))

(defn get-passport-field
  "Creates a {key value} map from a passport string of key:value."
  [x]
  (let [[key val] (clojure.string/split x #":")]
    {(keyword key) val}))

(defn create-passport
  "Creates a passport map from a passport string. A passport map
  is the collection of all its key, values."
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
  "Checks if a passport has all required keys. A passport is a map
  of key, values as returned from create-passport()."
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
  [color-string]
  (let [leading-hash (take 1 color-string)
        color-value (drop 1 color-string)]
    (and (boolean (seq leading-hash))
         (boolean (seq color-value))
         (= "#" (apply str leading-hash))
         (= 6 (count color-value))
         (every? true? (map #(or (and (>= (int %) (int '\0)) (<= (int %) (int '\9)))
                                 (and (>= (int %) (int '\a)) (<= (int %) (int '\f))))
                            (map char color-value))))))

(defn isEyeColorValid?
  "Checks if the eye color is valid."
  [color-string]
  (contains? eye-colors color-string))

(defn isPassportIDValid?
  "Checks if the passport id is valid."
  [pid-string]
  (= 9 (count (filter true? (map #(and (>= (int %) (int '\0)) (<= (int %) (int '\9)))
                                 (map char pid-string))))))

(def validity-functions
  {:byr isBirthYearValid?
   :iyr isIssueYearValid?
   :eyr isExpirationYearValid?
   :hgt isHeightValid?
   :hcl isHairColorValid?
   :ecl isEyeColorValid?
   :pid isPassportIDValid?})

(defn isFieldValid?
  "Checks if the value of a required key is a valid passport value."
  [key passport]
  ((get validity-functions key) (passport key)))

(defn isPassportValid2?
  "Checks if a passport string for problem2 is valid. Should be called
  only if isPassportValid1() returns true."
  [passport]
  (every? true? (map #(isFieldValid? % passport)
                     required-keys)))

; ---------------------------------------
; results

(defn day04-1
  []
  (count (filter true? (map isPassportValid1? passports))))

(defn day04-2
  []
  (count (filter true? (map #(and (isPassportValid1? %)
                                  (isPassportValid2? %))
                            passports))))

(defn -main
  []
  (println (day04-1))
  (println (day04-2)))
