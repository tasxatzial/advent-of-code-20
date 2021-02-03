(ns day22.core
  (:gen-class))

; ---------------------------------------
; common

(def input-file "resources\\input.txt")

(defn parse
  "Splits the input string by \n\n"
  [s]
  (clojure.string/split s #"\n\n"))

(def input (parse (slurp input-file)))

(defn gen-cards
  "Accepts a string describing either player1 data or player2 data. This string is one of
  the two items from the list returned by parse(). Returns a list of numbers that describes
  a player's cards."
  [s]
  (let [data (rest (clojure.string/split s #"\n"))]
    (map #(Integer. ^String %) data)))

(def player1-cards (gen-cards (first input)))
(def player2-cards (gen-cards (second input)))

(defn calculate-score
  "Calculates the score of a list of cards."
  [cards]
  (let [card-values (range (count cards) 0 -1)
        scores (map #(* %1 %2) card-values cards)]
    (apply + scores)))

; ---------------------------------------
; problem 1

(defn play-game1
  "Plays the game according to problem 1 rules. Returns the winning card list."
  [player1-cards player2-cards]
  (cond
    (empty? player1-cards) player2-cards
    (empty? player2-cards) player1-cards
    :else (let [player1-play (first player1-cards)
                player2-play (first player2-cards)]
            (if (> player1-play player2-play)
              (let [player1-new-cards (concat (rest player1-cards) (list player1-play player2-play))
                    player2-new-cards (rest player2-cards)]
                (recur player1-new-cards player2-new-cards))
              (let [player2-new-cards (concat (rest player2-cards) (list player2-play player1-play))
                    player1-new-cards (rest player1-cards)]
                (recur player1-new-cards player2-new-cards))))))

; ---------------------------------------
; problem 2

(defn same-configuration?
  "Returns true if p1-cur-cards & p2-cur-cards are equal to the i-th
  item in p1-rest-cards & p2-rest-cards respectively"
  [p1-cur-cards p1-rest-cards p2-cur-cards p2-rest-cards]
  (if (empty? p1-rest-cards)
    false
    (let [p1-cards (first p1-rest-cards)
          p2-cards (first p2-rest-cards)]
      (if (and (= p1-cards p1-cur-cards) (= p2-cards p2-cur-cards))
        true
        (recur p1-cur-cards (rest p1-rest-cards) p2-cur-cards (rest p2-rest-cards))))))

; ---------------------------------------
; results

(defn day22-1
  []
  (calculate-score (play-game1 player1-cards player2-cards)))

(defn -main
  []
  (println (day22-1)))
