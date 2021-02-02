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
  the two items from the list returned by parse(). Returns a lits of numbers that describes
  a player's cards."
  [s]
  (let [data (rest (clojure.string/split s #"\n"))]
    (map #(Integer. ^String %) data)))

(def player1-cards (gen-cards (first input)))
(def player2-cards (gen-cards (second input)))

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

(defn -main
  []
  (println (play-game1 player1-cards player2-cards)))
