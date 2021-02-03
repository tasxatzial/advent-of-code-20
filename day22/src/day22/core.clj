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

(defn play-game2
  "Plays the game according to problem 2 rules. Returns a vector of the winning player id
  (1 or 2) and the winning card list."
  ([]
   (play-game2 [1 (list player1-cards)] [2 (list player2-cards)]))
  ([p1 p2]
   (let [all-p1-cards (second p1)
         all-p2-cards (second p2)
         p1-cur-cards (first all-p1-cards)
         p2-cur-cards (first all-p2-cards)]
     (cond
       (empty? p1-cur-cards) [2 p2-cur-cards]
       (empty? p2-cur-cards) [1 p1-cur-cards]
       :else (if (same-configuration? p1-cur-cards (rest all-p1-cards) p2-cur-cards (rest all-p2-cards))
               [1 p1-cur-cards]
               (let [p1-play (first p1-cur-cards)
                     p2-play (first p2-cur-cards)]
                 (if (and (>= (dec (count p1-cur-cards)) p1-play)
                          (>= (dec (count p2-cur-cards)) p2-play))
                   (let [[win-player-id _] (play-game2 [1 (list (take p1-play (rest p1-cur-cards)))]
                                                       [2 (list (take p2-play (rest p2-cur-cards)))])]
                     (if (= 1 win-player-id)
                       (let [p1-new-cards (concat (rest p1-cur-cards) (list p1-play p2-play))
                             p2-new-cards (rest p2-cur-cards)]
                         (recur [1 (conj all-p1-cards p1-new-cards)] [2 (conj all-p2-cards p2-new-cards)]))
                       (let [p2-new-cards (concat (rest p2-cur-cards) (list p2-play p1-play))
                             p1-new-cards (rest p1-cur-cards)]
                         (recur [1 (conj all-p1-cards p1-new-cards)] [2 (conj all-p2-cards p2-new-cards)]))))
                   (if (> p1-play p2-play)
                     (let [p1-new-cards (concat (rest p1-cur-cards) (list p1-play p2-play))
                           p2-new-cards (rest p2-cur-cards)]
                       (recur [1 (conj all-p1-cards p1-new-cards)] [2 (conj all-p2-cards p2-new-cards)]))
                     (let [p2-new-cards (concat (rest p2-cur-cards) (list p2-play p1-play))
                           p1-new-cards (rest p1-cur-cards)]
                       (recur [1 (conj all-p1-cards p1-new-cards)] [2 (conj all-p2-cards p2-new-cards)]))))))))))

; ---------------------------------------
; results

(defn day22-1
  []
  (calculate-score (play-game1 player1-cards player2-cards)))

(defn day22-2
  []
  (calculate-score (second (play-game2))))

(defn -main
  []
  (println (day22-1))                                       ;36257
  (println (day22-2)))                                      ;33304
