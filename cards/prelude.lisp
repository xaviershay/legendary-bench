(def choice tuple)
(defn id [x] x)
(defn is-odd [x] (== 1 (mod x 2)))

(defn drop [n xs]
  (if (<= n 0) xs (drop (- n 1) (tail xs))))

(defn map [f xs] (reduce (fn [a x] (concat [a [(f x)]])) [] xs))
(defn filter [f xs] (reduce
                      (fn [a x] (concat [a (if (f x) [x] [])]))
                      []
                      xs))
(defn length [xs] (reduce (fn [a x] (add 1 a)) 0 xs))
(defn take [n xs]
  (if (<= (length xs) n) xs (concat [[(head xs)] (take (- n 1) (tail xs))])))
(defn any [f] (. (< 0) length (filter f)))
(defn concat-map [f] (. concat (map f)))
(defn empty [xs] (== 0 (length xs)))
(defn apply-with [f default xs] (reduce (fn [a x] (f a x)) default xs))
(defn apply [f xs] (apply-with f (head xs) (tail xs)))
(defn replicate [n x] (if (== 0 n) [] (concat [[x] (replicate (- n 1) x)])))

(defn guard [cond action] (if cond action noop))
(defn cards-at-current-player-location [scope]
  (cards-at (player-location current-player scope)))
(defn is-type [t c] (== t (card-type c)))
(defn is-team [t c] (== t (card-team c)))
(defn played [type] ((.
                       (any (is-type type))
                       (filter (. not (== current-card)))
                       cards-at
                     ) (player-location current-player "Played")))
(defn cards-player-has [p]
  (concat-map (. cards-at (player-location p)) ["Played" "Hand"]))
(defn heroes-player-has [p]
  (concat-map (. heroes-at (player-location p)) ["Played" "Hand"]))

(def player-must-discard @(fn [player]
  (player-choose-card player
    "Choose a card in hand to discard"
    (cards-at (player-location player "Hand"))
    discard
    noop)))

(defn choose-upto [n desc cards f]
  (if (== n 0)
    noop
    (choose-card
      desc
      cards
      (fn [card] (combine
        (f card)
        (choose-upto (- n 1) desc (filter (. not (== card)) cards) f)))
      noop
   )))
