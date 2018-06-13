(defn id [x] x)

(defn drop [n xs] (if (<= n 0) xs (drop (- n 1) (tail xs))))
(defn map [f xs] (reduce (fn [a x] (concat [a [(f x)]])) [] xs))
(defn filter [f xs] (reduce
                      (fn [a x] (concat [a (if (f x) [x] [])]))
                      []
                      xs))
(defn length [xs] (reduce (fn [a x] (add 1 a)) 0 xs))
(defn any [f] (. (< 0) length (filter f)))
(defn concat-map [f] (. concat (map f)))

(defn guard [cond action] (if cond action noop))
(defn cards-at-current-player-location [scope]
  (cards-at (player-location current-player scope)))
(defn is-type [t c] (== t (card-type c)))
(defn is-team [t c] (== t (card-team c)))
(defn played [type]
  ((. (any (is-type type)) cards-at) (player-location current-player "Played")))
(defn cards-player-has [p] (concat-map (. cards-at (player-location current-player)) ["Played" "Hand"]))
