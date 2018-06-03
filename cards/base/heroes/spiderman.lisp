(hero-set "Spider-Man" "Avengers"

  (make-hero "Astonishing Strength" "Strength" 2 5
      "Reveal top card of deck, if cost ≤ 2 then draw it."
      @(let [location (player-location current-player "Deck" 0)]
        (combine [
          (recruit 1)
          (reveal location)
          (guard (<= card-cost (card-at location))
            (move location (player-location current-player "Hand")))
        ])))

  (make-hero "Great Resonsibility" "Instinct" 2 5
      "Reveal top card of deck, if cost ≤ 2 then draw it."
      (attack 1))

)

(defn cards-for-current-player [location board] [])

(defn filter-by-hero-type [t cs] (filter (fn [c] (== (hero-type c) t)) cs)
;(defn filter-by-hero-type (filter #(== (hero-type %))

(hero-set "Captain America" "Avengers"

  (make-hero "A Day Unlike Any Other" "Covert" 7 1
      "You get +3 Attack for each other Avengers Hero you played this turn"
      (combine
        (attack 1)
        (attack (- (length (filter-by-hero-type "Avengers" (cards-for-current-player "Played"))) 1))
        ; (attack ((. (+ -1) length (filter-by-hero-type "Avengers") cards-for-current-player) "Played")
        ; (attack (comp (+ -1) length (filter-by-hero-type "Avengers") cards-for-current-player "Played")
      )

)

;(cons 1 Nil)
;(cons 1 (cons 2 (Nil))

;(def hero-name "S.H.E.I.L.D")
;(def hero-team "S.H.E.I.L.D")
;
;(make-hero "S.H.E.I.L.D Agent" :shield 0 0
;    ""
;    ["play" (recruit 1)])
;
;(make-hero "S.H.E.I.L.D Trooper" :shield 0 0
;    ""
;    ["play" (attack 1)])
