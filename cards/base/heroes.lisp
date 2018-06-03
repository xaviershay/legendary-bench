(defn make-hero [name type cost amount desc post]
  (make-hero-full "Spider-Man" "Spider Friends" name type cost amount desc post))

(make-hero "Astonishing Strength" "Strength" 2 5
    "Reveal top card of deck, if cost ≤ 2 then draw it."
    (add-play-effect @(attack 1))
    )

(make-hero "Great Resonsibility" "Instinct" 2 5
    "Reveal top card of deck, if cost ≤ 2 then draw it."
    (add-play-effect @(recruit 1))
    )

(defn make-hero [name type cost amount desc post]
  (make-hero-full "Captain America" "Avengers" name type cost amount desc post))

(make-hero "Diving Block" "Tech" 6 3
  "If you would gain a Wound, you may reveal this card and draw a card instead."
  (add-play-effect @(attack 4))
;  {
;    "play" (attack 4)
;    "gain-card" @(fn [continue player source card]
;                   (if (and (== player (owner self)) (is-wound card))
;                     (choose-yesno "Reveal Diving Block instead of gaining wound?"
;                       (append [(reveal self) (draw (owner self) 1)])
;                       continue)))
;  }
)
;
;  (make-hero "Astonishing Strength" "Strength" 2 5
;      "Reveal top card of deck, if cost ≤ 2 then draw it."
;      @(let [location (player-location current-player "Deck" 0)]
;        (append [
;          (recruit 1)
;          (reveal location)
;          (guard (<= card-cost (card-at location))
;            (move location (player-location current-player "Hand")))
;        ])))

;  (make-hero "Great Resonsibility" "Instinct" 2 5
;      "Reveal top card of deck, if cost ≤ 2 then draw it."
;      (attack 1))

;(hero-set "Spider-Man" "Avengers"
;
;;  (make-hero "Astonishing Strength" "Strength" 2 5
;;      "Reveal top card of deck, if cost ≤ 2 then draw it."
;;      @(let [location (player-location current-player "Deck" 0)]
;;        (append [
;;          (recruit 1)
;;          (reveal location)
;;          (guard (<= card-cost (card-at location))
;;            (move location (player-location current-player "Hand")))
;;        ])))
;
;  (make-hero "Great Resonsibility" "Instinct" 2 5
;      "Reveal top card of deck, if cost ≤ 2 then draw it."
;      (attack 1))
;
;)

;(defn cards-for-current-player [location board] [])
;
;(defn filter-by-hero-type [t cs] (filter (fn [c] (== (hero-type c) t)) cs)
;;(defn filter-by-hero-type (filter #(== (hero-type %))
;
;(hero-set "Captain America" "Avengers"
;  (make-hero "Diving Block" "Tech" 6 3
;    "If you would gain a Wound, you may reveal this card and draw a card instead."
;    {
;      "play" (attack 4)
;      "gain-card" @(fn [continue player source card]
;                     (if (and (== player (owner self)) (is-wound card))
;                       (choose-yesno "Reveal Diving Block instead of gaining wound?"
;                         (append [(reveal self) (draw (owner self) 1)])
;                         continue)))
;    }
;  )
;
;  (make-hero "A Day Unlike Any Other" "Covert" 7 1
;      "You get +3 Attack for each other Avengers Hero you played this turn"
;      (append
;        (attack 1)
;        (attack (- (length (filter-by-hero-type "Avengers" (cards-for-current-player "Played"))) 1))
;        ; (attack ((. (+ -1) length (filter-by-hero-type "Avengers") cards-for-current-player) "Played")
;        ; (attack (comp (+ -1) length (filter-by-hero-type "Avengers") cards-for-current-player "Played")
;      ))
;
;)
;
;(hero-set "Black Widow" "Avengers")
;
;(make-hero
;  "Dangerous Rescue" "Covert" 3 5
;  "You may KO a card from your hand or discard pile. If you do, rescue a Bystander."
;  (append
;    (attack 2)
;    (choose-card
;      "Choose a card from hand or discard to KO"
;      (concat-map cards-for-current-player ["Hand" "Discard"])
;      #(append (ko %) (rescue-bystander 1)))))
;
;(make-hero
;  "Mission Accomplished" "Tech" 2 5
;  "Draw a card.\n|tech|: Rescue a Bystander."
;  (append
;    (draw 1)
;    (guard (played "Tech") rescue-bystander)))
;
;(hero-set "Cyclops" "X-Men")
;
;(make-hero "Determination" "Strength" 2 5
;  "To play this card, you must discard a card from your hand."
;  {
;    "play"     @(recruit 3)
;    "play-pre" @(fn [continue]
;                    (choose-card
;                      "Choose a card in hand to discard"
;                      (cards-for-current-player "Hand")
;                      #(append (discard %) continue)
;                      noop))
;  }
;)
;
;(make-hero "Unending Energy" "Ranged" 6 3
;  "If a card effect makes you discard this card, you may return this card to your hand."
;  {
;    "play"             @(attack 4)
;    "effect-discarded" @(#(choose-yesno "Return Unending Energy to your hand?"
;                            (move % (player-location (owner %) "Hand"))
;                            noop))
;  }
;)
;
;;(cons 1 Nil)
;;(cons 1 (cons 2 (Nil))
;
;;(def hero-name "S.H.E.I.L.D")
;;(def hero-team "S.H.E.I.L.D")
;;
;;(make-hero "S.H.E.I.L.D Agent" :shield 0 0
;;    ""
;;    ["play" (recruit 1)])
;;
;;(make-hero "S.H.E.I.L.D Trooper" :shield 0 0
;;    ""
;;    ["play" (attack 1)])
