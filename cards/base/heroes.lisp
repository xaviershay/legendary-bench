(hero-set "S.H.E.I.L.D" "")

(make-hero "S.H.E.I.L.D Agent" "Shield" 0 0
    ""
    (add-play-effect @(recruit 1)))

(make-hero "S.H.E.I.L.D Trooper" "Shield" 0 0
    ""
    (add-play-effect @(attack 1)))

(hero-set "Spider-Man" "Spider Friends")

(defn spiderman-action [custom]
  (let [location (card-location (player-location current-player "Deck") 0)]
    (combine
      custom
      (reveal location)
      (guard (<= (card-cost location) 2) (draw 1)))))

(make-hero "Astonishing Strength" "Strength" 2 5
    "Reveal top card of deck, if cost ≤ 2 then draw it."
    (add-play-effect @(spiderman-action (attack 1))))

(make-hero "Great Resonsibility" "Instinct" 2 5
    "Reveal top card of deck, if cost ≤ 2 then draw it."
    (add-play-effect @(spiderman-action (recruit 1))))

(make-hero "Web Shooters" "Instinct" 2 5
    "Rescue a Bystander.\nReveal top card of deck, if cost ≤ 2 then draw it."
    (add-play-effect @(spiderman-action (rescue-bystander 1))))

(hero-set "Black Widow" "Avengers")

(make-hero
  "Dangerous Rescue" "Covert" 3 5
 "You may KO a card from your hand or discard pile. If you do, rescue a Bystander."
  (add-play-effect
    @(combine
       (attack 2)
       (choose-card
         "Choose a card from hand or discard to KO"
         (concat-map cards-at-current-player-location ["Hand" "Discard"])
         (fn [card] (combine (ko card) (rescue-bystander 1)))
         noop)
       )))
;
(make-hero
  "Mission Accomplished" "Tech" 2 5
  "Draw a card.\n|tech|: Rescue a Bystander."
  (add-play-effect @(combine
    (draw 1)
    (guard (played "Tech") (rescue-bystander 1)))))

(hero-set "Captain America" "Avengers")

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

(make-hero "A Day Unlike Any Other" "Covert" 7 1
    "You get +3 Attack for each other Avengers Hero you played this turn"
    (add-play-effect @(combine
      (attack 1)
      (attack ((. length (filter (is-team "Avengers")) cards-at-current-player-location) "Played"))
    )))

(hero-set "Cyclops" "X-Men")

(make-hero "Determination" "Strength" 2 5
  "To play this card, you must discard a card from your hand."
  (.
    (add-play-effect @(recruit 3))
    (add-play-guard
      @(fn [continue] (choose-card
                        "Choose a card in hand to discard"
                        (cards-at-current-player-location "Hand")
                        (fn [card] (combine (discard card) continue))
                        noop)))
  ))

(make-hero "Unending Energy" "Ranged" 6 3
  "If a card effect makes you discard this card, you may return this card to your hand."
  (.
    (add-play-effect @(attack 4))
    (add-discarded-effect @(fn [self]
                              (choose-yesno "Return Unending Energy to your hand?"
                                (move self (player-location (card-owner self) "Hand"))
                                noop)))
  ))
