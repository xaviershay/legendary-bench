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

(make-hero "Web Shooters" "Tech" 2 3
    "Rescue a Bystander.\nReveal top card of deck, if cost ≤ 2 then draw it."
    (add-play-effect @(spiderman-action (rescue-bystander 1))))

(make-hero "The Amazing Spider-Man" "Covert" 2 1
    "Reveal the top three cards of your deck. Put any that cost 2 or less into your hand. Put the rest back in any order."
    (.
      (add-play-effect
        @(let
          [
            topcard (card-location (player-location current-player "Deck") 0)
            dest    (player-location current-player "Working")
          ]
          (apply combine (replicate 3 (combine
            (reveal topcard)
            (move topcard dest))))))
      (add-play-effect
        @(apply-with combine noop (
          (.
            (map (fn [card] (combine (reveal-to-owner card) (move-top card (player-location current-player "Hand")))))
            (filter (fn [card] (<= (card-cost card) 2))))
          (cards-at (player-location current-player "Working")))))
      ; At most the player will need to select 2 cards, so replicate this
      ; action that many times. It will noop if no selection is needed.
      (apply compose (replicate 2 (add-play-effect
        @(let
           [
             remaining (cards-at (player-location current-player "Working"))
             put-back (fn [card] (combine (hide card) (move-top card (player-location current-player "Deck"))))
           ]
           (if
             (<= (length remaining) 1)
             (apply-with combine noop (map put-back remaining))
             (must-choose-card "Choose card to put back on deck." remaining put-back))))))
))

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

(make-hero
  "Mission Accomplished" "Tech" 2 5
  "Draw a card.\n|tech|: Rescue a Bystander."
  (add-play-effect @(combine
    (draw 1)
    (guard (played "Tech") (rescue-bystander 1)))))

(make-hero
  "Covert Operation" "Covert" 4 3
  "You get +1 Attack for each Bystander in your Victory pile."
  (add-play-effect @(attack
    ((. length (filter is-bystander) cards-at (player-location current-player)) "Victory"))))

(make-hero
  "Silent Sniper" "Covert" 7 1
  "Defeat a Villain or Mastermind that has a Bystander."
  (add-play-effect @(combine
    (attack 4)
    (choose-card
      "Choose a Villian or Mastermind that has a Bystander"
      ((. (concat-map villians-at) (filter (. (any is-bystander) cards-at))) city-locations)
      defeat
      noop)
  )))

(hero-set "Captain America" "Avengers")

; TODO: Need to make this exclude wounds
(defn uniq-card-types [player] ((. length uniq (map card-type) cards-player-has) player))

(make-hero "Avengers Assemble!" "Instinct" 3 5
  "You get +1 Recruit for each color of Hero you have."
  (add-play-effect @(recruit (uniq-card-types current-player))))

(make-hero "Perfect Teamwork" "Strength" 4 5
  "You get +1 Attack for each color of Hero you have."
  (add-play-effect @(attack (uniq-card-types current-player))))

(make-hero "Diving Block" "Tech" 6 3
  "If you would gain a Wound, you may reveal this card and draw a card instead."
  (.
    (add-play-effect @(attack 4))
    ; TODO: This is untested and unimplemented. Do so when adding Hulk.
    ; Think through how to handle two of these cards in hand. Since replacement
    ; effect, should only be able to trigger one of them (right?).
    (add-gain-effect
      @(fn [continue player self card]
        (let [owning-player (card-owner self)]
          (if (and (== player owning-player) (is-wound card))
            (choose-yesno "Reveal Diving Block instead of gaining wound?"
              (combine (reveal self) (draw-player owning-player 1))
              continue) continue)))
    )))

(make-hero "A Day Unlike Any Other" "Covert" 7 1
    "You get +3 Attack for each other Avengers Hero you played this turn"
    (add-play-effect @(combine
      (attack 1)
      (attack ((. (* 3) length (filter (is-team "Avengers")) cards-at-current-player-location) "Played"))
    )))

(hero-set "Cyclops" "X-Men")

(def must-discard @(fn [continue] (choose-card
                        "Choose a card in hand to discard"
                        (cards-at-current-player-location "Hand")
                        (fn [card] (combine (discard card) continue))
                        noop)))

(make-hero "Determination" "Strength" 2 5
  "To play this card, you must discard a card from your hand."
  (.
    (add-play-effect @(recruit 3))
    (add-play-guard must-discard)
  ))

(make-hero "Optic Blast" "Ranged" 3 5
  "To play this card, you must discard a card from your hand."
  (.
    (add-play-effect @(attack 3))
    (add-play-guard must-discard)
  ))

(make-hero "Unending Energy" "Ranged" 6 3
  "If a card effect makes you discard this card, you may return this card to your hand."
  (.
    (add-play-effect @(attack 4))
    (add-discarded-effect
      @(fn [self]
        (choose-yesno "Return Unending Energy to your hand?"
          ; TODO: This relies on move sending the card to the back of the hand,
          ; and not changing the indices of existing cards, because the played
          ; card is still "on the stack" and hasn't been moved out of hand yet.
          ; Identifying cards by ID would be more reliable, but still need to
          ; be able to identify cards by index (in the case of unrevealed
          ; cards). OR as long as card IDs are recalculated whenever they are
          ; shuffled or hidden, that could work..
          ;
          ; In any case, need a way to easily create an integration test to
          ; ensure this behaviour keeps working.
          ;
          ; ACTUALLY: Based on FAQ, this effect should resolve _after_ the
          ; previous card's effect. To do that, pass in "continue" action and
          ; chain this one on to it. Avoids the above issue.
          ; ACTUALLY ACTUALLY: This is probably a replacement effect? Otherwise
          ; weird things can happen say if a card causes you to discard then
          ; draw, this card can "dissapper" if discard is shuffled back into
          ; deck.
          (move self (player-location (card-owner self) "Hand"))
          noop)))
  ))

(make-hero "X-Men United" "Ranged" 8 1
  "|x-men|: You get +2 Attack for each other X-Men Hero you played this turn."
  (add-play-effect @(attack ((. (* 2) length (filter (is-team "X-Men")) cards-at-current-player-location) "Played"))))

(hero-set "Deadpool" "")

(make-hero "Here, Hold This for a Second" "Tech" 3 5
  "A Villain of your choice captures a Bystander."
  (add-play-effect @(combine
    (recruit 2)
    (let [villians (concat-map villians-at city-locations)]
      (must-choose-card
        "Choose a Villian"
        (concat-map villians-at city-locations)
        (fn [card] (capture-bystander card 1)))
    ))))

(make-hero "Oddball" "Covert" 5 5
  "You get +1 Attack for each other Hero with an odd-numbered Cost you played this turn."
  (add-play-effect @(combine
    (attack 2)
    (attack ((. length (filter is-odd) (map card-cost)) (cards-at-current-player-location "Played")))
    )))

; TODO: Currently this discards the card being played also, since it isn't
; moved out of hand until after this action function evalutes. Consider whether
; a new location ("the stack") is needed when playing cards.
(defn discard-hand [player]
  (let [hand (player-location player "Hand")]
    (if (empty (cards-at hand))
      noop
      ((. (apply combine) (map discard) cards-at) hand)
                                )))

(make-hero "Hey, Can I Get a Do-Over?" "Instinct" 3 3
  "If this is the first Hero you played this turn, you may discard the rest of your hand and draw four cards."
  (add-play-effect @(combine
    (attack 2)
    (if
      ((. empty cards-at-current-player-location) "Played")
      (choose-yesno "Discard your hand?"
        (combine
          (discard-hand current-player)
          (draw 4))
        noop
        )
      noop
      )
    )))

(make-hero "Random Acts of Unkindness" "Instinct" 7 1
  "You may gain a Wound to your hand. Then each player passes a card from their hand to the player on their left."
  (.
    (add-play-effect @(attack 6))
    (add-play-effect @(choose-yesno "Gain wound to hand?"
                        (gain-wound-to (player-location current-player "Hand") 1)
                        noop))
    ; TODO: The way concurrently works, the chosen card can show up in
    ; someone's hand before they themselves have selected a card to pass. But
    ; they can't select it, because selection is limited to what was in their
    ; hand at the beginning of the effect! So it's a weird UI experience, but
    ; results in correct gameplay behaviour. To fix, consider making
    ; ActionConcurrent apply all-or-nothing rather than partial. This probably
    ; makes the most sense for villian escape discarding also.
    (add-play-effect @(concurrently (map (fn [player]
                        (player-must-choose-card player
                          "Choose card to pass to left"
                          (cards-at (player-location player "Hand"))
                          (fn [card] (move card (player-location (player-left player) "Hand"))))
                      ) all-players)))
  ))
