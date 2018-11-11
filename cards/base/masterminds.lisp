(make-mastermind
  "Dr. Doom"
  "Doombot Legion" 9 5
  (.
    (add-master-strike
      "Each player with exactly 6 cards in hand reveals a Tech Hero or puts 2 cards from their hand on top of their deck."
       @(let [
        ps (filter (fn [player] (== 6 (length (cards-at (player-location player "Hand"))))) all-players)
        action (fn [player]
          (player-choose-card player
            "Choose |tech| Hero to reveal."
            (filter (is-type "Tech") (cards-at (player-location player "Hand")))
            (fn [card] (reveal card))
            (let [
              hand (cards-at (player-location player "Hand"))
              put-on-deck (fn [card] (combine
                (hide card)
                (move-top card (player-location player "Deck"))))
              ]

              (player-must-choose-card player
                "Choose first card to put on top of deck."
                hand
                (fn [first-card]
                  (player-must-choose-card player
                    "Choose second card to put on top of deck."
                    (filter (. not (== first-card)) hand)
                    (fn [second-card]
                      (combine
                        (put-on-deck first-card)
                        (put-on-deck second-card)
                    ))
                    ))))

          ))
        ]
        (concurrently (map action ps))))

    (add-tactic "Dark Technology" "You may recruit a |tech| or |ranged| Hero from the HQ for free."
      @(choose-card
         "Choose a |tech| or |ranged| Hero from HQ to recruit."
         (let
           [hq (cards-at (location "HQ"))]
           (concat [
             (filter (is-type "Ranged") hq)
             (filter (is-type "Tech") hq)
           ]))
         gain
         noop
        ))

    (add-tactic "Monarch's Decree" "Choose one: each other player draws a card or each other player discards a card."
      @(let [ps (filter (. not (== current-player)) all-players)]
        (must-choose [
          (choice "Each other player draws a card"
            (concurrently (map (fn [pid] (draw-player pid 1)) ps)))
          (choice "Each other player discards a card"
            (concurrently (map player-must-discard ps)))
        ])))

    (add-tactic "Secrets of Time Travel" "Take another turn after this one."
      @(add-turn current-player))
    (add-tactic "Treasures of Latveria" "When you draw a new hand of cards at the end of this turn, draw three extra cards."
      @(at-end-step (draw 3)))
  )
)

(make-mastermind
  "Loki"
  "Enemies of Asgard" 10 5
  (.
    (add-master-strike
      "Each player reveals a Strength Hero or gains a Wound"
      @(let [
       action (fn [player]
         (player-choose-card player
           "Choose |strength| Hero to reveal."
           (filter (is-type "Strength") (cards-at (player-location player "Hand")))
           (fn [card] (reveal card))
           (player-gain-wound player 1)))
       ]

       (concurrently (map action all-players))))

    (add-tactic "Cruel Ruler" "Defeat a Villain in the City for free."
      @(choose-card
        "Choose a Villian"
        (concat-map villains-at city-locations)
        defeat
        noop))

    (add-tactic "Maniacal Tyrant" "KO up to four cards from your discard pile."
      @(choose-upto 4
         "Choose a card to KO from discard"
         (cards-at-current-player-location "Discard")
         (ko)))

    (add-tactic "Vanishing Illusions" "Each other player KOs a Villain from their Victory Pile."
      @(let [
          ps (filter (. not (== current-player)) all-players)
          action (fn [player]
            (player-must-choose-card player
              "Choose a Villain from Victory Pile to KO."
              (villains-at (player-location player "Victory"))
              (ko)))
          ]

         (concurrently (map action ps))))

    (add-tactic "Whispers and Lies" "Each other player KOs two Bystanders from their Victory Pile."
      @(let [
          ps (filter (. not (== current-player)) all-players)
          action (fn [player]
                     (apply-with combine noop
                       ((.
                         (map ko)
                         (take 2)
                         (filter is-bystander)
                         cards-at
                       ) (player-location player "Victory"))))
          ]

         (apply-with combine noop (map action ps))))
  ))

(make-mastermind
  "Magneto"
  "Brotherhood" 8 5
  (.
    (add-master-strike
    ; TODO
      "Each player reveals an X-Men Hero or discards down to four cards."
      @(noop))

    (add-tactic "Bitter Captor" "Recruit an X-Men Hero from the HQ for free."
      @(choose-card
         "Choose an |x-men| Hero from HQ to recruit."
         (filter (is-team "X-Men") (cards-at (location "HQ")))
         gain
         noop
        ))
    (add-tactic "Crushing Shockwave" "Each other player reveals an X-Men Hero or gains two Wounds."
      @(let [
          ps (filter (. not (== current-player)) all-players)
           action (fn [player]
             (player-choose-card player
               "Choose |x-men| Hero to reveal."
               (filter (is-team "X-Men") (cards-at (player-location player "Hand")))
               reveal
               (player-gain-wound player 2)))
           ]

       (concurrently (map action ps))))
    ; TODO
    (add-tactic "Electromagnetic Bubble" "Choose one of your X-Men Heroes. When you draw a new hand of cards at the end of this turn, add that Hero to your hand as a seventh card." @(noop))
    (add-tactic "Xavier's Nemesis" "For each of your X-Men Heroes, rescue a Bystander."
      @((. rescue-bystander length (filter (is-team "X-Men")) (concat-map cards-at-current-player-location)) ["Hand" "Played"]))
  ))

(make-mastermind
  "Red Skull"
  "HYDRA" 7 5
  (.
    (add-master-strike
      "Each player KOs a Hero from their hand."
      @(let [
         ps all-players
         action (fn [player]
           (player-must-choose-card player
             "Choose Hero to KO."
             (heroes-at (player-location player "Hand"))
             ko))
         ]
         (concurrently (map action ps))))

    (add-tactic "Endless Resources" "You get +4 Recruit"
      @(recruit 4))
    (add-tactic "HYDRA Conspiracy" "Draw two cards. Then draw another card for each HYDRA Villain in your Victory Pile."
      ; TODO
      @(combine (draw 2) noop))

    (add-tactic "Negablast Grenades" "You get +3 Attack."
      @(attack 3))
    ; TODO
    (add-tactic "Ruthless Dictator" "Look at the top three cards of your deck. KO one, discard one and put one back on top of your deck." @(noop))
  ))
