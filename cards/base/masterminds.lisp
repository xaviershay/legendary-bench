(make-mastermind
  "Dr. Doom"
  "Doombot Legion" 9 5
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
      (concurrently (map action ps)))))
