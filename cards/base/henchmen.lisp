(make-henchmen "Doombot Legion" 3 1
  (.
    (add-fight-effect "Look at the top two cards of your deck."
      @(let
        [
          topcard (card-location (player-location current-player "Deck") 0)
          dest    (player-location current-player "Working")
        ]
        (apply combine (replicate 2 (combine
          (reveal-to-owner topcard)
          (move topcard dest))))))
    (add-fight-effect "KO one of them and put the other back."
      @(let
         [
            working (player-location current-player "Working")
            deck    (player-location current-player "Deck")
         ]
         (must-choose-card
           "Choose a card to KO"
           (cards-at working)
           (fn [card] (combine
             (ko card)
             (hide (card-location working 0))
             (move-top (card-location working 0) deck))))))
))

(make-henchmen "Hand Ninjas" 3 1
  (add-fight-effect "You get +1 Recruit" @(recruit 1)))

(make-henchmen "Savage Land Mutates" 3 1
  (add-fight-effect "When you draw a new hand of cards at the end of this turn, draw an extra card."
    @(at-end-step (draw 1))
))

(make-henchmen "Sentinel" 3 1
  (add-fight-effect
    "KO one of your Heroes."
    @(must-choose-card
      "Choose a Hero"
      (heroes-player-has current-player)
      ko)
))
