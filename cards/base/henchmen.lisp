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
