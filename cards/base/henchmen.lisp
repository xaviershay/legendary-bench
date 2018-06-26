(make-henchmen "Hand Ninjas" 3 1
  (add-fight-effect "You get +1 Recruit" @(recruit 1)))

(make-henchmen "Sentinel" 3 1
  (add-fight-effect
    "KO one of your Heroes."
    @(must-choose-card
      "Choose a Hero"
      (heroes-player-has current-player)
      ko)
))
