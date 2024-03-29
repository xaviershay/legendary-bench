Legendary Bench
===============

An experiment in implementing the Legendary ruleset. Very incomplete. Includes:

* A Haskell API server component in `api`. This is the
  most interesting part.
* A react frontend in `frontend`. This is pretty jank and
  currently mostly for ease of testing the API.

![Screenshot](https://raw.githubusercontent.com/xaviershay/legendary-bench/main/screenshot.png)

Development
-----------

Requires standard Haskell `stack` and `npm` development environments.

### Backend

    cd api
    stack ghci # choose legendary-bench:exe:legendary-bench-exe
    main

### Frontend

    npm start

Concepts
--------

### Actions and Choices

The state of the game is represented by the _board_. The only way the board can
be modified is by applying an _action_. An action is able to _halt_ application
if it is unable to complete due to insufficient player _choices_ (i.e. the
player hasn't selected a card yet). When an action halts, it provides a
_resume_ action to be stored in the board. When the set of player choices
changes, the action can be resumed by applying the saved action.

Perhaps surprisingly, there is no way to apply an action directly via the API!
The entire game is modeled as a sequence of actions that halt when player
action is needed. Applying the intial _prepare game_ action will halt on the
first need for player choice, and all subsequence resumes will also halt until
the game terminates (win/loss/draw).

Actions are [monoidal][1], meaning that two actions can always be combined into
a single one using the `ActionCombine` action. When applying a combined action,
if the left side halts then its resume action is _wrapped_ such that the right
hand side will be applied after it is successfully resumed. In this way, long
action chains can easily be constructed and will behave as expected.

### Cards

The core engine code knows nothing about any specific cards in the game. They
are implemented using a custom LISP-like language in the `cards` directory
(customizable with `CARDS_LANG` env var). For kicks, this language also
features static type inference and checking.

    (make-hero "Unstoppable Hulk" "Instinct" 4 5
      "You may KO a Wound from your hand or discard pile. If you do, you get +2 Attack."
      (.
        (add-attack-plus 2)
        (add-play-effect
          @(choose-card
            "Choose a wound from hand or discard to KO"
            (filter
              is-wound
              (concat-map cards-at-current-player-location ["Hand" "Discard"])
            )
            (fn [card] (combine (ko card) (attack 2)))
            noop)
          )))

    (make-hero "Hey, Can I Get a Do-Over?" "Instinct" 3 3
      "If this is the first Hero you played this turn, you may discard the rest of your hand and draw four cards."
      (.
        (add-attack 2)
        (add-play-effect
          @(if
            (== [current-card] (cards-at-current-player-location "Played"))
            (choose-yesno "Discard your hand?"
              (combine
                (discard-hand current-player)
                (draw 4))
              noop
              )
            noop
            )
          )))

There's no really great reason for this I just thought it would be fun. Which
it was.

[1]: http://hackage.haskell.org/package/base-4.11.1.0/docs/Data-Monoid.html
