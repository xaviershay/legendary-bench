Legendary Bench
===============

An experiment in implementing the Legendary ruleset. Very incomplete. Includes:

* A Haskell API server component in `api`. This is the
  most interesting part.
* A react frontend in `frontend`. This is pretty jank and
  currently mostly for ease of testing the API.

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

The state of the game is represented by the _board_. The
only way the board can be modified is by applying an
_action_. An action is able to _halt_ application if it is unable to complete
due to insufficient player _choices_ (i.e. the player hasn't selected a card
yet). When an action halts, it provides a _resume_ action to be stored in the
board. When the set of player choices changes, the action can be resumed by
applying the saved action.

Perhaps surprisingly, there is no way to apply an action
directly via the API! The entire game is modeled as a
sequence of actions that halt when player action is
needed. Applying the intial _prepare game_ action will
halt on the first need for player choice, and all
subsequence resumes will also halt until the game
terminates (win/loss/draw).

Actions are [monoidal][1], meaning that two actions can always
be combined into a single one using the `ActionCombine`
action. When applying a combined action, if the left side
halts then its resume action is _wrapped_ such that the
right hand side will be applied after it is successfully
resumed. In this way, long action chains can easily be
constructed and will behave as expected.

[1]: http://hackage.haskell.org/package/base-4.11.1.0/docs/Data-Monoid.html
