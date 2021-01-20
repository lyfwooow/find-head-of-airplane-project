# Final Project

For the final project, you must work alone. You will design a video
game in ISL+λ. This must be a big-bang game in the same style as the
falling game from assignment 4 (but not, obviously, the falling game).

## Proposal (due during class on Nov 11, worth 5 points)

For your proposal, prepare two games that you would be willing to
implement.  Almost any game is acceptable, but you _may not_ choose
(any variation of) the snake game. You also may not choose any
variant of the fallers game. Find a new game.

For inspiration, check out http://www.gamedesign.jp/. See also [Chat
Noir](https://docs.racket-lang.org/games/chat-noir.html) in the PLT
Games app that comes with Racket.

Pick games that you think you'll enjoy playing and sharing with your
friends and family.

For each game, be sure you know how to play it and then write a
*brief* description of how it plays. Just a paragraph or two is fine.

Note that no two students may choose the same game; after the proposal
class meeting, the students and instructors will make sure everyone
has one game that they are actually going to implement and that no two
people have the same game.

## Specification (workshop during class on Nov 16th, worth 5 points)

The specification of your game must list at least 8 functional
requirements that your game will satisfy when it is complete. These
are things that the game does, explained as clearly and concisely as
you can. The number 8 is not a hard and fast rule as combining two
requirements into one or splitting one requirement into three isn't
difficult. Instead, think that you should try to find 8 different
aspects of the game. In your feedback for the proposal, we will let
you know if we agree that the game is complex enough and we will let
you know if the description contains enough detail.

For example, for the faller game, you might write these requirements:

> 1. Objects fall from the top of the screen.
>
> 2. The user controls a continuously moving paddle at the bottom of the
>    screen by hitting a key to change its direction.
>
> 3. A score is maintained and displayed.
>
> 4. Whenever the user changes directions of the paddle, the score
>    decrements (but does not go negative).
>
> 5. Whenever an object strikes the paddle, it disappears and the score
>    increases by 10.
>
> 6. Objects that are directly above the paddle (en route to hit it) are
>    displayed differently than objects that aren't.
>
> 7. A special kind of object, displayed differently, causes the paddle
>    to increase in size when caught.
>
> 8. A special kind of object, displayed differently, causes the paddle
>    to decrease in size when caught.

Be prepared to show your list of 8 items and discuss them with the
class -- we may ask for changes as we discuss them.

## Data Definition (due in private meetings on Nov 19th)

Your data definition fleshes out your specification. It must contain:

1. The data definition for your world and all its parts.

2. A wishlist of functions that you expect you will need to write,
   each with a signature and purpose. (We expect this to be at least
   10 functions, but if you think carefully, you may have many
   more. Some will be obvious helpers for others. No trivial
   functions, however.)

The data definition meeting is your chance to get feedback on the
essentials of your design before you actually write lots of
code. Because your instructors are experienced designers, we are likely
to have suggestions for you that will improve your design. If you
think carefully about the data definitions and get feedback on them,
it will be valuable to you and will save you more trouble later.

Feel free to implement a few functions as a way to test out your data
definitions; the better quality data definitions you provide at this
stage the better feedback you will get and the less time the next step
will take.

Before the meeting, push a file that has a clear and accurate listing
of all of the elements of the specification (in a comment) and the
data definition. This commit will affect the grading of the next
section.

## Code (due on Dec 3rd, worth 75 points)

You should have a complete working version of the game by the final
project presentations on Dec 3rd. (Even if you end up presenting on
Dec 4th your code is still due on Dec 3rd.)

If you ask for a code review before Nov 30th, you will get one with a
2 day turnaround on the code. No code reviews may be requested after
Nov 30th. A code review is not required.

Of the 75 points, 50 points will be awarded based on your
specification. For each item in your specification, you must point to
a test case (or test cases) that demonstrate that the game supports
that item and you must describe how, when one plays the game, to
experience that specification item. The 50 points will be evenly
divided among the specification items, awarding an equal number for
each specification item that was correctly implemented.

The remaining 25 points will be awarded based on following the design
recipe. That is, each place in the code where the design recipe was
not followed will result in deductions, up to a maximum of 25 points
off.

## Presentation (on Dec 3rd and 4th, worth 5 points)

Each student will present their game for about 10 minutes during the
final exam week, time *TBA*. Divide the time into two parts, a demo
of your game and some highlights of the code.

- For the game demo, use your implementation and play it, narrating
  what's happening. If necessary, use a slide or two to introduce the
  game.

- For the code, prepare slides that contain specific parts of your
  code, starting with your world data definition (and other
  supplementary data definitions). Connect the different aspects of
  the data to the demo you gave (possibly including screenshots to
  clarify connections). If they are especially important or
  interesting, show us the signature/purpose/header for a few of the
  most interesting functions. Use nicely formatting slides for any code
  you show us, don't just scroll around in DrRacket.
