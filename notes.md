---
title: Notes for talk: *Fun with Hylomorphisms*
...

 <!-- References -->

[Origami programming]: http://www.cs.ox.ac.uk/publications/publication2335-abstract.html "paper by Jeremy Gibbons in The Fun of Programming"

 <!-- -->


Misc:

*   Explain names: "catamorphism", "anamorphism", "hylomorphism".
    Devote a slide with some graphics.
    Use down & up consistently through the talk.
*   Commutative diagrams.
*   "Forestation"
*   Derive `hylo` from `unfold` and `fold`.
*   Fundamentals:
    *   Functors
    *   Algebraic data types
    *   fixpoints and recursion

# Outline

*   Recursive functional programming:
    *   Lists:
        *   Data type
        *   Functions (folds): `length`, `sum`, `product`, `take`, `reverse` (quadratic & linear)
        *   What's in common? `fold`
        *   Functions (unfolds): `units :: Int -> [()]`, `range` (`enumFromTo`), `primes`, `zip`, `iterate`
        *   What's in common? `unfold`
    *   Trees:
        *   Data type
        *   Functions (folds): `size`, `sum`, `product`, `trim`, `reverse`
        *   What's in common? `fold`
        *   Functions (unfolds): `units :: Int -> T ()`, `range` (`enumFromTo`)
        *   What's in common? `unfold`
    *   General folds & unfolds
        *   "In a sense, recursive equations are the `assembly language' of functional programming, and direct recursion the `goto`." Advantages: "one can discover general properties of the abstraction once and for all, and infer those properties of the specific instances for free." (Jeremy Gibbons--*[Origami programming]*)
        *   What does recursion mean -- values?
            Recursive data types as function fixpoints.
        *   What does recursion mean -- types?
            Recursive data types as *functor* fixpoints.
            *   Lists
            *   Trees
        *   `fold` via `Fix`
            *   List `foldr` as `fold`
            *   Tree `foldr` as `fold`
        *   `unfold` via `Fix`
            *   List `unfoldr` as `unfold`
            *   Tree `unfoldr` as `unfold`
        *   Note the duality! (Reverse arrows.)
    *   Hidden folds and unfolds
        *   Much of the switch from imperative to functional programming is about replacing control structure by data structure.
            With `fold` and `unfold`, we can do so systematically and eliminate explicit recursion.
        *   Example: factorial via lists
        *   Example: factorial via trees
        *   General recipe
    *   Hylomorphisms
