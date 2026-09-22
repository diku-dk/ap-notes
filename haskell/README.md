# Haskell code

This directory contains instructive Haskell code that is included
directly into the book.

## Principles

The following principles govern the construction of this code and should be
followed whenever making new contributions. Since the purpose of the code is to
explain concepts, rather than to write real software, it has some oddities
compared to "real" Haskell code. The ideal (which we may or may not reach) is
that we minimise the number and power of (language) concepts we introduce, but
we try to explain them in depth. Some of the principles conflict with each
other, so we have to use our judgment when they conflict.

0. *Every line is teaching material*. We should respect the students' time, and
   not have anything that is not useful to them.

1. If in doubt, follow the style of the existing code base. It may not be
   perfect, but if we change it, we should change it comprehensively, rather
   than making it internally incongruent.

2. Minimise usage of Haskell features. We use Haskell to demonstrate concepts,
   but our goal is not to teach Haskell as a language beyond what is necessary
   to demonstrate the concepts as code.

   a. In particular, avoid clever use of laziness. We cannot ignore it in all
      cases (do not use `Strict`), but we should try to steer around cases where it
      appears in a nontrivial way.

   2. Ideally, use no Haskell language extensions. Exceptions *can* be made, but
      consider it extremely carefully, and it is almost always better to change
      the didactic approach than to enable an extension.

3. It is OK to discuss use of real Haskell libraries pertinent to the material
   as it can help convince the reader that these concepts are not just academic
   toys, but we should minimise how much of the API surface we use. Examples of
   libraries we use:

   1. `megaparsec`, but only in a fairly shallow way, and we still use `String`
      instead of `Text`. The goal of this is to show students a real parser
      combinator library, and to get better error messages when students screw
      up their grammars.

   2. `QuickCheck`, but again we are careful not to use too much of the API - we
      make basic use of generators and the `Arbitrary` type class.

   3. `tasty` as overall test harness, including its adaptors `tasty-hunit` and
      `tasty-quickcheck`. The harness itself is unrelated to any course learning
      goal.

   Instead of using some advanced library feature, it may be better to use just
   the basic library features and show how complex stuff can be built on top.

4. Accept Haskell details that we may not think are didactically optimal, but
   are inescapable as long as we use Haskell. An example of the
   `Functor`/`Applicative`/`Monad` hierarchy, and how the various monadic
   operations (which are our main interest) are distributed among them. Another
   is the somewhat odd numeric hierarchy. We do not go "against the grain" of
   the language as it just creates unnecessary friction. This also means we say
   `pure` instead of `return`. In practice we just do not emphasize
   `Applicative` that much, and once we are done introducing it, we use
   boilerplate definitions for its instances based on the "real work" done for
   `Functor` and `Monad` (chapter 2 covers this).
