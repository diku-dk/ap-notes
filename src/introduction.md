# Introduction

These notes serve as material for the *Advanced Programming* (AP)
course at [DIKU](https://diku.dk). They do not form a complete
textbook and may not be comprehensible outside the context of the
course. In particular, they assume that basic Haskell programming
skills are acquired through other means, for example via textbooks
such as [Programming in
Haskell](https://www.cs.nott.ac.uk/~pszgmh/pih.html). The notes serve
to emphasize the course perspectives, as well as contain material that
we could not find of sufficient quality and brevity elsewhere.

The text will contain many links to other resources online. Unless
explicitly indicated, you can consider these to be supplementary,
rather than required reading.

## Course Principles

Although the [Course
Description](https://kurser.ku.dk/course/ndaa09013u/2024-2025)
contains a rather dry list of learning goals, you may also benefit
from keeping the following *principles* in mind when perusing the
course material. They reflect the philosophy behind the course, and
our rationale for picking the material and constructing the
assignments.

In AP, we study programming methodologies based on the following
principles.

* **Precision**. Code should clearly specify what it does and does not
  do; largely accomplished through the use of advanced type systems
  and type-directed design. This is why we use Haskell as our language
  in the course.

* **Separation of concerns**. It is a fairly mainstream point of view
  that modular programs, separated into independent units with minimal
  functional overlap and interdependence, are easier to write and
  maintain. By making use of programming techniques that provide
  precision, we can ensure and verify that our programs are structured
  thus. Monadic programming is one particularly clear example of this
  principle, and the one that is our principal object of study, which
  separates the *use* of effects from the *definitions* of effects.

* **Principled design**. By structuring programs along rigorously
  defined abstractions, such as monads or similar effect boundaries,
  we can develop principles for systems design that are both simple
  and effective, and elegantly support features such as resilience,
  that can often become quite messy.

We demonstrate these principles through code written in the Haskell
programming language, but they are language-agnostic, and can be
applied in any language (although often in more awkward forms, for
languages that do not provide the requisite flexibility of
expression).

While AP is a course focused on practical programming, the time
constraints of a seven week course means that we cannot directly study
the large programs where these techniques are the most valuable.
Instead our examples, exercises, and assignments will be small "toy
programs" that cut any unnecessary detail or functionality in order to
focus on the essential principles. This does not mean that the
techniques we teach in AP to not scale up to large programs; merely
that we do not have time for you to observe it for yourselves. You
will just have to use your imagination.

## Why so many interpreters?

Many of the examples, exercises, and assignments in AP will be in the
context of writing interpreters, type checkers, or parsers for
programming languages. This is not *solely* because the teachers
happen to enjoy this aspect of computer science, but rather because
these domains contain the essence of problems that occur frequently in
real-world programming.

* **Interpretation** is about making operational decisions and
  changing state based on program input.

* **Type checking** is input validation.

* **Parsing** is recovering structured information from unstructured
  input.

For didactical reasons, AP mostly focuses on problems that
*exclusively* contains these problems (and little "business logic"),
but the ideas we study are applicable even outside the rather narrow
niche of implementing programming languages.
