# Dotty encoding of the WYNTKAY Functional Pearl
## That is: **What you need to know about Yoneda** [Profunctor Optics and the Yoneda Lemma]

The project is a [Dotty](https://dotty.epfl.ch/) implementation of the concepts described in the [paper](http://www.cs.ox.ac.uk/publications/publication12072-abstract.html) from *G.Boisseau and J.Gibbons*

The paper explains the concepts and application of the Yoneda Lemma and its haskell encoding.
It also deteails how it can be used to describe Profunctor definitions of Optics (Lens, Adapters, Prisms and more) in Categorical terms.

It's based on a `sbt` g8 template.

---
### Usage

This is a normal sbt project, you can compile code with `sbt compile` and run it
with `sbt run`, `sbt console` will start a Dotty REPL.

For more information on the sbt-dotty plugin, see the
[dotty-example-project](https://github.com/lampepfl/dotty-example-project/blob/master/README.md).
