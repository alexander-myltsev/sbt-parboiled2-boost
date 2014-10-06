sbt-parboiled2-boost
====================

`sbt-parboiled2-boost` is a SBT plugin that works with entire AST of extended `parboiled2`-like grammar. It parses all rules of particular grammar, performs deep optimizations for all rules, and produces `parboiled2` Scala parsers.

Usage
-----

Extended `parobiled2` grammar for `ABC-parser` is as follows:

```
InputLine ::= &(A ~ "c") ~ oneOrMore("a") ~ B ~ !("a" | "b" | "c") ~ EOI
A: Rule0  ::= "a" ~ optional(A) ~ "b"
B: Rule0  ::= "b" ~ optional(B) ~ "c"
```

Source code should be placed in `$(sourceDirectory in Compile)/parboiled` folder. 

During compilation the plugin parses these sources, performs optimizations and places output code to `$(sourceDirectory in Compile)/scala` folder:

```scala
import org.parboiled2._

class ABCParser(val input: ParserInput) extends Parser {
  def InputLine = rule { &(A ~ "c") ~ oneOrMore("a") ~ B ~ !("a" | "b" | "c") ~ EOI }

  def A: Rule0 = rule { "a" ~ optional(A) ~ "b" }

  def B: Rule0 = rule { "b" ~ optional(B) ~ "c" }
}
```

Optimizations
==============

Rule Inlining
---------

Simple rules calls inlined as is.

String Squashing
------------

`"a" ~ "b" ~ "c"` rule is squashed to `"abc"`.

Meta-Rules
--------

[Meta-rules](https://github.com/sirthias/parboiled2#advanced-techniques) without overhead.

https://groups.google.com/forum/#!msg/parboiled-user/kgS30ZQqDYM/0hdXe3AaUy0J

Samples
========

https://github.com/alexander-myltsev/sbt-parboiled2-boost-samples

Note
====

Originally based on `sbt-boilerplate`