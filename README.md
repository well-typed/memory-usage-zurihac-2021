# Introduction

This workshop will introduce two tools for profiling Haskell programs, eventlog2html
and ghc-debug. These form a family of new heap profiling tools which use
the new [info table location](https://well-typed.com/blog/2021/01/first-look-at-hi-profiling-mode/) feature in GHC 9.2. The info table locations allow a debugger to very precisely map
a source of allocation to a place in the source program. This gives much more detailed
and actionable profiling output.

The workshop is intended to be interactive, follow along and get familiar with
running the tools for yourself.

No advanced knowledge of language features is necessary, but you shouldn't
be afraid of internal implementation details. It is also a bit fiddly to set-up the
correct environment.

# Pre-tutorial

Before the workshop, you should have the following installed:

* jq
* ghc-9.2
* cabal-3.6
* eventlog2html
* ghc-debug

Or, alternatively, use this `nix` invocation to set-up the environment.

# Part 1: Introduction to RTS and ghc-debug

* (Ben) Basic heap layout principles, strucutre of closures, what is an info table, what do thunks look like
* (Ben) Basics of memory usage, difference between blocks allocated, mblocks allocated, live bytes
* (Ben) Explanation of info table profiling


* (Matt) Introduction to ghc-debug, simple toy examples using the TUI.
  - How to instrument an application with ghc-debug
  - Basic exploration with the TUI

# Part 2: Using eventlog2html

* Demonstrating using eventlog2html on an example, profiling building (Cabal).
* Explain the build options which are needed to build with eventlog enabled.
* Explain the two different profiling modes `-hT` and `-hi`.
* Explain the user interface of eventlog2html
  - Area vs Linechart views
  - Heap view
  - Detailed View
* Discussion about Detailed View, examples of searching
* Compare and constrast `-hT` and `-hi` mode. High-level vs detailed

* (Interactive) Use eventlog2html on the sample application
* What do we observe?

# Part 3a: Using ghc-debug on a bigger application

We are going to use ghc-debug to investigate the memory leaks in the sample
application which we ran before.

* Use ghc-debug to pause the application

* How to write a debugging script

What else can ghc-debug analyse out of the box?

* Fragmentation issues
* Duplicate heap objects
* Thunk census

## Two-level profile

## Finding Retainers

The killer application of ghc-debug is finding out what is retaining

For this there is a built-in function which finds paths from the GC roots to
closures which match a certain predicate.

# Part 3b: Using ghc-debug/eventlog2html on your own application

In this section you can do what you want and are welcome to try ghc-debug or eventlog2html on
your own application where we can help you debug any issues.





