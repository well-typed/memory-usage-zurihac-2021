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

```
nix-shell
```

# Part 1: Introduction to RTS and ghc-debug

In order to effectively profile a Haskell application it's very useful to have a mental
model of how values are represented at runtime. Heap profiling tools are just ways
to visualise this information, so unless you have at least a basic understanding of
what they are trying to visualise then making progress with a heap profile can
be very tricky.

## Generational Scheme

Nursery -> Gen0 -> Gen1

## Block Structure

## Closure Structure

### Standard Closure Layout

Most Haskell values are represented in a uniform way.

![Heap layout](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/rts/storage/heap-object.png)

### Stack Closures

### A selection of other closure types

## Info Table Profiling

* (Ben) Basic heap layout principles, strucutre of closures, what is an info table, what do thunks look like
* (Ben) Basics of memory usage, difference between blocks allocated, mblocks allocated, live bytes
* (Ben) Explanation of info table profiling

## What are profiling tools?

Heap profiling tools allow you to answer questions about live memory on the heap.
With a heap profiling tool you can understand both high and low-level questions about
memory usage. It's normally useful to start by asking high-level questions:

* Is the memory usage of my application increasing over time?
* Are there any particuar source of allocations or closure types which account
  for a high percentage of residency?
* Are there any obvious memory usage spikes visible in the profile?


# Part 1a: Getting going with ghc-debug

ghc-debug is a suite of applications and libraries which allow you to inspect
the heap as a Haskell application from a debugger written in Haskell. The
heap structure is represented using Haskell datatypes and traversal functions
written using normal recursive Haskell traversal functions.

Debugging an application has two parts:

1. Instrument the `main` function of your application using a simple wrapper.
   When the program starts a socket will be created, which a debugger can
   connect to and query information about the heap.
2. Write a debugging script which connects to the opened socket, and queries
   and analyses information about the heap.

A key design principle of ghc-debug is that the debugger doesn't run in the same
process as your application. Therefore, there are no instrumentation artefacts
present in the profiles. When a debugger connects to your process, it pauses it
and therefore the heap won't mutate throughout the run of your debugging script.
This is critical to be able to traverse stack closures properly.

## Q: How is ghc-debug different to ghc-heap?

ghc-heap is limited to decoding normal closures, it can't traverse stack frames
and therefore a full heap traversal is not possible. ghc-heap runs in-process and
the instrumentation can affect the heap structure, for example, if you not careful then forcing
particular values while analysing them can lead to false analysis results.


## Architecture of ghc-debug

To get us thinking a bit more closely about what things on the heap look like,
we're going to try some simple examples using `ghc-debug-tui`. The examples
should help test some of your intuitions about how simple Haskell values are
represented on the heap.

The examples are in `simple/app/Main.hs`. In one terminal run:

```
cabal run simple
Enter a number:
100
Pausing for interruption by ghc-debug
```

And then in another terminal you can launch the tui to inspect the heap of the
running program.

```
cabal run tui
```

When the TUI starts, the dialog will list the sockets which it has found by looking
in `$XDG_DATA_DIR/ghc-debug/debuggee/`. The socket is opened by the call to `withGhcDebug` in
`App.hs`.

<Screenshot>

After the right socket has been selected, the pause request can be sent to the
process. The debugger then requests the GC roots for the process and renders them
in a list.

<Screenshot>

At the top of the list you can see the 5 saved objects from the examples.
Hovering over the first object you can see the source position the thunk arose
from.

<Screenshot>

Looking through the different examples you can tune your expectation about
how objects are laid out on the heap.

<Screenshot>

### Summary

At the moment the TUI is more of an exploratory toy than a serious debugging application.
For serious debugging you should write your own debugging scripts, which we will
get onto later.

# Part 2: Using eventlog2html

If you are trying to debug the memory usage of a large application then
you can't get stuck into the nitty-gritty straight away, there's too much information.
You first need to get a high-level overview of what's going on, and that's what
`eventlog2html` is for.

## Heap Profiling

GHC has built-in heap profiling which can be used to obtain a high-level overview
of the memory usage of your program. The heap profiler is a sampling profiler,
at a predetermined interval the execution of your program is stopped, the profiler
traverses the whole heap and then reports a summary of what's there depending
on the profiling mode.

The samples are then emitted into the eventlog, the eventlog can then be processed
by `eventlog2html` in order to create a human readable profile. The result is
an interactive HTML page with several different renderings of the profile samples.

<screenshot>

We're going to focus on two modes in particular because they do **not** require your
application to be rebuilt in the profiling way. In order to use one of these heap
profiling modes you just pass `+RTS -hT/-hi` when running your application.

<dl>
  <dt>Profile by closure type (<pre>-hT</pre>)</dt>
  <dd>Each bucket corresponds to a different <a href="https://hackage.haskell.org/package/ghc-heap-9.0.1/docs/GHC-Exts-Heap-ClosureTypes.html"> closure type </a>. This provides a high-level view. </dd>
  <dt>Profile by info table (<pre>-hi</pre>)</dt>
  <dd>Each bucket correponds to a distinct info table, each thunk, function, data constructor
      gets it's own info table so this provices very precise information.</dd>
</dl>

## Eventlog Basics

The eventlog is a file produce by the RTS which logs specific events as they
happen in the RTS.

In order to use the eventlog:

1. Link your application with `-eventlog`
2. Run your application with `+RTS -l`.

The result will be a file called <executable>.eventlog which contains information about
RTS events, such as, how much memory was used, when GC happened, information about
threads and crucially for us, information about heap profiling samples.

## Aside: What is the profiling way?

It's maybe a bit confusing that we can perform two modes of heap profiling without
building with the profiling way. The profiling way is primarily meant to
support *cost centre profiling*.

An issue with the profiling way is that because it inserts cost centres into your
program before optimisation, this can affect how the program is optimised. A program
built with the profiling way will also use more memory as the standard heap layout
contains an extra word per closure.

#### Aside: Increase the sample interval for programs

If you program has a large residency (> 1GB) then the default profiling interval (0.1s) is
too low and will cause your program to take a long time to complete. You can increase the
profiling interval by passing the `-i` flag. I find setting the interval to 1s is a
good compromise between an informative profile and a speedy finish.

### Profiling by closure type

Profiling by closure type is a great way to get a high-level overview of
the heap usage of your program.

Profiling `ghc` compiling the `Cabal` library produces a `ghc.eventlog` file.
Which can then be converted into a html file using `eventlog2html`.

```
eventlog2html ghc.eventlog
```

The profile can be viewed by opening the [resulting file]().

#### Interpreting the profile

The default view of the profile shows the 10 biggest


#### The detailed pane

A recent addition to `eventlog2html` is the "detailed" pane. This provides
a summary of each band in the profile.

<screenshot>

What does each band mean in the detailed pane?

<screenshot>

<dl>
  <dt>Profile</dt>
  <dd>A sparkline chart showing the residency over time</dd>
  <dt>n</dt>
  <dd>The "ranking" of the band by the integrated size</dd>
  <dt>Label</dt>
  <dd>A human readable label for the band</dd>
  <dt>Integrated Size</dt>
  <dd>The total area under the residency graph</dd>
  <dt>Stddev</dt>
  <dd>The standard deviation of the samples</dd>
  <dt>Intercept</dt>
  <dd>The intercept of the line of best fit calculated by least squares regression</dd>
  <dt>Slope</dt>
  <dd>The slope of the line calculated by least sequares regression</dd>
  <dt>Fit</dt>
  <dd>How well the regression fits the points</dd>
</dl>


The detailed pane is useful for several reasons.

1. You can easily identify each band of residency and consider it in turn.
2. You can see sources of residency which are too small to appear
3. You can search the bands to find specific sources of interest. For example,
   the allocations from a certain constructor.
4. Patterns between different bands can be identified by eye.

Something I regularly do is go to the detailed pane, and click through each
page looking to see if there are common patterns. Once getting to the smaller bands
this can be particularlly useful because small bands are often not polluted to
the same extent as large bands. For example, your profile might contain contribution
of ARR_WORDS from many different sources but there's likely to be a correlated
band further down the profile for `ByteString` or another wrapper type.

** The root cause of memory issues is not usually the biggest band in the profile **

Another trick is to sort by the "slope" column to find bands which have high slope
value, these ones are bands which increase steadily over time and might indicate leaks.

The detailed pane is useful for several reasons.

1. You can easily identify each band of residency and consider it in turn.
2. You can see sources of residency which are too small to appear
3. You can search the bands to find specific sources of interest. For example,
   the allocations from a certain constructor.
4. Patterns between different bands can be identified by eye.

Something I regularly do is go to the detailed pane, and click through each
page looking to see if there are common patterns. Once getting to the smaller bands
this can be particularlly useful because small bands are often not polluted to
the same extent as large bands. For example, your profile might contain contribution
of ARR_WORDS from many different sources but there's likely to be a correlated
band further down the profile for `ByteString` or another wrapper type.

** The root cause of memory issues is not usually the biggest band in the profile **

#### The heap pane

The heap pane shows information about memory

* Live bytes: Green line: Amount of live data (should match top of area pane)
* Blocks Size: Red line: Total size of allocated blocks
* Heap Size: Blue Line: Total size of allocated megablocks

OS memory usage corresponds roughly to the heap size, and the size of the nursery
is approximatey the difference between the red and blue lines.

This view can also be useful in identifying back fragmentation situations. A
very badly fragmented heap will have low live bytes (green) bit much higher
blocks size (red line).




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





