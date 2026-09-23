---
author: "Robert Pearce"
authorTwitter: "@RobertWPearce"
desc: "What Ramda is, the JavaScript you should know first, how to include Ramda in a project, where to find its docs and REPL, and related projects to explore next."
keywords: "ramda, ramda.js, ramdajs, ramda tutorial, ramda guide, functional programming, javascript, sanctuary, crocks, folktale"
title: "Ramda Chops: Getting Started with Ramda"
updated: "2021-08-11T12:00:00Z"
---

_This post is adapted from my [Ramda
Guide](https://github.com/rpearce/ramda.guide) book project. Thanks to Steve
Purr, Tom Wilson, and Ian Greulich for their reviews._

* * *

_Other posts in this series:_

* [Ramda Chops: Converting Temperature Units](/ramda-chops-converting-temperature-units.html)
* [Ramda Chops: Comparing Two Temperatures](/ramda-chops-comparing-two-temperatures.html)
* [Ramda Chops: Reading Shallow Object Properties](/ramda-chops-reading-shallow-object-properties.html)
* [Ramda Chops: Testing Shallow Object Properties](/ramda-chops-testing-shallow-object-properties.html)
* [Ramda Chops: Introducing FP at Work](/ramda-chops-introducing-fp-at-work.html)

## What is Ramda?

[Ramda.js](https://ramdajs.com) is a JS library of helper functions that have
some cool principles baked into every function:

* never mutate the user's data
* always provide the same output given the same input
* allow users to build new functions from old ones by not supplying all of a
  function's parameters

These ideas — regardless of what library or even language we use — allow us to
write safe, extendable code. The last of those ideas is called currying, and
[Ramda Chops: Function Currying](/ramda-chops-function-currying.html) explores
it in depth.

## Suggested Prerequisite Knowledge

The presentation of concepts in this series assumes the reader is familiar with
some JS fundamentals, so take a look below and see how comfortable you are with
the concepts.

If you're not comfortable with them, each subsection below has a link to more
information about its topic, and if that's not enough, here are some resources
for learning more JS:

* [Scrimba's Courses](https://scrimba.com/topic/javascript)
* [Codecademy's Courses](https://www.codecademy.com/catalog/language/javascript)
* [Wes Bos' Courses](https://wesbos.com/courses)
* [Egghead's Courses](https://egghead.io/q/javascript)

### Arrow Functions

[MDN: Arrow Functions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Functions#arrow_functions)

```javascript
const add = (a, b) => a + b

add(4, 5) // 9
```

### Nested Functions

[MDN: Nested Functions and Closures](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Functions#nested_functions_and_closures)

```javascript
const addExpr = a => b => a + b

addExpr(4)(5) // 9

// or

function addFn(a) {
  return function (b) {
    return a + b
  }
}

addFn(4)(5) // 9
```

### Passing Functions as Arguments (Callbacks)

[MDN: First-Class Function](https://developer.mozilla.org/en-US/docs/Glossary/First-class_Function)

```javascript
const log = x => console.log(x)

[1, 2, 3].forEach(log)
// 1
// 2
// 3
```

### Map, Filter, & Reduce

MDN: [`map`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/Map),
[`filter`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/Filter),
and [`reduce`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/Reduce)

```javascript
[1, 2, 3].map(x => x * 2)                      // [2, 4, 6]

[1, 2, 3].filter(x => x % 2 !== 0)             // [1, 3]

[1, 2, 3].reduce((sum, item) => sum + item, 0) // 6
```

### Calling Functions

[MDN: Calling Functions](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Functions#calling_functions)

```javascript
const add5    = x => x + 5
const times10 = x => x * 10
const div2    = x => x / 2

div2(times10(add5(15))) // 100
```

## Including Ramda in a Project

There are instructions on [Ramda's homepage](https://ramdajs.com) detailing how
to install Ramda, and [deno.land's Ramda page](https://deno.land/x/ramda) has
instructions for deno.

If you're building for the frontend and are using a build tool that has
[tree-shaking or dead-code elimination](https://developers.google.com/web/fundamentals/performance/optimizing-javascript/tree-shaking),
then here is how you should import functions from Ramda:

```js
import { compose, lensProp, map, over } from 'ramda'
```

However, if you do not have a build tool that does tree-shaking, you may want to
import directly from the files you use to avoid importing the entire Ramda
library when you only want to use a few functions. The two options with
[`v0.27.1`](https://www.npmjs.com/package/ramda/v/0.27.1) are ESModules- and
CommonJS-based.

```js
// ESModules
import compose from 'ramda/es/compose'
import lensProp from 'ramda/es/lensProp'

// CommonJS
const compose = require('ramda/src/compose')
const lensProp = require('ramda/src/lensProp')
```

## Documentation, Source Code, REPL, & Cookbook

[Ramda's documentation](https://ramdajs.com/docs/) is the place to go when you
are looking for a function to use, trying to remember how to use a function,
want to view a function's [source code on
GitHub](https://github.com/ramda/ramda/tree/master/source), or open it in [the
Ramda REPL](https://ramdajs.com/repl/).

[The Ramda REPL](https://ramdajs.com/repl/) is something I used _all the time_
when learning Ramda, for all the functions are automatically loaded in that
environment for you to use. You can even get a link to your current state in the
REPL and share it with others!

Some official resources of additional note are [Ramda's
GitHub wiki](https://github.com/ramda/ramda/wiki) and the [Ramda
Cookbook](https://github.com/ramda/ramda/wiki/Cookbook). The wiki contains
numerous resources hand-picked by the Ramda folks, and the Cookbook is
chock-a-block with Ramda recipes (helpful functions built with Ramda functions).

## Community Resources

There are a _lot_ of great articles and resources out there, and Iain's list
(see below) should cover most of your needs.

* Iain Freestone's [Ramda: My library of
  resources](https://dev.to/iainfreestone/ramda-my-library-of-resources-1ebm) is
  an impressive compendium of blog posts, videos, podcasts, REPLs, Ramda-related
  libraries / tools, examples, and more.
* Dave Sancho's [Learn Ramda](https://davesnx.github.io/learn-ramda/) project is
  fun and helpful, for it uses dropdown selects and English sentences to help you
  find a function based on your needs, and it tells you how to use it.
* [ramda-adjunct](https://github.com/char0n/ramda-adjunct) bills itself as "the
  most popular and most comprehensive set of functional utilities for use with
  Ramda, providing a variety of useful, well tested functions with excellent
  documentation". It is filled with useful helper functions that make working
  with Ramda even easier.
* (Shameless plug) My earlier Ramda Chops articles on
  [currying](/ramda-chops-function-currying.html),
  [function composition](/ramda-chops-function-composition.html), and
  [map, filter, & reduce](/ramda-chops-map-filter-reduce.html), as well as a
  lengthy article on [implementing your own functional programming-style `map`
  function](/javascript-writing-a-functional-programming-style-map-function.html),
  were the inspiration for this series and cover some of the same ground.

## Related Projects

Ramda is great for teams who are stepping into the functional programming in JS
world and want to dip their toes in the water. Once you've gotten comfortable
with writing code in a functional style, consider checking out these projects,
as well, to take it to the next level.

Each of these projects below include some amount of [algebraic data
types (ADTs)](https://en.wikipedia.org/wiki/Algebraic_data_type) for safety and
expressiveness.

It should also be said that there are numerous great projects out there, so
consider these merely a starting point.

### Sanctuary

[Sanctuary](https://sanctuary.js.org) is a successor to Ramda that is quite a
bit stricter and more likely to be unfamiliar to web developers, for it feels
more like [the ML
language family](https://en.wikipedia.org/wiki/ML_(programming_language)).

Its goal is to provide refuge from unsafe JS. Check out [its section on
Ramda](https://sanctuary.js.org/#section:ramda) to read about the differences.

### Crocks

[Crocks](https://crocks.dev), like Sanctuary, has a bit of overlap with Ramda's
functions, but it goes much deeper than Ramda by providing a variety of ADTs
and functions for working with them.

It is worth checking out at least for its [`Async`
ADT](https://crocks.dev/docs/crocks/Async.html) (goodbye, `Promise`)!

### Folktale

[Folktale](https://folktale.origamitower.com), like Sanctuary and Crocks,
includes some ADTs and other useful functions for working with a functional
programming style in JS.
