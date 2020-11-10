---
author: "Robert Pearce"
authorTwitter: "@RobertWPearce"
desc: "Learn about function composition with ramda's compose function."
image: "/images/sheep-hill-building.jpg"
keywords: "ramda, ramda.js, ramdajs, ramda tutorial, ramda compose, function composition, ramda fp"
photoCredit: "Alex Kotomanov"
photoWebsite: "https://unsplash.com/@kotomanov"
title: "Ramda Chops: Function Composition"
---

_Thanks to [@evilsoft](https://twitter.com/evilsoft) and
[@zerkms](https://twitter.com/zerkms) for their review of this post._

_Composition_ is defined as "the combining of distinct parts or elements to
form a whole." <sup>[source](https://www.wordnik.com/words/composition)</sup>
If we apply this thinking to functions in programming, then _function
composition_ can be seen as the combining of functions to form a new function
that is _composed_ of said functions. Now that that word salad is over, let's
get to work.

We have a task, and our task is to write a function that
1. accepts a list of objects containing `score` (`Number`) and `name` (`String`)
   properties
1. returns the top 3 scorers' names from highest to lowest

Here are the unordered results that we have to work with:

```js
const results = [
  { score: 40, name: 'Aragorn' },
  { score: 99, name: 'Bilbo' },
  { score: 63, name: 'Celeborn' },
  { score: 77, name: 'Denethor' },
  { score: 100, name: 'Eowin' },
  { score: 94, name: 'Frodo' }
]
```

* * *

_Other ramda posts:_

* [Ramda Chops: Function Currying](/ramda-chops-function-currying.html)
* [Ramda Chops: Safely Accessing Properties](/ramda-chops-safely-accessing-properties.html)
* [Ramda Chops: Map, Filter & Reduce](/ramda-chops-map-filter-and-reduce.html)

## First Approach

```js
// getHighScorers :: [Object] -> [String]
const getHighScorers = xs =>
  [...xs]
    .sort((a, b) => b.score - a.score)
    .slice(0, 3)
    .map(x => x.name)

getHighScorers(results) // => [ 'Eowin', 'Bilbo', 'Frodo' ]
```

As cautious JavaScript developers, we know to reach for our functions and
methods that don't mutate the objects we're receiving. We use a _copy_ of the
original list and chain together operations that `sort`, `slice` and `map` the
return values of each operation until we arrive at `[ 'Eowin', 'Bilbo', 'Frodo' ]`.
Many folks would stop here, write a few unit tests and be done with it. We, on
the other hand, will take this to the next level.

## Extracting Reusable Functions
Our `getHighScorers` function has some functionality that we may want to use
elsewhere in the future. Let's break down what we might be able to extract:

* a _sort by some prop in descending order_ function (from `sort`)
* a _take n items_ function (from `slice`)
* a _map prop_ function (from `map`)

```js
// Altered slightly to allows us to compare
// things like strings and numbers.
//
// descBy :: (String, [a]) -> [a]
const descBy = (prop, xs) =>
  [...xs].sort((a, b) =>
    a[prop] < b[prop] ? 1 : (a[prop] === b[prop] ? 0 : -1)
  )

// takeN :: (Number, [a]) -> [a]
const takeN = (n, xs) =>
  xs.slice(0, n)

// mapProp :: (String, [a]) -> [b]
const mapProp = (prop, xs) =>
  xs.map(x => x[prop])

// 1. pass `score` and `xs` to `descBy`
// 2. pass the return value of `descBy`
//    to `takeN(3, __)`
// 3. pass the return value of `takeN`
//    to `mapProp('name', __)` where we map over
//    the list and pull out each one's `name`
//
// getHighScorers :: [Object] -> [String]
const getHighScorers = xs =>
  mapProp('name', takeN(3, descBy('score', xs)))

// results object here...

getHighScorers(results) // => [ 'Eowin', 'Bilbo', 'Frodo' ]
```

_[Try this code in the ramda REPL.](https://goo.gl/9XsTQx)_

This is starting to look good, but that `getHighScorers` function is looking a
bit dense. Since we have a seeming pipeline of transformations that we're
applying to a list, wouldn't it be great if we could simply list these
transformations in a "flat" way (instead of a "nested" way like we do above) and
then pass the data to this list of transformations?

## Enter `compose`
Let's take our `getHighScorers` function and rewrite it using [ramda's compose
function](http://ramdajs.com/docs/#compose):

```js
import compose from 'ramda/src/compose'

// const getHighScorers = xs =>
//   mapProp('name', takeN(3, descBy('score', xs)))

// getHighScorers :: [Object] -> [String]
const getHighScorers = xs =>
  compose(mapProp('name'), takeN(3), descBy('score'))(xs)
```

Let's first clarify what `compose` is doing:

```js
compose(f, g)(x) === f(g(x))
```

Say it aloud: "f after g." With `compose`, the function furthest to the _right_
is applied first with the value (`x`), and the return value of that function is
passed to the next function to its _left_, and repeat this until all functions
have been applied.

Cool – but wait! How can `descBy`, `takeN` and `mapProp` only accept one
argument at a time when they all accept two?! In order to make these a reality,
we can make use of [ramda's curry function](http://ramdajs.com/docs/#curry)
which we dove into in my [previous post on function currying](/ramda-chops-function-currying.html]).

```js
import compose from 'ramda/src/compose'
import curry from 'ramda/src/curry'

// descBy :: String -> [a] -> [a]
const descBy = curry((prop, xs) =>
  [...xs].sort((a, b) =>
    a[prop] < b[prop] ? 1 : (a[prop] === b[prop] ? 0 : -1)
  )
)

// takeN :: Number -> [a] -> [a]
const takeN = curry((n, xs) =>
  xs.slice(0, n)
)

// mapProp :: String -> [a] -> [b]
const mapProp = curry((prop, xs) =>
  xs.map(x => x[prop])
)

// getHighScorers :: [Object] -> [String]
const getHighScorers =
  compose(mapProp('name'), takeN(3), descBy('score'))
```

_[Try this code in the ramda REPL.](https://goo.gl/NxEFhi)_

You may also notice that we removed `xs =>` from `getHighScorers` because when
we use compose and pass the final argument in at the end, it in fact becomes
redundant. Our composition sits and waits for either the data to be applied or
for it to be used another way: more compositions! This leads us down a
powerful path whereby we can now compose different functions together and
combine them into a final composition.

## Composing a Composition
```js
// getTop3 :: [a] -> [a]
const getTop3 =
  compose(takeN(3), descBy('score'))

// getHighScorers :: [Object] -> [String]
const getHighScorers =
  compose(mapProp('name'), getTop3)
```

_[Try this code in the ramda REPL.](https://goo.gl/jRoCWZ)_

This is where we truly begin to see the power of `compose`, for we are able to
break our functions or function compositions out into tiny little pieces that we
chain together like water pipes or guitar pedals.

<figure>
  <img src="/images/guitar-pedals.jpg" alt="guitar pedals" />
  <figcaption>
    _Guitar pedals by [Henrik Hjortshøj](https://unsplash.com/@hfranke)_
  </figcaption>
</figure>

We are now empowered (nay – encouraged!) to provide meaningful names in the
context of what we're trying to accomplish.

Composing compositions also allows us to use our type signatures to tell a story
about what behavior is expected with each little part on our path to the
ultimate goal.

## `pipe` vs `compose`
For various reasons that are usally a matter of opinion, many people prefer
function application to flow from _left to right_ instead of _right to left_
(the latter being what you get with `compose`). So if you find yourself thinking
the same thing, [pipe](http://ramdajs.com/docs/#pipe) is for you:

```js
//      <-------------   <------   <-------------
compose(mapProp('name'), takeN(3), descBy('score'))(xs)

// versus

//   -------------->  ------->  -------------->
pipe(descBy('score'), takeN(3), mapProp('name'))(xs)
```

## Composing Promises
There's really nothing to it! Instead of `compose` or `pipe`, use
[composeP](http://ramdajs.com/docs/#composeP) or
[pipeP](http://ramdajs.com/docs/#pipeP).

## Debugging
Once you adopt this pattern, you may find it initially difficult to inspect your
data at a given point in the pipeline; however, here's a tip that will solve
most of your problems:

```js
compose(
  mapProp('name'),
  x => (console.log(x), x),
  takeN(3),
  descBy('score')
)

// or

const logIt = x => (console.log(x), x)

compose(
  mapProp('name'),
  logIt,
  takeN(3),
  descBy('score')
)
```

This logs whatever the value in the pipeline is at that time and returns that
value to pass it on just as it would have.

* * *

Thanks for reading! Until next time,
<br>
Robert
