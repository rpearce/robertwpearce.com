---
author: "Robert Pearce"
authorTwitter: "@RobertWPearce"
desc: "How to use ramda's map, filter and reduce functions to filter and transform a list of popular films."
keywords: "ramda, ramda.js, ramdajs, ramda tutorial, ramda map, ramda filter, ramda reduce, ramda fp"
title: "Ramda Chops: Map, Filter & Reduce"
---

_Thanks to [Jillian Silver](https://github.com/jsilve), [Patrick
Eakin](https://patrickweakin.com) and [@zerkms](https://twitter.com/zerkms) for
their review of this post._

The [`map`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/map),
[`filter`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/filter)
and [`reduce`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/Reduce)
methods on `Array.prototype` are essential to adopting a functional programming
style in JavaScript, and in this post we're going to examine how to use these
three concepts with [ramda.js](http://ramdajs.com/).

If you are unfamiliar with these three concepts, then be sure to first read the
MDN documentation on each (linked above).

* * *

_Pre-requisite ramda posts:_

* [Ramda Chops: Function Currying](/ramda-chops-function-currying.html)
* [Ramda Chops: Function Composition](/ramda-chops-function-composition.html)

_Other ramda posts:_

* [Ramda Chops: Safely Accessing Properties](/ramda-chops-safely-accessing-properties.html)

## Our Data Set: Films!
This is the test data set we will reference throughout the post:

```js
const films = [
  { title: 'The Empire Strikes Back', rating: 8.8 },
  { title: 'Pulp Fiction', rating: 8.9 },
  { title: 'The Deer Hunter', rating: 8.2 },
  { title: 'The Lion King', rating: 8.5 }
]
```

## Our Goal
There are a few conditions that are required for us to meet our goal. We must
construct a function that:
* only selects those with an 8.8 rating or higher
* returns a list of the selected titles interpolated in an HTML string that
  has this structure:
  ```html
  <div>TITLE: <strong>SCORE</strong></div>
  ```

Given these requirements, a pseudotype signature for this might be:

```js
// `output` takes in a list of films
// and returns a list of HTML strings
//
// output :: [Film] -> [Html]
```

## Step 1: Get the HTML Part Working
```js
films.map(film => `<div>${film.title}, <strong>${film.rating}</strong></div>`)
// => [
//      "<div>The Empire Strikes Back, <strong>8.8</strong></div>",
//      "<div>Pulp Fiction, <strong>8.9</strong></div>",
//      "<div>The Deer Hunter, <strong>8.2</strong></div>",
//      "<div>The Lion King, <strong>8.5</strong></div>"
//    ]
```
_[Try this code in the ramda REPL](https://goo.gl/mQmFBm)_

## Step 2: Extract the `map` Callback
```js
// filmHtml :: Film -> Html
const filmHtml = film =>
  `<div>${film.title}, <strong>${film.rating}</strong></div>`

films.map(filmHtml)
```
_[Try this code in the ramda REPL](https://goo.gl/C4a1kZ)_

## Step 3: `filter` Out Lower Scores
```js
films
  .filter(x => x.rating >= 8.8)
  .map(filmHtml)

// => [
//      "<div>The Empire Strikes Back, <strong>8.8</strong></div>",
//      "<div>Pulp Fiction, <strong>8.9</strong></div>",
//    ]
```
_[Try this code in the ramda REPL](https://goo.gl/yTxfKQ)_

But wait! We can extract that `filter` callback, as well:

```js
// hasHighScore :: Film -> Bool
const hasHighScore = x =>
  x.rating >= 8.8

films
  .filter(hasHighScore)
  .map(filmHtml)
```
_[Try this code in the ramda REPL](https://goo.gl/qG4kZ7)_

## Step 4: Composing `filter` and `map`
We can use ramda's [function currying
capabilities](/ramda-chops-function-currying.html) and [function
composition](/ramda-chops-function-composition.html) to create some very
clear and concise [pointfree functions](https://wiki.haskell.org/Pointfree).

```js
import { compose, filter, map } from 'ramda'

// output :: [Film] -> [Html]
const output =
  compose(map(filmHtml), filter(hasHighScore))

output(films)
```
_[Try this code in the ramda REPL](https://goo.gl/7VMNTV)_

One thing to remember with ramda functions (like `map` and `filter`) is that
ramda typically orders arguments from least likely to change to most likely to
change. Callback/transformation functions here are passed as the _first_
argument, and the data comes last. To understand this further, check out the
following links:

* [ramda's documentation for `map`](http://ramdajs.com/docs/#map)
* [ramda's documentation for `filter`](http://ramdajs.com/docs/#filter)

## Step 5: Extracting The Composition Further
If we want to not only reuse our filtering and mapping functions but also make
them more readable, we can pull out the pieces that make up our `output`
function into smaller bits:

```js
// filmsToHtml :: [Film] -> [Html]
const filmsToHtml =
  map(filmHtml)

// highScores :: [Film] -> [Film]
const highScores =
  filter(hasHighScore)

// output :: [Film] -> [Html]
const output =
  compose(filmsToHtml, highScores)

output(films)
```
_[Try this code in the ramda REPL](https://goo.gl/dFXCEK)_

## Step 6: Another Way With `reduce`
We can accomplish the same goals as `filter` and `map` by making use of
`reduce`.

```js
films.reduce((acc, x) => {
  return hasHighScore(x)
    ? acc.concat(filmHtml(x))
    : acc
}, [])

// or, for better performance

films.reduce((acc, x) => {
  if (hasHighScore(x)) {
    acc.push(filmHtml(x))
  }

  return acc
}, [])
```
_[Try this code in the ramda REPL](https://goo.gl/b86Kdq)_

If you're not familiar with reduce, be sure to play with the live example to
better understand how those pieces work before moving on.

It's also worth noting that you can do just about _anything_ in JavaScript with
the `reduce` function. I highly recommend going through [Kyle Hill](https://twitter.com/kylehill)'s
slides on [reduce Is The Omnifunction](https://omnifunction.herokuapp.com).

But wait! We can extract the `reduce` callback like we did with `map` and
`filter` before:

```js
// highScoresHtml :: ([Html], Film) -> [Html]
const highScoresHtml = (acc, x) =>
  hasHighScore(x)
    ? acc.concat(filmHtml(x))
    : acc

films.reduce(highScoresHtml, [])
```
_[Try this code in the ramda REPL](https://goo.gl/F6NovJ)_

## Step 7: Making Our Reduce Arguments Reusable
```js
import { reduce } from 'ramda'

const output =
  reduce(highScoresHtml, [])

output(films)
```
_[Try this code in the ramda REPL](https://goo.gl/wjbAFY)_

As before with `map` & `filter`, `output` can be reused over and over again and
passed any set of films to generate HTML for. To further understand the
parameter order used here, check out the [docs for ramda's
`reduce`](http://ramdajs.com/docs/#reduce).

* * *

This step-by-step process we've walked through is as close to real-life
refactoring/rethinking as I could do in a post. Thanks for making it this far.

Until next time,
<br />
Robert
