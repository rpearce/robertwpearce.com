---
author: "Robert Pearce"
authorTwitter: "@RobertWPearce"
desc: "Write your own implementation of JavaScript's Array.prototype.map method."
keywords: "javascript, map, Array.prototype.map, reimplement map, js, programming"
title: "JavaScript: Understand Array.prototype.map by Reimplementing It"
---

In this post, we will reimplement JavaScript's [`Array.prototype.map`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/map)
function in order to not only understand `map` better but also to get an idea of
how to implement instance methods on `Array.prototype`.

If you'd prefer to see a ~5 minute recording of what we'll do in this post, you
can watch the video below; otherwise, carry on!

<iframe
  allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture; web-share"
  allowfullscreen
  frameborder="0"
  height="315"
  loading="lazy"
  src="https://www.youtube.com/embed/kaqu-e3Q7IY"
  style="margin-top:2rem;"
  title="JavaScript Implement Your Own map method, mappy, on Array prototype"
  width="560"
></iframe>

## Initial Example: Use `map` to Convert Film Data to HTML Strings

First, we will start with some code that will demonstrate _one_ way to take an
array of films and output certain HTML strings.

Here is the `films` array:

```javascript
// films :: [Film]
const films = [
  { title: `Pulp Fiction`, score: 8.9 },
  { title: `Forrest Gump`, score: 8.8 },
  { title: `Interstellar`, score: 8.6 },
  { title: `The Prestige`, score: 8.5 }
]
```

and here is the output we are going for:

```javascript
[
  '<li class="film">#1 Pulp Fiction: <b>8.9</b></li>',
  '<li class="film">#2 Forrest Gump: <b>8.8</b></li>',
  '<li class="film">#3 Interstellar: <b>8.6</b></li>',
  '<li class="film film--last">#4 The Prestige: <b>8.5</b></li>'
]
```

Let's take a closer look at that output. We can see that the following data
needs to be included for each item:
* position in the list (`#3`)
* `title` (`Interstellar`)
* `score` (`8.6`)
* CSS class of `film`, unless it is the last item, in which case it gets `film`
  and `film--last`

Here is the (somewhat unusual) implementation we will use today in order to
later test that we successfully reimplemented `Array.prototype.map`:

```javascript
// filmToHtml :: (Film, Index, Films) -> HtmlString
function filmToHtml(film, i, films) {
  return this.format({
    index: i + 1,
    isLast: i === films.length - 1,
    score: film.score,
    title: film.title,
  })
}

function format({ index, isLast, score, title }) {
  const cn = isLast ? `film film--last` : `film`
  return `<li class="${cn}">#${index} ${title}: <b>${score}</b></li>`
}

console.log(
  films.map(filmToHtml, { format })
)
// [
//   '<li class="film">#1 Pulp Fiction: <b>8.9</b></li>',
//   '<li class="film">#2 Forrest Gump: <b>8.8</b></li>',
//   '<li class="film">#3 Interstellar: <b>8.6</b></li>',
//   '<li class="film film--last">#4 The Prestige: <b>8.5</b></li>'
// ]
```

This is probably two-to-three times more complicated than it needs to be, but it
is a sufficient example for today, for we make use of _all_ of
`Array.prototype.map`'s features.

_Note: it's rare to use the second argument to `map`, but we are doing so today
in order to test our implementation._

So what is going on here?

The `map` method iterates over each film and calls `filmToHtml` with a few
arguments:

1. the film object
1. the film's index in the array
1. the `films` array

It also calls the `filmToHtml` function with an optional `this` scope. To
demonstrate how this works, we pass an object with the method `format` that
`filmToHtml` then accesses via `this.format`. The `format` function then
receives some data points and ultimately returns to us the `<li>...</li>` HTML
for each film.

## Defining Our Own `map` Method, `mappy`

If we want to write a new method that can be called on our `films` `Array`
instance, we add it to the `Array.prototype` like this:

```javascript
Array.prototype.mappy = function mappy(/* ??? */) {
  // our implementation will go here
}
```

Since a _method_ is a _function_ defined on an object, we know we are working
with a function, but what arguments does our function accept?

## What Is `map`'s Syntax?

As hinted at in a prior section, if we look at [MDN's `Array.prototype.map`
syntax documentation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/map#Syntax),
we can see that we need:

1. a `callback` that gets called with an optional scope and 3 arguments:
   1. the currently iterated item
   1. the current item's array index (an integer)
   1. the source array that `map` is called upon
1. an optional value to use as `this` when calling the callback

## Filling In The Argument Blanks

Let's give our `mappy` method a `callback` parameter, as well as an optional
`thisArg`, which we'll simply name `_this`.

```javascript
Array.prototype.mappy = function mappy(callback, _this) {

  // Let's then have it return our array instance
  // by returning the special `this` keyword.
  return this
}

console.log(
  films.map(filmToHtml, { format })
)
// [
//  { title: `Pulp Fiction`, score: 8.9 },
//  { title: `Forrest Gump`, score: 8.8 },
//  { title: `Interstellar`, score: 8.6 },
//  { title: `The Prestige`, score: 8.5 }
// ]
```

Since our `mappy` method, like `map`, will not alter the original array, we know
we'll need to return a new array, so let's do that and return the empty array:

```javascript
Array.prototype.mappy = function mappy(callback, _this) {
  const newArray = []
  return newArray
}

console.log(
  films.map(filmToHtml, { format })
)
// []
```

## The Implementation

Now that we have a `newArray`, know we can work with `this`, have a `callback`
to call and a `_this` scope to call the `callback` with, we can populate the
`newArray` with the result of calling the `callback` function with each item in
our array (and with the appropriate arguments, of course):

```javascript
Array.prototype.mappy = function mappy(callback, _this) {
  const newArray = []

  // We'll use a for loop to iterate over
  // each item in our list,
  for (let i = 0; i < this.length; i++) {
    // and then at the end of our `newArray`
    // we'll append the result of calling
    // the callback function with the optional
    // scope and its 3 arguments:
    //   1. the item,
    //   2. the current item's index in the array,
    //   3. and lastly the original list, itself.
    newArray.push(
      callback.call(_this, this[i], i, this)
    )
  }

  // Ultimately, we return the `newArray`
  // containing our transformed items.
  return newArray
}

// And when we log out the result,
// we can see our `filmToHtml` function
// works as expected.
console.log(
  films.map(filmToHtml, { format })
)
// [
//   '<li class="film">#1 Pulp Fiction: <b>8.9</b></li>',
//   '<li class="film">#2 Forrest Gump: <b>8.8</b></li>',
//   '<li class="film">#3 Interstellar: <b>8.6</b></li>',
//   '<li class="film film--last">#4 The Prestige: <b>8.5</b></li>'
// ]
```

## Providing Useful Error Messaging

What happens if someone tries to use our `mappy` method but doesn't provide a
callback function? For example:

```javascript
films.mappy(123)
// TypeError: callback.call is not a function

films.map(123)
// TypeError: 123 is not a function
```

Unfortunately, our `mappy` method doesn't take this scenario into account! But
the `map` method's error messaging isn't totally clear at a glance, either, so
let's try a different approach:

```javascript
Array.prototype.mappy = function mappy(callback, _this) {
  if (typeof callback !== 'function') {
    throw new TypeError(
      'Array.prototype.mappy: ' +
      'A callback function was expected ' +
      'as the first argument, but we received ' +
      '`' + JSON.stringify(callback) + '`'
    )
  }

  const newArray = []

  for (let i = 0; i < this.length; i++) {
    newArray.push(
      callback.call(_this, this[i], i, this)
    )
  }

  return newArray
}

films.mappy(123)
// TypeError:
//   Array.prototype.mappy: A callback function was
//   expected as the first argument, but we received `123`

films.mappy({ foo: 'bar' })
// TypeError:
//   Array.prototype.mappy: A callback function was
//   expected as the first argument, but we received `{"foo":"bar"}`
```

## Wrapping Up

I hope this post has helped de-mystify how `Array.prototype.map` conceptually
works under the hood! Next time, we'll look at how to implement `map` without
polluting the `Array.prototype`, and we might even be able to use `map` on more
data structures than just `Array`! Stay tuned.

* * *

Thank you for reading!
<br />
Robert
