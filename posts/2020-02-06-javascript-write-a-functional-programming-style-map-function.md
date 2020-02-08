---
author: "Robert Pearce"
authorTwitter: "@RobertWPearce"
description: ""
image: "/images/.jpg"
keywords: "javascript, map, map function, functional programming, js, programming"
photoCredit: "robertwpearce"
photoWebsite: "https://www.instagram.com/robertwpearce"
title: "JavaScript: Writing a Functional Programming-Style map Function"
---

In this post, we will write a functional programming-style implementation of
JavaScript's [`map`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/map)
function that not only works with [`Array`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array)
but any data structure that implements a `map` method. Such data structures are
known as
[`Functors`](https://github.com/hemanth/functional-programming-jargon#functor),
and some examples of `Functors` are the [Algebraic Data Types](https://github.com/hemanth/functional-programming-jargon#algebraic-data-type)
[`Maybe`](https://crocks.dev/docs/crocks/Maybe.html) and
[`Async`](https://crocks.dev/docs/crocks/Async.html) (prior knowledge of them is
not required, and out of the two, we'll only use `Maybe`). By the end of this
post, you will:
* know how to implement a generic `map` function that includes functions for
  `map`ping `Array`s, `Object`s, and `Functor`s
* understand how to use `map` in a variety of scenarios
* know how to write a simple `compose` function and use composition
* know how to reliably test values for their types
* have received a small introduction to Algebraic Data Types via [`crocks.js`](https://crocks.dev)

This is a big post, so buckle up!

<!--If you'd prefer to see a ~5 minute recording of what we'll do in this post, you-->
<!--can watch the video below; otherwise, carry on!-->

<!--<iframe-->
<!--  width="560"-->
<!--  height="315"-->
<!--  src="https://www.youtube-nocookie.com/embed/n_VTQJARW-o"-->
<!--  frameborder="0"-->
<!--  allow="accelerometer; autoplay; encrypted-media; gyroscope; picture-in-picture"-->
<!--  allowfullscreen-->
<!--</iframe>-->

* * *

_Note: if you're not familiar with `Array.prototype.map` already, check out my
video on [Using JavaScript's Array.prototype.map
Method](https://www.youtube.com/watch?v=tjjg3_jyD7M) or my post on [JavaScript:
Understand Array.prototype.map by Reimplementing It](https://robertwpearce.com/javascript-understand-array-prototype-map-by-reimplementing-it.html)._

_Also, a library that implements a solid `map` function is
[crocks.js](https://crocks.dev), and we will use its implementation as our
template. If you want to skip this article entirely, you can go and view [its
source](https://github.com/evilsoft/crocks/blob/e4517493079538960d53715ef25d72c264cfecf0/src/pointfree/map.js#L15-L38)._

## The Goal: `map` All the Things
Today we are going to write a `map` function that does the following:
* accepts a transformation function that takes in some argument of type `a` and
  transforms it into a value of type `b`; i.e., `(a -> b)`
* accepts and handles any of the following data types:
  * [`Array`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array)
  * [`Object`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object)
  * [`Function`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Function)
  * [`Functors`](https://github.com/hemanth/functional-programming-jargon#functor)
    (e.g., [`Maybe`](https://crocks.dev/docs/crocks/Maybe.html))

Sounds easy, right? We'll see!

## Defining Our `map` Function
There are some things we already know about our `map` function:
* it's called `map` (yay! nailed it!)
* it takes a function and some data
* it returns the data as transformed by said function

Let's sketch it out:

```javascript
const map = (fn, m) => {
  // ???
}
```

Okay, it's a start. This could conceivably be used like this:

```javascript
map(x => x.id, [{ id: 1 }, { id: 2 }])     // [1, 2]
map(x => x.id, [{ id: 'a' }, { id: 'b' }]) // ['a', 'b']
```

But we can quickly see that our `x => x.id` is starting to get repetitive. We
could pull that into a function, `propId`:

```javascript
const propId = x => x.id
map(propId, [{ id: 1 }, { id: 2 }])     // [1, 2]
map(propId, [{ id: 'a' }, { id: 'b' }]) // ['a', 'b']
```

Alas, we're back where we started – it's still repetitive!

Instead, what if we could [_partially
apply_](https://github.com/hemanth/functional-programming-jargon#partial-application)
the `x => x.id` to `map`, store that partially applied function in a variable,
and then use that to call with our different data?

```javascript
const mapId = map(x => x.id)
mapId([{ id: 1 }, { id: 2 }])     // [1, 2]
mapId([{ id: 'a' }, { id: 'b' }]) // ['a', 'b']
```

_Wondering why the data comes last? Check out [Brian Lonsdorf's "Hey Underscore,
You're Doing It Wrong!" talk](https://youtu.be/m3svKOdZijA). The tl;dr is that
you should arrange your arguments from least likely to change to most likely to
change in order to pave the way for partial application and greater code reuse._

Nice! Now, let's go back to our sketch. Let's turn our _binary_ function (which
means it has _two_ parameters) to instead be a series of _unary_ functions
(which means they only accept _one_ parameter at a time; [read more about
function arity here](https://github.com/hemanth/functional-programming-jargon#arity)):

```javascript
const map = fn => m => {
  // ???
}
```

Wow, that was easy. By default, languages like
[Haskell](http://learnyouahaskell.com/higher-order-functions) and
[Elm](https://guide.elm-lang.org) automatically
[curry](https://robertwpearce.com/ramda-chops-function-currying.html) all of
their function parameters. There are ways to automate that in JavaScript (see
[my article on
currying](https://robertwpearce.com/ramda-chops-function-currying.html)), but
for today, we will _manually_ curry functions by using arrow functions to simulate
it: `const sum = a => b => a + b`, for example.

Lastly, on the function definition side, it would be helpful for readers of our
code to understand more about the types that are intended. In lieu of JavaScript
not having a static type checker and me not knowing
[TypeScript](https://www.typescriptlang.org/) yet, we'll do this using a
Haskell-style pseudo-type signature:

```haskell
map :: Functor f => (a -> b) -> f a -> f b
```

And we can use that as a comment above our function:

```javascript
// map :: Functor f => (a -> b) -> f a -> f b
const map = fn => m => {
  // ???
}
```

Woah, woah, woah! What's all this? Let's break it down.

```haskell
map :: Functor f => (a -> b) -> f a -> f b
--  |     |            |     |   |      |
--  1     2            3     4   5      6
```

1. Can be read, "has the type of"
1. Anything after the `::` and before the `=>` in a signature is a [class
   constraint](http://www.learnyouahaskell.com/types-and-typeclasses). This
   dictates that we're going to use something in the type signature that obeys
   the [Functor Laws](https://wiki.haskell.org/Functor), _identity_ and
   [_composition_](https://robertwpearce.com/ramda-chops-function-composition.html).
   The lowercase `f` represents what the `Functor` will be in the signature.
1. Our `map`ping function; e.g., `x => x.id`, like we did above.
1. `->` Arrows are used in type signatures to say "then return...". In our
   `map` signature, we say, "We accept a function from `a` to `b` then return a
   function that accepts `f` of `a` and then return `f` of `b`". If we were
   summing three numbers, `sum3 :: Number -> Number -> Number -> Number`, this
   would read, "`sum3` has the type of an expression that accepts a `Number`
   that returns a function that accepts a `Number` then return a function that
   accepts a `Number` and then return a `Number`."
1. `f a` says that a `Functor`, `f`, wraps some other type, `a`. A concrete
   example of this would be `[Number]`, which would be a list of numbers; in
   JavaScript parlance, an `Array` of `Number`s.
1. `f b` says that a `Functor`, `f`, wraps some other type, `b`. Why isn't it
   `a`? This signifies that when we take in the `Functor` of any type `a`, it's
   totally cool if you want to change the return type inside the `Functor`. For
   example, when we take `[{ id: 'a' }, { id: 'b' }]` and use `map` to turn that
   into `['a', 'b']`, we're taking `[Object]` (a list of `Object`s) and turning
   that into `[String]` (a list of `String`s).

All together now! "`map` has the type of an expression where `f` is a `Functor`,
and it accepts a function from `a` to `b` then returns a function that accepts
`f` of `a` and then returns `f` of `b`."

## `map` an `Array`
Let's `map` an `Array`!

_If you're not familiar with `Array.prototype.map` already, check out my video
video on [Using JavaScript's Array.prototype.map Method](https://www.youtube.com/watch?v=tjjg3_jyD7M)
or my post on [JavaScript: Understand Array.prototype.map by Reimplementing
It](https://robertwpearce.com/javascript-understand-array-prototype-map-by-reimplementing-it.html)._

Remember our `Functor` class constraint?

```haskell
map :: Functor f => (a -> b) -> f a -> f b
```

Guess what? `Array`s are `Functor`s! The easy way to figure this out is to ask,
"Is this `map`pable?" Here's how we can do a quick check that `Array` adheres to
the laws of _identity_ and _composition_:

```javascript
// identity
[1,2,3].map(x => x) // [1,2,3]

// composition
const add10 = x => x + 10
const mult2 = x => x * 2
[1,2,3].map(add10).map(mult2)     // [ 22, 24, 26 ]
// is equivalent to...
[1,2,3].map(x => mult2(add10(x))) // [ 22, 24, 26 ]

// another example of the composition law
const compose = (f, g) => x => f(g(x))
mult2(add10(2)) === compose(mult2, add10)(2) // true

// and applied back to our prior example
[1,2,3].map(add10).map(mult2)      // [ 22, 24, 26 ]
[1,2,3].map(x => mult2(add10(x)))  // [ 22, 24, 26 ]
[1,2,3].map(compose(mult2, add10)) // [ 22, 24, 26 ]
```

Since we kow that `Array` is `map`pable, we can use our `map` function to check
if the `f a` parameter is an `Array` and then use `map` with the function that
goes from `a` to `b`:

```javascript
// map :: Functor f => (a -> b) -> f a -> f b
const map = fn => m => {
  if (isArray(m)) {
    return mapArray(fn, m)
  }
}

// isArray :: a -> Bool
const isArray = x => Array.isArray(x)

// mapArray :: (a -> b) -> Array -> Array
const mapArray = (fn, m) => m.map(x => fn(x))
```

Here, we check to see if the argument, `m`, is an `Array` ([read more about
Array.isArray()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Array/isArray)),
then we call a function, `mapArray`, that handles the `map`ping of the `Array`.

You might be thinking: why `m.map(x => fn(x))` and not `m.map(fn)`? As you might
remember from [my article on re-implementing
`Array.prototype.map`](https://robertwpearce.com/javascript-understand-array-prototype-map-by-reimplementing-it.html),
there are a few other arguments that the native implementation of `map` provide,
as well as some potential changes to the `this` keyword in your callback
function scope. Instead of allowing those to pass through, we simply take the
first argument, the currently iterated value, and send that to the callback
function.

Now that we've seen the easy way to do `map` with `Array`, let's see what this
would look like if we felt like implementing `mapArray` ourselves:

```javascript
// mapArray :: (a -> b) -> Array -> Array
const mapArray = (fn, m) => {
  const newArray = []

  for (let i = 0; i < m.length; i++) {
    newArray[i] = fn(m[i])
  }

  return newArray
}
```

Not too shabby! All we do is create a new `Array` and set the results of
calling the callback function with each item to its index in the new `Array`
and then return that `Array`.

Do you think our `map` function can handle an `Array` of `Array`s?

```javascript
map(x => x * 2)([ [1,2], [3,4], [5,6] ])
// Array(3) [ NaN, NaN, NaN ]
```

While we can successfully iterate over the 3 items in the top-level `Array`, our
callback function can't perform operations like `[1,2] * 2`! We need to do
another `map` on the nested `Array`s:

```javascript
map(map(x => x * 2))([ [1,2], [3,4], [5,6] ])
// [ [2,4], [6,8], [10,12] ]
```

Well done! What else can you `map`? We're now going to get out of charted waters
and go out into the unknown.

## `map` an `Object`
Let's say we have an `i18n` (short for "internationalization") object that we've
been given that has a terribly annoying issue: every translation is prefixed
with an underscore (`_`)!

```javascript
const i18n = {
  'en-US': {
    dayMode: '_Day mode',
    greeting: '_Hello!',
    nightMode: '_Night Mode'
  },
  'es-ES': {
    dayMode: '_Modo día',
    greeting: '_¡Hola!'
    nightMode: '_Modo nocturno'
  }
}
```

We could manually delete each one, or we could find and replace with our text
editor, or we could write a `for` loop to do this. But because we're super
awesome functional programmers, we'll try to `map` over the `Object` and write a
function that removes the prefixed underscore.

Before we can do this, we need to see what happens when we call `.map()` on an
`Object`:

```javascript
i18n['en-US'].map(x => x.slice(1))
// TypeError: i18n['en-US'].map is not a function
```

Oh no! If we can't even fix the `en-US` `Object`, how are are we supposed to fix
_all_ of them? Let's update our `map` function to handle working with `Object`s.

```javascript
// map :: Functor f => (a -> b) -> f a -> f b
const map = fn => m => {
  if (isArray(m)) {
    return mapArray(fn, m)
  }

  if (isObject(m)) {
    return mapObj(fn, m)
  }
}

// isObject :: a -> Bool
const isObject = x =>
  !!x && Object.prototype.toString.call(x) === '[object Object]'

// mapObj :: (a -> b) -> { k: a } -> { k: b }
const mapObj = (fn, m) => {
  const obj = {}

  for (const [k, v] of Object.entries(m)) {
    obj[k] = fn(v)
  }

  return obj
}
```

Here, we test if something is an object by using [`Object.prototype.toString`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/toString)
and make sure to `.call(x)` instead of just `.toString(x)`, for this reason:

```javascript
Object.prototype.toString(null)
// "[object Object]"

Object.prototype.toString.call(null)
// "[object Null]"

Object.prototype.toString([])
// "[object Object]"

Object.prototype.toString.call([])
// "[object Array]"

Object.prototype.toString.call({})
// "[object Object]"
```

We then use our new `mapObj` function whose signature is

```haskell
mapObj :: (a -> b) -> { k: a } -> { k: b }
```

`mapObj` takes a function from `a` to `b` then returns a function that accepts
an `Object` with a key(s) and some value, `a` and returns an `Object` with a
key(s) and some value `b`. In short, it maps the _values_ of an `Object`. Our
`mapObj` function is nothing more than a `for` loop over each value! It calls
the callback function with value and returns a new object with the same key but
an updated value.

Let's try it out:

```javascript
const i18n = {
  'en-US': {
    dayMode: '_Day mode',
    greeting: '_Hello!',
    nightMode: '_Night Mode'
  },
  'es-ES': {
    dayMode: '_Modo día',
    greeting: '_¡Hola!',
    nightMode: '_Modo nocturno'
  }
}
map(x => x.slice(1))(i18n['en-US'])
// {
//   dayMode: 'Day mode',
//   greeting: 'Hello!',
//   nightMode: 'Night Mode'
// }
```

Okay – what about our enire `i18n` object?

```javascript
map(map(x => x.slice(1)))(i18n)
// {
//  'en-US': {
//    dayMode: 'Day mode',
//    greeting: 'Hello!',
//    nightMode: 'Night Mode'
//  },
//  'es-ES': {
//    dayMode: 'Modo día',
//    greeting: '¡Hola!',
//    nightMode: 'Modo nocturno'
//  }
// }
```

Since we're dealing with nested objects, we need to use `map` on an `Object`
inside an `Object`! We pass a nested `map`ping function, and our little
underscore problem is gone!

## `map` a `Function`

## `map` a `Functor`

## Wrapping Up

* * *

Thank you for reading!
<br />
Robert
