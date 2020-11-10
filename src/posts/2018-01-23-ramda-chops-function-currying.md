---
author: "Robert Pearce"
authorTwitter: "@RobertWPearce"
desc: "How function currying works under the hood."
image: "/images/shepherd-flock.jpg"
keywords: "ramda, ramda.js, ramdajs, ramda tutorial, ramda curry, crocks curry, function currying, ramda fp"
photoCredit: "Biegun Wschodni"
photoWebsite: "https://unsplash.com/@biegunwschodni"
title: "Ramda Chops: Function Currying"
---

_Thanks to [Jillian Silver](https://github.com/jsilve) and
[@evilsoft](https://twitter.com/evilsoft) for their review of this post._

Functional Programming concepts have been pouring into the JavaScript community
for a number of years now, and many of us struggle to keep up. I've been lucky
enough to be able to work with some mentors and functional tools that have
helped me along the way. One of these tools is [ramda.js](http://ramdajs.com),
and it was my gateway to the larger Functional Programming world. I hope it will
be for you, as well.

To understand ramda, you first have to understand a concept known as "currying."
The ramda website states,

> The parameters to Ramda functions are arranged to make it convenient for
> currying.

There are some function currying articles on
the ramda site, such as [Favoring Curry](http://fr.umio.us/favoring-curry/) and
[Why Curry Helps](https://hughfdjackson.com/javascript/why-curry-helps/) by
Scott Sauyet, which are great for explaining the benefits and power of currying.
Those articles (and many other resources) do great jobs of explaining how to use
currying and why, so I'll briefly touch on those points, but I really want to
focus on how it works under the hood and how this funny little concept will
completely change the way that you program.

* * *

_Other ramda posts:_

* [Ramda Chops: Function Composition](/ramda-chops-function-composition.html)
* [Ramda Chops: Safely Accessing Properties](/ramda-chops-safely-accessing-properties.html)
* [Ramda Chops: Map, Filter & Reduce](/ramda-chops-map-filter-and-reduce.html)

## Rudimentary Currying
Many articles already cover this, so I'll keep it short.

Let's start with a function that takes two numbers and adds them together:

```js
// add :: (Number, Number) -> Number
const add = (a, b) =>
  a + b
```

As our fake type signature describes, `add` takes two arguments (essentially, a
[tuple](https://en.wikibooks.org/wiki/Haskell/Lists_and_tuples#Tuples<Paste>))
that are both of type `Number` and returns a value of type `Number`.

But if we wanted to create a function that adds `10` to anything, we could write
the following:

```js
// add :: Number -> Number -> Number
const add = a => b =>
  a + b

// which is the same as

function add(a) {
  return function(b) {
    return a + b
  }
}

// and then

// add10 :: Number -> Number
const add10 = add(10)

add10 // => Function
add10(4) // => 14
```

Note the change in type signature: we now have singular arguments that are
accepted at a time instead of the tuple style. When we provide the first
argument, we are then returned a function that will sit and wait until all the
functions are applied before giving us a value. This method can be useful in
many situations, but consider the following:

```js
add(10)(4)
```

That feels awkward, right? Fear not! There is a way.

## The `curry` Function
Ramda provides us a function named `curry` that will take what might be
considered a "normal" JavaScript function definition with multiple parameters
and turn it into a function that will keep returning a function until all of
its parameters have been supplied. Check it out!

```js
import curry from 'ramda/src/curry'

const oldAdd = (a, b) =>
  a + b

const add = curry(oldAdd)

add(10) // => Function
add(10)(4) // => 14
add(10, 4) // => 14
```

Or if you want to have `curry` baked in to your original `add` function:

```js
// add :: Number -> Number -> Number
const add = curry((a, b) ->
  a + b
)
```

The magical `curry` function doesn't care when you provide arguments or how you
do so – it will just keep returning you partially applied functions until all
arguments have been applied, at which point it will give you back a value.

## Cool... Now How Does `curry` Work?
This might seem blasphemous, but to understand how `curry` works under the hood,
we're going to dive into a different library's implementation of it:
[crocks](https://github.com/evilsoft/crocks) by
[@evilsoft](https://twitter.com/evilsoft). (Crocks is similar to ramda but dives
more into abstract data types (ADTs) and is more towards the deeper end of the
Functional Programming pool.) I think crocks' implementation is excellent, and
99% of it being in one file makes for a great teaching tool.

If you want to jump ahead, here is a link to crocks' `curry` function:
[https://github.com/evilsoft/crocks/blob/master/src/core/curry.js](https://github.com/evilsoft/crocks/blob/master/src/core/curry.js)

Where do we start with understanding this next-level JavaScript? Always start
with the types, as they can tell a story.

### Reading `curry`'s Story
What does this tell us?

```js
// curry :: ((a, b, c) -> d) -> a -> b -> c -> d
```

1. `((a, b, c) -> d)` tells us that it accepts a function that has _n_
   parameters of any type and returns a value of any type
1. `-> a -> b -> c` tells us that it then accepts each parameter – but only 1 at
   a time!
1. `-> d` tells us that it ultimately returns the value as specified in the
   function

Sounds simple, right? Easier said than done!

### What we need to do
1. we need to first accept a function (the one to be curried)
1. we need to then accept [any number of arguments (variadic behavior)](/simple-variadic-behavior.html)
1. when this happens, we need to either
  * return a value (when all arguments have been applied)
  * return a function that accepts the remaining arguments and repeat this
  condition

### Breaking Down Curry
```js
// curry :: ((a, b, c) -> d) -> a -> b -> c -> d
//
// 1. we accept a function
const curry = (fn) => {
  // 2. we return a function taking any `n` arguments
  return (...xs) => {
    // make sure we have a populated list to work with;
    // `undefined` is the value for the Unit type in
    // crocks and calling our function must utilize some
    // sort of value.
    const args =
      xs.length ? xs : [ undefined ]

    // if the number of args sent are
    // less than that required, then
    // don't do more work; go ahead and
    // return a new version of our function
    // that is still waiting for more
    // arguments to be applied.
    if (args.length < fn.length) {
      // way of safely creating a new function
      // and binding arguments to it without
      // calling it.
      return curry(Function.bind.apply(fn, [ null ].concat(args)))
    }

    // if we've provided all arguments,
    // then let's apply them and give
    // back the result.
    //
    // otherwise, let's do some work
    // and see if, based on the number
    // of arguments, we return a new
    // function with fewer arguments
    // or go ahead and call the function
    // with the final argument so we can
    // get back a value.
    //
    // NOTE: `applyCurry` is defined below.
    const val =
      args.length === fn.length
        ? fn.apply(null, args)
        : args.reduce(applyCurry, fn)

    // 3. if our value is still a function, then
    // let's return the curried version of our
    // function that still needs some arguments
    // to be applied and repeat everything above.
    //
    // otherwise, we're all done here, so
    // let's return the value.
    return isFunction(val)
      ? curry(val)
      : val
  }
}

const applyCurry = (fn, arg) => {
  // return whatever we received if
  // fn is actually NOT a function.
  if (!isFunction(fn)) { return fn }

  // if we have more than 1 argument
  // remaining to be applied, then let's
  // bind a value to the next argument and
  // keep going.
  //
  // otherwise, then yay let's go ahead
  // and call that function with the argument;
  // our `[ undefined ]` default saves us from
  // some potential headache here.
  return fn.length > 1
    ? fn.bind(null, arg)
    : fn.call(null, arg)
}

const isFunction = x =>
  typeof x === 'function'
```

With all of these checks in here, we can now run the following code and have it
all work:

```js
const add = curry((a, b) => a + b)

add // => Function
add(1) // => Function
add(1)(2) // => 3
add(1, 2) // => 3
add(1, 2, 99) // => 3 (we don't care about the last one!)
add(1, 2, 99, 2000) // => 3 (we don't care about the last two!)
```

## `curry` In Action
If all of your functions are curried, you can start writing code that you never
would have been able to before. Here is a small taste that we will cover more
fully in a future Ramda Chops:

```js
// addOrRemove :: a -> Array -> Array
const addOrRemove = x =>
  ifElse(
    contains(x),
    without(of(x)),
    append(x)
  )

// addOrRemoveTest :: Array -> Array
const addOrRemoveTest =
  addOrRemove('test')

addOrRemoveTest([ 'thing' ]) // => ["thing", "test"]
addOrRemoveTest([ 'thing', 'test' ]) // => ["thing"]
```

([View this example in a live REPL](https://goo.gl/5uRhS2))

The `addOrRemove` function almost reads like English: "If something contains
`x`, give me back that something without `x`; otherwise, append `x` to that
something." What is worth understanding here is that these functions each accept
a number of arguments where _the most generic/reusable are provided
first_ (this is a tenet of Functional Programming). Here, we are able to create
a very reusable function with partially applied values that sits and waits until
the final bit – an array – is provided.

* * *

Thanks for reading! Until next time,
<br>
Robert
