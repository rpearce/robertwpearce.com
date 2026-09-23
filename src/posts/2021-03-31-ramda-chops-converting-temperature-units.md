---
author: "Robert Pearce"
authorTwitter: "@RobertWPearce"
desc: "Rip apart a perfectly fine Celsius-to-Fahrenheit function, rebuild it with Ramda's add, multiply, and divide, and walk backwards into function composition."
keywords: "ramda, ramda.js, ramdajs, ramda tutorial, ramda add, ramda multiply, ramda divide, ramda compose, ramda pipe, function composition, celsiusToFahrenheit, javascript"
title: "Ramda Chops: Converting Temperature Units"
---

_This post is adapted from my [Ramda
Guide](https://github.com/rpearce/ramda.guide) book project. Thanks to Steve
Purr, Tom Wilson, and Ian Greulich for their reviews._

> Life is far too important a thing ever to talk seriously about.
>
> — <cite>Oscar Wilde, Lady Windermere's Fan</cite>

To kick off our Ramda journey, we're going to do something ridiculous: transform
very clear temperature conversion functions that use JS math operators to use
only functions for the operations!

Sometimes, when we forego the obvious and choose to approach problems in
different ways, interesting patterns may emerge that can expand our
understanding.

Let's get introduced to some perfectly fine conversion functions — one of which
we are going to rip apart and make anew.

```javascript
function celsiusToFahrenheit(celsius) {
  return celsius * (9 / 5) + 32
}

function fahrenheitToCelsius(fahrenheit) {
  return 5 / 9 * (fahrenheit - 32)
}

function easyCelsiusToFahrenheit(celsius) {
  return celsius * 2 + 30
}

function easyFahrenheitToCelsius(fahrenheit) {
  return (fahrenheit - 30) / 2
}
```

While the `celsiusToFahrenheit` and `fahrenheitToCelsius` functions are exact
formulas, they're not practical for everyday use. I've lived in the UK and New
Zealand, and since I'm married to a Kiwi, I need to easily convert between
Celsius and Fahrenheit. While not exact, the `easyCelsiusToFahrenheit` and
`easyFahrenheitToCelsius` formulas are easy to do in one's head and are close
enough to the real values.

We are going to single out `celsiusToFahrenheit` for this extended example.

```javascript
function celsiusToFahrenheit(celsius) {
  return celsius * (9 / 5) + 32
}
```

* * *

_Other posts in this series:_

* [Ramda Chops: Getting Started with Ramda](/ramda-chops-getting-started-with-ramda.html)
* [Ramda Chops: Comparing Two Temperatures](/ramda-chops-comparing-two-temperatures.html)
* [Ramda Chops: Reading Shallow Object Properties](/ramda-chops-reading-shallow-object-properties.html)
* [Ramda Chops: Testing Shallow Object Properties](/ramda-chops-testing-shallow-object-properties.html)
* [Ramda Chops: Introducing FP at Work](/ramda-chops-introducing-fp-at-work.html)

## Enter the Ramda

In it, we:

1. multiply the Celsius value by the result of `9 / 5`
1. add `32` to the result of the prior step(s)

Before we go further, let's first convert it to an arrow function expression,
for doing so will open some interesting doors.

```javascript
const celsiusToFahrenheit = celsius =>
  celsius * (9 / 5) + 32
```

Next, let's get Ramda pulled into the picture.

Ramda has a number of math functions, namely
[`multiply`](https://ramdajs.com/docs/#multiply),
[`divide`](https://ramdajs.com/docs/#divide), and
[`add`](https://ramdajs.com/docs/#add) that we can leverage in place of `*`,
`/`, and `+`. Each function takes two arguments, and each function will wait to
evaluate itself until you provide all the arguments. Check this out:

```javascript
add(1, 2)     // 3
add(1)(2)     // 3
add()(1, 2)   // 3
add()(1)()(2) // 3
```

This is indeed weird, and it's called currying; [Ramda Chops: Function
Currying](/ramda-chops-function-currying.html) covers it fully.

For now, let's import those and use them!

```javascript
import { add, divide, multiply } from 'ramda'

const celsiusToFahrenheit = celsius =>
  add(multiply(celsius, divide(9, 5)), 32)

celsiusToFahrenheit(100) // 212
```

Woah, woah, woah! What's going on here?!

It looks like we're...

1. adding `32` to the result of
1. multiplying the Celsius value by the result of dividing `9` by `5`

That's the same process we did before, but it's merely explained differently!

With addition and multiplication, there's something called the
[commutative law](https://www.mathsisfun.com/definitions/commutative-law.html)
that states we can provide the arguments to an addition and multiplication
operation in any order. Let's leverage this law in order to move our variable,
`celsius`, further toward the edge of our function to judge how it feels.

```javascript
// this is what we're starting with
add(multiply(celsius, divide(9, 5)), 32)

// first, swap `celsius` and `divide(9/5)`
add(multiply(divide(9, 5), celsius), 32)
//               ^------------^

// next, swap the multiplication and `32`
add(32, multiply(divide(9, 5), celsius))
//  ^------^

// the result
const celsiusToFahrenheit = celsius =>
  add(32, multiply(divide(9, 5), celsius))
```

Interesting! Do you see it yet? The forwarding of a result from function to
function? Let's look at this another way:

```javascript
const celsiusToFahrenheit = celsius => {
  const multiplied = multiply(divide(9, 5), celsius)
  const added = add(32, multiplied)

  return added
}
```

We provide `celsius` as the second argument to `multiply`, then we provide the
result of that as the second argument to `add`. We're simply forwarding the
evaluated result of a computation to another function; kind of like passing an
electric guitar's signal through a few effects pedals and then out the
amplifier.

What if we had a cleaner way to link these functions together so we can easily
understand what `celsiusToFahrenheit` is composed of and then provide the data
at the end?

It's time to take this first lesson into overdrive.

## A Taste of Composition

We need a way of passing the result of calling one function to another function
and having that run. It'd be easier if we could abstract an API... let's try
that.

```javascript
// this is essentially what we have
// with our celsiusToFahrenheit
f2(f1(value))

// but we want something like this;
// let's call it `link` because
// we're linking functions together
link(f2, f1)(value)
```

With that desired outcome in mind, let's try to write `link`!

```javascript
const link = (f2, f1) => value =>
  f2(f1(value))
```

Ha! We're still doing the difficult to follow `f2(f1(value))`, but now we can
use this like `link(f2, f1)(value)`.

Circling back to `celsiusToFahrenheit`, let's try to use this `link`
abstraction:

```javascript
// before
const celsiusToFahrenheit = celsius =>
  add(32, multiply(divide(9, 5), celsius))

// after
const celsiusToFahrenheit = celsius =>
  link(add(32), multiply(divide(9, 5)))(celsius)

celsiusToFahrenheit(100) // 212
```

Nice! We can now do a little less inside-out reading. But something doesn't feel
quite right... Why are we accepting the argument `celsius` in our
`celsiusToFahrenheit` function only to turn right back around and call `link()`
with the `celsius` value? Do we need it?

Nope.

```javascript
// before
const celsiusToFahrenheit = celsius =>
  link(add(32), multiply(divide(9, 5)))(celsius)

// after
const celsiusToFahrenheit =
  link(add(32), multiply(divide(9, 5)))

celsiusToFahrenheit(100) // 212
```

You may be wondering why `link` reads right to left. Two short answers are:

1. Mathematics writes `f(x)` and not `(x)f`
1. Evaluation is done from right to left (inside -> outside), so we are
  [right-associative](https://en.wikipedia.org/wiki/Operator_associativity)

However, let me ease your worried mind and make a `linkL` (`L` for "left")
function for us to use:

```javascript
const linkL = (f1, f2) => value =>
  f2(f1(value))
```

And when we compare that to the original function, we realize that we've come
nearly full circle but with a whole new perspective:

```javascript
// where we started
const celsiusToFahrenheit = celsius =>
  celsius * (9 / 5) + 32
//        ^    ^    ^
//  multiply   |    |
//           divide |
//                 add

// where we ended up
const celsiusToFahrenheit =
  linkL(multiply(divide(9, 5)), add(32))
```

Ramda provides a few functions, [`compose`](https://ramdajs.com/docs/#compose)
(or [`o`](https://ramdajs.com/docs/#o)) and
[`pipe`](https://ramdajs.com/docs/#pipe) that do the `link` and `linkL` work for
us!

```javascript
import {
  add,
  compose,
  divide,
  multiply,
  pipe,
} from 'ramda'

// `compose` and `o` are very similar
const celsiusToFahrenheit =
  compose(add(32), multiply(divide(9, 5)))

// `pipe`
const celsiusToFahrenheit =
  pipe(multiply(divide(9, 5)), add(32))
```

[Ramda Chops: Function Composition](/ramda-chops-function-composition.html)
covers function composition a bit more.

## Your Turn

Can you convert the remaining temperature conversion functions to use Ramda
functions? Give them a try in [a pre-loaded Ramda
REPL][repl-ygbkvfj4].

Here they are again, in case that link doesn't work:

```javascript
function fahrenheitToCelsius(fahrenheit) {
  return 5 / 9 * (fahrenheit - 32)
}

function easyCelsiusToFahrenheit(celsius) {
  return celsius * 2 + 30
}

function easyFahrenheitToCelsius(fahrenheit) {
  return (fahrenheit - 30) / 2
}

const result = () => ({
  '212F = 100C': fahrenheitToCelsius(212),
  '25C ≈ 80F': easyCelsiusToFahrenheit(25),
  '60F ≈ 15C': easyFahrenheitToCelsius(60),
})

result()
```

When you're done, compare them against [my
solutions][repl-yhe4pm2w]!

And here are my solutions, in case that link doesn't work either:

```javascript
//function fahrenheitToCelsius(fahrenheit) {
//  return 5 / 9 * (fahrenheit - 32)
//}

// This is the best I could do before I had to cheat... see below!
//const fahrenheitToCelsius = fahrenheit =>
//  multiply(divide(5, 9), subtract(fahrenheit, 32))

// If you're feeling clever, check this out:
// https://ramdajs.com/docs/#__
const fahrenheitToCelsius =
  compose(multiply(divide(5, 9)), subtract(__, 32))

// ===============================================================

//function easyCelsiusToFahrenheit(celsius) {
//  return celsius * 2 + 30
//}

// Step 1:
//const easyCelsiusToFahrenheit = celsius =>
//  add(30, multiply(2, celsius))

// Step 2:
const easyCelsiusToFahrenheit =
  compose(add(30), multiply(2))

// ===============================================================

//function easyFahrenheitToCelsius(fahrenheit) {
//  return (fahrenheit - 30) / 2
//}

// This is the best I could do before I had to cheat... see below!
//const easyFahrenheitToCelsius = fahrenheit =>
//  divide(subtract(fahrenheit, 30), 2)

// If you're feeling clever, check this out:
// https://ramdajs.com/docs/#__
const easyFahrenheitToCelsius =
  compose(divide(__, 2), subtract(__, 30))

// ===============================================================

const result = () => ({
  '212F = 100C': fahrenheitToCelsius(212),
  '25C ≈ 80F': easyCelsiusToFahrenheit(25),
  '60F ≈ 15C': easyFahrenheitToCelsius(60),
})

result()
```

## Wrapping Up

This turned out to be far from a gentle introduction!

We started with some addition, division, and multiplication to convert
temperature values, and we ended up walking backwards into the heart of
functional programming.

Way to go!

[repl-ygbkvfj4]: https://ramdajs.com/repl/#?function%20fahrenheitToCelsius%28fahrenheit%29%20%7B%0A%20%20return%205%20%2F%209%20%2A%20%28fahrenheit%20-%2032%29%0A%7D%0A%0Afunction%20easyCelsiusToFahrenheit%28celsius%29%20%7B%0A%20%20return%20celsius%20%2A%202%20%2B%2030%0A%7D%0A%0Afunction%20easyFahrenheitToCelsius%28fahrenheit%29%20%7B%0A%20%20return%20%28fahrenheit%20-%2030%29%20%2F%202%0A%7D%0A%0Aconst%20result%20%3D%20%28%29%20%3D%3E%20%28%7B%0A%20%20%27212F%20%3D%20100C%27%3A%20fahrenheitToCelsius%28212%29%2C%0A%20%20%2725C%20%E2%89%88%2080F%27%3A%20easyCelsiusToFahrenheit%2825%29%2C%0A%20%20%2760F%20%E2%89%88%2015C%27%3A%20easyFahrenheitToCelsius%2860%29%2C%0A%7D%29%0A%0Aresult%28%29%0A
[repl-yhe4pm2w]: https://ramdajs.com/repl/#?%2F%2Ffunction%20fahrenheitToCelsius%28fahrenheit%29%20%7B%0A%2F%2F%20%20return%205%20%2F%209%20%2A%20%28fahrenheit%20-%2032%29%0A%2F%2F%7D%0A%0A%2F%2F%20This%20is%20the%20best%20I%20could%20do%20before%20I%20had%20to%20cheat...%20see%20below%21%0A%2F%2Fconst%20fahrenheitToCelsius%20%3D%20fahrenheit%20%3D%3E%0A%2F%2F%20%20multiply%28divide%285%2C%209%29%2C%20subtract%28fahrenheit%2C%2032%29%29%0A%0A%2F%2F%20If%20you%27re%20feeling%20clever%2C%20check%20this%20out%3A%0A%2F%2F%20https%3A%2F%2Framdajs.com%2Fdocs%2F%23__%0Aconst%20fahrenheitToCelsius%20%3D%0A%20%20compose%28multiply%28divide%285%2C%209%29%29%2C%20subtract%28__%2C%2032%29%29%0A%0A%2F%2F%20%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%0A%0A%2F%2Ffunction%20easyCelsiusToFahrenheit%28celsius%29%20%7B%0A%2F%2F%20%20return%20celsius%20%2A%202%20%2B%2030%0A%2F%2F%7D%0A%0A%2F%2F%20Step%201%3A%0A%2F%2Fconst%20easyCelsiusToFahrenheit%20%3D%20celsius%20%3D%3E%0A%2F%2F%20%20add%2830%2C%20multiply%282%2C%20celsius%29%29%0A%0A%2F%2F%20Step%202%3A%0Aconst%20easyCelsiusToFahrenheit%20%3D%0A%20%20compose%28add%2830%29%2C%20multiply%282%29%29%0A%0A%2F%2F%20%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%0A%0A%2F%2Ffunction%20easyFahrenheitToCelsius%28fahrenheit%29%20%7B%0A%2F%2F%20%20return%20%28fahrenheit%20-%2030%29%20%2F%202%0A%2F%2F%7D%0A%0A%2F%2F%20This%20is%20the%20best%20I%20could%20do%20before%20I%20had%20to%20cheat...%20see%20below%21%0A%2F%2Fconst%20easyFahrenheitToCelsius%20%3D%20fahrenheit%20%3D%3E%0A%2F%2F%20%20divide%28subtract%28fahrenheit%2C%2030%29%2C%202%29%0A%0A%2F%2F%20If%20you%27re%20feeling%20clever%2C%20check%20this%20out%3A%0A%2F%2F%20https%3A%2F%2Framdajs.com%2Fdocs%2F%23__%0Aconst%20easyFahrenheitToCelsius%20%3D%0A%20%20compose%28divide%28__%2C%202%29%2C%20subtract%28__%2C%2030%29%29%0A%0A%2F%2F%20%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%3D%0A%0Aconst%20result%20%3D%20%28%29%20%3D%3E%20%28%7B%0A%20%20%27212F%20%3D%20100C%27%3A%20fahrenheitToCelsius%28212%29%2C%0A%20%20%2725C%20%E2%89%88%2080F%27%3A%20easyCelsiusToFahrenheit%2825%29%2C%0A%20%20%2760F%20%E2%89%88%2015C%27%3A%20easyFahrenheitToCelsius%2860%29%2C%0A%7D%29%0A%0Aresult%28%29
