---
author: "Robert Pearce"
authorTwitter: "@RobertWPearce"
desc: "Compare any two temperatures, Celsius or Fahrenheit, with Ramda's min, max, minBy, and maxBy, and learn to ask better questions about requirements along the way."
keywords: "ramda, ramda.js, ramdajs, ramda tutorial, ramda min, ramda max, ramda minBy, ramda maxBy, javascript"
title: "Ramda Chops: Comparing Two Temperatures"
---

_This post is adapted from my [Ramda
Guide](https://github.com/rpearce/ramda.guide) book project. Thanks to Steve
Purr, Tom Wilson, and Ian Greulich for their reviews._

> It doesn't matter what temperature the room is — it's always room temperature.
>
> — <cite>Steven Wright</cite>

We're at work, and we are tasked with writing two functions that compare any
two temperatures:

* One that gets the minimum value
* One that gets the maximum value

We're not sure why they don't use `Math.min` and `Math.max` themselves, but we
do what they ask.

```javascript
const minTemperature = (a, b) => Math.min(a, b)
const maxTemperature = (a, b) => Math.max(a, b)

minTemperature(100, 200) // 100
maxTemperature(100, 200) // 200
```

Once we share that solution, we are reminded that task is to compare
_any two temperatures_; for example, `100`, `200`, `'50F'`, `'25C'`.

Dangit... now we look silly. Okay, let's try this again to make it handle
strings:

```javascript
const minTemperature = (a, b) => a > b ? b : a
const maxTemperature = (a, b) => a < b ? b : a

minTemperature(100, 200)       // 100
maxTemperature(100, 200)       // 200
minTemperature('200F', '100F') // '100F'
maxTemperature('200F', '100F') // '200F'
```

Then we realize that Ramda has [`min`](https://ramdajs.com/docs/#min) and
[`max`](https://ramdajs.com/docs/#max) functions, so we use those and
pat ourselves on the back:

```javascript
import { max, min } from 'ramda'

min(100, 200)       // 100
max(100, 200)       // 200
min('200F', '100F') // '100F'
max('200F', '100F') // '200F'
```

We go back and tell our team to use Ramda's functions, but we're wrong again,
and we realize that we probably should have asked for a thorough list of inputs.

But what could we have missed?

> Compare any two temperatures.

Oh, no... They might not be part of the same measurement system! This
means that `'50F'` could be compared with `'15C'`! Luckily, we're told that we don't
have to worry about Kelvin, Rankine, nor Réaumur; we only care about Celsius
and Fahrenheit. There will also _never_ be any unit-less numbers, so we can
ignore that case.

This problem just took on a whole new dimension!

Good news: in [the last
post](/ramda-chops-converting-temperature-units.html), we wrote
our `celsiusToFahrenheit` and `fahrenheitToCelsius` functions, so we have those
functions we can use to do those calculations for us.

Let's try again:

```javascript
import { max, min } from 'ramda'

const celsiusToFahrenheit =
  compose(add(32), multiply(divide(9, 5)))

const minTemperature = (a, b) => {
  const unitA = a.slice(-1) // get the last character, like C or F
  const unitB = b.slice(-1)
  const floatA = parseFloat(a)
  const floatB = parseFloat(b)

  // same temperature type
  if (unitA === unitB) {
    const maxT = min(floatA, floatB)
    return maxT === floatA ? a : b
  }

  // a is Fahrenheit but b is Celsius
  if (unitA === 'F') {
    const floatBAsF = celsiusToFahrenheit(floatB)
    const minF = min(floatA, floatBAsF)

    return minF === floatA ? a : b
  }

  // a is Celsius but b is Fahrenheit
  const floatAAsF = celsiusToFahrenheit(floatA)
  const minF = min(floatAAsF, floatB)

  return minF === floatB ? b : a
}

const maxTemperature = (a, b) => {
  const unitA = a.slice(-1) // get the last character, like C or F
  const unitB = b.slice(-1)
  const floatA = parseFloat(a)
  const floatB = parseFloat(b)

  // same temperature type
  if (unitA === unitB) {
    const maxT = max(floatA, floatB)
    return maxT === floatA ? a : b
  }

  // a is Fahrenheit but b is Celsius
  if (unitA === 'F') {
    const floatBAsF = celsiusToFahrenheit(floatB)
    const maxF = max(floatA, floatBAsF)

    return maxF === floatA ? a : b
  }

  // a is Celsius but b is Fahrenheit
  const floatAAsF = celsiusToFahrenheit(floatA)
  const maxF = max(floatAAsF, floatB)

  return maxF === floatB ? b : a
}

minTemperature('200F', '100F') // '100F'
maxTemperature('200F', '100F') // '200F'
minTemperature('50F', '25C')   // '50F'
maxTemperature('50F', '25C')   // '25C'
```

([View this large min/max temperature example in the Ramda
REPL][repl-yhpevc5c])

Goodness gracious! While we could extract some common functionality into many
small functions, there must be a simpler way. After all, isn't functional
programming supposed to help us simplify things?

(Hint: yes!)

Ramda has [`minBy`](https://ramdajs.com/docs/#minBy) and
[`maxBy`](https://ramdajs.com/docs/#maxBy) functions that will compare two
values _after_ they have been transformed by some transformation function.
Here's their example from the docs:

```javascript
import { maxBy } from 'ramda'

const square = n => n * n
maxBy(square, -3, 2) // -3
```

In this example, `maxBy` will call `square` with `-3` and `2`, and it will then
compare each of _those_ results. Whatever value has the largest result after
being applied to `square` will be the returned value. Here, `-3 * -3` is `9`,
whereas `2 * 2` is `4`, so since `9 > 4`, `-3` is our result.

Let's refactor our functions:

```javascript
import { maxBy, minBy } from 'ramda'

const asF = x => {
  if (x.slice(-1) === 'C') {
    return celsiusToFahrenheit(parseFloat(x))
  }

  return x
}

const minTemperature = (a, b) => minBy(asF, a, b)
const maxTemperature = (a, b) => maxBy(asF, a, b)

minTemperature('200F', '100F') // '100F'
maxTemperature('200F', '100F') // '200F'
minTemperature('50F', '25C')   // '50F'
maxTemperature('50F', '25C')   // '25C'
maxTemperature('50C', '25C')   // '50C'
```

([View this v1 minBy/maxBy temperature example in the Ramda
REPL][repl-yex7k5y5])

By casting all temperatures as a single unit (I chose Fahrenheit here), we can
compare any temperatures and get back their original values! `25C` is indeed
hotter than `50F`!

But wait – there's more.

Just like we noticed in the last post, our `minTemperature` and
`maxTemperature` functions are taking in `(a, b)` and are merely forwarding
those arguments on. If you recall, Ramda doesn't care _when_ you provide
arguments, so check this out...

```javascript
// before
const minTemperature = (a, b) => minBy(asF, a, b)
const maxTemperature = (a, b) => maxBy(asF, a, b)

// after
const minTemperature = minBy(asF)
const maxTemperature = maxBy(asF)
```

([View this v2 minBy/maxBy temperature example in the Ramda
REPL][repl-ygk3lg7y])

Oh, and one more thing; here's a preview of some more advanced Ramda usage
retroactively applied to making our `asF` function cleaner:

```javascript
// before
const asF = x => {
  if (x.slice(-1) === 'C') {
    return celsiusToFahrenheit(parseFloat(x))
  }

  return x
}

// after

// Celsius     = String // "100C"
// Fahrenheit  = String // "100F"
// Temperature = Celsius | Fahrenheit

// isC :: Temperature -> Bool
const isC = compose(equals('C'), slice(-1))

// stringCToF :: Celsius -> Number
const stringCToF = compose(celsiusToFahrenheit, parseFloat)

// asF :: Temperature -> Fahrenheit
const asF = when(isC, stringCToF)
```

And here is what all of the new functions together could look like!

```javascript
// Celsius     = String // "100C"
// Fahrenheit  = String // "100F"
// Temperature = Celsius | Fahrenheit

// isC :: Temperature -> Bool
const isC = compose(equals('C'), slice(-1))

// stringCToF :: Celsius -> Number
const stringCToF = compose(celsiusToFahrenheit, parseFloat)

// asF :: Temperature -> Fahrenheit
const asF = when(isC, stringCToF)

// minTemperature :: Temperature -> Temperature -> Temperature
const minTemperature = minBy(asF)

// maxTemperature :: Temperature -> Temperature -> Temperature
const maxTemperature = maxBy(asF)
```

([View this v3 minBy/maxBy temperature example in the Ramda
REPL][repl-yhb4g6dh])

Cool, right?

Fear not! You can read up on [`when`](https://ramdajs.com/docs/#when) and
[`slice`](https://ramdajs.com/docs/#slice) in Ramda's docs, and you should know
the pseudo-type signatures are optional, not exact, and aren't a substitute for
tools like [jsdoc](https://jsdoc.app).

* * *

_Other posts in this series:_

* [Ramda Chops: Getting Started with Ramda](/ramda-chops-getting-started-with-ramda.html)
* [Ramda Chops: Converting Temperature Units](/ramda-chops-converting-temperature-units.html)
* [Ramda Chops: Reading Shallow Object Properties](/ramda-chops-reading-shallow-object-properties.html)
* [Ramda Chops: Testing Shallow Object Properties](/ramda-chops-testing-shallow-object-properties.html)
* [Ramda Chops: Introducing FP at Work](/ramda-chops-introducing-fp-at-work.html)

## Wrapping Up

The person who requested these functions is going to be blown away!

What started as a simple "which temperature is smaller or larger?" question
turned out to be an exercise in asking good questions about requirements up
front.

We also walked through implementing a naïve solution and refactored it to a more
elegant one by leveraging Ramda and functional programming.

[repl-yhpevc5c]: https://ramdajs.com/repl/#?const%20celsiusToFahrenheit%20%3D%0A%20%20compose%28add%2832%29%2C%20multiply%28divide%289%2C%205%29%29%29%0A%0Aconst%20minTemperature%20%3D%20%28a%2C%20b%29%20%3D%3E%20%7B%0A%20%20const%20unitA%20%3D%20a.slice%28-1%29%20%2F%2F%20get%20the%20last%20character%2C%20like%20C%20or%20F%0A%20%20const%20unitB%20%3D%20b.slice%28-1%29%0A%20%20const%20floatA%20%3D%20parseFloat%28a%29%0A%20%20const%20floatB%20%3D%20parseFloat%28b%29%0A%0A%20%20%2F%2F%20same%20temperature%20type%0A%20%20if%20%28unitA%20%3D%3D%3D%20unitB%29%20%7B%0A%20%20%20%20const%20maxT%20%3D%20min%28floatA%2C%20floatB%29%0A%20%20%20%20return%20maxT%20%3D%3D%3D%20floatA%20%3F%20a%20%3A%20b%0A%20%20%7D%0A%0A%20%20%2F%2F%20a%20is%20Fahrenheit%20but%20b%20is%20Celsius%0A%20%20if%20%28unitA%20%3D%3D%3D%20%27F%27%29%20%7B%0A%20%20%20%20const%20floatBAsF%20%3D%20celsiusToFahrenheit%28floatB%29%0A%20%20%20%20const%20minF%20%3D%20min%28floatA%2C%20floatBAsF%29%0A%0A%20%20%20%20return%20minF%20%3D%3D%3D%20floatA%20%3F%20a%20%3A%20b%0A%20%20%7D%0A%0A%20%20%2F%2F%20a%20is%20Celsius%20but%20b%20is%20Fahrenheit%0A%20%20const%20floatAAsF%20%3D%20celsiusToFahrenheit%28floatA%29%0A%20%20const%20minF%20%3D%20min%28floatAAsF%2C%20floatB%29%0A%0A%20%20return%20minF%20%3D%3D%3D%20floatB%20%3F%20b%20%3A%20a%0A%7D%0A%0Aconst%20maxTemperature%20%3D%20%28a%2C%20b%29%20%3D%3E%20%7B%0A%20%20const%20unitA%20%3D%20a.slice%28-1%29%20%2F%2F%20get%20the%20last%20character%2C%20like%20C%20or%20F%0A%20%20const%20unitB%20%3D%20b.slice%28-1%29%0A%20%20const%20floatA%20%3D%20parseFloat%28a%29%0A%20%20const%20floatB%20%3D%20parseFloat%28b%29%0A%0A%20%20%2F%2F%20same%20temperature%20type%0A%20%20if%20%28unitA%20%3D%3D%3D%20unitB%29%20%7B%0A%20%20%20%20const%20maxT%20%3D%20max%28floatA%2C%20floatB%29%0A%20%20%20%20return%20maxT%20%3D%3D%3D%20floatA%20%3F%20a%20%3A%20b%0A%20%20%7D%0A%0A%20%20%2F%2F%20a%20is%20Fahrenheit%20but%20b%20is%20Celsius%0A%20%20if%20%28unitA%20%3D%3D%3D%20%27F%27%29%20%7B%0A%20%20%20%20const%20floatBAsF%20%3D%20celsiusToFahrenheit%28floatB%29%0A%20%20%20%20const%20maxF%20%3D%20max%28floatA%2C%20floatBAsF%29%0A%0A%20%20%20%20return%20maxF%20%3D%3D%3D%20floatA%20%3F%20a%20%3A%20b%0A%20%20%7D%0A%0A%20%20%2F%2F%20a%20is%20Celsius%20but%20b%20is%20Fahrenheit%0A%20%20const%20floatAAsF%20%3D%20celsiusToFahrenheit%28floatA%29%0A%20%20const%20maxF%20%3D%20max%28floatAAsF%2C%20floatB%29%0A%0A%20%20return%20maxF%20%3D%3D%3D%20floatB%20%3F%20b%20%3A%20a%0A%7D%0A%0AminTemperature%28%27200F%27%2C%20%27100F%27%29%20%2F%2F%20%27100F%27%0AmaxTemperature%28%27200F%27%2C%20%27100F%27%29%20%2F%2F%20%27200F%27%0AminTemperature%28%2750F%27%2C%20%2725C%27%29%20%20%20%2F%2F%20%2750F%27%0AmaxTemperature%28%2750F%27%2C%20%2725C%27%29%20%20%20%2F%2F%20%2725C%27
[repl-yex7k5y5]: https://ramdajs.com/repl/#?const%20celsiusToFahrenheit%20%3D%0A%20%20compose%28add%2832%29%2C%20multiply%28divide%289%2C%205%29%29%29%0A%0Aconst%20asF%20%3D%20x%20%3D%3E%20%7B%0A%20%20if%20%28x.slice%28-1%29%20%3D%3D%3D%20%27C%27%29%20%7B%0A%20%20%20%20return%20celsiusToFahrenheit%28parseFloat%28x%29%29%0A%20%20%7D%0A%20%20%0A%20%20return%20x%0A%7D%0A%0Aconst%20minTemperature%20%3D%20%28a%2C%20b%29%20%3D%3E%20minBy%28asF%2C%20a%2C%20b%29%0Aconst%20maxTemperature%20%3D%20%28a%2C%20b%29%20%3D%3E%20maxBy%28asF%2C%20a%2C%20b%29%0A%0AminTemperature%28%27200F%27%2C%20%27100F%27%29%20%2F%2F%20%27100F%27%0AmaxTemperature%28%27200F%27%2C%20%27100F%27%29%20%2F%2F%20%27200F%27%0AminTemperature%28%2750F%27%2C%20%2725C%27%29%20%20%20%2F%2F%20%2750F%27%0AmaxTemperature%28%2750F%27%2C%20%2725C%27%29%20%20%20%2F%2F%20%2725C%27%0AmaxTemperature%28%2750C%27%2C%20%2725C%27%29%20%20%20%2F%2F%20%2750C%27
[repl-ygk3lg7y]: https://ramdajs.com/repl/#?const%20celsiusToFahrenheit%20%3D%0A%20%20compose%28add%2832%29%2C%20multiply%28divide%289%2C%205%29%29%29%0A%0Aconst%20asF%20%3D%20x%20%3D%3E%20%7B%0A%20%20if%20%28x.slice%28-1%29%20%3D%3D%3D%20%27C%27%29%20%7B%0A%20%20%20%20return%20celsiusToFahrenheit%28parseFloat%28x%29%29%0A%20%20%7D%0A%20%20%0A%20%20return%20x%0A%7D%0A%0Aconst%20minTemperature%20%3D%20minBy%28asF%29%0Aconst%20maxTemperature%20%3D%20maxBy%28asF%29%0A%0AminTemperature%28%27200F%27%2C%20%27100F%27%29%20%2F%2F%20%27100F%27%0AmaxTemperature%28%27200F%27%2C%20%27100F%27%29%20%2F%2F%20%27200F%27%0AminTemperature%28%2750F%27%2C%20%2725C%27%29%20%20%20%2F%2F%20%2750F%27%0AmaxTemperature%28%2750F%27%2C%20%2725C%27%29%20%20%20%2F%2F%20%2725C%27%0AmaxTemperature%28%2750C%27%2C%20%2725C%27%29%20%20%20%2F%2F%20%2750C%27
[repl-yhb4g6dh]: https://ramdajs.com/repl/#?const%20celsiusToFahrenheit%20%3D%0A%20%20compose%28add%2832%29%2C%20multiply%28divide%289%2C%205%29%29%29%0A%0A%2F%2F%20Celsius%20%20%20%20%20%3D%20String%20%2F%2F%20%22100C%22%0A%2F%2F%20Fahrenheit%20%20%3D%20String%20%2F%2F%20%22100F%22%0A%2F%2F%20Temperature%20%3D%20Celsius%20%7C%20Fahrenheit%0A%0A%2F%2F%20isC%20%3A%3A%20Temperature%20-%3E%20Bool%0Aconst%20isC%20%3D%20compose%28equals%28%27C%27%29%2C%20slice%28-1%29%29%0A%0A%2F%2F%20stringCToF%20%3A%3A%20Celsius%20-%3E%20Number%0Aconst%20stringCToF%20%3D%20compose%28celsiusToFahrenheit%2C%20parseFloat%29%0A%0A%2F%2F%20asF%20%3A%3A%20Temperature%20-%3E%20Fahrenheit%0Aconst%20asF%20%3D%20when%28isC%2C%20stringCToF%29%0A%0A%2F%2F%20minTemperature%20%3A%3A%20Temperature%20-%3E%20Temperature%20-%3E%20Temperature%0Aconst%20minTemperature%20%3D%20minBy%28asF%29%0A%0A%2F%2F%20maxTemperature%20%3A%3A%20Temperature%20-%3E%20Temperature%20-%3E%20Temperature%0Aconst%20maxTemperature%20%3D%20maxBy%28asF%29%0A%0AminTemperature%28%27200F%27%2C%20%27100F%27%29%20%2F%2F%20%27100F%27%0AmaxTemperature%28%27200F%27%2C%20%27100F%27%29%20%2F%2F%20%27200F%27%0AminTemperature%28%2750F%27%2C%20%2725C%27%29%20%20%20%2F%2F%20%2750F%27%0AmaxTemperature%28%2750F%27%2C%20%2725C%27%29%20%20%20%2F%2F%20%2725C%27%0AmaxTemperature%28%2750C%27%2C%20%2725C%27%29%20%20%20%2F%2F%20%2750C%27
