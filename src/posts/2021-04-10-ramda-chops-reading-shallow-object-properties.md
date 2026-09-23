---
author: "Robert Pearce"
authorTwitter: "@RobertWPearce"
desc: "Pull data out of objects at a single level with Ramda's prop, propOr, props, pick, and pluck, using the International Space Station's location and crew as our data."
keywords: "ramda, ramda.js, ramdajs, ramda tutorial, ramda prop, ramda propOr, ramda props, ramda pick, ramda pluck, javascript"
title: "Ramda Chops: Reading Shallow Object Properties"
updated: "2021-08-12T12:00:00Z"
---

_This post is adapted from my [Ramda
Guide](https://github.com/rpearce/ramda.guide) book project. Thanks to Steve
Purr, Tom Wilson, and Ian Greulich for their reviews._

> Too much gravity argues a shallow mind.
>
> — <cite>Johann Kaspar Lavater</cite>

Let's dive (but not too _deep_!) into pulling out data at a single level from
objects!

Outline:

* [`prop`](#prop)
* [`propOr`](#propor)
* [`props`](#props)
* [`pick`](#pick)
* [`pluck`](#pluck)

We'll be using some International Space Station (ISS) and astronaut data as
our datasets. They come from Open Notify's [ISS
Now](http://open-notify.org/Open-Notify-API/ISS-Location-Now/) and [How Many
People Are In Space Right
Now?](http://open-notify.org/Open-Notify-API/People-In-Space/) endpoints (which
I found via the [Awesome JSON
Datasets](https://github.com/jdorfman/Awesome-JSON-Datasets) repository), and
they were captured on 2021-04-08.

Here is the ISS' current location:

```json
{
  "message": "success",
  "timestamp": 1617930803,
  "iss_position": {
    "latitude": "27.7270",
    "longitude": "133.2581"
  }
}
```

And here is "How Many People Are In Space Right Now?":

```json
{
  "message": "success",
  "number": 7,
  "people": [
    {
      "craft": "ISS",
      "name": "Sergey Ryzhikov"
    },
    {
      "craft": "ISS",
      "name": "Kate Rubins"
    },
    {
      "craft": "ISS",
      "name": "Sergey Kud-Sverchkov"
    },
    {
      "craft": "ISS",
      "name": "Mike Hopkins"
    },
    {
      "craft": "ISS",
      "name": "Victor Glover"
    },
    {
      "craft": "ISS",
      "name": "Shannon Walker"
    },
    {
      "craft": "ISS",
      "name": "Soichi Noguchi"
    }
  ]
}
```

We will assume we're storing those objects as variables named `iss` and
`astros`.

* * *

_Other posts in this series:_

* [Ramda Chops: Getting Started with Ramda](/ramda-chops-getting-started-with-ramda.html)
* [Ramda Chops: Converting Temperature Units](/ramda-chops-converting-temperature-units.html)
* [Ramda Chops: Comparing Two Temperatures](/ramda-chops-comparing-two-temperatures.html)
* [Ramda Chops: Testing Shallow Object Properties](/ramda-chops-testing-shallow-object-properties.html)
* [Ramda Chops: Introducing FP at Work](/ramda-chops-introducing-fp-at-work.html)

## `prop`

In vanilla JS, we can get the `timestamp` key off the ISS object like this:

```javascript
iss.timestamp

// or

iss['timestamp']
```

If that's the only functionality we'll ever need, then that's great! We can stop
here.

But what if we want to do more? For example,

1. get the `timestamp`
1. multiply the timestamp by `1000` to convert it to milliseconds
1. convert it to a `Date` string

Our first attempt might be to do this inline:

```javascript
new Date(iss.timestamp * 1000)
// "Sat Apr 10 2021 15:06:50 GMT+0000 (Coordinated Universal Time)"
```

Then we realize that we want to do this for many different ISS location objects,
so we write a function:

```javascript
const issTimeToDate = data =>
  new Date(data.timestamp * 1000)

issTimeToDate(iss)
// "Sat Apr 10 2021 15:06:50 GMT+0000 (Coordinated Universal Time)"
```

It is totally acceptable to stop at this point.

Maybe we should, but we don't.

Squinting at that code a little harder, we notice that there are three
transformations happening:

1. from the ISS object, we get the shallow property, `timestamp`
1. we multiply that value by `1000`
1. we instantiate a new `Date` with the prior result

And we also notice that if `data` is ever `undefined` or `null` (or anything
that isn't an instance of `Object`), we're going to have a problem!

```javascript
issTimeToDate(null)
// Uncaught TypeError: can't access property "timestamp", data is null
```

As you may recall from our ["First Taste of
Composition"](/ramda-chops-converting-temperature-units.html#a-taste-of-composition),
if we extract each operation into its own function, there is a way we can "link"
these functions together: [`compose`](https://ramdajs.com/docs/#compose)!

```javascript
// Here we create a reusable function that
// receives an object property, then returns
// a function that accepts an object, then
// tries to access that property on the object
const getProp = property => data => {
  if (data instanceof Object) {
    return data[property]
  }
}

const toMilliseconds = n => n * 1000
const toDate         = n => new Date(n)

const issTimeToDate =
  compose(toDate, toMilliseconds, getProp('timestamp'))

issTimeToDate(iss)
// "Sat Apr 10 2021 15:06:50 GMT+0000 (Coordinated Universal Time)"
```

[View this `getProp` with `compose` example in the Ramda
REPL.][repl-yz93mlt6]

While this doesn't handle all edge cases, at least passing `null` to
`issTimeToDate` will give us an `Invalid Date` message.

That `getProp` function looks like it's fairly generic, but could it handle an
`Array`? Could we leverage it to figure out who the first astronaut is in the
`astros.people` list?

```javascript
compose(getProp(0), getProp('people'))(astros)
// {
//   "craft": "ISS",
//   "name": "Sergey Ryzhikov"
// }

// which can be refactored and reused
// with any group of astronauts

const getFirstAstro =
  compose(getProp(0), getProp('people'))

getFirstAstro(astros)

// and if you really want to get some
// reusable functions

const getPeople     = getProp('people')
const getFirst      = getProp(0)
const getFirstAstro = compose(getFirst, getPeople)

getFirstAstro(astros)
// {
//   "craft": "ISS",
//   "name": "Sergey Ryzhikov"
// }
```

[View this `getFirstAstro` example in the Ramda
REPL.][repl-ye4ja9b9]

It can handle an `Array`! Why?

```javascript
[] instanceof Object // true
```

An `Array` of `[5, 10, 15]` is an `Object` instance whose keys are `Array`
indices!

```javascript
Array(3) {
  0: 5,
  1: 10,
  2: 15,
  length: 3
}
```

This means `getProp(1)([5, 10, 15]) === 10`. Neat!

As you probably guessed by now, Ramda has a
[`prop`](https://ramdajs.com/docs/#prop) function that does what our `getProp`
function does (and more), and there are a couple of other functions we could
pull in to help us. Let's refactor!

The ISS example:

```javascript
import { compose, multiply, prop } from 'ramda'

const getTimestamp   = prop('timestamp')
const toMilliseconds = multiply(1000)
const toDate         = n => new Date(n)

const issTimeToDate =
  compose(toDate, toMilliseconds, getTimestamp)

issTimeToDate(iss)
// "Sat Apr 10 2021 15:06:50 GMT+0000 (Coordinated Universal Time)"
```

[View this final `issTimeToDate` example in the Ramda
REPL.][repl-yzqgh9qs]

Finding the first astronaut example:

```javascript
import { compose, head, prop } from 'ramda'

const getPeople     = prop('people')
const getFirst      = prop(0)
const getFirstAstro = compose(getFirst, getPeople)

getFirstAstro(astros)
// {
//   "craft": "ISS",
//   "name": "Sergey Ryzhikov"
// }
```

[View this final `getFirstAstro` example in the Ramda
REPL.][repl-yzzfuo2y]

But beware: if the property doesn't exist, or it returns `null` or `undefined`,
then your composed functions will also need to be able to handle those
scenarios or risk throwing an error.

## `propOr`

When dealing with code or data that can give us back `null` or
`undefined` values, we often try to be safe. Consider this code trying to access
the ISS data:

```javascript
iss.iss_position.latitude
```

That doesn't look so bad, does it? But what happens if the API endpoint changes
its response on us or is having a bad day? Consider what would happen if the
endpoint returned an empty object, `{}`, and that was our `iss` value:

```javascript
iss.iss_position.latitude
// Uncaught TypeError: can't access property "latitude", iss.iss_position is undefined
```

Okay, so let's make that a little more fault tolerant:

```javascript
(iss.iss_position || {}).latitude
// undefined
```

At least we're not throwing an error right now... but what if the response is
`null` instead of `{}`?

```javascript
(iss.iss_position || {}).latitude
// Uncaught TypeError: null is not a function
```

Argh! We need to do the same thing for `iss.iss_position`:

```javascript
((iss || {}).iss_position || {}).latitude
// undefined
```

We're now able to handle these edge cases, but imagine two new requirements
arise:

1. we need to have the `latitude` fall back to a previous `latitude`
   value if the current one is unattainable
1. the value needs to be a floating point `number` and not a `string`

```javascript
const prevLatitude = '-44.7894'
parseFloat(((iss || {}).iss_position || {}).latitude || prevLatitude)
// 27.7270
```

This is starting to get messy, so we think breaking it into variables will help:

```javascript
const prevLatitude = '-44.7894'
const issObj = iss || {}
const issPosition = issObj.iss_position || {}
const issLatitude = issPosition.latitude || prevLatitude
parseFloat(issLatitude)
// 27.7270
```

Not bad, but there must be a cleaner way to do this!

The [`propOr`](https://ramdajs.com/docs/#propOr) function, whose signature is
`a → String → Object → a`, takes the following arguments:

1. a fallback value of some type `a`
1. a property name as a `String`
1. some `Object` to look the property up on

and then returns some value which is also of some type `a`.

Let's convert our variables to use `propOr` and walk things back from there:

```javascript
const prevLatitude = '-44.7894'
// no need for `issObj` anymore
const issPosition = propOr({}, 'iss_position', iss)
const issLatitude = propOr(prevLatitude, 'latitude', issPosition)
parseFloat(issLatitude)
// 27.7270
```

While we removed the `issObj` line of code, it looks like we have almost the
same amount of code. The difference, though, is what we can now do with this.

Do you see how these lines all basically use the return value from the line
above? We've got a composition on our hands again!

```javascript
const prevLatitude = '-44.7894'

const latitudeOrPrevLatitude =
  compose(
    parseFloat,
    propOr(prevLatitude, 'latitude'),
    propOr({}, 'iss_position')
  )

latitudeOrPrevLatitude(iss)       // 27.727
latitudeOrPrevLatitude({})        // -44.7894
latitudeOrPrevLatitude(null)      // -44.7894
latitudeOrPrevLatitude(undefined) // -44.7894
```

[View this `latitudeOrPrevLatitude` example in the Ramda
REPL.][repl-yf48n6lv]

Let's quickly walk through what passing `undefined` would have each line result
in.

```javascript
const latitudeOrPrevLatitude =
  compose(
    parseFloat,                       // 3. converts string to -44.7894
    propOr(prevLatitude, 'latitude'), // 2. falls back to "-44.7894"
    propOr({}, 'iss_position')        // 1. falls back to `{}`
  )
```

If you have good, generic fallbacks, you can then take it a step further and
simplify:

```javascript
const safeIssPosition = propOr({}, 'iss_position')
const safeLatitude = propOr(prevLatitude, 'latitude')

const latitudeOrPrevLatitude =
  compose(parseFloat, safeLatitude, safeIssPosition)
```

## `props`

There may be times where we'd like to apply `prop`'s functionality to more than
a single prop; naturally, Ramda calls this
[`props`](https://ramdajs.com/docs/#props).

Our `iss` object has the following keys:

* `message`
* `timestamp`
* `iss_position`

We've been told that we need to report on the success or failure of the API
request, and we need the timestamp of when it happened in a format like
`"success @ 1617930803"`. In this case, we don't need the position of the ISS,
so we can ignore it.

Without getting fancy, here's our first attempt:

```javascript
// responseToStatus :: IssResponse -> String
const responseToStatus = ({ message, timestamp }) =>
  `${message} @ ${timestamp}`
```

This is great, and for this simple example, we could probably stop here;
however, let's see if we can take this a few steps further.

Here's how we can use `props` to get the values of only `message` and
`timestamp`:

```javascript
props(['message', 'timestamp'], iss)
// ["success", 1617930803]
```

We can then join these together and accomplish our goal with `.join(' @ ')`

```javascript
props(['message', 'timestamp'], iss).join(' @ ')
// "success @ 1617930803"
```

This `.join()` dot notation might be starting to feel a little funny after we've
been exposed to "linking" functions together with `compose`, so let's use
Ramda's [`join`](https://ramdajs.com/docs/#join) function to clean this up:

```javascript
const responseToStatus =
  compose(join(' @ '), props(['message', 'timestamp']))

responseToStatus(iss) // "success @ 1617930803"
```

[View this `responseToStatus` example in the Ramda
REPL.][repl-yggfjkeu]

While this use case is admittedly small, there are times where we'll want to
select only a few values from an object and have them in array format, and
`props` is the right tool for helping us do that.

## `pick`

Here's a new scenario for us to work with: our colleague needs an array of all
the astronauts' names, but each array item must be an object with a key of
`name` only.

Think we're up for the task? You bet!

```javascript
astros.people.map(({ name }) => ({ name }))
// [
//  { name: "Sergey Ryzhikov" },
//  { name: "Kate Rubins" },
//  { name: "Sergey Kud-Sverchkov" },
//  { name: "Mike Hopkins" },
//  { name: "Victor Glover" },
//  { name: "Shannon Walker" },
//  { name: "Soichi Noguchi" }
// ]
```

This seems like a generic enough task that Ramda must surely have a helper
function!

For the times we want to select a subset of keys and values from an object, we
can use [`pick`](https://ramdajs.com/docs/#pick).

Similar to `props`, `pick` takes an array of keys and selects each key and its
value from an object. Instead of returning an array of values, however, `pick`
returns an object containing the keys and values you asked for.

This is great, for example, for whittling down an object in a data pipeline to
only the properties that you need. Sending too many properties to a function can
sometimes lead to confusion and even bugs!

Let's take our original implementation and use `pick`:

```javascript
astros.people.map(pick(['name']))
```

[View this initial `pick` example in the Ramda
REPL.][repl-ydwn3vaz]

Nice! But we're also doing the work of accessing the `people` property and
calling `map` – both generic operations – so let's see if we can use Ramda's
`propOr` and `map` helpers to `compose` something together.

```javascript
const getAstroNames =
  compose(map(pick(['name'])), propOr([], 'people'))

getAstroNames(astros)
```

[View this `getAstroNames` composition in the Ramda
REPL.][repl-yzay2ssb]

While we're here, let's extract the reusable functions out of this for
potential future use:

```javascript
const pickName      = pick(['name'])
const pickNames     = map(pickName)
const getPeople     = propOr([], 'people')
const getAstroNames = compose(pickNames, getPeople)

getAstroNames(astros)
```

[View this refactored `getAstroNames` group of functions in the Ramda
REPL.][repl-yzqng9fn]

## `pluck`

Someone from marketing is trying to update our organization's emoji game, and
they want us to display the astronauts' names separated by rocket ships (🚀).
Here's what they want:

> "Sergey Ryzhikov 🚀 Kate Rubins 🚀 Sergey Kud-Sverchkov 🚀 Mike Hopkins 🚀 Victor Glover 🚀 Shannon Walker 🚀 Soichi Noguchi"

Without thinking too hard about it, we come up with a simple solution:

```javascript
astros.people.map(x => x.name).join(' 🚀 ')
// "Sergey Ryzhikov 🚀 Kate Rubins 🚀 ..."
```

We've been introduced to [`prop`](https://ramdajs.com/docs/#prop), so let's
update that `map`:

```javascript
astros.people.map(prop('name')).join(' 🚀 ')
```

As we hop on this refactor train, we wonder:

_"What if we want to `map` a property other than `name`, and what if we want to
call this `map(prop('whatever'))` on any list of objects?"_

Thinking in generic terms, we establish that our list and our object key could
be variable, so we make them variables in our function we'll call `pluckKey`:

```javascript
const pluckKey = (key, xs) => xs.map(x => x[key])

// or with some nice ramda functions

const pluckKey = (key, xs) => map(prop(key), xs)

// and with some manual function currying
// (covered in depth in Ramda Chops: Function Currying)

const pluckKey = key => xs => map(prop(key), xs)

// example
pluckKey('name')(astros.people)
```

With that last `pluckKey` function definition, we can link together a few
functions to get the same result:

```javascript
const pluckKey = key => xs => map(prop(key), xs)

const astrosPeopleWithRockets =
  compose(join(' 🚀 '), pluckKey('name'), propOr([], 'people'))

astrosPeopleWithRockets(astros)
```

Like before, we can pull those composed functions into named variables:

```javascript
const joinRocket = join(' 🚀 ')
const pluckName  = pluckKey('name')
const getPeople  = propOr([], 'people')

const astrosPeopleWithRockets =
  compose(joinRocket, pluckName, getPeople)
```

This is looking pretty clean!

Naturally, this `pluckKey` function we so cleverly made has already been
included in Ramda, and it's called `pluck`. It is equivalent to `map(prop(key),
list)`, just like our function definition, so we can delete our function and
replace `pluckKey('name')` with Ramda's `pluck`. Here it is in its entirety:

```javascript
const joinRocket = join(' 🚀 ')
const pluckName  = pluck('name')
const getPeople  = propOr([], 'people')

const astrosPeopleWithRockets =
  compose(joinRocket, pluckName, getPeople)

astrosPeopleWithRockets(astros)
// "Sergey Ryzhikov 🚀 Kate Rubins 🚀 ..."
```

[View the `pluck` and `astrosPeopleWithRockets` set of functions in the Ramda
REPL.][repl-ygjjq5gt]

[repl-yz93mlt6]: https://ramdajs.com/repl/#?const%20iss%20%3D%20%7B%0A%20%20%22message%22%3A%20%22success%22%2C%0A%20%20%22timestamp%22%3A%201617930803%2C%0A%20%20%22iss_position%22%3A%20%7B%0A%20%20%20%20%22longitude%22%3A%20%22133.2581%22%2C%0A%20%20%20%20%22latitude%22%3A%20%2227.7270%22%0A%20%20%7D%0A%7D%0A%0Aconst%20getProp%20%3D%20property%20%3D%3E%20data%20%3D%3E%20%7B%0A%20%20if%20%28data%20instanceof%20Object%29%20%7B%0A%20%20%20%20return%20data%5Bproperty%5D%0A%20%20%7D%0A%7D%0A%0Aconst%20toMilliseconds%20%3D%20multiply%281000%29%0Aconst%20toDate%20%20%20%20%20%20%20%20%20%3D%20n%20%3D%3E%20new%20Date%28n%29%0A%0Aconst%20issTimeToDate%20%3D%0A%20%20compose%28toDate%2C%20toMilliseconds%2C%20getProp%28%27timestamp%27%29%29%0A%0AissTimeToDate%28iss%29
[repl-ye4ja9b9]: https://ramdajs.com/repl/#?const%20astros%20%3D%20%7B%0A%20%20%22message%22%3A%20%22success%22%2C%0A%20%20%22number%22%3A%207%2C%0A%20%20%22people%22%3A%20%5B%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Sergey%20Ryzhikov%22%0A%20%20%20%20%7D%2C%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Kate%20Rubins%22%0A%20%20%20%20%7D%2C%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Sergey%20Kud-Sverchkov%22%0A%20%20%20%20%7D%2C%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Mike%20Hopkins%22%0A%20%20%20%20%7D%2C%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Victor%20Glover%22%0A%20%20%20%20%7D%2C%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Shannon%20Walker%22%0A%20%20%20%20%7D%2C%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Soichi%20Noguchi%22%0A%20%20%20%20%7D%0A%20%20%5D%0A%7D%0A%20%0Aconst%20getProp%20%3D%20property%20%3D%3E%20data%20%3D%3E%20%7B%0A%20%20if%20%28data%20instanceof%20Object%29%20%7B%0A%20%20%20%20return%20data%5Bproperty%5D%0A%20%20%7D%0A%7D%0A%0A%2F%2F%20Example%201%0A%2F%2F%0A%2F%2Fcompose%28getProp%280%29%2C%20getProp%28%27people%27%29%29%28astros%29%0A%0A%2F%2F%20Example%202%0A%2F%2F%0A%2F%2Fconst%20getFirstAstro%20%3D%0A%2F%2F%20%20compose%28getProp%280%29%2C%20getProp%28%27people%27%29%29%0A%2F%2F%0A%2F%2FgetFirstAstro%28astros%29%0A%0A%2F%2F%20Example%203%0A%0Aconst%20getPeople%20%20%20%20%20%3D%20getProp%28%27people%27%29%0Aconst%20getFirst%20%20%20%20%20%20%3D%20getProp%280%29%0Aconst%20getFirstAstro%20%3D%20compose%28getFirst%2C%20getPeople%29%0A%0AgetFirstAstro%28astros%29%0A
[repl-yzqgh9qs]: https://ramdajs.com/repl/#?const%20iss%20%3D%20%7B%0A%20%20%22message%22%3A%20%22success%22%2C%0A%20%20%22timestamp%22%3A%201617930803%2C%0A%20%20%22iss_position%22%3A%20%7B%0A%20%20%20%20%22longitude%22%3A%20%22133.2581%22%2C%0A%20%20%20%20%22latitude%22%3A%20%2227.7270%22%0A%20%20%7D%0A%7D%0A%20%0Aconst%20getTimestamp%20%20%20%3D%20prop%28%27timestamp%27%29%0Aconst%20toMilliseconds%20%3D%20multiply%281000%29%0Aconst%20toDate%20%20%20%20%20%20%20%20%20%3D%20n%20%3D%3E%20new%20Date%28n%29%0A%0Aconst%20issTimeToDate%20%3D%0A%20%20compose%28toDate%2C%20toMilliseconds%2C%20getTimestamp%29%0A%0AissTimeToDate%28iss%29%0A
[repl-yzzfuo2y]: https://ramdajs.com/repl/#?const%20astros%20%3D%20%7B%0A%20%20%22message%22%3A%20%22success%22%2C%0A%20%20%22number%22%3A%207%2C%0A%20%20%22people%22%3A%20%5B%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Sergey%20Ryzhikov%22%0A%20%20%20%20%7D%2C%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Kate%20Rubins%22%0A%20%20%20%20%7D%2C%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Sergey%20Kud-Sverchkov%22%0A%20%20%20%20%7D%2C%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Mike%20Hopkins%22%0A%20%20%20%20%7D%2C%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Victor%20Glover%22%0A%20%20%20%20%7D%2C%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Shannon%20Walker%22%0A%20%20%20%20%7D%2C%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Soichi%20Noguchi%22%0A%20%20%20%20%7D%0A%20%20%5D%0A%7D%0A%0Aconst%20getPeople%20%20%20%20%20%3D%20prop%28%27people%27%29%0Aconst%20getFirst%20%20%20%20%20%20%3D%20prop%280%29%0Aconst%20getFirstAstro%20%3D%20compose%28getFirst%2C%20getPeople%29%0A%0A%0AgetFirstAstro%28astros%29%0A
[repl-yf48n6lv]: https://ramdajs.com/repl/#?const%20iss%20%3D%20%7B%0A%20%20%22message%22%3A%20%22success%22%2C%0A%20%20%22timestamp%22%3A%201617930803%2C%0A%20%20%22iss_position%22%3A%20%7B%0A%20%20%20%20%22latitude%22%3A%20%2227.7270%22%2C%0A%20%20%20%20%22longitude%22%3A%20%22133.2581%22%0A%20%20%7D%0A%7D%0A%0Aconst%20prevLatitude%20%3D%20%22-44.7894%22%0Aconst%20prevLongitude%20%3D%20%22-33.2580%22%0A%20%0Aconst%20latitudeOrPrevLatitude%20%3D%0A%20%20compose%28%0A%20%20%20%20parseFloat%2C%0A%20%20%20%20propOr%28prevLatitude%2C%20%27latitude%27%29%2C%0A%20%20%20%20propOr%28%7B%7D%2C%20%27iss_position%27%29%0A%20%20%29%0A%0AlatitudeOrPrevLatitude%28iss%29%20%20%20%20%20%20%20%2F%2F%2027.727%0A%0A%2F%2F%20uncomment%20a%20line%20below%20to%20see%20its%20result%0A%2F%2FlatitudeOrPrevLatitude%28%7B%7D%29%20%20%20%20%20%20%20%20%2F%2F%20-44.7894%0A%2F%2FlatitudeOrPrevLatitude%28null%29%20%20%20%20%20%20%2F%2F%20-44.7894%0A%2F%2FlatitudeOrPrevLatitude%28undefined%29%20%2F%2F%20-44.7894
[repl-yggfjkeu]: https://ramdajs.com/repl/#?const%20iss%20%3D%7B%0A%20%20%22message%22%3A%20%22success%22%2C%0A%20%20%22timestamp%22%3A%201617930803%2C%0A%20%20%22iss_position%22%3A%20%7B%0A%20%20%20%20%22latitude%22%3A%20%2227.7270%22%2C%0A%20%20%20%20%22longitude%22%3A%20%22133.2581%22%0A%20%20%7D%0A%7D%0A%0A%0Aconst%20responseToStatus%20%3D%0A%20%20compose%28join%28%27%20%40%20%27%29%2C%20props%28%5B%27message%27%2C%20%27timestamp%27%5D%29%29%0A%0AresponseToStatus%28iss%29%20%2F%2F%20%22success%20%40%201617930803%22
[repl-ydwn3vaz]: https://ramdajs.com/repl/#?const%20astros%20%3D%20%7B%0A%20%20%22message%22%3A%20%22success%22%2C%0A%20%20%22number%22%3A%207%2C%0A%20%20%22people%22%3A%20%5B%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Sergey%20Ryzhikov%22%0A%20%20%20%20%7D%2C%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Kate%20Rubins%22%0A%20%20%20%20%7D%2C%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Sergey%20Kud-Sverchkov%22%0A%20%20%20%20%7D%2C%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Mike%20Hopkins%22%0A%20%20%20%20%7D%2C%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Victor%20Glover%22%0A%20%20%20%20%7D%2C%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Shannon%20Walker%22%0A%20%20%20%20%7D%2C%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Soichi%20Noguchi%22%0A%20%20%20%20%7D%0A%20%20%5D%0A%7D%0A%0Aastros.people.map%28pick%28%5B%27name%27%5D%29%29
[repl-yzay2ssb]: https://ramdajs.com/repl/#?const%20astros%20%3D%20%7B%0A%20%20%22message%22%3A%20%22success%22%2C%0A%20%20%22number%22%3A%207%2C%0A%20%20%22people%22%3A%20%5B%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Sergey%20Ryzhikov%22%0A%20%20%20%20%7D%2C%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Kate%20Rubins%22%0A%20%20%20%20%7D%2C%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Sergey%20Kud-Sverchkov%22%0A%20%20%20%20%7D%2C%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Mike%20Hopkins%22%0A%20%20%20%20%7D%2C%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Victor%20Glover%22%0A%20%20%20%20%7D%2C%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Shannon%20Walker%22%0A%20%20%20%20%7D%2C%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Soichi%20Noguchi%22%0A%20%20%20%20%7D%0A%20%20%5D%0A%7D%0A%0Aconst%20getAstroNames%20%3D%0A%20%20compose%28map%28pick%28%5B%27name%27%5D%29%29%2C%20propOr%28%5B%5D%2C%20%27people%27%29%29%0A%0AgetAstroNames%28astros%29
[repl-yzqng9fn]: https://ramdajs.com/repl/#?const%20astros%20%3D%20%7B%0A%20%20%22message%22%3A%20%22success%22%2C%0A%20%20%22number%22%3A%207%2C%0A%20%20%22people%22%3A%20%5B%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Sergey%20Ryzhikov%22%0A%20%20%20%20%7D%2C%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Kate%20Rubins%22%0A%20%20%20%20%7D%2C%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Sergey%20Kud-Sverchkov%22%0A%20%20%20%20%7D%2C%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Mike%20Hopkins%22%0A%20%20%20%20%7D%2C%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Victor%20Glover%22%0A%20%20%20%20%7D%2C%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Shannon%20Walker%22%0A%20%20%20%20%7D%2C%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Soichi%20Noguchi%22%0A%20%20%20%20%7D%0A%20%20%5D%0A%7D%0A%0Aconst%20pickName%20%20%20%20%20%20%3D%20pick%28%5B%27name%27%5D%29%0Aconst%20pickNames%20%20%20%20%20%3D%20map%28pickName%29%0Aconst%20getPeople%20%20%20%20%20%3D%20propOr%28%5B%5D%2C%20%27people%27%29%0Aconst%20getAstroNames%20%3D%20compose%28pickNames%2C%20getPeople%29%0A%0AgetAstroNames%28astros%29
[repl-ygjjq5gt]: https://ramdajs.com/repl/#?const%20astros%20%3D%20%7B%0A%20%20%22message%22%3A%20%22success%22%2C%0A%20%20%22number%22%3A%207%2C%0A%20%20%22people%22%3A%20%5B%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Sergey%20Ryzhikov%22%0A%20%20%20%20%7D%2C%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Kate%20Rubins%22%0A%20%20%20%20%7D%2C%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Sergey%20Kud-Sverchkov%22%0A%20%20%20%20%7D%2C%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Mike%20Hopkins%22%0A%20%20%20%20%7D%2C%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Victor%20Glover%22%0A%20%20%20%20%7D%2C%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Shannon%20Walker%22%0A%20%20%20%20%7D%2C%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Soichi%20Noguchi%22%0A%20%20%20%20%7D%0A%20%20%5D%0A%7D%0A%0Aconst%20joinRocket%20%3D%20join%28%27%20%F0%9F%9A%80%20%27%29%0Aconst%20pluckName%20%20%3D%20pluck%28%27name%27%29%0Aconst%20getPeople%20%20%3D%20propOr%28%5B%5D%2C%20%27people%27%29%0A%0Aconst%20astrosPeopleWithRockets%20%3D%0A%20%20compose%28joinRocket%2C%20pluckName%2C%20getPeople%29%0A%0AastrosPeopleWithRockets%28astros%29
