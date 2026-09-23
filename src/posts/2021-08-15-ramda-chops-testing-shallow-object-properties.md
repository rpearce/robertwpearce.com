---
author: "Robert Pearce"
authorTwitter: "@RobertWPearce"
desc: "Make logical decisions by testing object properties with Ramda's has and propEq, and learn why checking 'toString' in data is not the check you think it is."
keywords: "ramda, ramda.js, ramdajs, ramda tutorial, ramda has, ramda propEq, hasOwnProperty, javascript"
title: "Ramda Chops: Testing Shallow Object Properties"
updated: "2022-03-21T12:00:00Z"
---

_This post is adapted from my [Ramda
Guide](https://github.com/rpearce/ramda.guide) book project. Thanks to Steve
Purr, Tom Wilson, and Ian Greulich for their reviews._

> Our destinations are Booleans – we reach them or we don’t – but our journeys
> are spectrums, because there are so many paths we can take to our destination
> that make getting there that much better.
>
> — <cite>A.J. Darkholme</cite>

In this post, we'll make logical decisions in our code (`if`/`else`) by
testing our datasets' properties with a few boolean-returning helper functions.

Outline:

* [`has`](#has)
* [`propEq`](#propeq)

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
* [Ramda Chops: Reading Shallow Object Properties](/ramda-chops-reading-shallow-object-properties.html)
* [Ramda Chops: Introducing FP at Work](/ramda-chops-introducing-fp-at-work.html)

## `has`

We're working one morning, eating scones and refactoring some code, when a
fellow developer, who lives in Iceland and started work a few hours before we
did today, pings us with the following message:

> Hjálp!
>
> We shipped some astronaut code this morning that puts a method, `toString`, on
> some `astros` objects, and it joins all the astronauts' names together with
> rocket ships like this:
>
> "Sergey Ryzhikov 🚀 Kate Rubins 🚀 ..."
>
> Pretty cool, right?
>
> We only want to use this method on the `astros` objects where it's
> defined, but we forgot that `toString` is already a defined method defined on
> an `Object` instance, so some `astros` objects are calling that method and
> returning `"[object Object]"` when we want them to do something else!
>
> Can you help us? Takk!

Our Icelandic coworker then sends us the code:

```javascript
const astrosWithToString = {
  "message": "success",
  "number": 7,
  "people": [/* omitted for brevity */],
  toString() {
    return astrosPeopleWithRockets(this)
  }
}

const astrosWithoutToString = {
  "message": "success",
  "number": 7,
  "people": [/* omitted for brevity */],
}

const astrosToString = data => {
  if ('toString' in data) { // THIS IS WHERE THE BUG HAPPENS!
    return data.toString()
  }

  return `There are ${data.number} astronauts`
}

// astrosToString(astrosWithToString) // this works
astrosToString(astrosWithoutToString) // this doesn't!
```

[View this buggy `astrosToString` code in the Ramda
REPL.][repl-yzqnul84]

_The `astrosPeopleWithRockets` code comes from the prior post on ["Reading
Shallow Object Properties"](/ramda-chops-reading-shallow-object-properties.html),
so check
that out to see how we arrived at the nifty little helper functions you'll find
in the REPL linked above._

Aha! We see where the misunderstanding happened. The `'toString' in data` code
is checking that there is a property defined on the object called `toString` —
whether or not it inherited that property! All object instances have a
`.toString()` method that they inherit, so it'll always have `toString` defined
no matter what. What we want is to check if `toString` was explicitly defined by
us on the `astros` object.

We first confirm our assumption and fix the bug by making this change:

```javascript
// before
if ('toString' in data) {/*...*/}

// after
if (Object.prototype.hasOwnProperty.call(data, 'toString')) {/*...*/}
```

The `hasOwnProperty` exists on all objects, but it could be overwritten like
`toString` was, so we use an [_external_ `hasOwnProperty`](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Object/hasOwnProperty)
to do our check.

This check seems rather lengthy, so let's convert that to a function named
`hasProp`:

```javascript
const hasProp = (prop, obj) =>
  Object.prototype.hasOwnProperty.call(obj, prop)
```

Surprise! Ramda already has a helper for this,
[`has`](https://ramdajs.com/docs/#has), so we can replace our `hasProp` with
`has`.

We then write a message back to our Icelandic colleague:

> Hæ!
>
> We found the issue, and it's a matter of testing whether _we_ defined the
> object property or not. Here you go!
>
> Eigðu góðan dag!

```javascript
import { has } from 'ramda'

// ...

const astrosToString = data => {
  if (has('toString', data)) {
    return data.toString()
  }

  return `There are ${data.number} astronauts`
}
```

[View the updated `astrosToString` and `hasProp` functions in the Ramda
REPL.][repl-ygl3tr4u]

## `propEq`

One fine afternoon, our error monitoring service lets us know that our code is
throwing errors when trying to access the latitude property from the ISS'
location API response.

We quickly realize that if there is a problem with the API, we won't get the
data back, so `iss.iss_position.latitude` won't work, for `iss_position` is
`undefined` in the response:

```json
{
  "message": "error",
  "timestamp": 1617930803
}
```

There are many ways to handle this error safely, but we're going to address it
by simply checking if the `message` property is `"success"` or not:

```javascript
if (iss.message === 'success') {
  // carry on...
}
```

Great! Call it a day!

...But our solution nags at us. We are accessing a property's value and equating
it with an expected value. What if we made this a function?

```javascript
const isMessageSuccess = data =>
  (data || {}).message === 'success' // or `data?.message === 'success'`
```

_Note: the Ramda REPL doesn't currently support optional chaining like
`data?.message === 'success'`._

Not bad, but we've simply moved the operations to a single place. What if we
wrote a function that looked up any property on an object and then compared it
with another value?

```javascript
const doesPropEq = (key, val, data) =>
  (data || {})[key] === val // or `data?.[key] === val`
```

Nice! Let's try it out:

```javascript
const isMessageSuccess = data =>
  doesPropEq('message', 'success', data)

isMessageSuccess(iss) // true
```

[View `doesPropEq` and `isMessageSuccess` in the ramda
REPL][repl-yckj6vnd].

Now that we understand our need for `doesPropEq`, we can swap that out with
ramda's [`propEq`](https://ramdajs.com/docs/#propEq).

```javascript
import { propEq } from 'ramda'

// ...

const isMessageSuccess = data =>
  propEq('message', 'success', data)

isMessageSuccess(iss) // true
```

Stopping the `isMessageSuccess` implementation work at this point is totally
acceptable, but we can take it a little further.

Since all functions in ramda are auto-curried, that means that we can refactor
`isMessageSuccess` like this:

```javascript
// Step 0
const isMessageSuccess = data =>
  propEq('message', 'success', data)

// Step 1
const isMessageSuccess = data =>
  propEq('message', 'success')(data)

// Step 2
const isMessageSuccess =
  propEq('message', 'success')

// Step 3
// isMessageSuccess :: ISSData -> Bool
const isMessageSuccess =
  propEq('message', 'success')
```

1. In Step 1, we demonstrate that `propEq` will accept our last argument as a
   separate function call (the result of calling `propEq` the first time will
   wait until it has all the arguments).
2. In Step 2, we realize that accepting an argument and passing it on again is
   redundant, and so we can remove the need for a function closure and instead
   let the result of calling `propEq` with the first two values be what is bound
   to `isMessageSuccess`.
3. In Step 3. we acknowledge that implicitly forwarding a function argument
   comes at the cost of remembering, "What am I passing in, again?" If you don't
   have a type-checker, you can provide some pseudo-types (these ones are in a
   Haskell style) where the data is defined in order to explain what `ISSData`
   is:

   ```javascript
   // ISSData = { message      :: Message
   //           , timestamp    :: UnixTimestamp
   //           , iss_position :: LatLong
   //           }
   //
   // Message = 'success' | 'error'
   //
   // UnixTimeStamp = Number
   //
   // LatLong = { latitude  :: String
   //           , longitude :: String
   //           }
   ```

   The whole point of Step 3 is simply to identify what the expected input and
   output types are so that someone else (or you in 3 months) can easily
   understand a terse function at a glance.

[Check out this `propEq` usage plus these pseudo-types in the ramda
REPL.][repl-5n8p6k7f]

[repl-yzqnul84]: https://ramdajs.com/repl/#?const%20astrosWithToString%20%3D%20%7B%0A%20%20%22message%22%3A%20%22success%22%2C%0A%20%20%22number%22%3A%207%2C%0A%20%20%22people%22%3A%20%5B%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Sergey%20Ryzhikov%22%0A%20%20%20%20%7D%2C%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Kate%20Rubins%22%0A%20%20%20%20%7D%2C%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Sergey%20Kud-Sverchkov%22%0A%20%20%20%20%7D%2C%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Mike%20Hopkins%22%0A%20%20%20%20%7D%2C%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Victor%20Glover%22%0A%20%20%20%20%7D%2C%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Shannon%20Walker%22%0A%20%20%20%20%7D%2C%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Soichi%20Noguchi%22%0A%20%20%20%20%7D%0A%20%20%5D%2C%0A%20%20toString%28%29%20%7B%0A%20%20%20%20return%20astrosPeopleWithRockets%28this%29%0A%20%20%7D%0A%7D%0A%0Aconst%20astrosWithoutToString%20%3D%20%7B%0A%20%20%22message%22%3A%20%22success%22%2C%0A%20%20%22number%22%3A%207%2C%0A%20%20%22people%22%3A%20%5B%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Sergey%20Ryzhikov%22%0A%20%20%20%20%7D%2C%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Kate%20Rubins%22%0A%20%20%20%20%7D%2C%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Sergey%20Kud-Sverchkov%22%0A%20%20%20%20%7D%2C%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Mike%20Hopkins%22%0A%20%20%20%20%7D%2C%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Victor%20Glover%22%0A%20%20%20%20%7D%2C%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Shannon%20Walker%22%0A%20%20%20%20%7D%2C%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Soichi%20Noguchi%22%0A%20%20%20%20%7D%0A%20%20%5D%0A%7D%0A%0Aconst%20joinRocket%20%3D%20join%28%27%20%F0%9F%9A%80%20%27%29%0Aconst%20pluckName%20%20%3D%20pluck%28%27name%27%29%0Aconst%20getPeople%20%20%3D%20propOr%28%5B%5D%2C%20%27people%27%29%0A%0Aconst%20astrosPeopleWithRockets%20%3D%0A%20%20compose%28joinRocket%2C%20pluckName%2C%20getPeople%29%0A%0Aconst%20astrosToString%20%3D%20data%20%3D%3E%20%7B%0A%20%20if%20%28%27toString%27%20in%20data%29%20%7B%0A%20%20%20%20return%20data.toString%28%29%0A%20%20%7D%0A%20%20%0A%20%20return%20%60There%20are%20%24%7Bdata.number%7D%20astronauts%60%0A%7D%0A%0A%2F%2F%20astrosToString%28astrosWithToString%29%20%2F%2F%20this%20works%0AastrosToString%28astrosWithoutToString%29%20%2F%2F%20this%20doesn%27t%21
[repl-ygl3tr4u]: https://ramdajs.com/repl/#?const%20astrosWithToString%20%3D%20%7B%0A%20%20%22message%22%3A%20%22success%22%2C%0A%20%20%22number%22%3A%207%2C%0A%20%20%22people%22%3A%20%5B%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Sergey%20Ryzhikov%22%0A%20%20%20%20%7D%2C%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Kate%20Rubins%22%0A%20%20%20%20%7D%2C%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Sergey%20Kud-Sverchkov%22%0A%20%20%20%20%7D%2C%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Mike%20Hopkins%22%0A%20%20%20%20%7D%2C%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Victor%20Glover%22%0A%20%20%20%20%7D%2C%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Shannon%20Walker%22%0A%20%20%20%20%7D%2C%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Soichi%20Noguchi%22%0A%20%20%20%20%7D%0A%20%20%5D%2C%0A%20%20toString%28%29%20%7B%0A%20%20%20%20return%20astrosPeopleWithRockets%28this%29%0A%20%20%7D%0A%7D%0A%0Aconst%20astrosWithoutToString%20%3D%20%7B%0A%20%20%22message%22%3A%20%22success%22%2C%0A%20%20%22number%22%3A%207%2C%0A%20%20%22people%22%3A%20%5B%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Sergey%20Ryzhikov%22%0A%20%20%20%20%7D%2C%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Kate%20Rubins%22%0A%20%20%20%20%7D%2C%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Sergey%20Kud-Sverchkov%22%0A%20%20%20%20%7D%2C%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Mike%20Hopkins%22%0A%20%20%20%20%7D%2C%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Victor%20Glover%22%0A%20%20%20%20%7D%2C%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Shannon%20Walker%22%0A%20%20%20%20%7D%2C%0A%20%20%20%20%7B%0A%20%20%20%20%20%20%22craft%22%3A%20%22ISS%22%2C%0A%20%20%20%20%20%20%22name%22%3A%20%22Soichi%20Noguchi%22%0A%20%20%20%20%7D%0A%20%20%5D%0A%7D%0A%0Aconst%20joinRocket%20%3D%20join%28%27%20%F0%9F%9A%80%20%27%29%0Aconst%20pluckName%20%20%3D%20pluck%28%27name%27%29%0Aconst%20getPeople%20%20%3D%20propOr%28%5B%5D%2C%20%27people%27%29%0A%0Aconst%20astrosPeopleWithRockets%20%3D%0A%20%20compose%28joinRocket%2C%20pluckName%2C%20getPeople%29%0A%0Aconst%20hasProp%20%3D%20%28prop%2C%20obj%29%20%3D%3E%0A%20%20Object.prototype.hasOwnProperty.call%28obj%2C%20prop%29%0A%0Aconst%20astrosToString%20%3D%20data%20%3D%3E%20%7B%0A%20%20if%20%28hasProp%28%27toString%27%2C%20data%29%29%20%7B%0A%20%20%20%20return%20data.toString%28%29%0A%20%20%7D%0A%0A%20%20return%20%60There%20are%20%24%7Bdata.number%7D%20astronauts%60%0A%7D%0A%0A%0A%2F%2F%20astrosToString%28astrosWithToString%29%0AastrosToString%28astrosWithoutToString%29
[repl-yckj6vnd]: https://ramdajs.com/repl/#?const%20iss%20%3D%20%7B%0A%20%20%22message%22%3A%20%22success%22%2C%0A%20%20%22timestamp%22%3A%201617930803%2C%0A%20%20%22iss_position%22%3A%20%7B%0A%20%20%20%20%22latitude%22%3A%20%2227.7270%22%2C%0A%20%20%20%20%22longitude%22%3A%20%22133.2581%22%0A%20%20%7D%0A%7D%0A%0Aconst%20doesPropEq%20%3D%20%28key%2C%20val%2C%20data%29%20%3D%3E%0A%20%20%28data%20%7C%7C%20%7B%7D%29%5Bkey%5D%20%3D%3D%3D%20val%0A%0Aconst%20isMessageSuccess%20%3D%20data%20%3D%3E%0A%20%20doesPropEq%28%27message%27%2C%20%27success%27%2C%20data%29%0A%0AisMessageSuccess%28iss%29
[repl-5n8p6k7f]: https://ramdajs.com/repl/#?%2F%2F%20ISSData%20%3D%20%7B%20message%20%20%20%20%20%20%3A%3A%20Message%0A%2F%2F%20%20%20%20%20%20%20%20%20%20%20%2C%20timestamp%20%20%20%20%3A%3A%20UnixTimestamp%0A%2F%2F%20%20%20%20%20%20%20%20%20%20%20%2C%20iss_position%20%3A%3A%20LatLong%0A%2F%2F%20%20%20%20%20%20%20%20%20%20%20%7D%0A%2F%2F%0A%2F%2F%20Message%20%3D%20%27success%27%20%7C%20%27error%27%0A%2F%2F%0A%2F%2F%20UnixTimeStamp%20%3D%20Number%0A%2F%2F%0A%2F%2F%20LatLong%20%3D%20%7B%20latitude%20%20%3A%3A%20String%0A%2F%2F%20%20%20%20%20%20%20%20%20%20%20%2C%20longitude%20%3A%3A%20String%0A%2F%2F%20%20%20%20%20%20%20%20%20%20%20%7D%0A%0A%2F%2F%20iss%20%3A%3A%20ISSData%0Aconst%20iss%20%3D%20%7B%0A%20%20%22message%22%3A%20%22success%22%2C%0A%20%20%22timestamp%22%3A%201617930803%2C%0A%20%20%22iss_position%22%3A%20%7B%0A%20%20%20%20%22latitude%22%3A%20%2227.7270%22%2C%0A%20%20%20%20%22longitude%22%3A%20%22133.2581%22%0A%20%20%7D%0A%7D%0A%0A%2F%2F%20isMessageSuccess%20%3A%3A%20ISSData%20-%3E%20Bool%0Aconst%20isMessageSuccess%20%3D%0A%20%20propEq%28%27message%27%2C%20%27success%27%29%0A%0AisMessageSuccess%28iss%29
