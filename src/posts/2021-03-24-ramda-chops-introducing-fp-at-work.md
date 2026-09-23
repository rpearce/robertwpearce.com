---
author: "Robert Pearce"
authorTwitter: "@RobertWPearce"
desc: "Start small: a few Ramda functions at a time, kept as familiar as possible, is one way to bring functional programming to a team that is not on the train yet."
keywords: "ramda, ramda.js, ramdajs, functional programming, functional javascript, ramda propOr, ramda map, introducing fp at work, javascript"
title: "Ramda Chops: Introducing FP at Work"
---

_This post is adapted from my [Ramda
Guide](https://github.com/rpearce/ramda.guide) book project. Thanks to Steve
Purr, Tom Wilson, and Ian Greulich for their reviews._

* * *

_Other posts in this series:_

* [Ramda Chops: Getting Started with Ramda](/ramda-chops-getting-started-with-ramda.html)
* [Ramda Chops: Converting Temperature Units](/ramda-chops-converting-temperature-units.html)
* [Ramda Chops: Comparing Two Temperatures](/ramda-chops-comparing-two-temperatures.html)
* [Ramda Chops: Reading Shallow Object Properties](/ramda-chops-reading-shallow-object-properties.html)
* [Ramda Chops: Testing Shallow Object Properties](/ramda-chops-testing-shallow-object-properties.html)

If your team isn't on the functional programming train already, it can be
difficult to start using tools that introduce different paradigms like [function
composition](https://en.wikipedia.org/wiki/Function_composition_(computer_science)),
[algebraic data types](https://en.wikipedia.org/wiki/Algebraic_data_type), and
the heaps of [function programming
jargon](https://github.com/hemanth/functional-programming-jargon).

One way of introducing functional programming concepts at your organization is
by starting small: introduce a few functions and concepts at a time and keep
things as familiar as possible.

For example, instead of writing code like

```js
user.name || 'N/A'
```

you could include `propOr` like

```js
import { propOr } from 'ramda'

propOr('N/A', 'name', user)
```

And then when you're mapping over a list of users to retrieve their names, you
might think you could do the same thing,

```js
users.map(x => x.name)

// changes to...

users.map(propOr('N/A', 'name'))
```

but then you realize that you might have a reusable function on your hands!

```js
import { propOr } from 'ramda'

const nameOrDefault = propOr('N/A', 'name')

nameOrDefault(user)       // 'Fred'
users.map(nameOrDefault) // ['Fred', 'Wilma', 'N/A']
```

Taking this even further, what if we have two lists of users, `users` and
`otherUsers`? Let's pull in `map`, as well!

```js
import { map } from 'ramda'

map(nameOrDefault, users)      // ['Fred', 'Wilma', 'N/A']
map(nameOrDefault, otherUsers) // ['Wonder Woman', 'N/A', 'Batman']

// which can then be refactored to

const mapNameOrDefault = map(nameOrDefault)
mapNameOrDefault(users)      // ['Fred', 'Wilma', 'N/A']
mapNameOrDefault(otherUsers) // ['Wonder Woman', 'N/A', 'Batman']
```

Before you know it, you're finding quick and convincingly effective ways to
bring Ramda (or whatever tool) into your day-to-day work.
