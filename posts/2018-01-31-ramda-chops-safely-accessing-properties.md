---
title:        "Ramda Chops: Safely Accessing Properties"
date:         2018-01-31
image:        /images/sheep-countryside.jpg
description:  Get introduced to ramda's prop, propOr, path & pathOr functions.
photoCredit:  George Hiles
photoWebsite: "https://unsplash.com/@hilesy"
---

_Thanks to [@joshsloat](https://twitter.com/joshsloat) and
[@zerkms](https://twitter.com/zerkms) for their review of this post._

One of the most prevalent causes of bugs I've seen in latter-day JavaScript
revolves around expectations with regard to data modeling. With the rise of
[react](https://reactjs.org/), [redux](https://redux.js.org/), et al, many of us
store our application state in an object whose keys and hierarchy can easily
change, leaving us sometimes with or without values that were in fact expected:
for example, `undefined is not a function` or trying to call `.map(...)` on a
non-mappable data type (such as `null` or `undefined`). While there are any
number of solutions for this issue that might even include diving into algebraic
data types, the [ramda library](http://ramdajs.com) gives us a few
helper methods that we can use right away to dig into our data structures and
extract values:

* [`prop`](http://ramdajs.com/docs/#prop)
* [`propOr`](http://ramdajs.com/docs/#propOr)
* [`path`](http://ramdajs.com/docs/#path)
* [`pathOr`](http://ramdajs.com/docs/#pathOr)

* * *

_Other ramda posts:_

* [Ramda Chops: Function Currying](/blog/ramda-chops-function-currying.html)
* [Ramda Chops: Function Composition](/blog/ramda-chops-function-composition.html)
* [Ramda Chops: Map, Filter & Reduce](/blog/ramda-chops-map-filter-and-reduce.html)

## `prop` & `propOr`
What happens normally if you expect an array, try to access the third item
(index position 2), but are actually provided `undefined` instead of an array?

```js
const arr = undefined
arr[2] // TypeError is thrown
```

What happens if you try to access the `length` property on what you think should
be an array but ends up being `null` or `undefined`?

```js
const arr = null
arr.length // TypeError is thrown
```

One solution is to do the "value or default" approach to keep the errors at bay:

```js
const arr = undefined
const xs = arr || []
xs[2] // undefined
xs.length // 0
```

An approach we could take to avoid the errors being thrown would be to use
ramda's [`prop`](http://ramdajs.com/docs/#prop) helper:

```js
import prop from 'ramda/src/prop'

const arr = undefined
prop(2, arr) // undefined
prop('length', arr) // undefined
```

_Ramda's [`length`](http://ramdajs.com/docs/#length) function would accomplish
a similar goal for `prop('length')`._

But if we want a default to be returned in lieu of our data not being present,
we can turn to [`propOr`](http://ramdajs.com/docs/#propOr):

```js
import propOr from 'ramda/src/propOr'

const arr = undefined
propOr({}, 2, arr) // {}
propOr(0, 'length', arr) // 0
```

If you need to select multiple properties without fear, then the
[`props`](http://ramdajs.com/docs/#props) or
[`pick`](http://ramdajs.com/docs/#pick) functions may be for you.

## `path` & `pathOr`
What if we are working in a deeply nested data structure where multiple keys in
our hierarchy may or may not exist? Enter [`path`](http://ramdajs.com/docs/#path)
and [`pathOr`](http://ramdajs.com/docs/#pathOr). These work similarly to `prop`
and `propOr` except that they use an array syntax to dive into data structures
and ultimately check for a _value_, whereas the `prop` family checks for a
property's presence.

```js
import path from 'ramda/src/path'

const data = {
  courses: {
    abc123: {
      title: 'How To Build a Tiny House',
      dueAt: '2018-01-30'
    }
  }
}

// getCourseTitle :: String -> String | undefined
const getCourseTitle = courseId =>
  path(['courses', courseId, 'title'])

getCourseTitle('abc123')(data) // "How To Build a Tiny House"
getCourseTitle('def456')(data) // undefined
```

_[Try this code in the ramda REPL](https://goo.gl/fdujHu)_

Or if we'd always like to default to a value, we can use `pathOr`:

```js
import pathOr from 'ramda/src/path'

const data = {
  courses: {
    abc123: {
      title: 'How To Build a Tiny House',
      dueAt: '2018-01-30'
    }
  }
}

// getCourseTitle :: String -> String
const getCourseTitle = courseId =>
  pathOr('My Course', ['courses', courseId, 'title'])

getCourseTitle('abc123')(data) // "How To Build a Tiny House"
getCourseTitle('def456')(data) // "My Course"
```

_[Try this code in the ramda REPL](https://goo.gl/PXD1ju)_

* * *

As I said before, there are _many_ different ways to solve this problem, but
I've found the `propOr` and `pathOr` family of ramda functions to be a great
starting point.

Until next time,
<br />
Robert
