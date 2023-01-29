---
author: "Robert Pearce"
authorTwitter: "@RobertWPearce"
desc: "Tips on how to unsuccessfully introduce and keep functional programming styles and principles in your workplace."
image: ""
keywords: "functional programming, functional javascript, javascript, ramda, fp js"
photoCredit: ""
photoWebsite: ""
title: "How To Lose Functional Programming At Work"
updated: "2022-01-24T12:00:00Z"
---

Hi! If you're looking to lose functional programming at work, here are a bunch
of mistakes I've made on JS-heavy web teams over the years that can help you do
the same.

Enjoy!

_Note: if sarcasm and self-deprecation aren't your thing, you can skip to the
[real-talk takeaways](#real-talk-takeaways)._

## PDF version

Here is the much prettier PDF version that is also useful for sending to your
teammates or using in your own lunch-n-learn tech talk.

<iframe
  class="pdf"
  loading="lazy"
  src="/pdfs/2023-01-24-how-to-lose-fp-at-work.pdf"
  title="How to Lose Functional Programming at Work - PDF"
></iframe>

<a class="pdf-link" href="/pdfs/2023-01-24-how-to-lose-fp-at-work.pdf">
  How to Lose Functional Programming at Work - PDF
</a>

## "Do"s and "Don't"s

These can be read in any order; choose your own adventure.

### Don't have static type checking

* No [TypeScript](https://www.typescriptlang.org)
* No [Flow](https://flow.org)
* No [ReasonML](https://reasonml.github.io)
* No [Elm](https://elm-lang.org/)
* No (insert language with static type checking that compiles to JS)

```javascript
const processData = composeP(syncWithBackend, cleansePII, validateData)

// * What arguments and their types are expected here?
//
// * If each function is written like this, how can
//   one suss out what data are flowing where?
//
// * How hard is this going to be to debug?
//   Use this everywhere: `(x) => (console.log(x), x)`
```

Oh, so point-free style programming is the problem? Not so fast:

```javascript
async function processData(data) {
  await validateData(data)
  const cleansedData = cleansePII(data)
  await syncWithBackend(cleansedData)
  return data
}

// or for the Promise-chainers…

const processData = data =>
  validateData(data)
    .then(cleansePII)
    .then(syncWithBackend)
    .then(() => data)
```

### Don't use well-known code documentation tools

* No [jsdoc](https://jsdoc.app)
* …are there any other contenders?

Deprive your team of this clarity and helpful auto-completion:

```javascript
// NOTE: this is an untested, small example

/**
 * @typedef {Object} ReportingInfo
 * @property {("light"|"dark")} userTheme - Current user's preferred theme
 * @property {string} userName - Current user's name
 * @property {UUID} postId - The current post's ID
 */

/**
 * Validates that the reporting data (current user site prefences and post info)
 * is OK, removes personally identifiable information, syncs this info with the
 * backend, and gives us back the original data.
 *
 * @param {ReportingInfo} data - The current user's site preferences and post info
 * @returns {Promise<ReportingInfo>} - The original reporting data
 */
const processData = data => // …
```

### Don't properly train new and existing colleagues

Truly believe, in your heart, that you can write a pile of blog posts, collect a
bunch of other great learning resources, hand them all to a new FP learner,
recommend they read as much as they can then come back with questions, and
expect them to come out the other side at all.

Conversely, spend all your time and energy on a couple of individuals, neglect
the others, fail to write any useful learnings down, and forget to encourage
these initiates to turn around and help teach their other colleagues, in turn.

### Don't bother getting the other engineering teams on board and rowing in the same direction

* "If I build it, they will notice… right?"
* _Idea: Lunch 'n learn about FP?_ Me: They'll find out I don't know things.
* _Idea: Meet with other team leaders and see if they're interested in adoption, what
  could be better, or share why they're not interested?_ Me: This includes
  managers who report on things, and if they think I'm dumb or rocking the boat
  too much, I might look worse than I potentially already do.

Instead, if you keep it to yourself, other teams won't get to contribute and
probably improve the state of things.

### Do live by the creed, "Point-free or die"

Watch the video, ["Point-Free or Die: Tacit Programming in Haskell and
Beyond"](https://www.youtube.com/watch?v=seVSlKazsNk), by Amar Shah

Contrived example:

```javascript
import { __, any, lt } from 'ramda'
const anyLt0 = any(lt(0, __)) // hint: this has a bug in it
anyLt0([1, 2, 3]) // true — ugh…

// vs. the probably pretty simple…

const anyLt0 = numbers => numbers.some(n => n < 0)
anyLt0([0, 1, 2, 3]) // false
anyLt0([0, 1, 2, -1, 3]) // true — looks good

// 👆 should we resist eta-converting this?!
// …
// NOT ON MY WATCH

const any = fn => array => array.some(fn)
const isLtN = x => n => x < n
const isLt0 = isLtN(0)
const anyLt0 = any(isLt0)
anyLt0([1, 2, 3]) // true — ugh; the bug is back
```

Real, but altered, example:

```javascript
const finishItems = compose(
  flip(merge)({ isDone: true, amtComplete: 100 }),
  over(
    lensProp('indexedObjects'),
    mapVals(
      compose(
        over(lensProp('indexedObjects'), mapVals(assoc('isDone', true))),
        assoc('isDone', true)
      )
    )
  )
)
```

### Do prefer the wrong abstraction over the right duplication

I was at Sandi Metz’ RailsConf 2014 Chicago talk, [All the Little
Things](https://www.youtube.com/watch?v=8bZh5LMaSmE), where she blew my mind
with the simplicity of “preferring duplication over the wrong abstraction”. Two
years later, she followed it up with some great blog commentary, [The Wrong
Abstraction](https://sandimetz.com/blog/2016/1/20/the-wrong-abstraction).

But in this case, dilute your core business logic to broad generalizations that
can be extracted and abstracted over and over, fail to understand category
theory enough for this to be useful, and be the only one who knows how these
abstractions work.

You'll know you've lost people when normally thorough PR reviews now look like,
"👍".

### Don't refactor old patterns that clearly don't work for the team

Make sure that people coming into the project have your old code patterns to
emulate that you cringe looking at years later but never made the time to
update.

While you could allocate investment time to this or reading up on how to
improve your technical leadership skills, spend that time making new features,
instead.

### Do force functional patterns into a language that wasn't built for them (bonus: cryptic stack traces)

* Lean into recursive functions, realize ES2015 tail call optimization is
  only a thing in Safari nowadays (if that?), and sprinkle a
  [`trampoline`](https://github.com/getify/Functional-Light-JS/blob/master/manuscript/ch8.md/#trampolines) function over
  the issue to make it go away and not blow out your call stack
* Get cryptic error messages and stack traces because JS isn't going to be able
  to follow your custom `curry` and `compose` functions by default, meaning
  you'll have to go the extra mile like Brian does in [Debugging
  functional](https://medium.com/@drboolean/debugging-functional-7deb4688a08c) to
  prevent the issues described by Thai in [Partially-applied (or curried)
  functions could obfuscate the JavaScript stack trace](https://medium.com/hackernoon/partially-applied-curried-functions-could-obfuscate-the-javascript-stack-trace-84d66bd8032e)
  (Thai's ultimate recommendations are _"use a typed language that guarantees that
  your functions will never receive an invalid data"_ or _"just don't go
  overboard with pointfree style JavaScript"_).
* The Glasgow Haskell Compiler can be optimized to fuse `map g . map f` into a
  single `map` thanks to composition, knocking out the work in one go at
  runtime. While `.map(…).map(…).map(…)` seems to be optimized pretty ok
  in JS runtimes, you're still asking it do _N_ times the work, and you may not
  realize it. Oops.
* BYO algebraic data type libraries (many of these are awesome, though)

### Do opaquely compose and sequence the entirety of your API endpoints and make them hard to debug

On the surface, this isn't so difficult to read…

```javascript
// handler for POST /posts

import { createPost } from 'app/db/posts'
import { authenticateUser, authorizeUser } from 'app/lib/auth'
import { trackEvent } from 'app/lib/tracking'

const validateRequestSchema = payload => { /* … */ }

export const handleCreatePost = curry(metadata =>
  pipeP(
    authenticateUser(metadata),
    authorizeUser(metadata),
    validateRequestSchema,
    createPost(metadata),
    tapP(trackEvent('post:create', metadata)),
    pick([ 'id', 'authorId', 'title' ])
  )
)
```

Did you catch that this expects 2 arguments? Did you also know that
`authenticateUser` ignores the 2nd argument sent to it? How would you? And what
about `trackEvent`? Does it receive the `payload`, or does `createPost()` return
post-related data?

Let's write this another way:

```javascript
export async function handleCreatePost(metadata, payload) {
  await authenticateUser(metadata)
  await authorizeUser(metadata, payload)
  await validateRequestSchema(payload)

  const post = await createPost(metadata, payload)

  await trackEvent('post:create', metadata, payload)

  return {
    id: post.id,
    authorId: post.authorId,
    title: post.title,
  }
}
```

I'm not saying that option #2 is an awesome handler, but if you want to make it
trickier for people, go with option #1.

### Do recreate imperative, procedural programming while calling it "declarative"

```javascript
const setBookReadPercentByType = (contentType, statusObject) =>
  assoc(
    'readPercent',
    pipe(
      prop('subItems'),
      values,
      filter(propEq(contentType, 'chapter')),
      length,
      flip(divide)(compose(length, keys, prop('subItems'))(statusObject)),
      multiply(100),
      Math.round
    )(statusObject),
    statusObject
  )
```
Do have 8+-ish different patterns for function composition

```javascript
// 👇 These 4, plus Promisified versions of them,
//    plus combinations of them all used at once;
//    doesn't include ramda's pipeWith and composeWith

// compose
const getHighScorers =
  compose(
    mapProp('name'),
    takeN(3),
    descBy('score')
  )

// pipe
const getHighScorers =
  pipe(
    descBy('score'),
    takeN(3),
    mapProp('name')
  )

// composeWithValue
const getHighScorers = players =>
  composeWithValue(
    mapProp('name'),
    takeN(3),
    descBy('score'),
    players
  )

// pipeWithValue
const getHighScorers = players =>
  pipeWithValue(
    players,
    descBy('score'),
    takeN(3),
    mapProp('name')
  )

// …but then now mix and match them with actual,
// real-life business logic.
```

### Do make yourself one of the few who can debug algebraic data types during midnight incidents

Ensure your team is surprised by all of the following words when debugging or
altering your code in the pursuit of their own work tasks:

* `Task`, `Maybe`, `Either`, `Result`, `Pair`, `State`
* `bimap`
* `chain`
* `bichain`
* `option`
* `coalesce`
* `fork`
* `sequence`
* `ap`
* `map` — and I don't mean `Array.prototype.map`, nor a `new Map()`, nor a
  key/value object

### Don’t have SQL (a declarative language) do data transformations for you — DIWHY??? it yourself

Instead, and in the name of immutability, use data pipelines in your app to
apply changes to your data, one transformation at a time, and accidentally do as
many key/value iterations and memory allocations as possible. 😬

### Do suggest, on PRs, that colleagues completely refactor what they've done to fit <em>your</em> functional style

> What you have here works great, but what could this look like if we flipped
all the function arguments around, removed all these intermediate variables, and
mapped these operations over an `Either`?

or

> I noticed you're explicitly constructing these objects in their functions. If
you were to use &lt;UTILITY-FUNCTION&gt;, you could declare the shape of your
outputted object and use functions as the values to look up or compute each
value given some data.

And for some quick final ones:

* Do sow imposter syndrome in others and exclude them by sharing non-beginner FP articles
* Do keep writing code using FP tools even when nobody else on the team is
* Do achieve peak perceived passive-aggression by getting tired and commenting PRs with emojis
* Do have "the FP talk" at work, and then publicly own your mistakes

## Real-talk takeaways

Much of the backwards recommendations here can be, on the surface, written off
as symptoms of inexperience, a lack of technical leadership from me, and
obviously not the right paths.

But I think it's something deeper than those easy explanations.

Most things in life need to be tended to in order for them to go the ways that
we'd like them to; our relationships, our physical & mental health, our gardens.
With most of these things in life, we strive to purposefully sculpt our futures.

However, there are many things that we accidentally sculpt. For example, if the
fastest way from your back door to your garden is through your grassy yard, the
simplest thing is to walk over the grass to get there. It makes sense for a
while, but over time, your stepping on the grass carves a path that you never
intended to create — it was an unintended consequence of your gardening.

This same thing happens with our minds and in our work. If we're not paying
attention to the big picture, the path of least resistance can carve canyons.

In my case, here, not taking responsibility of a path I helped create, coupled
with persistent imposter syndrome and a feeling I needed to ship features and
just look out for myself, instead of making time for re-evaluation, helped lead
to the difficulties above for others and a loss of "higher" functional
programming in a pretty good workplace that gives teams the freedom to choose
their own tools.

But all is not lost! The core tenets of FP seem to remain:

* Immutability: Preferring to _efficiently_ recreate objects over mutating the
  originals
* Purity: call the same function with the same arguments, and you
  get the same results
* Moving effects to the conceptual edge of an application
* Very few classes (if any; `React` doesn't count), no inheritance,
  `map`/`filter`/`reduce`, etc.

It seems a happy balance has been collectively decided on, and I'm excited to
see where it goes. Perhaps, this time around, I'll be better.

* * *

Thanks for reading,
<br />
Robert
