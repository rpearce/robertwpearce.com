---
author: "Robert Pearce"
authorTwitter: "@RobertWPearce"
desc: "A quick-dive in to RiotJS, with examples."
keywords: "riotjs, riot.js, riotjs tutorial, tutorial, javascript"
title: "RiotJS Example"
---

When I look at any of the myriad JavaScript UI libraries out there, I now ask
two questions:

* Does data flow in one direction?
* Do updates to the UI happen intelligently?

I am a fan of Facebook's [ReactJS](https://facebook.github.io/react/) library
because of its DOM diffing (via the "virtual DOM") and one-way data binding.
React is a tool I use every day and have come to enjoy (sans-JSX), but I am
always on the lookout for way to do things simpler.

A colleague of mine recently shared the second iteration of
[RiotJS](https://muut.com/riotjs/) with me. Of course, I was sucked in because
it compared itself with React (a bold statement). You can [view the comparisons
between Riot and React](https://muut.com/riotjs/compare.html) for yourself.

One unfortunate fact about fledgling JS libraries is that they lack examples of
how to accomplish common goals for the web. This is an attempt to help out with
that.

## Installation

### Project-Specific

Given you have a `package.json` set up, you can easily install the Riot compiler
as a development dependency:

```text
npm install riot --save-dev
```

and then set up Riot to run as an NPM script and watch for any changes

```javascript
"scripts": {
  "watch:riot": "riot -w src/ build/"
}
```

which is then run by a simple

```text
npm run watch:riot
```

Alternatively, you can [download the riot.js
library](https://muut.com/riotjs/download.html) via any of their recommended
methods.

### Global Installation (alternative)

If you don't have a `package.json` and want to install Riot globally:

```text
npm install riot -g
riot -w src/ build/
```

## Tab Example

I decided to start small and make a tabbing example where clicking a tab shows
content related to it underneath. Here is the final product:

<iframe
  class="iframe--example"
  frameborder="0"
  height="550"
  loading="async"
  src="https://rpearce.github.io/riotjs-examples/tabs.html"
></iframe>

### Step 1: Start With Markup

Starting with a blank HTML document, add the `<riot-tabs></riot-tabs>` tag to
your document:

```html
<!DOCTYPE html>
<html>
  <head></head>
  <body>
    <riot-tabs></riot-tabs>
  </body>
</html>
```

As mentioned previously, we know we need the (very tiny) RiotJS library, so
don't forget to include it:

```html
<body>
  <riot-tabs></riot-tabs>

  <script src="path/to/riot-2.0.1.js"></script>
</body>
```

Easy enough, right? Given Riot doesn't write our applications for us, we will
need to tell Riot to mount some component, which in this case is "tabs."

```html
<body>
  <riot-tabs></riot-tabs>

  <script src="path/to/riot-2.0.1.js"></script>
  <script>riot.mount('riot-tabs')</script>
</body>
```

When we run this code through the browser, we're going to receive an error
telling us that `'tabs'` is not a thing. Congrats! Time for Step 2.

### Step 2: Creating Your Custom Tag

Riot's NPM package, as mentioned earlier, allows us to write and compile
pseudo-markup mixed with a little JS. To get started, create a `src` folder and
add a `tabs.tag` file to it, then run

```text
npm run watch:riot
```

if you have an NPM script set up or

```text
riot -w src/ build/
```

to compile and watch for more changes to the file/folder.

Back in the `tabs.tag` file, add this:

```html
<riot-tabs>
  <h2>Tabs</h2>
  <ul>
    <li class={ tabItem: true }>Tab 1</li>
    <li class={ tabItem: true }>Tab 2</li>
    <li class={ tabItem: true }>Tab 3</li>
  </ul>
</riot-tabs>
```

That looks almost exactly like vanilla HTML, save for the conditional class(es),
which we will use later with `is-active` classes. Also, they are way better than
concatenating `className` strings yourself.

Refreshing your browser will show you that you now have content that is nested
within a `<riot-tabs></riot-tabs>` tag.

Next up, we can add in the different tabs' contents:

```html
<riot-tabs>
  <h2>Tabs</h2>
  <ul>
    <li class={ tabItem: true }>Tab 1</li>
    <li class={ tabItem: true }>Tab 2</li>
    <li class={ tabItem: true }>Tab 3</li>
  </ul>
  <div class="tabContent">
    <div class={ tabContent__item: true }>(1) Lorem ispum dolor...</div>
    <div class={ tabContent__item: true }>(2) Lorem ispum dolor...</div>
    <div class={ tabContent__item: true }>(3) Lorem ispum dolor...</div>
  </div>
</riot-tabs>
```

Okay, this is no big deal, so far.

Being software developers, we hate writing things over and over, so let's start
with the tabs.

```html
<riot-tabs>
  <h2>Tabs</h2>
  <ul>
    <li each={ tab, i in tabs } class={ tabItem: true }>{tab.title}</li>
  </ul>
  <div class="tabContent">
    <div class={ tabContent__item: true }>(1) Lorem ispum dolor...</div>
    <div class={ tabContent__item: true }>(2) Lorem ispum dolor...</div>
    <div class={ tabContent__item: true }>(3) Lorem ispum dolor...</div>
  </div>

  this.tabs = [
    { title: 'Tab 1' },
    { title: 'Tab 2' },
    { title: 'Tab 3' }
  ]
</riot-tabs>
```

Riot has a nice each={ item, i in array } attribute, similar to JavaScript's
for ... in ...

While we're at it, why not iterate over the content items, as well?

```html
<riot-tabs>
  <h2>Tabs</h2>
  <ul>
    <li each={ tab, i in tabs } class="tabItem">{tab.title}</li>
  </ul>
  <div class="tabContent">
    <div each={ tab, i in tabs } class="tabContent__item">{tab.content}</div>
  </div>

  this.tabs = [
    { title: 'Tab 1', content: "(1) Lorem ipsum dolor" },
    { title: 'Tab 2', content: "(2) Lorem ipsum dolor" },
    { title: 'Tab 3', content: "(3) Lorem ipsum dolor" }
  ]
</riot-tabs>
```

Next, we need to set a default "active tab" and "active content."

### Step 3: Conditional Classes

We want to be able to specify a default tab and tab content. This is
accomplished via a conditional `is-active` class on both the `.tabItem` as well
as the corresponding `.tabContent__item`. To keep track of what tab/content is
active, we can

* add a property to the `this.tabs` array objects
* set an `activeTab` property and
* create a function to check if the currently iterated tab is the `activeTab`

```html
<riot-tabs>
  <h2>Tabs</h2>
  <ul>
    <li each={ tab, i in tabs } class="tabItem { is-active: parent.isActiveTab(tab.ref) }">{tab.title}</li>
  </ul>
  <div class="tabContent">
    <div each={ tab, i in tabs } class="tabContent__item { is-active: parent.isActiveTab(tab.ref) }">{tab.content}</div>
  </div>

  this.tabs = [
    { title: 'Tab 1', ref: 'tab1', content: "(1) Lorem ipsum dolor" },
    { title: 'Tab 2', ref: 'tab2', content: "(2) Lorem ipsum dolor" },
    { title: 'Tab 3', ref: 'tab3', content: "(3) Lorem ipsum dolor" }
  ]

  this.activeTab = 'tab1'

  isActiveTab(tab) {
    return this.activeTab === tab
  }
</riot-tabs>
```

Since these are _conditional_ classes, they will either be evaluated as true or
false (I believe that anything that is not falsy is considered true; for
example, `new Date()` is considered `true`). Here, we create a function called
`isActiveTab` and call it from the item itself, but because the function is not
scoped to the item, we need to access the `parent` scope and call the function
on that.

Finally, we need a way to react to _events_.

### Step 4: Toggling the Tabs

When we click on a tab, we want that tab to now be active, and we want the
corresponding tab content to be displayed. This can be done via an `onclick`
handler that calls a function on the parent called `toggleTab`

```html
<riot-tabs>
  <h2>Tabs</h2>
  <ul>
    <li each={ tab, i in tabs } class="tabItem { is-active: parent.isActiveTab(tab.ref) }" onclick={ parent.toggleTab }>{tab.title}</li>
  </ul>
  <div class="tabContent">
    <div each={ tab, i in tabs } class="tabContent__item { is-active: parent.isActiveTab(tab.ref) }">{tab.content}</div>
  </div>

  this.tabs = [
    { title: 'Tab 1', ref: 'tab1', content: "(1) Lorem ipsum dolor" },
    { title: 'Tab 2', ref: 'tab2', content: "(1) Lorem ipsum dolor" },
    { title: 'Tab 3', ref: 'tab3', content: "(1) Lorem ipsum dolor" }
  ]

  this.activeTab = 'tab1'

  isActiveTab(tab) {
    return this.activeTab === tab
  }

  toggleTab(e) {
    this.activeTab = e.item.tab.ref
    return true
  }
</riot-tabs>
```

The `onclick` event handler receives an event object that is packed with
information. What we want is the current tab that we are clicking on, and this
is accessed through `e.item.tab.ref`, which is just the `ref` property on the
`tab` object of the currently iterated `item`.

According to the Riot docs, when an event handler is called, Riot will
automatically call `this.update()` and re-render the component. However, I found
that after I altered my data, I had to `return true`.

Once this event handler is completed and the component is re-rendered, the
correct tab and content will be displayed, and you will be happy.

## Wrap up

In sum, playing with Riot was a mostly enjoyable experience, and I am thankful
to the [Muut](https://muut.com/) folks for releasing it.

While there are quirks (single quote vs double quote issues, among others) and
opinions (neglecting the use of semi-colons, as well as `return`s), this is a
promising UI library that I am _definitely_ going to consider vs. React in my
future projects.
