---
author: "Robert Pearce"
authorTwitter: "@RobertWPearce"
desc: "A programmer's first steps into a larger accessibility world"
keywords: "button reset, css, accessibility, a11y, dear-dia11y"
title: "Dear Dia11y: Hello, accessibility world!"
---

_Originally published on my accessibility blog, dear-dia11y._

Dear dia11y,

Today I started an accessibility blog!

I normally write about functional programming, but I've been doing a _lot_ of
accessibility (a11y — a…11 letters…y) work this year, and I thought it would be
a good idea to write down some of the useful things I've learned.

Don't worry; these will generally be short.

For this first entry, I'd like to share about one of the most common things I
come across: when to use `<div>`s versus `<button>`s versus `<a>`s for
"clickable" elements.

Unless I have a _very_ good reason not to follow these rules, it's very
simple:

* use `<a>` if it changes the window's location
* use `<button>` if I can press on it and something happens
* try to not use `<div>` as a trigger for anything

If I'm concerned about the default browser styling of buttons, here are CSS
styles collected from around the internet that will allow me to reset a button's
default styling.

```css
.btn-reset {
  -webkit-appearance: none;
  -moz-appearance: none;
  appearance: none;
  background: none;
  border-radius: 0;
  border: none;
  color: inherit;
  font: inherit;
  margin: 0;
  padding: 0;
}
```

If I have to use `<div>`, make sure I remember to read [MDN's role=button
page](https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA/Roles/button_role)
and the [WAI ARIA Practices 1.2 button specification](https://www.w3.org/TR/wai-aria-practices-1.2/#button)
to figure out how to do it correctly. Most importantly, if I decide I'm going
to try to reproduce native browser behavior that all manner of programs and
devices rely on, __make sure my team and I know how to test it and not break it
in the future__.

I must confess that I've made and continue to make many a11y mistakes, but it's
in learning from our mistakes that we are able to grow.

Yours,<br />
Robert W. Pearce
