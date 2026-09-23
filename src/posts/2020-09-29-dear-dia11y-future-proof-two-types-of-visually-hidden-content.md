---
author: "Robert Pearce"
authorTwitter: "@RobertWPearce"
desc: "How to future-proof content intended for screen readers and other assistive devices"
keywords: "visually-hidden, css, hidden, accessibility, a11y, dear-dia11y"
title: "Dear Dia11y: Future-proof two types of visually hidden content"
---

_Originally published on my accessibility blog, dear-dia11y._

Dear dia11y,

I would prefer to not hide content from anyone, but there are times when I need to provide extra context and functionality when a design is not inclusive enough.

The [`.visually-hidden` "clip pattern" from The A11y Project](https://www.a11yproject.com/posts/2013-01-11-how-to-hide-content/) is excellent for hiding content that is intended for screen reader users, and it can be leveraged to provide interactive elements, like `<button>`s, that reveal themselves on focus for screen reader, keyboard, and other assistive device users alike. Here's what that website's recommendation is:

```css
.visually-hidden:not(:focus):not(:active) {
  clip: rect(0 0 0 0);
  clip-path: inset(50%);
  height: 1px;
  overflow: hidden;
  position: absolute;
  white-space: nowrap;
  width: 1px;
}
```

For most use cases, this is great. But my work in large codebases has shown me that if I want this to not take up any space, I need to get bossy and guard against future changes, as well.

```css
.visually-hidden:not(:focus):not(:active) {
  border: none !important;
  clip-path: inset(50%) !important;
  clip: rect(0, 0, 0, 0) !important;
  height: 1px !important;
  margin: 0 !important;
  overflow: hidden !important;
  padding: 0 !important;
  position: absolute !important;
  white-space: nowrap !important;
  width: 1px !important;
}
```

While most things I do in my life aren't important to anyone but me, getting visually hidden content right is _very_ `!important`.

With these additions we can ensure that we don't break any existing designs.

However, what if we transition from one page / view to another and need to call `element.focus()` on a visually hidden element in order to manage focus context?  We might have something like this:

```html
<h1 class="visually-hidden" tabindex="-1">
  A necessary hidden h1 for whatever reason
</h1>
```

If we call `.focus()` on that `<h1>`, the `:not(:focus)` part of the `.visually-hidden` definition will cause it to be _not_ hidden!

The `.visually-hidden` class is intended to be a catch-all to also account for links and buttons, and we definitely want to leverage that, but there are times where we _know_ we will never want it to be focused, so we can be explicit about doing so by having an additional class, `.visually-hidden-always`, and reuse our code via a grouped selector.

```css
.visually-hidden:not(:focus):not(:active),
.visually-hidden-always {
  border: none !important;
  clip-path: inset(50%) !important;
  clip: rect(0, 0, 0, 0) !important;
  height: 1px !important;
  margin: 0 !important;
  overflow: hidden !important;
  padding: 0 !important;
  position: absolute !important;
  white-space: nowrap !important;
  width: 1px !important;
}
```

Something important to note is the usage of `1px`. I may be tempted to use `em` or `rem` or `ch` or `ex` or something clever, _but I should not do this_!  Depending on the `font-size`, the browser, the monitor DPI, etc, something like `0.1em` can end up as a half pixel and be rounded down to `0`, thus making this inaccessible to screen readers! Use `1px`, I beg of my future self; it's okay to use `px` for certain things.

Yours,<br />
Robert W. Pearce
