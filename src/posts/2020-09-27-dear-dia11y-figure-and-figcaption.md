---
author: "Robert Pearce"
authorTwitter: "@RobertWPearce"
desc: "I need to use role & aria-labelledby with figure elements in order for all screen readers across all browsers to recognize the figure and figcaption correctly."
keywords: "figure, figcaption, JAWS, NVDA, VoiceOver, accessibility, a11y, dear-dia11y"
title: "Dear Dia11y: Figure & Figcaption – Supporting IE11, JAWS, NVDA, & VO"
---

_Originally published on my accessibility blog, dear-dia11y._

Dear dia11y,

At work, I must support IE11 along with all the other major browsers, so that
means I need to support
[JAWS](https://www.freedomscientific.com/products/software/jaws/) &
[NVDA](https://www.nvaccess.org/download/) across these browsers on Windows in
addition to VoiceOver on macOS & iOS Safari (I don't do much in the Android
world at the moment).

About 1-2 months ago, I did a lot of work fixing up issues around `<figure>`s
and `<figcaption>`s. For a refresher, here's
[the example that MDN uses](https://developer.mozilla.org/en-US/docs/Web/HTML/Element/figcaption):

```html
<figure>
  <img
    alt="Elephant at sunset"
    src="/media/cc0-images/elephant-660-480.jpg"
  />
  <figcaption>
    An elephant at sunset
  </figcaption>
</figure>
```

This is straightforward, but JAWS + IE11 didn't quite recognize this as a
figure, and we had issues with the caption because we had a `<div>` as the
parent of the `<figcaption>`. NVDA + Firefox didn't like this setup, either.
Here's what our example was structured like:

```html
<figure>
  <img
    alt="Elephant at sunset"
    src="/media/cc0-images/elephant-660-480.jpg"
  />
  <div>
    <figcaption>
      An elephant at sunset
    </figcaption>
  </div>
</figure>
```

After going back and forth with different combinations of elements and
attributes, here's what was settled on to make this work across all these
browsers and screen readers:

```html
<figure aria-labelledby="caption-id" role="figure">
  <img
    alt="Elephant at sunset"
    src="/media/cc0-images/elephant-660-480.jpg"
  />
  <figcaption id="caption-id">
    An elephant at sunset
  </figcaption>
</figure>
```

Adding `role="figure"` to the `<figure>`, explicitly stating that it was
labelled by the `<figcaption>`, and placing `<figcaption>` as an immediate child
of `<figure>` resolved all our issues.

This was a helpful table of related compatibility:
https://www.powermapper.com/tests/screen-readers/labelling/img-figcaption/

Yours,<br />
Robert W. Pearce
