---
author: "Robert Pearce"
authorTwitter: "@RobertWPearce"
desc: "Resolving an image-with-SVG-source issue in Safari using VoiceOver"
keywords: "SVG, VoiceOver, img src, accessibility, a11y, dear-dia11y"
title: "Dear Dia11y: Image with SVG source VoiceOver bug"
---

_Originally published on my accessibility blog, dear-dia11y._

Dear dia11y,

I recently documented an issue where some decorative illustration `<img />`
elements with SVG sources were, when using VoiceOver in macOS Safari, announcing
every `<g>` (group) element by narrating the word "image". Here's what the HTML
essentially was:

```html
<img
  alt=""
  class="foo"
  src="path/to/file.svg"
/>
```

The `alt=""` should have alerted the screen reader that this was a [decorative
image](https://www.w3.org/WAI/tutorials/images/decorative/), but no matter —
let's add `aria-hidden="true"` to it.

I recommended this issue and possible fix to a colleague and forgot about it.

This colleague read up on `role="presentation"` versus `aria-hidden="true"` and
whatnot and eventually discovered that neither solved the problem! What a
mystery.

This person did some digging and discovered that __image elements with SVG
sources that aren't marked as `role="img"` can have VoiceOver jump into the SVG
document and start reading things__.

The fix?

```html
<img
  alt=""
  class="foo"
  role="img"
  src="path/to/file.svg"
/>
```

That's it! HTML and a11y linters will complain about the repetitive `role`, but
we do what we have to do.

It also turned out that this was a problem with non-decorative images. Image
alternative text was being read along with the innards of SVG sources (even
`<text>` elements!).

Here are some nicely documented test cases of the issue:

* http://pauljadam.com/demos/svg-role-img.html
* http://a11yideas.com/testcode/mwp/iOSsvg.html

Yours,<br />
Robert W. Pearce
