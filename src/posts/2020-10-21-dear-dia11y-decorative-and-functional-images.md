---
author: "Robert Pearce"
authorTwitter: "@RobertWPearce"
desc: "How to leverage decorative and functional images for clearer accessibility"
keywords: "decorative image, functional image, aria-hidden, alt text, accessibility, a11y, dear-dia11y"
title: "Dear Dia11y: Decorative & functional images"
---

_Originally published on my accessibility blog, dear-dia11y._

Dear dia11y,

There have been times in the past where I've used what I'll momentarily call
"helper images" like user avatars, button icons, and illustrations, and in
trying to make them more accessible, I've given them alternative text like
"user avatar" and "rubbish bin". While I should almost always accurately
describe and label images, there are times when it's safe for me to mark images
as _decorative_ or provide a _functional_ label instead of alternative text.

For my future reference, the W3 website has a few good articles on these
subjects:

* [WAI Decorative Images](https://www.w3.org/WAI/tutorials/images/decorative/)
* [WAI Functional Images](https://www.w3.org/WAI/tutorials/images/functional/)
* [WAI Images Tips & Tricks](https://www.w3.org/WAI/tutorials/images/tips/)

I'm leaving myself some notes and examples on these subjects below.

## Decorative images

If I have an image that doesn't add any extra content to the page, I can
probably assume that this image is _decorative_.

A great example would be user avatars right next to user names (see example
just below).

<p class="codepen" data-height="381" data-theme-id="dark" data-default-tab="html,result" data-user="rpearce" data-slug-hash="wvWzYbO" style="height: 381px; box-sizing: border-box; display: flex; align-items: center; justify-content: center; border: 2px solid; margin: 1em 0; padding: 1em;" data-pen-title="Decorative Images">
  <span>See the Pen <a href="https://codepen.io/rpearce/pen/wvWzYbO">
  Decorative Images</a> by Robert Pearce (<a href="https://codepen.io/rpearce">@rpearce</a>)
  on <a href="https://codepen.io">CodePen</a>.</span>
</p>
<script async src="https://static.codepen.io/assets/embed/ei.js"></script>

I probably shouldn't label these images "avatar", "user avatar" or even "avatar
for ${userName}", for labels like these don't describe the content of the
images and therefore likely cannot describe them in a way that adds valuable
content to the document.

If I have a list of users with their avatars and names but no explicit
descriptions of their avatars, what is the point of announcing "avatar" for each
user?

Instead, I can provide an `alt` attribute of `""` to tell assistive
technologies that I want to ignore this image.

```html
<li ...>
  <img
    alt=""
    class="..."
    height="60"
    src="..."
    width="60"
  />
  <span>Robert W. Pearce</span>
</li>
```

If this image is an SVG or any other type of decorative element that might get
picked up by a screen reader, I can use
[`aria-hidden="true"`](https://developer.mozilla.org/en-US/docs/Web/Accessibility/ARIA/ARIA_Techniques/Using_the_aria-hidden_attribute)
to completely hide it from the screen reader, or I can use
[`role="presentation"`](https://www.w3.org/TR/using-aria/#presentation) to
effectively set `role="none"` and remove any existing semantics (I prefer the
former approach).

## Functional images

There are times when I want to use images as visual aids to accompany actionable
text like "Delete email", or perhaps I only want to present an image, but I
really intend for that image to mean "Delete email" — note to self: this is
called a _functional image_.

<p class="codepen" data-height="265" data-theme-id="dark" data-default-tab="html,result" data-user="rpearce" data-slug-hash="XWKjyJp" style="height: 265px; box-sizing: border-box; display: flex; align-items: center; justify-content: center; border: 2px solid; margin: 1em 0; padding: 1em;" data-pen-title="Functional image">
  <span>See the Pen <a href="https://codepen.io/rpearce/pen/XWKjyJp">
  Functional image</a> by Robert Pearce (<a href="https://codepen.io/rpearce">@rpearce</a>)
  on <a href="https://codepen.io">CodePen</a>.</span>
</p>
<script async src="https://static.codepen.io/assets/embed/ei.js"></script>

Here is an HTML summary of the example above:

```html
<button type="button" ...>
  <svg aria-hidden="true" ...>
    <!-- ... -->
  </svg>
  <span>Delete email</span>
</button>
```

The only thing of note is that I have `aria-hidden="true"` on the `<svg>`.

While it _could_ be argued that narrating `"button, rubbish bin image, Delete
email"` could give some added context, the image is intended as a visual aid,
and labelling it seems to add a bit of noise, distracting from the simple point
of what this does: `"button, Delete email"`.

What if I want to only use the image, but I intend for it to mean
`"Delete email"`?

<p class="codepen" data-height="265" data-theme-id="dark" data-default-tab="html,result" data-user="rpearce" data-slug-hash="OJXRaZJ" style="height: 265px; box-sizing: border-box; display: flex; align-items: center; justify-content: center; border: 2px solid; margin: 1em 0; padding: 1em;" data-pen-title="Functional image + label">
  <span>See the Pen <a href="https://codepen.io/rpearce/pen/OJXRaZJ">
  Functional image + label</a> by Robert Pearce (<a href="https://codepen.io/rpearce">@rpearce</a>)
  on <a href="https://codepen.io">CodePen</a>.</span>
</p>
<script async src="https://static.codepen.io/assets/embed/ei.js"></script>

Here is an HTML summary of the example above:

```html
<button aria-label="Delete email" type="button" ...>
  <svg aria-hidden="true" ...>
    <!-- ... -->
  </svg>
</button>
```

In addition to having `aria-hidden="true"` on the `<svg>`, instead of the text
node being there, there is an `aria-label="Delete email"` on the surrounding
`<button>` element. This button will also narrate, `"button, Delete email"`.

* * *

That's it for decorative and functional images for today!

Yours,<br />
Robert W. Pearce
