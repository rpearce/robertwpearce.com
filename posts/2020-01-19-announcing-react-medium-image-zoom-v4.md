---
author: "Robert Pearce"
authorTwitter: "@RobertWPearce"
description: "react-medium-image-zoom is an open source image zooming library based on medium.com's implementation of image zooming"
image: "/images/te-mata-peak.jpg"
keywords: "react, react-medium-image-zoom, image zoom, image, zoom, reactjs"
photoCredit: "robertwpearce"
photoWebsite: "https://www.instagram.com/robertwpearce"
title: "Announcing react-medium-image-zoom v4"
---

_tl;dr => I've release v4 of [react-medium-image-zoom](https://www.npmjs.com/package/react-medium-image-zoom),
and you should consider use it for zooming images. Check out [the Storybook
Examples](https://rpearce.github.io/react-medium-image-zoom/) to see it in
action._

## History of `react-medium-image-zoom`

I wrote the first version of [`react-medium-image-zoom`](https://www.npmjs.com/package/react-medium-image-zoom)
in 2016 in a 6m x 6m flat in London that my (now) wife and I lived in. At the
time, I had been enamored with [medium.com's image zooming](https://medium.design/image-zoom-on-medium-24d146fc0c20)
and wanted to share that with the [React.js](https://reactjs.org/) masses, so I
wrote the first implementation on nights and weekends, and once it published,
it was quickly added to projects at my day job.

## People Use It?!

Since then, [`react-medium-image-zoom`](https://www.npmjs.com/package/react-medium-image-zoom)
has **22** [All Contributors](https://allcontributors.org/), has reached up to
**50k** downloads a month, is used by **638** open source projects on GitHub,
has **49** dependent packages on NPM, and has over **708** stars on GitHub.
While that might not be staggering to anyone, that means the world to me –
somebody else found value in something I made and put out into the world for
free!

## Why v4 Was Needed

Over the past 3.5 years, a number of issues were opened to ask for bug fixes,
features and general questions, and there have even been a few pull requests,
too! I am so grateful for all the effort put in by others to help me help them
solve their issues.

A point was eventually reached, however, where there were bugs that were
unfixable with the implementation of the component, and the codebase was not
something I wanted to work with any more.

I _knew_ it could be simpler!
I _knew_ it could be more accessible!

## Using `react-medium-image-zoom` v4

Here is what using [`react-medium-image-zoom`](https://www.npmjs.com/package/react-medium-image-zoom)
looks like now.

First, you import the default, [uncontrolled
component](https://reactjs.org/docs/uncontrolled-components.html) and the static
CSS file:

```js
import Zoom from 'react-medium-image-zoom'
import 'react-medium-image-zoom/dist/styles.css'
```

And then you go about your day adding zooming capabilities to your images:

```jsx
<Zoom>
  <img
    alt="that wanaka tree"
    src="/path/to/thatwanakatree.jpg"
    width="500"
  />
</Zoom>
```

Did I mention that you can now zoom _anything you like_?

```jsx
// <picture>
<Zoom>
  <picture>
    <source
      media="(max-width: 800px)"
      srcSet="/path/to/teAraiPoint.jpg"
    />
    <img
      alt="that wanaka tree"
      src="/path/to/thatwanakatree.jpg"
      width="500"
    />
  </picture>
</Zoom>

// <figure>
<figure>
  <Zoom>
    <img
      alt="that wanaka tree"
      src="/path/to/thatwanakatree.jpg"
      width="500"
    />
  </Zoom>
  <figcaption>That Wanaka Tree</figcaption>
</figure>

// <div> that looks like a circle
<Zoom>
  <div
    aria-label="A blue circle"
    style={{
      width: 300,
      height: 300,
      borderRadius: '50%',
      backgroundColor: '#0099ff'
    }}
  />
</Zoom>
```

If you find that you want to use the library as a [controlled
component](https://reactjs.org/docs/forms.html#controlled-components), you
import the `Controlled` component like this:

```js
import { Controlled as Zoom } from 'react-medium-image-zoom'
```

And then you tell dictate whether or not it should be zoomed and provide a
callback for the library to give you hints about when you should probably
zoom or unzoom based on events like clicks and scrolling:

```jsx
<Zoom
  isZoomed={true}
  onZoomChange={isZoomed => { console.log({ isZoomed }) }}
>
  <img
    alt="that wanaka tree"
    src="/path/to/thatwanakatree.jpg"
    width="500"
  />
</Zoom>
```

## What's Next For `react-medium-image-zoom`?

* Smarter detection for image `naturalWidth` and `naturalHeight` so we don't try
  to zoom anything when it's already at its maximum dimensions. This would also
  re-enable the ability to not zoom beyond a source image's natural dimensions
  once zoomed.
* Performance improvements (`requestAnimationFrame`, etc.)
* Explore re-enabling switching out lower-quality images with higher-quality
  ones as part of the zoom process

## Thank You

Thank you for reading this and for having an interest in [`react-medium-image-zoom`](https://www.npmjs.com/package/react-medium-image-zoom)!
If you'd like to contribute to the project, need help or have constructive
feedback, please open an issue on [the `react-medium-image-zoom` issue
tracker](https://github.com/rpearce/react-medium-image-zoom/issues).

Thank you for reading!
<br />
Robert
