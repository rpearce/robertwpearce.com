---
author:        "Robert Pearce"
authorTwitter: "@RobertWPearce"
title:       Asynchronously Loading Scripts
date:        2015-10-05
image:       /images/rock-creek-pk.jpg
description: Beginner-friendly introduction to loading scripts on to a web page.
keywords:    javascript, javascript tutorial, asynchronous scripts, website basics
---

_This article is intended for HTML & JavaScript beginners._

* * *

Where do you place your `<script>` tags to load your JavaScript for your
website? If you're doing this within the `<head>` element, you might
want to consider whether or not this is the best option for you.

## Fetching JavaScript Synchronously

So long as HTTP/1.1 is what your website is accessed via (which it will be
a long while), `<script>` tags will be used to fetch external JavaScript
files whose contents will be included on the page. These typically look
like `<script src="app.js"></script>`. `<script>` tags are by default "blocking," meaning that the web page has to pause its download & render cycle, fetch and load the JavaScript and then continue on. Here is what this looks like:

```html
<html>
  <head>
    <script src="app.js"></script>
  </head>
  <body>
    <div>My Website</div>
  </body>
</html>
```

The worst thing you can do is load multiple scripts in this blocking
fashion:

```html
<html>
  <head>
    <script src="jquery.js"></script>
    <script src="jquery.lightbox.js"></script>
    <script src="some_file.js"></script>
    <script src="app.js"></script>
  </head>
  <body>
    <div>My Website</div>
  </body>
</html>
```

so make sure you combine (concatenate) all your JavaScript files in to
one file. But this is still not ideal, for you have a blocking script
that will have to download before anything else happens.

When we throw `<script>` tags at the end of the `<body>`, we allow for the page to paint and then go and fetch the JS synchronously (this lets the user see and utilize the page, but the scripts still haven't finished loading).

```html
<html>
  <head></head>
  <body>
    <div>My Website</div>
    <script src="app.js"></script>
  </body>
</html>
```

However, since the page is still loading, search engines might punish you for a
long(er) loading time. What we might need, instead, is the ability to
asynchronously fetch the JavaScript _after_ the page is finished
loading.

## Fetching JavaScript Asynchronously
There are two popular methods for fetch JavaScript in an asynchronous
manner.

The first is to simply include the HTML5 `async` property:

```html
<script src="app.js" async></script>
```

Or, if you need to support older browsers, add an event listener to the window's `load` function to dynamically build a script tag and append it to the page (note how I do not use `window.onload =`):

```html
<script>
  window.addEventListener('load', buildScriptTag);
  function buildScriptTag() {
    var script = document.createElement('script');
    script.src = 'app.js';
    document.body.appendChild(script); // append it wherever you want
  }
</script>
```

Why didn't I use `window.onload =` here? When you assign a browser callback trigger a value, it can only have one value! When you add an event listener, you allow the window's load functionality to have more values in the future.

## Conclusion
If your app/website is architected to rely on JavaScript before it
renders anything, then you can utilize this asynchronous
technique with a "Loading..." graphic that is removed when the
JavaScript loads. Ultimately, you want to decrease the amount of time it
takes for a web page to perform an initial load so that the user can get
started using your project as quickly as possible. With asynchronous
loading of JavaScript, you enable your users to get going ASAP and then
allow for them to have a fancier experience once things load in the
background.
