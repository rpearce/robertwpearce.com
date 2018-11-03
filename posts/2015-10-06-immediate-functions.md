---
author:      Robert Pearce
title:       Immediate Functions
date:        2015-10-06
image:       /images/sunrise.jpg
description: Beginner-friendly introduction to immediate functions.
keywords:    javascript, javascript tutorial, immediate functions, IIFE, kelvin to fahrenheit,
---

_This article is intended for JavaScript beginners._

The browser environment is one big JavaScript `closure` that will encapsulate in its `scope` all of the code that is to be run. Because of this, any functions or variables that are created in `<script>` tags or external `.js` files that are _not_ defined within a function will end up as global variables! And we all know that global varibles are bad. Let's dig in to this some more.

Every time you define a function and then define a variable with `var` inside of that function, that variable _only_ exists inside of that function.
For example, what is the value of `result` that is logged to the console?

```js
// app.js
function kelvinToFahrenheit(kelvin) {
  var result = Math.round(kelvin * (9/5) - 459.67);
  return result;
}
kelvinToFahrenheit(274.3);
console.log(result);
```

The correct answer would be `undefined` (with a nice error), for `result` only exists within the `scope` of the `kelvinToFahrenheit` function. However, the function `kelvinToFahrenheit` now exists globally.

Why does this matter? Well, when you include a script on to a web page, its code now becomes part of this global closure. So if you define `function kelvinToFahrenheit()` without giving it a separate closure or namespace (more on namespaces in a second), then it is now a "global function," meaning that it exists in the global namespace. If any other library you ever include uses a variable called `router`, your variable (or that library's) is going to overwrite whichever came before it and cause massive issues. The same thing is true for variables:

```js
// app.js
var currentTempInKelvin = 294.11;
```

So what are your options?

## Immediately Invoked Function Expressions (IIFEs)

```js
// app.js
;(function() {
  // your code here
})();
```

The semi-colon here is a defensive technique used for when files are concatenated together--if somebody in one file forgets to close their file/library/definition out with a semi-colon, then your code is going to be an extension of theirs.

The `()` towards the end is nothing more than the invocation of the immediate function we've defined.

Thus, when you write

```js
// app.js
;(function() {
  var currentTempInKelvin = 294.11;
})();
```

and then you try to `console.log(currentTempInKelvin);` from the browser's JavaScript console, you will get `undefined`, for `currentTempInKelvin` now only exists within that anonymous function's scope. Hurray! No more globals.

* * *

But what if we want to access something in a global fashion? We know about the problems of name-clashing, so let's also try to reduce that. Let's combine what you did with the immediate function and do global variables in a less-bad way using _namespacing_.

## Namespacing

Namespacing allows us to limit our use of global variables to one global
by nesting all of our functionality within one global object that we'll
call `WeatherApp`.

```js
// app.js
// No var declaration means global!
;(function() {
  WeatherApp = {
    kelvinToFahrenheit: {}
  };
})();
```

or

```js
// app.js
;(function() {
  WeatherApp = {};
  WeatherApp.kelvinToFahrenheit = {};
})();
```

or (better)

```js
// app.js
;(function() {
  window.WeatherApp = {};
  window.WeatherApp.kelvinToFahrenheit = {};
})();
```

or (recommended)

```js
// app.js
;(function(scope) {
  scope.WeatherApp = {};
  scope.WeatherApp.router = {};
})(this);
```

This last method allows you to use this code and pass in any contextual scope. Since `this` is equivalent to `window` at the global level, when you run this in the browser, `this` is `window`, so `WeatherApp` will be added to the `window` global.

When you leave out the `var`, you create a global variable, so be careful! I recommend being explicit with to what object you are adding a `namespace`. If you're going the global variable route, then you should nest every single thing you're doing inside of your `WeatherApp` namespace in order to avoid having more than 1 global variable.

## Conclusion
This is a great pattern to utilize when you have relatively simple
JavaScript you would like to add to a webpage and not have its contents
clash with other libraries & code. If your code begins to get too
complicated for this file, then we can start to look at the [CommonJS
module exporting & requiring pattern](http://wiki.commonjs.org/wiki/Modules/1.1) that is currently implemented by the
wonderful [Browserify](http://browserify.org) library (aka, Node.js but in the browser). I will cover this in the future, but in the
mean time, leverage the power of immediate functions for great good!
