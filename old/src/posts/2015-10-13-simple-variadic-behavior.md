---
author: "Robert Pearce"
desc: "Beginner-friendly introduction to variadic behavior."
keywords: "javascript, javascript tutorial, variadic behavior, function arguments, variable arguments, function parameters"
title: "Simple Variadic Behavior"
---

<p style="text-decoration:line-through;">
  Recently, I have started working on my own static site generator,
  <a href="https://github.com/rpearce/react-static">react-static</a>, to
  accomodate my markdown blog posting & static site needs. Another benefit is that
  I get to work on my Node.js and ES2015+ JavaScript skills. While I am
  reinventing the wheel on some levels, it is good practice.
</p>

__Update__: I made a library, [parse-md](https://www.npmjs.com/package/parse-md),
out of some of this behavior in order to address the need of parsing metadata
from markdown files.

My latest problem to solve was how, once I had a `.md` (Markdown) file's
contents, to go about parsing out the blog post's metadata (see below: the
key/value pairs between the two `---`s).

```markdown
---
title: This is a test
description: Once upon a time, there was a test...
---

# Title of my great post
Lorem ipsum dolor...

## Some heading
Bacon ipsum...
```

Once I `split` this file based on newlines, I needed a way of finding the
indices of the metadata boundary, `---`, so that I could `splice` the array in
to two pieces and be on my way. My first attempt at getting the indices looked
like this:

```javascript
function getMetadataIndices(lines) {
  var arr = [];
  lines.forEach((line, i) => {
    if (/^---/.test(line)) {
      arr.push(i);
    }
  });
  return arr;
}

getMetadataIndices(lines); // [0, 3]
```

This is a simple solution that any junior dev can do, and it accomplishes the
task... but it doesn't feel right. I am iterating over each item, testing each
line and mutating an array variable when a condition is true. While it doesn't
look like much, that is a good bit going on all at once. Instinct tells me that
each action could be its own simple method. I also don't want to use a temporary
variable that I mutate. However, this removes `forEach` from our options, as
`forEach` returns the original array. `map()` to the rescue! (or so we think).

```javascript
function getMetadataIndices(lines) {
  return lines.map(testForBoundary);
}

function testForBoundary(item, i) {
  if (/^---/.test(item)) {
    return i;
  }
}

getMetadataIndices(lines); // [0, undefined, undefined, 3, undefined, undefined, undefined, undefined, undefined, undefined]
```

Crap. Because I only return when the test is true, `map` doesn't know what to
return, so it returns `undefined` and moves on. It would be nice if we could
clean out these `undefined`s!

## Cleaning Up Our Array

How can we achieve the following desired functionality?

```javascript
function getMetadataIndices(lines) {
  return lines.map(testForBoundary).clean(undefined);
}

getMetadataIndices(lines); // [0, 3]
```

Let's make a function on the `prototype` of `Array` called `clean`:

```javascript
Array.prototype.clean = function(trash) {
};
```

Here, we access `Array`'s `prorotype` and add our own custom method, `clean` and
pass it one argument. Next, we need to `filter` out all of the `undefined`s in
our array.

```javascript
Array.prototype.clean = function(trash) {
  return this.filter(item => item !== trash);
};
```

But what if we need to clean more than one value out? What if we need to clean
`null`, `""` and `undefined`?

## Variadic Behavior

In JavaScript, _variadic behavior_ is a fancy term applied to functions that can
accept and handle any number of arguments, and these are typically accessed
within the function via the `arguments` object, which looks like an `Array` but
is _not_. For example, this code will give you an error about `indexOf` not
being defined on `arguments`.

```javascript
Array.prototype.clean = function(trash) {
  return this.filter(item => arguments.indexOf(item) === -1);
};
```

Drats! `arguments` is very similar to an array &mdash; how can we get this to work? `slice` to the rescue!

```javascript
Array.prototype.clean = function() {
  const args = [].slice.call(arguments);
  return this.filter(item => args.indexOf(item) === -1);
};
```

Without any additional arguments, `slice` makes a copy of an array and allows us
to provide a custom receiver of array-like functionality: `arguments`. What is
returned from the second line above is an array-ized _copy_ of `arguments`. Now
that `args` is an array of all the arguments that are passed to `clean`, we can
pass as many options as we would like to clean out our array!

Here is more example usage of such a method:

```javascript
// Usage
const arr = ["", undefined, 3, "yes", undefined, undefined, ""];
arr.clean(undefined); // ["", 3, "yes", ""];
arr.clean(undefined, ""); // [3, "yes"];
```

## All Together

In attempting to refactor some fairly simple, though multiple-responsibility
code, we end up creating a few reusable functions that will benefit us in the
future, and we make our code more maintainable, testable and readable in the
end. Here it is once we have finished:

```javascript
function getMetadataIndices(lines) {
  return lines.map(testForBoundary).clean(undefined);
}

function testForBoundary(item, i) {
  if (/^---/.test(item)) {
    return i;
  }
}

Array.prototype.clean = function() {
  const args = [].slice.call(arguments);
  return this.filter(item => args.indexOf(item) === -1);
};
```

But could this be done _even_ simpler?

## p.s. Use reduce next time

You may have been wondering why we didn't use `reduce` like this from the start:

```javascript
lines.reduce(function(mem, item, i) {
  if (/^---/.test(item)) {
    mem.push(i);
  }
  return mem;
});
```

or, cleaned up a bit,

```javascript
function getMetadataIndices(mem, item, i) {
  if (/^---/.test(item)) {
    mem.push(i);
  }
  return mem;
}

lines.reduce(getMetadataIndices, []);
```

Surprise! We totally could have, but since `reduce` was not our first thought
when refactoring, we managed to solve our problem in another way. There are 1000
ways to solve problems, and sometimes you don't think of the best one first, but
you can still make the best with what you have at the time and refactor later.
