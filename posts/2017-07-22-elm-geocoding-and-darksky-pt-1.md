---
author:       Robert Pearce
title:        "Elm, Geocoding & DarkSky: Pt. 1 – Setup Elm & Proxy Servers"
date:         2017-07-22
image:        /images/snow-dogs.jpg
description:  In Part 1 we will begin building a small weather forecast app using Elm, Google's Geocoding API and the DarkSky API.
keywords:     elm, elm tutorial, elmlang, elm geocoding, elm darksky, elm weather, elm functional programming
photoCredit:  James Padolsey
photoWebsite: "https://unsplash.com/@padolsey"
---

This is part 1 of a multipart series where we will be building a small weather forecast app using [Elm](http://elm-lang.org/), [Google's Geocoding API](https://developers.google.com/maps/documentation/geocoding/start) and the [DarkSky API](https://darksky.net/dev/). Instead of doing everything in one massive post, I've broken the steps down into parts of a series. Here is the series plan:

* Pt. 1 – Setup Elm & Proxy Servers
* [Pt. 2 – Geocoding an Address](/blog/elm-geocoding-and-darksky-pt-2-geocoding-an-address.html)
* [Pt. 3 – Fetching the Current Weather](/blog/elm-geocoding-and-darksky-pt-3-fetching-the-current-weather.html)
* [Pt. 4 – Extracting Our Elm Code](/blog/elm-geocoding-and-darksky-pt-4-extracting-our-elm-code.html)

## Overview
This post will cover setting up [Elm](http://elm-lang.org/), a [geocoding proxy](http://localhost:3000/blog/node-js-geocoding-proxy-with-paperplane.html) and a [DarkSky proxy](https://github.com/rpearce/DarkSky-proxy/). We'll need all of these things set up in order to get our weather app to work and not sacrifice our API keys.

By the end of this post, you will have a "Hello, world!" Elm app with a simple `./build` command, and you should be able to `cURL` both your geocoding and DarkSky proxies to receive response data that we will use in the coming lessons.

## Project Source Code
The project we're making will be broken into parts here (branches will be named for each part): [https://github.com/rpearce/elm-geocoding-darksky/](https://github.com/rpearce/elm-geocoding-darksky/). Be sure to check out the other branches to see the other parts as they become available.

The code for this part is located in the `pt-1` branch: [https://github.com/rpearce/elm-geocoding-darksky/tree/pt-1](https://github.com/rpearce/elm-geocoding-darksky/tree/pt-1).

## Pre-requisites
This tutorial assumes that you already have installed Node.js (I use [NVM](https://github.com/creationix/nvm) for managing Node versions and am using `v8.3`).

Once you've got Node installed, we can begin.

## Project Setup
From your favorite project folder, let's create a new project folder named `elm-geocoding-darksky` and change the current working directory to be the new folder:

```bash
λ mkdir elm-geocoding-darksky
λ cd elm-geocoding-darksky
```

## Elm

### Installing Elm
You can install elm via any of the methods on the [elm install page](https://guide.elm-lang.org/install.html) or by one of these methods:

* Homebrew: `brew install elm`
* Node.js: `npm i elm -g` for a global binary or `npm init -y && npm i elm` to create a `package.json` file and install `elm` to it; you'll have to run this latter method via `npx elm`, as it'll be looking for the binary in your `./node_modules/.bin/` directory)

### elm-format
I've found that having a tool re-format my Elm code to an agreed-upon format makes me more efficient and makes it easier for others to read my code. Check out these projects for more on how to do this:

* [https://github.com/avh4/elm-format](https://github.com/avh4/elm-format)
* [elm-vim](https://github.com/ElmCast/elm-vim)

### Adding Elm to a Webpage
Our goal here is to compile our elm project to an `elm.js` file and include that on a webpage (which we'll make in a minute).

First, let's create a `src/` directory to house our source code and a `Main.elm` file within it:

```bash
λ mkdir src
λ touch src/Main.elm
```

Next, we want to install Elm's HTML package so that so that we can access its HTML-related functions:

```bash
λ elm package install elm-lang/html
```

Within the `Main.elm` file, add the following:

```elm
module Main exposing (..)

import Html exposing (text)


main =
    text "Hello, world!"
```

Here, we import the `Html` package that we installed, specifically expose the `text` function from it and then use that function to tell Elm that we want some HTML-friendly text.

_Note: to learn more about the Elm language and syntax, check out the [Elm Tutorial](https://www.elm-tutorial.org/en/), the [EggHead.io Elm course](https://egghead.io/courses/start-using-elm-to-build-web-applications), subscribe to [DailyDrip's Elm Topic](https://www.dailydrip.com/topics/elm), [James Moore's Elm Courses](http://courses.knowthen.com) or check out [Elm on exercism.io](http://exercism.io/languages/elm/about)._

We can then compile this and output it to `elm.js`:

```bash
λ elm make src/Main.elm --output=elm.js
```

You should now have a (quite large) file, `elm.js`, in your project's root. We're almost done!

Finally, create a new file, `index.html`, and add the following to it:

```html
<!DOCTYPE html>
<html lang="en">
  <head>
    <title>Weather</title>
    <meta charset="utf-8">
    <meta name="viewport" content="width=device-width, initial-scale=1, maximum-scale=1, minimum-scale=1">
  </head>
  <body>
    <script type="text/javascript" src="elm.js"></script>
    <script>
      (function(global) {
        var node = document.createElement('div')
        document.body.appendChild(node)
        var app = Elm.Main.embed(node)
        global.app = app
      })(window)
    </script>
  </body>
</html>
```

If you run `λ open index.html`, you should be able to view the file in the browser and see `Hello, world!`. Congrats! You're primed and ready to start building.

### Creating a Build File
If you're lazy like me, you can create an executable file, `build`, that will perform our `elm make ...` command for us:

```bash
λ touch build
λ chmod +x build
λ cat <<EOF > ./build
#!/bin/bash

elm-make src/Main.elm --output ./elm.js
EOF
```

This executable can now handle whatever build options and processes we'll add for the future (such as JavaScript minification & uglifying):

```bash
λ ./build
Success! Compiled 1 module.
Successfully generated ./elm.js
```

## Proxies
In order to not expose our API keys for geocoding and weather forecasts, we'll be using a separate proxy server for each service. I wrote recently wrote a post entitled [Node.js Geocoding Proxy with Paperplane](/blog/node-js-geocoding-proxy-with-paperplane.html) where you can see a full explanation of what we're doing and how to do it. If you don't care about the how and why about setting up these little servers, then just continue on!

### Setting Up Your Geocoding Proxy
First, you'll need to get a Google Maps API Key from here: [https://developers.google.com/maps/documentation/geocoding/start#get-a-key](https://developers.google.com/maps/documentation/geocoding/start#get-a-key).

Once you've done that, go ahead and clone or download [the geocoding-proxy project on GitHub](https://github.com/rpearce/geocoding-proxy) and follow the directions to get set up. Given you've got Node installed, you've copied over the `.env` file and set your API key in there, then running `λ node index.js` should start the server. From another command-line tab, run this and see if you get a similar result:

```bash
λ curl localhost:5050/geocode/Auckland
{"results":[...]}
```

If so, congrats! If you get stuck, create an issue on the [geocoding-proxy issues page](https://github.com/rpearce/geocoding-proxy/issues), and I'll see if I can help.

### Setting Up Your DarkSky Proxy
(This is almost exactly like the geocoding proxy setup.)

First, you'll need to get a DarkSky API Key from here: [https://darksky.net/dev/](https://darksky.net/dev/).

Once you've done that, go ahead and clone or download [the DarkSky-proxy project on GitHub](https://github.com/rpearce/DarkSky-proxy) and follow the directions to get set up. Given you've got Node installed, you've copied over the `.env` file and set your API key in there, then running `λ node index.js` should start the server. From another command-line tab, run this and see if you get a similar result:

```bash
λ curl http://localhost:5051/forecast/37.8267,-122.4233
{"latitude":37.8267,"longitude":-122.4233,...}
```

If so, congrats! If you get stuck, create an issue on the [DarkSky-proxy issues page](https://github.com/rpearce/DarkSky-proxy/issues), and I'll see if I can help.

## Wrapping Up
Thank you for reading this far! Now that we've got our Elm app build process set up and your proxy servers ready to work, we can start constructing our application piece-by-piece in the next article in the series.

If you'd like to be notified of when articles are published, subscribe!

Until next time,
<br>
Robert
