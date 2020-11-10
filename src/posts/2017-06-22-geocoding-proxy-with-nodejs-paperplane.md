---
author: "Robert Pearce"
authorTwitter: "@RobertWPearce"
desc: "Tutorial on creating a location geocoding proxy server in Node.js with Paperplane"
image: "/images/paper-plane.jpg"
keywords: "node geocoding, nodejs geocoding, nodejs proxy, nodejs geocoding proxy, nodejs paperplane"
photoCredit: "Ares Nguyen"
photoWebsite: "https://www.flickr.com/photos/91559340@N05/8478694579"
title: "Node.js Geocoding Proxy with Paperplane"
updated: "2017-07-29T00:00:00Z"
---

_tl;dr => use a proxy server when private API keys are involved; [paperplane](https://github.com/articulate/paperplane) is a great functional server framework._

Converting addresses, cities and other locations to latitude and logitude and back again is something that is expected in the software application world today.
Whether someone is asking for directions, [plotting optimal beer delivery routes](http://www.brewpublik.com) or tagging a photo of their cronut in a local cafe,
managing location data is an important skillset for developers to have. Numerous services, typically in the form of [application programming interfaces](https://en.wikipedia.org/wiki/Application_programming_interface) (APIs), exist
to provide folks with ways of accessing this data. Today we'll be using the [Google Maps Geocoding API](https://developers.google.com/maps/documentation/geocoding/start) to
complete the task of acquiring the geo-data for any place name; however, we will be creating a [Node.js](https://nodejs.org) server as a proxy
(a go-between) for our request instead of embedding this request in a browser.

## Why A Proxy Server?
If you are granted an API key for a service that is private and mapped to you, it is a good idea to keep it that way.
If you commit this API key to source control or expose it via your frontend code, then someone could take your key and pretend to be you. In order to avoid
this, it is recommended that you keep such keys hidden, for example, as environment variables set on a server. Thus, we are going to create a small server
to act as a proxy between the client (a web browser, app or cURL) and the API in question: the Google Maps Geocoding API.

## Why Paperplane as a Node.js Server Framework?
It is possible to do everything you need with Node's `http` package, but I like the approach [paperplane](https://github.com/articulate/paperplane) takes with viewing the request and response aspects of
handling an HTTP request as a pure function where the request is the input and the response is what is returned from it:

```haskell
Request -> Response
```

whereas many Node frameworks' handlers accept a function with the request and response as two arguments and not utilizing a return value, yielding the signature:

```haskell
(IncomingMessage, ServerResponse) -> ()
```

The paperplane approach makes a good deal more sense to me. You can read more about the "why" on [paperplane's getting started guide](https://github.com/articulate/paperplane/blob/master/docs/getting-started.md).

## Project Source Code
The project we'll be making can be seen in its entirety here: [https://github.com/rpearce/geocoding-proxy/](https://github.com/rpearce/geocoding-proxy/).

## Pre-requisites
_Note: what we'll be making is by no means a production-level application, as that would be outside the scope of this post. However, there are some slightly
advanced tangential topics that I will be glossing over (sometimes providing links to) in order to not write a book. Send me an email if I can be clearer in certain areas._

This tutorial assumes that you already have installed Node.js (I use [NVM](https://github.com/creationix/nvm) for managing Node versions and am using `v8.1`) and optionally the [yarn package manager](https://yarnpkg.com/lang/en/docs/install/).

Once you've got Node and yarn installed, we can begin.

## Project Setup
From your favorite project folder, let's create a new project folder named `geocoding-proxy` and change the current working directory to be the new folder:

```bash
λ mkdir geocoding-proxy
λ cd geocoding-proxy
```

### Installing Dependencies
Once we're in the project folder, let's initialize a `package.json` file to make it easy to manage and hang on to our project's dependencies:

```bash
λ npm init -y
```

or if you have yarn installed:

```bash
λ yarn init -y
```

You should now have a `package.json` file with some JSON values in it.

Next, let's install the tools that we're going to use:

```bash
λ npm install --save axios dotenv paperplane ramda
```

or

```bash
λ yarn add axios dotenv paperplane ramda
```

### Get A Google Maps Geocoding API Key
You can get yourself an API key from [this page](https://developers.google.com/maps/documentation/geocoding/get-api-key). Once you've done this, you'll need to copy
the `.env.example` file at your project's root (`λ cp .env.example .env`) and replace the value of the `GEO_KEY` with your API key. Your `.env` file should look like

```bash
GEO_KEY=abcdefg-hijklmn-op
PORT=5050
```

## Hello, World! With Paperplane
Once your dependencies are installed, let's create a server to see if we can get things working. First, create `index.js` at your project's root and open it in your
favorite text editor.

```bash
λ touch index.js
```

Next, let's import the packages we'll be using and create a basic "Hello, World!" server:

```js
// Make our .env configuration file available
require('dotenv').config()


// Import libraries
const http = require('http')
const { compose } = require('ramda')
const { json, logger, methods, mount, parseJson, routes } = require('paperplane')


// Application-specific code
const endpoints = routes({
  '/': methods({
    GET: req => (
      Promise
        .resolve('hello world')
        .then(json)
    )
  })
})

const app = compose(endpoints, parseJson)


// Server options
const opts = { errLogger: logger, logger }
const port = process.env.PORT || 3000
const listening = err => err ? console.error(err) : console.info(`Listening on port: ${port}`)


// Start the server
http.createServer(mount(app, opts)).listen(port, listening)
```

_(Read up more on how paperplane works on its [getting started page](https://github.com/articulate/paperplane/blob/master/docs/getting-started.md) or
by taking a look at [the demo application](https://github.com/articulate/paperplane/blob/master/demo/index.js). Also check out [Ramda's compose function](http://ramdajs.com/docs/#compose)
to learn about effective function composition.)_

We can start the server in a terminal window by running

```bash
λ node index.js
Listening on port: 5050
```

From another terminal window, let's use cURL to see if this works:

```bash
λ curl localhost:5050
"hello world"
```

It works!

## Hello, Location
Now that we know our server works, let's see if we can get it to echo back a location/address parameter we send it at a route we'll create called `/geocode`. Let's remove our `'/'` endpoint
and "hello, world!" code and add some for geocoding:

```js
const endpoints = routes({
  '/geocode/:address': methods({
    GET: req => (
      Promise
        .resolve(req.params.address)
        .then(json)
    )
  })
})
```

The `req` object gives us a `params` object with the key `address`, since that was what we specified we'd like our parameter to be named by setting the
`/geocode/:address` key in the `routes` function argument.

With the new endpoint added, save the file, restart your server (stop it with `Ctrl + C`), and run cURL with a city name this time:

```bash
λ curl localhost:5050/geocode/Auckland
"Auckland"
```

## Sending to the Geocoding API
We're almost there! Instead of echoing back whatever address the server receives, let's instead make an HTTP GET request to the geocode API using the `axios`
package:

```js
const endpoints = routes({
  '/geocode/:address': methods({
    GET: req => (
      axios({
        method: 'GET',
        url: 'https://maps.googleapis.com/maps/api/geocode/json',
        params: {
          key: process.env.GEO_KEY,
          address: req.params.address
        }
      })
      .then(json)
    )
  })
})
```

In this code, we are using the JavaScript Promise-based axios tool to create a GET request to the geocode API. Take note of our `params` object here; since we're
using the `dotenv` package and configuring that above, we get access to the `GEO_KEY` value in our `.env` file, and we separately get to pass on the `address`
param, as well. When this request is sent, the `url` will look like:

```bash
https://maps.googleapis.com/maps/api/geocode/json?key=abcdefg&address=Auckland
```

After restarting your server, run `λ curl localhost:5050/geocode/Auckland` again.

```bash
λ curl localhost:5050/geocode/Auckland
{"message":"Converting circular structure to JSON","name":"TypeError"}
```

Uh oh! If we log the axios result, we'll see a big response object that we don't care too much about right now.
The only key we want right now from this big response is the `data` key, so we can use [Ramda's prop method](http://ramdajs.com/docs/#prop) to simply
access this object key and pass its return value down the chain:

```js
// add `prop` to the require statement
const { compose, prop } = require('ramda')

// ...

const endpoints = routes({
  '/geocode/:address': methods({
    GET: req => (
      axios({
        method: 'GET',
        url: 'https://maps.googleapis.com/maps/api/geocode/json',
        params: {
          key: process.env.GEO_KEY,
          address: req.params.address
        }
      })
      .then(prop('data'))
      .then(json)
    )
  })
})
```

If all the stars have aligned and you restart and rerun the command again, you should see

```bash
λ curl localhost:5050/geocode/Auckland
{"results":[{"address_components":[{"long_name":"Auckland","short_name":"Auckland","types":["locality","political"]},{"long_name":"Auckland","short_name":"Auckland","types":["administrative_area_level_1","political"]},{"long_name":"New Zealand","short_name":"NZ","types":["country","political"]}],"formatted_address":"Auckland, New Zealand","geometry":{"bounds":{"northeast":{"lat":-36.660571,"lng":175.2871371},"southwest":{"lat":-37.0654751,"lng":174.4438016}},"location":{"lat":-36.8484597,"lng":174.7633315},"location_type":"APPROXIMATE","viewport":{"northeast":{"lat":-36.660571,"lng":175.2871371},"southwest":{"lat":-37.0654751,"lng":174.4438016}}},"place_id":"ChIJ--acWvtHDW0RF5miQ2HvAAU","types":["locality","political"]}],"status":"OK"}
```

Hooray! We now have geocode response data for Auckland like:

* `"status":"OK"`
* `"formatted_address":"Auckland, New Zealand"`
* `"location":{"lat":-36.8484597,"lng":174.7633315}`

## Refactoring the Routes
As you might imagine, having all of the request handling functions inside of paperplane's `routes` function might get difficult to follow and modularize. With that in mind, let's first pull the
handler function out and into its own function:

```js
const geocode = req =>
  axios({
    method: 'GET',
    url: 'https://maps.googleapis.com/maps/api/geocode/json',
    params: {
      key: process.env.GEO_KEY,
      address: req.params.address
    }
  })
  .then(prop('data'))
  .then(json)

const endpoints = routes({
  '/geocode/:address': methods({
    GET: geocode
  })
})
```

You could now abstract the `geocode` function to another file if you wanted to, as well as the object that is passed to routes (think of a routes file that requires in the different
handlers it needs).

### Leveraging Ramda
We can refactor the code above even further and make it a bit more functional and closer to being ["point-free"](https://lucasmreis.github.io/blog/pointfree-javascript) by including a
few Ramda helpers:

```js
const { compose, composeP, curryN, path, prop } = require('ramda')

// ...

// Application-specific code
const getGeocode = curryN(2, (key, address) =>
  axios({
    method: 'GET',
    url: 'https://maps.googleapis.com/maps/api/geocode/json',
    params: { key, address }
  })
  .then(prop('data'))
})

const geocode = compose(
  composeP(
    json,
    getGeocode(process.env.GEO_KEY),
  ),
  path(['params', 'address'])
)

const endpoints = routes({
  '/geocode/:address': methods({
    GET: geocode
  })
})

const app = compose(endpoints, parseJson)
```

This code accomplishes the same goal as before, but now we have accomplished a few things:

1. We no longer access `req.params.address` – what happens if any of those returned `null` or `undefined`? Instead, we use Ramda's [path helper](http://ramdajs.com/docs/#path).
1. Ramda's [compose](http://ramdajs.com/docs/#compose) rears its head again, allowing us to make a chain of functions. However, note the use of [composeP](http://ramdajs.com/docs/#composeP). The `getGeocode` function returns a `Promise` thanks to `axios`, so we need to use `composeP` to compose our Promise-returning function.
1. We can use [currying](http://ramdajs.com/docs/#curryN) to accept both `key` and `address` parameters at separate times. This is handy, for we could partially apply our `key` once, store that in a variable and reuse it over and over with different `address`es.
1. We have decoupled the use of paperplane's `json` helper from `getGeocode` and `axios`, meaning that function can now be leveraged in other ways instead of being hard-set to JSON.

If this scares the hell out of you, fear not! Check out [Andrew van Slaar's Ramda lessons on egghead.io](https://egghead.io/instructors/andrew-van-slaars) and if you're liking what you're
seeing, [Dr. Boolean's "Mostly Adequate Guide to Functional Programming"](https://github.com/MostlyAdequate/mostly-adequate-guide).


## All of the Code
The project itself can be found at [https://github.com/rpearce/geocoding-proxy](https://github.com/rpearce/geocoding-proxy), but here is our `index.js` file in its entirety:

```js
// Make our .env configuration file available
require('dotenv').config()


// Import libraries
const http = require('http')
const axios = require('axios')
const { compose, composeP, curryN, path, prop } = require('ramda')
const { json, logger, methods, mount, parseJson, routes } = require('paperplane')


// Application-specific code
const getGeocode = curryN(2, (key, address) =>
  axios({
    method: 'GET',
    url: 'https://maps.googleapis.com/maps/api/geocode/json',
    params: { key, address }
  })
  .then(prop('data'))
)

const geocode = compose(
  composeP(
    json,
    getGeocode(process.env.GEO_KEY),
  ),
  path(['params', 'address'])
)

const endpoints = routes({
  '/geocode/:address': methods({
    GET: geocode
  })
})

const app = compose(endpoints, parseJson)


// Server options
const opts = { errLogger: logger, logger }
const port = process.env.PORT || 3000
const listening = err => err ? console.error(err) : console.info(`Listening on port: ${port}`)


// Start the server
http.createServer(mount(app, opts)).listen(port, listening)
```

## Conclusion
Tools like Node.js with paperplane make it very easy to create proxy servers to handle your requests in a safe fashion, so use them and always keep your API keys secret!

## Update: 2017-07-30
I've seen a some feedback asking about CORS (cross-origin resource sharing), so here's how you can do it (useful for running things on localhost):

```js
const { cors, ... } = require('paperplane')

// ...

// Server options
const corsOpts = { methods: 'GET' }
const corsApp = cors(app, corsOpts)
// ...

// Start the server
http.createServer(mount(corsApp, opts)).listen(port, listening)
```

Read more about paperplane's CORS API in [paperplane's CORS docs](https://github.com/articulate/paperplane/blob/master/docs/API.md#cors).
