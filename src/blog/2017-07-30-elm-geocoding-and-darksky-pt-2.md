---
title:        "Elm, Geocoding & DarkSky: Pt. 2 – Geocoding an Address"
date:         2017-07-30
image:        /images/rain-window.jpg
description:  In Part 2 we will use Elm & the Google Maps API to geocode an address.
photoCredit:  Gabriele Diwald
photoWebsite: "https://unsplash.com/@gabrielediwald"
---

This is part 2 of a multipart series where we will be building a small weather forecast app using [Elm](http://elm-lang.org/), [Google's Geocoding API](https://developers.google.com/maps/documentation/geocoding/start) and the [DarkSky API](https://darksky.net/dev/). Instead of doing everything in one massive post, I've broken the steps down into parts of a series. Here is the series plan:

* [Pt. 1 – Setup Elm & Proxy Servers](/blog/elm-geocoding-and-darksky-pt-1-setup-elm-and-proxy-servers.html)
* Pt. 2 – Geocoding an Address
* [Pt. 3 – Fetching the Current Weather](/blog/elm-geocoding-and-darksky-pt-3-fetching-the-current-weather.html)
* [Pt. 4 – Extracting Our Elm Code](/blog/elm-geocoding-and-darksky-pt-4-extracting-our-elm-code.html)

If you'd like to code along with this tutorial, check out [part 1](/blog/elm-geocoding-and-darksky-pt-1-setup-elm-and-proxy-servers.html) first to get set up.

_Note: to learn more about the Elm language and syntax, check out the [Elm Tutorial](https://www.elm-tutorial.org/en/), the [EggHead.io Elm course](https://egghead.io/courses/start-using-elm-to-build-web-applications), subscribe to [DailyDrip's Elm Topic](https://www.dailydrip.com/topics/elm), [James Moore's Elm Courses](http://courses.knowthen.com) or check out [Elm on exercism.io](http://exercism.io/languages/elm/about)._

## Overview
Before we can send a weather forecast request to DarkSky, we need to geocode an address to get its latitude and longitutde. In this post, we're going to use Elm and our geocoding server from [Part 1](/blog/elm-geocoding-and-darksky-pt-1-setup-elm-and-proxy-servers.html) to geocode an address based on a user's input in a text box.

Warning: this is a hefty post.

## Project Source Code
The project we're making will be broken into parts here (branches will be named for each part): [https://github.com/rpearce/elm-geocoding-darksky/](https://github.com/rpearce/elm-geocoding-darksky/). Be sure to check out the other branches to see the other parts as they become available.

The code for this part is located in the `pt-2` branch: [https://github.com/rpearce/elm-geocoding-darksky/tree/pt-2](https://github.com/rpearce/elm-geocoding-darksky/tree/pt-2).

## Steps for Today
What we want to do with our program today is create an HTTP GET request with an address that is input by a user and returns the latitude and longitude. These steps will get us there:

1. Defining our primary data model
1. Understanding Google's geocode response data
1. Modeling the geocode response data
1. Creating JSON decoders
1. Building our view and listening for events
1. Adding message types
1. Writing our update function
1. Making our request
1. Handling the geocode response
1. Final wiring up with the main function & defaults

## 1: Defining our primary data model
At the top level for our app, we only care about an address and latitude and longitude coordinates. While the address' type will definitely be [String](http://package.elm-lang.org/packages/elm-lang/core/latest/String), we can choose between a [record](https://guide.elm-lang.org/core_language.html#records) or [tuple](https://guide.elm-lang.org/core_language.html#tuples) to house our coordinates; however, each of these values must be a `Float` type, as coordinates come in decimal format. For no particular reason, we're going to use a tuple.

```elm
type alias Model =
    { address : String
    , coords : Coords
    }


type alias Coords =
    ( Float, Float )
```

I like to keep my models/type aliases fairly clean and primed for re-use in type definitions, so I created a separate type alias, `Coords`, to represent `( Float, Float )`.

## 2: Understanding Google's geocode response data
Let's take a look at what a geocoding request's response data for `Auckland` looks like so we can understand what we're working with.

```js
{
  "results": [
    {
      "geometry": {
        "location": {
          "lat": -36.8484597,
          "lng": 174.7633315
        },
        // ...
      },
      // ...
    }
  ],
  "status": "OK"
}
```

If you've set up your [geocoding proxy](/blog/node-js-geocoding-proxy-with-paperplane.html), you can see these same results by running this command:

```bash
λ curl localhost:5050/geocode/Auckland
```

We can see here that we get back a `status` string and a `results` list where one of the results contains a `geometry` object, and inside of that, we find `location` and finally, our quarry: `lat` and `lng`. If we were searching for this with JavaScript, we might find this data like so:

```js
response.results.find(x => x['geometry']).geometry.location
// { lat: -36.8484597, lng: 174.7633315 }
```

What would happen in vanilla JavaScript if there were no results, or those object keys didn't exist? Elm steps up to help us solve for the unexpected.

## 3: Modeling the geocode response data
Based on the geocoding response, let's list out what we're looking at:

* a string, `status`
* a list of `results`
* each result has a `geometry` object
* a `geometry` object has a `location` object
* a `location` object has both `lat` and `lng` properties, each of which use decimal points

Since we're going to need decode these bits of data and reuse the types a few times, let's create type aliases for each of these concepts (prefixed with `Geo`):

```elm
type alias GeoModel =
    { status : String
    , results : List GeoResult
    }


type alias GeoResult =
    { geometry : GeoGeometry }


type alias GeoGeometry =
    { location : GeoLocation }


type alias GeoLocation =
    { lat : Float
    , lng : Float
    }
```

If you're not sure what `type alias` means, read more about [type aliases in _An Introduction to Elm_](https://guide.elm-lang.org/types/type_aliases.html).

## 4: Creating JSON decoders
There are a number of ways to decode JSON in Elm, and [Brian Hicks](https://www.brianthicks.com) has [written about this](https://www.brianthicks.com/post/2016/08/22/decoding-large-json-objects-a-summary/) (and has a [short book on decoding JSON](https://www.brianthicks.com/json-survival-kit/)), and so have many others, such as [Thoughtbot](https://robots.thoughtbot.com/decoding-json-structures-with-elm). Today, we're going to be working with [NoRedInk's elm-decode-pipeline](https://github.com/NoRedInk/elm-decode-pipeline).

First, we install the package into our project:

```bash
λ elm package install NoRedInk/elm-decode-pipeline
```

In our `Main.elm` file, we can import what we'll need from Elm's [core Json-Decode module]() as well as the package we've just installed.

```elm
-- Importing from elm core.
-- We know from our type aliases that all we're working
-- with right now are floats, lists and strings.
import Json.Decode exposing (float, list, string, Decoder)

-- importing from elm-decode-pipeline
import Json.Decode.Pipeline exposing (decode, required)
```

Now we can write our decoders!

```elm
decodeGeo : Decoder GeoModel
decodeGeo =
    decode GeoModel
        |> required "status" string
        |> required "results" (list decodeGeoResult)


decodeGeoResult : Decoder GeoResult
decodeGeoResult =
    decode GeoResult
        |> required "geometry" decodeGeoGeometry


decodeGeoGeometry : Decoder GeoGeometry
decodeGeoGeometry =
    decode GeoGeometry
        |> required "location" decodeGeoLocation


decodeGeoLocation : Decoder GeoLocation
decodeGeoLocation =
    decode GeoLocation
        |> required "lat" float
        |> required "lng" float
```

Here we declare that we'd like to decode the JSON string according to our type aliases, such as `GeoModel`, and we expect certain keys to have certain value types. In the case of `status`, that's just a string; however, with `results`, we actually have a list of some other type of data, `GeoResult`, and so we create another decoder function down the line until we dig deep enough to find what we're looking for. In short, we're opting for functions and type-checking over deep nesting.

Why does this feel so verbose? Personally, I'm not yet comfortable using [Json.Decode.at](http://package.elm-lang.org/packages/elm-lang/core/5.1.1/Json-Decode#at), which might look like

```elm
decodeString (at [ "results" ] (list (at [ "geometry", "location" ] (keyValuePairs float)))) jsonString
```

But with the former approach, we get to be _very_ specific with exactly what we are expecting our data to be shaped like while maintaining clarity.

## 5: Building our view and listening for events
It's time to add our `view` function. All we're going for today is

* a text input that will keep track of the `address` by responding to the `onInput` event
* a form around the input that listens for the `onSubmit` event
* a paragraph to display the coordinates; for example, `Coords: (123, 456)`

As usual, let's download [the official elm-lang/html package](https://github.com/elm-lang/html):

```bash
λ elm package install elm-lang/html
```

Then let's import what we need from it:

```elm
import Html exposing (Html, div, form, input, p, text)
import Html.Attributes exposing (placeholder, type_, value)
import Html.Events exposing (onInput, onSubmit)
```

Each import is a function that we can use to help generate HTML5 elements which Elm then works with behind the scenes.

```elm
view : Model -> Html Msg
view model =
    div []
        [ form [ onSubmit SendAddress ]
            [ input
                [ type_ "text"
                , placeholder "City"
                , value model.address
                , onInput UpdateAddress
                ]
                []
            ]
        , p [] [ text ("Coords: " ++ (toString model.coords)) ]
        ]
```

Our `view` function takes in our model and uses Elm functions to then render output. Great! But what are `SendAdress` and `UpdateAddress`? If you're coming from JavaScript, you might think these are callbacks or higher-order functions, but they are not. They are custom message types (that we'll define momentarily) that will be used in our `update` function to determine what flow our application should take next.

## 6: Adding message types
Thus far, we know of two message types, `Update` and `SendAddress`, but how do we define them? If you look at our `view` function again, you'll see the return type `Html Msg`. The second part of this will be the `type` that we create, and our custom message types will be a part of that! This is something called a [union type](https://guide.elm-lang.org/types/union_types.html).

```elm
type Msg
    = UpdateAddress String
    | SendAddress
    | NoOp
```

We will be adding more to this shortly, but this is all we have come across thus far.

## 7: Writing our update function
Staying consistent with [The Elm Architecture](https://guide.elm-lang.org/architecture/), we'll define our `update` function in order to update our data and fire off any commands that need happen. If you're familiar with Redux, this is where the idea for a "reducer" came from.

This is tough to do in a blog post, so please be patient, and we'll walk through this:

```elm
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateAddress text ->
            ( { model | address = text }
            , Cmd.none
            )

        SendAddress ->
            ( model, sendAddress model.address )

        -- more code here shortly...

        _ ->
            ( model, Cmd.none )
```

Let's walk through this step-by-step:

* if the message type is `UpdateAddress`, then
  1. we're expecting a `string` (defined in our union type)
  1. we'll call the argument `text`
  1. we'll then return a tuple of our updated model and a `Cmd` to essentially do nothing else (it'll pass through the union type and settle on the `NoOp`)
* if the message type is `SendAddress`, then
  1. we'll accept _no_ parameters
  1. we'll return a tuple of our model _with no changes_ and a command that we haven't defined yet. This is where we call the function that will actually go and get our geocode data!

## 8: Making our request
In order to build and send HTTP requests, we'll need to make sure we download the [elm-lang/http](https://github.com/elm-lang/http) package:

```bash
λ elm package install elm-lang/http
```

and import it:

```elm
import Http
```

In our `update` function, we referenced a function named `sendAddress` and passed it our model's address as a parameter. This function should accept a string, initiate our HTTP request and return a command with a message.

```elm
sendAddress : String -> Cmd Msg
sendAddress address =
    Http.get (geocodingUrl address) decodeGeo
        |> Http.send ReceiveGeocoding

geocodingUrl : String -> String
geocodingUrl address =
    "http://localhost:5050/geocode/" ++ address
```

Our `sendAddress` function does this:

1. it builds a GET request using two arguments: a URL (derived from `geocodingUrl`) and our `decodeGeo` decoder function
1. it then pipes the return value from `Http.get` to be the second argument for `Http.send`

Note that `Http.send`'s first argument is a `Msg` that we haven't defined yet, so let's add that to our `Msg` union type:

```elm
type Msg
    = UpdateAddress String
    | SendAddress
    | ReceiveGeocoding (Result Http.Error GeoModel)
    | NoOp
```

Basically, we'll either get back an HTTP error or a data structure in the shape of our `GeoModel`.

## 9: Handling the geocode response
Finally, we now need to handle the successful and erroneous responses in our update function:

```elm
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UpdateAddress text ->
            ( { model | address = text }
            , Cmd.none
            )

        SendAddress ->
            ( model, sendAddress model.address )

        ReceiveGeocoding (Ok { results, status }) ->
            let
                result =
                    case status of
                        "OK" ->
                            results
                                |> List.head
                                |> Maybe.withDefault initialGeoResult

                        _ ->
                            initialGeoResult

                location =
                    result.geometry.location

                newModel =
                    { model | coords = ( location.lat, location.lng ) }
            in
                ( newModel, Cmd.none )

        ReceiveGeocoding (Err _) ->
            ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )


-- This should go with other `init`s
-- but is placed here for relevance
initialGeoResult : GeoResult
initialGeoResult =
    { geometry =
        { location =
            { lat = 0
            , lng = 0
            }
        }
    }
```

Instead of having success/error logic inside one `ReceiveGeocoding` case match, we use Elm's pattern matching to allow us to match on the message and `Ok` or `Err` [results](http://package.elm-lang.org/packages/elm-lang/core/latest/Result).

Again, let's do this step-by-step:

* `ReceiveGeocoding` is `OK`
  1. we destructure the response into `results` and `status` variables
  1. we check the value of `status` from the response to make sure all is well
  1. if status is `"OK"`, we try to get the first item in the `results` list and fallback to `initialGeoResult` if there are no results (I love Elm for enforcing this)
  1. if status is _not_ `"OK"`, we fall back to the `initialGeoResult`
  1. we then access the `location` record, build an updated model record, and return it
* `ReceiveGeocoding` is `Err`
  1. we simply return the model

## 10: Final wiring up with the main function & defaults
Now that we're through the core of the application's contents, we can wire up the remaining bits and get it to compile:

```elm
-- Define our HTML program
main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


-- Here is our initial model
init : ( Model, Cmd Msg )
init =
    ( initialModel, Cmd.none )


initialModel : Model
initialModel =
    { address = ""
    , coords = ( 0, 0 )
    }


-- We're not using any subscriptions,
-- so we'll define none
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
```

Remember that you can look at the [source code for this part](https://github.com/rpearce/elm-geocoding-darksky/tree/pt-2) as a guide.

## Wrapping Up
This has been a massive post on simply fetching geocode data from an API. I've found it's difficult to write posts on Elm in little bits, for you have to have everything in the right place and defined before it'll work. Subsequent posts in this series will be shorter, as we'll have already done the heavy-lifting.

Until next time,
<br>
Robert
