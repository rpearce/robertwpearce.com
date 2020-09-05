---
author: "Robert Pearce"
authorTwitter: "@RobertWPearce"
description: "In Part 3 we query the DarkSky API with our geocoded address"
image: "/images/london-rain.jpg"
keywords: "elm, elm tutorial, elmlang, elm geocoding, elm darksky, elm weather, elm functional programming"
photoCredit: "Anjana Menon"
photoWebsite: "https://unsplash.com/@anjimenon"
title: "Elm, Geocoding & DarkSky: Pt. 3 – Fetching the Current Weather"
---

This is part 3 of a multipart series where we will be building a small weather forecast app using [Elm](http://elm-lang.org/), [Google's Geocoding API](https://developers.google.com/maps/documentation/geocoding/start) and the [DarkSky API](https://darksky.net/dev/). Instead of doing everything in one massive post, I've broken the steps down into parts of a series. Here is the series plan:

* [Pt. 1 – Setup Elm & Proxy Servers](/elm-geocoding-and-darksky-pt-1-setup-elm-and-proxy-servers.html)
* [Pt. 2 – Geocoding an Address](/elm-geocoding-and-darksky-pt-2-geocoding-an-address.html)
* Pt. 3 – Fetching the Current Weather
* [Pt. 4 – Extracting Our Elm Code](/elm-geocoding-and-darksky-pt-4-extracting-our-elm-code.html)

If you'd like to code along with this tutorial, check out [part 1](/elm-geocoding-and-darksky-pt-1-setup-elm-and-proxy-servers.html) and [part 2](/elm-geocoding-and-darksky-pt-2-fetching-the-current-weather.html) first to get set up.

_Note: to learn more about the Elm language and syntax, check out the [Elm Tutorial](https://www.elm-tutorial.org/en/), the [EggHead.io Elm course](https://egghead.io/courses/start-using-elm-to-build-web-applications), subscribe to [DailyDrip's Elm Topic](https://www.dailydrip.com/topics/elm), [James Moore's Elm Courses](http://courses.knowthen.com) or check out [Elm on exercism.io](http://exercism.io/languages/elm/about)._

## Overview
In this post we will use Elm to fetch and display the current weather based on the geocode data we receive from an input field.

## Project Source Code
The project we're making will be broken into parts here (branches will be named for each part): [https://github.com/rpearce/elm-geocoding-darksky/](https://github.com/rpearce/elm-geocoding-darksky/). Be sure to check out the other branches to see the other parts as they become available.

The code for this part is located in the `pt-3` branch: [https://github.com/rpearce/elm-geocoding-darksky/tree/pt-3](https://github.com/rpearce/elm-geocoding-darksky/tree/pt-3).

## Steps for Today
1. Understanding DarkSky's response data
1. Modeling the DarkSky response data
1. Creating DarkSky JSON decoders
1. Writing our fetchWeather HTTP function
1. Calling fetchWeather and handling the response
1. Displaying the current weather in our view

## 1. Understanding DarkSky's response data
Let's get the weather data for Auckland, NZ (-36.8484597,174.7633315). If we start up our [DarkSky proxy](https://github.com/rpearce/DarkSky-proxy) and run

```bash
λ curl localhost:5051/forecast/-36.8484597,174.7633315
```

then we will see response data like this:

```json
{
  "timezone": "Pacific\/Auckland",
  "currently": {
    "summary": "Overcast",
    "icon": "cloudy",
    "temperature": 61.42,
    ...
  },
  "hourly": { ... },
  "daily": { ... }
}
```

While all we care about are the `summary`, `icon` and `temperature` properties within the top-level `currently` property, we will only use `temperature` in this part.

Disclaimer: DarkSky units are in `us` by default. You can specify other unit types by appending a `units` query parameter to the end like this:

```bash
λ curl localhost:5051/forecast/-36.8484597,174.7633315?units=si
```

Read more about [DarkSky request parameters in the DarkSky docs](https://darksky.net/dev/docs/forecast) to customize your response data.

Now that we've got our data in the correct units, let's model this data in Elm!

## 2. Modeling the DarkSky response data
Based on our DarkSky response, let's list out what we're looking at:

* an object, `currently`, which has 3 notable properties:
  * a string, `summary`
  * a string, `icon`
  * a float, `temperature`

Since we have two levels of data, `currently` and its child properties, let's create two type aliases to represent this data.

```elm
type alias Weather =
    { currently : WeatherCurrently
    }


type alias WeatherCurrently =
    { icon : String
    , summary : String
    , temperature : Float
    }
```

And now we can add a property to our `Model` type alias that can be of our `Weather` type:

```elm
type alias Model =
    { address : String
    , coords : Coords
    , weather : Weather
    }
```

Uh oh! Our `Model` has a defaults function called `initialModel`, and now that we've added `weather` into the mix, we'll need to give that default values, as well:

```elm
initialModel : Model
initialModel =
    { address = ""
    , coords = ( 0, 0 )
    , weather = initialWeather
    }


initialWeather : Weather
initialWeather =
    { currently = initialWeatherCurrently
    }


initialWeatherCurrently : WeatherCurrently
initialWeatherCurrently =
    { icon = "–"
    , summary = "–"
    , temperature = 0
    }
```

These are defaults that we provide in the event that we have no data to work with (initially or if something goes wrong).

## 3. Creating DarkSky JSON decoders
Just as we did in the [geocoding post section on JSON decoding](/elm-geocoding-and-darksky-pt-2-geocoding-an-address.html#4-creating-json-decoders), we want to leverage [NoRedInk's elm-decode-pipeline](https://github.com/NoRedInk/elm-decode-pipeline) to define how our JSON response should be structured and thus parsed.

```elm
decodeWeather : Decoder Weather
decodeWeather =
    decode Weather
        |> required "currently" decodeWeatherCurrently


decodeWeatherCurrently : Decoder WeatherCurrently
decodeWeatherCurrently =
    decode WeatherCurrently
        |> required "icon" string
        |> required "summary" string
        |> required "temperature" float
```

While we could use [Json.Decode.at](http://package.elm-lang.org/packages/elm-lang/core/5.1.1/Json-Decode#at) to potentially have less code, there is absolutely nothing wrong with being verbose if it leads to clarity.

## 4. Writing our fetchWeather HTTP function
We know that we're going to have to send latitude and longitude `Coords` to our [DarkSky proxy server](https://github.com/rpearce/DarkSky-proxy), as well as any additional options, so let's define the URL for that and the fetching function [just like we did for geocoding](/elm-geocoding-and-darksky-pt-2-geocoding-an-address.html#8-making-our-request).

```elm
weatherUrl : Coords -> String
weatherUrl ( lat, lng ) =
    "http://localhost:5051/forecast/"
        ++ (toString lat)
        ++ ","
        ++ (toString lng)
        -- this is where you can add your query params


fetchWeather : Coords -> Cmd Msg
fetchWeather coords =
    Http.get (weatherUrl coords) decodeWeather
        |> Http.send ReceiveWeather
```

To define an HTTP request in Elm, we need
* ✅ a URL to point to
* ✅ a package like Http to help us build the request
* ✅ a decoder to handle parsing the response data
* 🤷 a `Msg` type that our `update` function can pattern match on

Right! We can't forget to add `ReceiveWeather` as a `Msg` type. It should be almost the same as `ReceiveGeocoding`:

```elm
type Msg
    = UpdateAddress String
    | SendAddress
    | ReceiveGeocoding (Result Http.Error GeoModel)
    | ReceiveWeather (Result Http.Error Weather)
    | NoOp
```

## 5. Calling fetchWeather and handling the response
When we [handled our geocode response in the prior post](/elm-geocoding-and-darksky-pt-2-geocoding-an-address.html#9-handling-the-geocode-response), inside of `ReceiveGeocoding` we returned `( newModel, Cmd.none )`, for we had no further actions to take. Instead of our action in this tuple being `Cmd.none`, let's instead call our `fetchWeather` function and pass it our geocoded coordinates:

```elm
update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        -- ...removed for brevity

        ReceiveGeocoding (Ok { results, status }) ->
            let
                -- ...
                newModel =
                    -- ...
            in
                ( newModel, fetchWeather newModel.coords )

        -- ...

        ReceiveWeather (Ok resp) ->
            ( { model | weather = { currently = resp.currently } }
            , Cmd.none
            )

        ReceiveWeather (Err _) ->
            ( model, Cmd.none )
```

Again, at the end of `ReceiveGeocoding`, we return our `newModel` as well as the command to go and fetch the weather with the coordinates we're storing on our `newModel`.

Whenever the HTTP request and decoding gives us back a result with the `Msg` type of `ReceiveWeather`, we then update the weather property on our model record to have the `currently` data parsed from the decoder.

## 6. Displaying the current weather in our view
Finally, to make sure we're doing each step correctly, let's add the temperature to our view:

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
        , p [] [ text ("Weather: " ++ (toString (round model.weather.currently.temperature))) ]
        ]
```

Here we use [Basics.round](http://package.elm-lang.org/packages/elm-lang/core/latest/Basics#round) because an approximation is alright for weather.

Now, when you rebuild your code with `./build`, open `index.html` and submit a city/address name, you'll first see the `Coords` update on the page and then see the `Weather` result once it's done.

## Wrapping Up
Hooray! We can geocode an address and fetch the weather via two different proxy servers and display a result! That's great, but our `Main.elm` file is getting quite large, so stay tuned for the next part where we pull our code into smaller chunks without losing clarity.

If you'd like to check out the code from this part, it is located here: [https://github.com/rpearce/elm-geocoding-darksky/tree/pt-3](https://github.com/rpearce/elm-geocoding-darksky/tree/pt-3).

Until next time,
<br>
Robert
