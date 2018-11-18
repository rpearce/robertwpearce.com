---
author: "Robert Pearce"
authorTwitter: "@RobertWPearce"
description: "In Part 4 we ectract our Elm code for clarity"
image: "/images/field-road-rain.jpg"
keywords: "elm, elm tutorial, elmlang, elm geocoding, elm darksky, elm weather, elm functional programming"
photoCredit: "Pop & Zebra"
photoWebsite: "https://unsplash.com/@popnzebra"
title: "Elm, Geocoding & DarkSky: Pt. 4 – Extracting our Elm Code"
---

This is part 4 of a multipart series where we will be building a small weather forecast app using [Elm](http://elm-lang.org/), [Google's Geocoding API](https://developers.google.com/maps/documentation/geocoding/start) and the [DarkSky API](https://darksky.net/dev/). Instead of doing everything in one massive post, I've broken the steps down into parts of a series. Here is the series plan:

* [Pt. 1 – Setup Elm & Proxy Servers](/elm-geocoding-and-darksky-pt-1-setup-elm-and-proxy-servers.html)
* [Pt. 2 – Geocoding an Address](/elm-geocoding-and-darksky-pt-2-geocoding-an-address.html)
* [Pt. 3 – Fetching the Current Weather](/elm-geocoding-and-darksky-pt-3-fetching-the-current-weather.html)
* Pt. 4 – Extracting our Elm Code

If you'd like to code along with this tutorial, check out [part 1](/elm-geocoding-and-darksky-pt-1-setup-elm-and-proxy-servers.html), [part 2](/elm-geocoding-and-darksky-pt-2-fetching-the-current-weather.html) and [part 3](/elm-geocoding-and-darksky-pt-3-fetching-the-current-weather.html) first to get set up.

_Note: to learn more about the Elm language and syntax, check out the [Elm Tutorial](https://www.elm-tutorial.org/en/), the [EggHead.io Elm course](https://egghead.io/courses/start-using-elm-to-build-web-applications), subscribe to [DailyDrip's Elm Topic](https://www.dailydrip.com/topics/elm), [James Moore's Elm Courses](http://courses.knowthen.com) or check out [Elm on exercism.io](http://exercism.io/languages/elm/about)._

## tl;dr
I meant to finish this blog series a few months ago, but while I didn't finish the writing part, I did manage to do the code for part 4.
If you've made it this far and would like to see the extracted elm code extracted into

* [Geocode.elm](https://github.com/rpearce/elm-geocoding-darksky/blob/pt-4/src/Geocode.elm)
* [Main.elm](https://github.com/rpearce/elm-geocoding-darksky/blob/pt-4/src/Main.elm)
* [Weather.elm](https://github.com/rpearce/elm-geocoding-darksky/blob/pt-4/src/Weather.elm)

then you can do so here:

[https://github.com/rpearce/elm-geocoding-darksky/tree/pt-4/src](https://github.com/rpearce/elm-geocoding-darksky/tree/pt-4/src).

I'd like to move on to other topics, and unfortunately, this is the best way I know how to do so.

Until next time,
<br>
Robert
