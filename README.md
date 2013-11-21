# Mathematical Modelling Project Yr 2

by Ollie Frolovs, 2013.

This is a set of tools i built to help my team with the project. Each tool is described in its respective section below.

*NB* I'm only a beginner `OCaml` programmer and writing those tools was a 'here be dragons' experience for me. Suggestions and improvements on my OCaml style are welcome.

## google_distance_matrix_query

This tool downloads the distances and durations for a list of trips between the given locations from the Google Distance Matrix using [Google Distance Matrix JSON API]. It observes the first and second usage limits by waiting the right amount of time between the requests, when necessary. The user is expected to observe the third usage limit.

[Usage limits] (as of 29-10-13): 
> * 100 elements per query.
> * 100 elements per 10 seconds.
> * 2 500 elements per 24 hour period.
> 
> The number of origins times the number of destinations defines the number of elements.

*NB* The following restriction is not checked by this tool. Passing of long URLs to Google Distance Matrix API will result in error code being returned by the API.
> Distance Matrix API URLs are restricted to approximately 2000 characters, after URL encoding.

## clusterise

This tool extracts the data for specified locations from the output saved by `google_distance_matrix_query`. It is user's responsibility to check that the locations requested are actually present in the dataset.

## dynamicpr

This is **the** most interesting part of the project. It computes the optimal tour through given locations using dynamical programming as described in [Bellman, 1962].

**FIXME** currently the program assumes that the input data are symmetric, that is the distance from A to B equals the distance from B to A, which may not be the case for the data returned by Google Distance Matrix. A proposed fix is to lookup both distances from A to B and from B to A and take the shortest route. This would work for our project which is about computing the distances between the cities, but on shorter scale this approach might fail as some routes have genuine differences due to one-way roads rather than whimsical behavious of Google API which returns just one of the many possible routes without regards to its distance.

-- Ollie Frolovs, @olliefr


[Google Distance Matrix JSON API]:https://developers.google.com/maps/documentation/distancematrix/
[Usage limits]:https://developers.google.com/maps/documentation/distancematrix/#Limits
[Bellman, 1962]:http://dl.acm.org/citation.cfm?doid=321105.321111
