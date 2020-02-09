# Salinas

A library written in Clojure(Script) dealing with tuning and arranging musical pitches. More to come ...

This is alpha, API may change.

### Data format:

A `pitch` or `interval` is a map, which has one obligatory key `:cents`.

Example: `{:cents 386}`

Moreover it can have a second key `:ratio`.

To create an interval, just use the function `interval`.
(Of course you can create these manually.)

If you give it two integers, it creates a ratio, and calculates the cent value:

`(interval 3 2)`
=> `{:ratio [3 2], :cents 701.9550008653874}`


I there is only one argument to `interval`, the number is interpreted as cent value:

`(interval 100)`
=> {:cents 100}

Ratios are preferred for creating and building pitch sets.
