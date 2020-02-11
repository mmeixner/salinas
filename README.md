# Salinas

This is alpha, API likely to change!

A library written in Clojure(Script) dealing with tuning and arranging musical pitches. More to come ...

### Data format:

A `pitch` or `interval` is a map, which has one obligatory key `:cents`.  
Example: `{:cents 386}`

Moreover it can have a second key `:ratio`.  
Example: `{:ratio [2 1] :cents 1200}`

To create an interval, use the function `interval`
(or create it manually, if you like).

With two integers, `interval` creates a `:ratio`, and calculates its `:cents` value:

`(interval 3 2)`  
=> `{:ratio [3 2], :cents 701.9550008653874}`


If there is only one argument to `interval`, the number is interpreted as cent value:

`(interval 100)`  
=> `{:cents 100}`

Ratios are preferred for creating and building pitch sets, and are maintained throughout the calculations, if possible.
