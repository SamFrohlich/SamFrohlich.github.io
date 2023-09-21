# Lists

Lists are a data structure that you will become very familiar with. You will use them **literally all the time**. We love lists. Lists are the best. Lists can be of infinite size, so if you ever want to freak out the general public: style your terminal with a black background and coloured font (something I hope you have already done like srsly if you have black font on white what are you doing with your life?), put your terminal full screen, open GHCi, type `[1..]`. All `[1..]` is, is an infinite list of numbers starting from one BUT your terminal will start spitting out these numbers onto the terminal as fast as it can. Looks like some srs hackery is going down.

```haskell
data [] a  =  []
           |  a : [a]
```
  where:

```haskell
 []  ::  [a]
(:)  ::  a -> [a] -> [a]
```

The list data type has two constructors: a list can either be empty `[]`, or it can have an element (of type `a`) prefixed onto another list. The type of `[]` is hopefully fairly straight forward: it has type list of `a`, it just so happens that in this context the empty list constructor has been chosen. The type of `(:)` is a little more involved. It takes an element and a list and returns a new list. This new list is just the old list but with the element added to the head. As indicated by the `a`, all the elements must be of the same type.
