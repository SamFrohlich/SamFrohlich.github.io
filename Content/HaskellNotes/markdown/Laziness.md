# Laziness

Much like how in the real world you have people that are lazy and people that are not, when it comes to evaluation order in programming languages you have lazy languages and eager languages. Haskell is lazy. It won't do something unless it absolutely has to. You can think of this much like students and lecturers:

```haskell
student :: Question -> Solution

lecturer :: Int -> [Question] -> [Solution]
lecturer n xs = take n (map student xs)
```

Think of a `student` as a function that will turn a `Question` into a `Solution`, and think of a `lecturer` as a function that assigns a certain number (`n`) of `Question`s (`xs`) to `student` in order to get a list of `Solution`s. In the world of Haskell everyone is lazy so here is how the `Solution`s would be extracted. Say the `lecturer` is going to assign 3 `Question`s, i.e. `n = 3`. `lecturer` sees that `n` isn't zero yet so it says to `(map student xs`) "Okay I need a `Solution`", and the `student` reluctantly replies spitting out one `Solution`. Now `n = 2`, the `lecturer` needs more `Solution`s so it kicks the inside function of `student` demanding that they do more work, and this pattern continues until the `lecturer` is satisfied. Like taking blood from a stone.

If this was not Haskell and we were working with an eager language things would be very different. The `student` would be a very keen bean and immediately do all the `Question`s in the list, regardless of how many the `lecturer` assigns. This seems every diligent but think about what would happen if the list of `Question`s was infinite. The function would never terminate. Our poor `student` would work themselves to the bone perpetually answering `Question`s. Not very healthy.
