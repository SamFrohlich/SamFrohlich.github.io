# Tetris

In this lab assignment you will implement a simple variant of Tetris, a popular computer game that was created in the 1980s. In Tetris, a sequence of random puzzle pieces, made up of four connected squares, fall towards the bottom of a well. The player can rotate the pieces and control where they end up by moving them left and right as they fall. If the pieces fit together and fill a complete row when they reach the bottom of the well, the row disappears and the player gets some points. If there are gaps in the rows, the pieces can start to pile up, and if the well gets so full of piled up pieces that there is no room for new pieces, the game ends.

You can [read more about tetris on Wikipedia](https://en.wikipedia.org/wiki/Tetris).

## Your Task

You will not need to write all the Haskell code needed to implement the game yourself: we provide some modules that help with the user interface, and you write the following to parts:
  * Part A: Basic functions for working with the geometric shapes that appear in the game: the falling pieces and the contents of the “well”.
  * Part B: Functions for combining shapes and basic actions on the state of the game: drawing the well, moving pieces and ticking of the clock.
  * Part C: Functions that complete the game experience: collisions, rotations and clearing rows.

## Prep 1

Some assignments have hints. Often, these involve particular standard Haskell functions that you could use. Some of these functions are defined in modules that you have to import yourself explicitly. You can use the following resources to find more information about those functions:
  * [Hoogle](https://hoogle.haskell.org/) the library function search engine
  * A compact list of useful functions from Prelude, Data.List, Data.Maybe, Test.QuickCheck. You can find this in this week's Notes.

We encourage you to actually go and find information about the functions that are mentioned in the hints!

## Prep 2

Before you start working:

  * Download and unzip the file `Tetris.zip`. This will give you the following Haskell modules:

    - `Shapes.hs`: the module you will work on in Part A and the first tasks of Part B.
    - `Tetris.hs`: the module you will work on in Part B and Part C.
    - `ConsoleGUI.hs`, `CodeWorldGUI.hs`, `GameInterface.hs`: user interface modules that you should not modify, and which you do not need to understand.

To be able to use the module `ConsoleGUI` needed in Part B, you need to install some Haskell libraries by typing the following command in a terminal window:

```bash
cabal install ansi-terminal
```
For more details (including how to use the optional CodeWorldGUI) see Running the Game below.

## Part A: Shapes

In this part you will add definitions in the file `Shapes.hs` that you downloaded as part of the preparations.

It might be a good idea to read through all of the assignment for Part A below before you start working, so you get an idea of what the overall goals are.

You can use the following command to play with your answers on GHCi:
```bash
ghci Shape.hs
```

### Types

We will need some types to represent shapes. A shape can be seen as a matrix of squares, where the squares are either empty or filled with a colour. We will use the following types to represent the squares:

```haskell
type Square = Maybe Colour

data Colour = Black | Red | Green | Yellow
            | Blue | Purple | Cyan | Grey
              deriving (Eq,Bounded,Enum,Show)
```

These types and others mentioned below are pre-defined for you in `Shapes.hs`.

A natural way to represent a matrix of squares in Haskell is as a list of rows, where a row is a list of squares. So essentially, a matrix of squares is a list of lists of squares. But not all lists of lists are matrices: the rows should all have the same length. So we will define a separate type to keep matrices apart from just any old lists of lists:

```haskell
data Shape = S [Row] deriving (Eq)
type Row   = [Square]

rows :: Shape -> [Row]
rows (S rs) = rs
```

### Showing Shapes

We have given you a function:

```haskell
showShape :: Shape -> String
```

and an instance

```haskell
instance Show Shape where
  show = showShape
  ...
```

which tells GHCi to use `showShape` when it shows values of type `Shape` in the Terminal window. We will see examples of this in the following exercises.

### The shapes used in the Tetris game

We have given you a list containing the 7 shapes used for the falling pieces in Tetris (the 7 tetrominoes):

```haskell
allShapes :: [Shape]
```

You can see how they are presented by the show function defined above.

```bash
*Shapes> allShapes
R
R
R
R

.G
.G
GG

.Y
YY
.Y

BB
BB

.P
PP
P.

CC
.C
.C

g.
gg
.g
```

### Some simple functions

**TASK A01:**

Define a function that returns an empty shape of a given size,

```haskell
emptyShape :: (Int,Int) -> Shape
```
>Hint: define a helper function that returns an empty row of a given length. The standard library function replicate can be useful.

```bash
*Shapes> emptyShape (4,2)
....
....
```

You can use the rows function if you want to see the internal representation of a shape:


```bash
*Shapes> rows (emptyShape (4,2))
[[Nothing,Nothing,Nothing,Nothing],[Nothing,Nothing,Nothing,Nothing]]
```

**TASK A02:**

Write a function that returns the size of a shape, i.e. the number of columns and rows.

```haskell
shapeSize :: Shape -> (Int,Int)
```

**TASK A03:**

Write a function that counts how many non-empty squares a shape contains:

```haskell
blockCount :: Shape -> Int
```

> Hint: it might be easier if you use concat to join all the rows together into one big list.

### The Shape invariant

**TASK A04:**

The `Shape` invariant. Define the property that all values of type `Shape` are expected to have: shapes should have at least one row, at least one column, and be rectangular (i.e. all rows should have the same length)

```haskell
prop_Shape :: Shape -> Bool
```

In the remaining questions you may always assume that the shape invariant holds whenever you have an argument of type `Shape`.

### Test data generators

**TASK A05:**

Write a random generator for colours:

```haskell
rColour :: Gen Colour
```
and make `Colour` an instance of the `Arbitrary` class.

**TASK A06:**

Write a random generator for shapes

```haskell
rShape :: Gen Shape
```

and make `Shape` an instance of the `Arbitrary` class.

We recommend that you simply pick a shape at random from `allShapes`.

Another (optional, but much more challenging) solution is to generate more random shapes, by first generating a random colour and two random numbers between 1 and 4 for the number of rows and columns of the shape. Then generate that many rows with that many columns. The `QuickCheck` function vectorOf can be useful here.

Run `sample rShape` in GHCi to see what the random shapes look like.

Run `quickCheck prop_Shape` to check that the randomly generated shapes satisfies the Shape invariant.

### Transforming shapes

**TASK A07:**

Define a function that rotates a shape by 90 degrees (clockwise or counterclockwise):

```haskell
rotateShape :: Shape -> Shape
```

> Hints: the function transpose from the standard library module `Data.List` can be helpful. Extra exercise: try to define transpose using recursion.

To verify that your `rotateShape` function works properly, test it on one of the asymmetric tetris pieces, rotate it, see that you get four different shapes, but after four rotations you get back the original shape:

```bash
*Shapes> allShapes !! 1
.G
.G
GG

*Shapes> rotateShape it
GGG
..G

*Shapes> rotateShape it
GG
G.
G.

*Shapes> rotateShape it
G..
GGG

*Shapes> rotateShape it
.G
.G
GG
```

**TASK A08:**

Define a function that shifts a shape right and down, i.e. adds empty squares by the top and left edges:

```haskell
shiftShape :: (Int,Int) -> Shape -> Shape
```

> Hints: define two helper functions, one that shifts horizontally by some number, and one that shifts a shape vertically.

```bash
*Shapes> shiftShape (1,2) (allShapes!!1)
...
...
..G
..G
.GG
```

**TASK A09:**

Define a function that pads a shape, i.e. adds empty squares by the bottom and right edges:

```haskell
padShape :: (Int,Int) -> Shape -> Shape
```

> Hint: `shiftShape` and `padShape` are similar. You can take advantage of this and use one of them to implement the other.

```bash
*Shapes> padShape (1,2) (allShapes!!1)
.G.
.G.
GG.
...
...
```

To simplify the definition of combine below, it is useful to have one more variant of the padding function:

**TASK A10:**

Define a function that pads a shape to a desired size.

```haskell
padShapeTo :: (Int,Int) -> Shape -> Shape
```

> Hint: use `shapeSize` to find out how big the shape is, then call `padShape` to add enough padding to return a shape of the desired size. The function does not need to shrink the shape if it is too big.

## Part B

In this part you will complete module `Shapes.hs` and then work on module `Tetris.hs` that you downloaded as part of the preparation.

For finishing off `Shapes.hs` you can continue to use:

```bash
ghci Shape.hs
```

but when you start using `Tetris.hs` you'll want to load everything in with:

```bash
ghci Tetris -package ansi-terminal -package random
```

(make sure that you have installed `ansi-terminal` using `cabal install ansi-terminal`, if it complains that you lack QuickCheck and/or random install then too)

Like in Part A, it might be a good idea to read through all of the assignment for Part B below before you start working, so you get a better picture of what needs to be done and how everything fits together.

### Comparing and Combining shapes

You are probably already familiar with the following standard list functions:

```haskell
zip     :: [a] -> [b] -> [(a, b)]
zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
or      :: [Bool] -> Bool
```

They might be useful in the following exercises.

**TASK B01:**

Write a function `overlaps` that tests if two shapes overlap (i.e. if they have non-empty squares in the same positions)

```haskell
overlaps :: Shape -> Shape -> Bool
```

> Hint: define a helper function to test if two rows overlap:

```haskell
rowsOverlap :: Row -> Row -> Bool
```

**TASK B02:**

Define the function `zipShapeWith`

```haskell
zipShapeWith :: (Square -> Square -> Square) -> Shape -> Shape -> Shape
```

that uses `zipWith` twice to combine two shapes in the same way that `zipWith` combines two lists. For example, we can combine `zipShapeWith` and the function clash defined below, to obtain the function `blackClashes`, which colours colliding squares of two given shapes black:

```haskell
blackClashes :: Shape -> Shape -> Shape
blackClashes s1 s2 = zipShapeWith clash s1 s2
  where clash :: Square -> Square -> Square
        clash Nothing Nothing = Nothing
        clash Nothing s       = s
        clash s       Nothing = s
        clash (Just c1) (Just c2) = Just Black
```

**TASK B03:**

Write a function `combine` that combines (takes the union of) two shapes. The functions is intended to be used on shapes that don’t overlap. The resulting shape should be big enough to fit both shapes.

```haskell
combine :: Shape -> Shape -> Shape
```

> Hint: use `shapeSize` to find out the sizes of the two shapes and compute the size of the combined shape. Then use `padShapeTo` and `zipShapeWith` to compute the combined shape.

### The State of the game

There are a number of things we need to keep track of while the game is running, so we will define a type that contains all the pieces of information that we need.

  * The position and shape of the currently falling piece.
  * The combined shape of the pieces that have landed at the bottom of the well.
  * A supply of random pieces.

The `Tetris.hs` included in `Tetris.zip` that you downloaded already contains a type for this:

```haskell
data Tetris = Tetris (Vector,Shape) Shape [Shape]
```

where the `Vector` type is used for the position of the currently falling piece.

```haskell
type Vector = (Int,Int)
```

There are a few more definitions that might be useful later, so take a look at them: `wellSize`, `startPosition`, `vAdd` and `place`.

### The main functions of the game

The following are the key functions we will define for the Tetris type:

```haskell
drawTetris  :: Tetris -> Shape
startTetris :: [Double] -> Tetris
stepTetris   :: Action -> Tetris -> Maybe (Int,Tetris)
```

  * `drawTetris` creates the visual representation of the game that is presented to the player by the functions in user interface modules.
  * `startTetris` creates the initial state of the game when it starts.
  * `stepTetris` updates the state in response to user input and timer ticks.

The latter is of course the most complex of these functions, and we will need to define a few helper functions to get the job done.

The `Tetris.hs` module included in the `Tetris.zip` download contains definitions of these functions. They are incomplete, but it is still possible to load the `Tetris.hs` module in GHCi and test them:

```bash
ghci Tetris -package ansi-terminal -package random
GHCi, version 8.6.5: http://www.haskell.org/ghc/  :? for help
Loaded package environment from /home/sam/.ghc/x86_64-linux-8.6.5/environments/default
[1 of 4] Compiling Shapes           ( Shapes.hs, interpreted )
[2 of 4] Compiling GameInterface    ( GameInterface.hs, interpreted )
[3 of 4] Compiling ConsoleGUI       ( ConsoleGUI.hs, interpreted )
[4 of 4] Compiling Main             ( Tetris.hs, interpreted )
Ok, four modules loaded.
*Main>
```

When you run the `main` function, the game keeps running until you quit by pressing Q. You should see something like this:

```bash
*Main> main
                     0 points
                     0 rows

                     ===TOP 10===
                      1:       0
                      2:       0
                      3:       0
                      4:       0
                      5:       0
                      6:       0
                      7:       0
                      8:       0
                      9:       0
                     10:       0

                     K = Left
                     J = Rotate
                     L = Right
                     P = Pause
                     sp = Down
Game over!
*Main>
```

As you can see, there is an empty space where the Tetris game should appear. The idea is to work in small steps towards a complete solution, and run the main function in GHCi after each step, to test that what you have done so far works as expected.

### An invariant

We have previously used random testing with `QuickCheck` to find bugs in the functions we define. We could do that here too, by defining a test data generator for the `Tetris` type. However it is rather difficult to generate sensible random values of this type, so we won’t do that.

We will still define an invariant, i.e. a property that should hold at all times, for the `Tetris` type, and then test it while the game is running. This is known as run-time verification. This might not be as effective as random testing, but it can still help make some subtle bugs more obvious.

**TASK B04:**

Define a property that checks the following things:

  * that the falling shape in the well satisfies the Shape Invariant (`prop_Shape`),
  * that the size of the well is correct, i.e. equal to `wellSize`.

```haskell
prop_Tetris :: Tetris -> Bool
```

### Displaying the game

**TASK B05:**

Write a function that adds black walls around a given shape.

```haskell
addWalls :: Shape -> Shape
```

This will be used to draw the walls around the well using the colour Black, which is shown as the `#` character, but you can test it on any shape:
```
*Main> addWalls (allShapes!!1)
####
#.g#
#.g#
#gg#
####
```

**TASK B06:**

Define the `drawTetris` function.

```haskell
drawTetris  :: Tetris -> Shape
```

> Hints: since the visual representation of the game is a Shape, the job of this function is fairly simple: it just needs to insert the currently falling piece at the right position in the well and add walls around it. This should be a fairly simple use of the functions `shiftShape`, `combine` and `addWalls`. Make sure that you add the walls after you have done everything else.

Run main to test that your definitions work as expected. You should see something like this:

![](b03.png)

### Action

The next thing that would be nice to see is the falling piece move. To make that happen, we need to start working on the `stepTetris` function. We have provided an incomplete version doesn’t do much.

```haskell
stepTetris :: Action -> Tetris -> Maybe (Int,Tetris)
stepTetris _ t = Just (0,t) -- incomplete !!!
```

The function is called whenever something has happened that the game needs to react to. The first argument (of type `Action`) indicates what has happened, and the second argument (of type `Tetris`) is the current state of the game.

The return value indicates how the game should continue: `Nothing` means the game should end, and `Just (n,t)` means the game should continue in a new state `t`. The `n` indicates how many rows were cleared in this step. In most cases `n` will be `0`.

The `Action` type is defined in `GameInterface` like this:

```haskell
data Action = Tick | MoveLeft | MoveRight | MoveDown | Rotate
              deriving ...
```

The `Tick` means that some time has passed and that the currently falling piece should move one step closer to the bottom. Let’s handle this case first. Let’s start by defining a more general helper function to move the falling piece.

**TASK B07:**

Define a function to move the falling piece:

```haskell
move :: Vector -> Tetris -> Tetris
```

The first argument determines how far the piece will be moved. For example `move (-1,0) t` moves the falling piece in tetris `t` to the left by one position, similarly `move (-1,0) t` moves it one position to the right, and `move (0,1) t` one position down.

The move function should just update the position of the falling piece, without checking if it has collided with something. We will worry about that later. Recall that we defined a simple function `vAdd` which you will find useful here.

**TASK B08:**

Define a function `tick` that can be called from `stepTetris` to handle the `Tick` action.

```haskell
tick :: Tetris -> Maybe (Int,Tetris)
```

Use the move function to calculate a new position for the falling piece and return `Just (0,t)` where `t` is the new state with the updated position and the other parts of the state unchanged. For the time being, we let the piece continue falling forever (through the bottom of the well and out of sight). We will add checks later to make it stop when it reaches the bottom of the well.

Also add an equation in `stepTetris` to call tick to handle the the `Tick` action.

Load the updated definitions in GHCi and run main to test that it works. You should see the falling piece moving towards the bottom of the well. Press Q to stop.

## Part C

In tetris, all moves that make the falling piece collide with another piece of the well are invalid. In the next task, we define an helper function that detects collisions and that will be useful in all the functions that move the falling piece.

### Collisions

**TASK C01:**

In this part you should write a function, and use it to improve the `tick` function.

Write a function to test if the falling piece has collided with the walls or something in the well:

```haskell
collision :: Tetris -> Bool
```

There are four possibilities:

  * The piece has moved too far to the left (the horizontal position of the piece is negative).
  * The piece has moved too far to the right (the horizontal position of the piece plus the width of the piece is greater than the width of the well).
  * The piece has moved too far down (the vertical position of the piece plus the height of the piece is greater than the height of the well).
  * The piece overlaps with something in the well.

> Hints: The functions `shapeSize` and overlaps from Part A will be useful here. Note that you should use the helper function `place` that we provided to shift the piece to the right position before you check whether it overlaps.

Now you should make a small improvement in the `tick` function: compute the new state and give it a name using a `where` clause, and use guards to check if there is a collision in the new state. If so return the old state to prevent the piece from falling further, otherwise return the new state.

(Optional) We can also use `collision` to improve the invariant: include a test in `prop_Tetris` to ensure that there is no collision if the currently falling piece is added to the well.

Load the updated definitions in GHCi and run main to test that falling piece now stops when it reaches the bottom of the well.

In this test you might have thought that it is boring to wait for the falling piece to reach the bottom of the well. Let’s fix this:

**TASK C02:**

Add an equation in `stepTetris` to handle the `MoveDown` action. It can be handled simply by calling the `tick` function. (In some versions of Tetris the player gets extra points for moving the falling piece down faster. To keep things simple, we refrain from that here.)

Reload the definition in GHCi and test again. You could now be able to use the space bar to move the falling piece down faster.

### Functions for moving and rotating the falling piece

**TASK C03:**

Define a function that can be called from `stepTetris` to handle the `MoveLeft` and `MoveRight` actions:

```haskell
movePiece :: Int -> Tetris -> Tetris
```

The first argument is `-1` to move left and `1` to move right.

> Hints: use move and then check with collision to see if the move was OK. If so, return the new state, otherwise return the old state.

Also add equations for `MoveLeft` and `MoveRight` in `stepTetris` to call `movePiece`.

Load the updated definitions in GHCi and run `main` to test that you can move the falling piece left and right by pressing J and L. Also check that the falling piece doesn’t move any further when it has already reached one of the walls.

**TASK C04:**

Define a function that rotates the falling piece:

```haskell
rotate :: Tetris -> Tetris
```

Like with the `move` function, `rotate` should just rotate the falling piece without checking if it has collided with something. Don’t forget that you already defined `rotateShape` in part A.

**TASK C05:**

(Optional: only try this if you have time) Write a function

```haskell
adjust :: Tetris -> Tetris
```
When a tall and narrow piece is rotated, it becomes wider. If it located close to a wall, it might then collide with the wall. The function `adjust` should adjust the horizontal position of the piece in these cases so that rotation isn’t prevented. (The game will work without this function, but it is more user friendly to have it.)

**TASK C06:**

Define a function that can be called from `stepTetris` to handle the `Rotate` action:

```haskell
rotatePiece :: Tetris -> Tetris
```

> Hints: use `rotate` and then check with `collision` if the rotation was OK. If so return the new state, otherwise return the old state. Optionally also call adjust (after `rotate` but before `collision`).

Also add an equation in `stepTetris` to call `rotatePiece` to handle the `Rotate` action.

Load the updated definitions in GHCi again, and run `main` to test that you can now rotate the falling piece by pressing K. If you implemented `adjust`, test that you can rotate the falling piece even when it is next to an edge.

### Piling up

The game is still not very interesting, because the `tick` function is still missing some key functionality. The falling piece should not just stop falling when it can’t move further down. Instead, it should be added to the pile of pieces already in the well, and a new piece should start falling.

**TASK C07:**

Define a function that handles the case when the falling piece can’t move any further down.

```haskell
dropNewPiece :: Tetris -> Maybe (Int,Tetris)
```

The following things need to be taken care of in this function (for now, we will improve it below):

  * The falling piece is added to the combined shape of the other pieces in the well.
  * A piece is removed from the supply and used as the new falling piece.
  * We have to check if the new piece overlaps with the pieces in the well. If so the game should end, otherwise it continues in the new state.

> Hints: Remember to use the function place from Part A in this task.

Also update the `tick` function: instead of returning the old state when there is a collision, it should call `dropNewPiece`. We provided a function `startPosition` to give the position of every new piece.

Load the updated definitions in GHCi again, and run main to test that a new piece starts falling when the currently falling piece reaches the bottom of the well. Also test that falling pieces pile up on top of each other as expected and that the game ends when the pile reaches the top of the well.

In this test you probably noticed that the supply of pieces isn’t random, it’s just repeating the same piece.

**TASK C08:**

The definition of `startTetris` is incomplete. But it has an argument that is an infinite list of random numbers `x` such that `0.0<=x<=1.0`. Modify the `startTetris` function so that it uses this parameter to create a list of random shapes for the supply.

### Clearing rows

One more thing that can happen when a new piece is added to the well is that one or more rows can become completely filled. These rows should be cleared and the rows above should move down to fill the gap.

**TASK C09:**

Define a function that removes completed lines from the well.

```haskell
clearLines :: Shape -> (Int,Shape)
```

This function needs to work with the rows in the shape, to remove the rows that are completely filled and add new empty rows at the top of shape so that the size of the well stays the same. The function returns how many rows were cleared and the new shape.

> Hints: `filter`, `length`, `null`, `shapeSize`, `shiftShape` and list comprehensions could be useful. Defining a helper function `isComplete` for testing rows is probably a good idea.

**TASK C10:**

Insert a call to the `clearLines` function in `dropNewPiece`. Instead of returning `Just (0,t)` where `t` is the new state, `dropNewPiece` should now return `Just (n,t)` where n is the number returned by `clearLines`. (The player will now get points for clearing rows.)

Load the updated definitions in GHCi again and run main. Verify that complete rows are cleared and that the score increases.

If you have gone through all the steps above, your implementation of Tetris is now complete! 

**Congratulations!**

## Running the Game

As you have seen while working on Part B, you can run the game inside GHCi by calling the main function:

```bash
*Main> main
```

Alternatively, since GHC is an optimising compiler that can create executable files containing native machine code, you can run the game without GHCi (even on computers where GHC isn’t installed):

```bash
$ ghc --make -O -threaded Tetris.hs
[1 of 4] Compiling Shapes           ( Shapes.hs, Shapes.o )
[2 of 4] Compiling GameInterface    ( GameInterface.hs, GameInterface.o )
[3 of 4] Compiling ConsoleGUI       ( ConsoleGUI.hs, ConsoleGUI.o )
[4 of 4] Compiling Main             ( Tetris.hs, Main.o )
Linking Tetris ...
```

The executable file will be called `Tetris` (or `Tetris.exe` on Windows) and you can run it directly:

```bash
$ ./Tetris
```

## Alternative user interfaces

In the `Tetris.zip` there is also a user interface based on CodeWorld (codeWorld is a web-based Haskell environment. You can run this by installing the CodeWorld libraries:

```bash
$ cabal install codeworld-api
```

This might take a while (5-10 minutes, maybe more). Then change the import in the beginning of your Tetris module:

```haskell
--import ConsoleGUI
import CodeWorldGUI
```

Recompile your program and run it.

```bash
$ ghc --make -O -threaded Tetris.hs
...
$ ./Tetris
```

Open me on http://127.0.0.1:3000/
Open the link in your web browser.