---
title: An Introduction to game development in Haskell using Apecs
date: 2018-09-10
subtitle: A post about how I used Apecs ECS framework to make 'HSRogue'.
description: There have been so many attempts to pioneer gamedev in Haskell, and yet still no commercial releases. In this post I hope to clear the air a little bit and encourage new developers to try Haskell.
tags:
  - Tutorial
  - Game
  - Haskell
  - Functional
image: https://raw.githubusercontent.com/Ashe/HSRogue/master/img/gameplay.gif
project: hsrogue
status: published
---

# Background

There have been so many frameworks, examples and prototypes released in an effort to pioneer games development in Haskell, and yet we still don't really have many (if any) commercial releases we can look to for inspiration. I want to help relax the entry point into Haskell game development and show people how much fun programming your game in Haskell can be. I used to make YouTube guides for video games and generally I got positive responses, and so I figured that I could do something similar here with my blog posts or even my YouTube channel.

Working with Haskell in the games industry is a little bit of a pioneering job, but I'm a firm believer that our technology is advanced enough such that we should be able to sacrifice a little bit of performance to make development easier. [My first experience wasn't amazing](/blog/of-boxes-and-threads/), but after a bit of time working on other things and a lot of thinking I decided to try again. This time, rather than researching through doing everything myself, I figured I'd try a framework made by someone else.

This guide has been written by myself, who is in-between the beginner and advanced stages of Haskell --- I've read the [Haskell Book](http://haskellbook.com/) but I'm still learning as I apply my knowledge in practice. I'm hoping that my beginnerish perspective will help people who are less academically inclined as myself digest things easier, I simply want to create shortcuts for those people close behind!

:::{.warning header="Some knowledge required"}
Please note that while I'm not going to assume Haskell mastery I am expecting at least a little bit of Haskell or functional programming knowledge. I'm not going to go through the absolute basics, but I will walk through what I'm doing so that new Haskellers understand what's going on. I will also not talk about how to set up a project with Stack, I will purely be focusing on Haskell.
:::

# Why Haskell for Games?

You can make a game in pretty much any programming language, and whether you *should* is entirely up to you. However, there are many barricades that have got in the way of Haskell being used in games. I've been working on my Haskell game for about a month now, and I wanted to really feel confident in the language before I started preaching that it's possible. It doesn't matter what framework you choose or whether you roll your own, the language itself is the bread and butter for your game and it has to be suited to the task.

One reason people choose to avoid Haskell is simply the difficulty of the language itself. Games development is as hard as you decide it to be, with many different engines, frameworks and languages to choose from. While I agree that for most people it may be difficult to get their head around Haskell, once you have the foundations set up the process of adding additional mechanics will get easier as time goes on.

I would like to take a moment to praise Apecs for how many problems it alleviates when using a functional language. I'm sure that the other frameworks out there will make Haskell comfy as well, and this is by no means an argument to use Apecs, it is simply a testament to its usability. Without going into massive detail on the ins and outs of Haskell and FP languages, Haskell paired with Apecs really does make development a whole lot easier, and I can really envision a future where people may choose languages like Haskell to simplify development.

* 9/10 times, if it compiles, your game will run. Maybe not as expected, but at least it won't crash!
* Using [Stack](https://docs.haskellstack.org/en/stable/README/) and [Stackage](https://www.stackage.org/), all of the dependencies for your game are downloaded automatically by simply listing them, making it easy to build your projects.
* [GHCJS](https://github.com/ghcjs/ghcjs) allows you to compile your Haskell code to Javascript if you wanted to make a web game or something similar.
* Haskell's type system ensures that you know the difference between pure and impure functions. By organising code in this way, you streamline your game's structure making it *extremely easy* to find logical errors in your game.
* Important functions for *doing* things can be wrapped up in monads and take advantage of `do` syntax, meaning that you can execute multiple side effects.
* Functions are first class citizens, they are treated in the exact same way as values and can be stored just as easily (hooray for spell making!).
* People say learning and using Haskell will help you even in imperative languages. I like Haskell because it makes me feel good when I solve something, so if you enjoy Haskell then why not make a game in a language you enjoy?

Without further ado, let's start making a game!

# Getting Started

## Initialising

For this project, we will start off with [Apecs](https://hackage.haskell.org/package/apecs) and [SDL2](https://hackage.haskell.org/package/sdl2). Choose whatever suits you, but I will be showing off a fair bit of Apecs and I will be starting the project with SDL2, so you'll have to research how to start your game off independently. I will also be using [SDL-ttf](https://hackage.haskell.org/package/SDL-ttf) and [SDL-image](https://hackage.haskell.org/package/SDL-image), remember, if you are also using packages that have bindings to C you will also need to install them onto your system, not just the Haskell bindings. Now, let's show some code.

:::{.help header="Qualified imports"}
I am importing SDL with `qualified`, so that every SDL function has SDL before it. I mainly do this in the main file as there's a lot going on. Do as you wish. Also, this won't compile yet due to us not having the `initialise` function implemented.
:::

```hs
import Apecs
import qualified SDL
import qualified SDL.Image(quit)
import qualified SDL.Font

-- Uses templateHaskell to create the data 'World'
-- also creates initWorld
makeWorld "World" []

main :: IO ()
main = do
  -- Initialise Apecs world
  world <- initWorld

  -- Initialise SDL
  SDL.initialize [SDL.InitVideo]
  SDL.Font.initialize

  -- Create a window and renderer
  window <- SDL.createWindow "App" SDL.defaultWindow
  renderer <-
      SDL.createRenderer window (-1)
        SDL.RendererConfig
          { SDL.rendererType = SDL.AcceleratedRenderer
          , SDL.rendererTargetTexture = False
          }

  -- Do any initialisation here!
  runSystem initialise world

  -- Display the game
  SDL.showWindow window

  -- Insert loop code here!

  -- Delay shutdown if you like?
  SDL.delay 1000

  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.Image.quit
  SDL.Font.quit
  SDL.quit
  putStrLn "Goodbye!"
  exitSuccess
```

Okay, so here we have a basic main function that creates a world and then closes it. Nothing special. I even threw in [SDL.Delay](https://hackage.haskell.org/package/sdl2-2.4.1.0/docs/SDL-Time.html#v:delay) so you can see the window before it closes. Note that `SDL.Image` is initialised when used unlike the core and font variations, which is why only quit is imported here. This code isn't anything special so I should probably stop typing and move on to describing a simple game loop. But first, let's look at [Apecs' `makeWorld` function](https://hackage.haskell.org/package/apecs-0.5.0.0/docs/Apecs.html#v:makeWorld).

:::{.help header="Template Haskell"}
If you actually clicked the link above you would have noticed that it says "turns into" at some point. This function uses Template Haskell, and simply writes a lot of things for you. The reason that you will never find the `World` type is that it is actually created in the `makeWorld` function described, and so the function `initWorld` is specialised to the type created in `makeWorld`. The function `makeWorld` accepts a string which will become the type (in our case, "World" becomes `World`) along with a list of components we are yet to make. To sum it up, `makeWorld` is simply a template that creates the world type and a bunch of functions for said world for you which your entire game will revolve around.
:::

## Loop function

Now for the loop function:

```hs
let loop prevTicks secondTick fpsAcc prevFps = do
      ticks <- SDL.ticks
      payload <- map SDL.eventPayload <$> SDL.pollEvents
      let quit = SDL.QuitEvent `elem` payload
          dt = ticks - prevTicks
          calcFps = secondTick + dt > 1000
          newFps = if calcFps then fpsAcc + 1 else prevFps
          newFpsAcc = if calcFps then 1 else fpsAcc + 1
          newSecondTick = if calcFps then mod (secondTick + dt) 1000 else secondTick + dt

      -- React to input
      runSystem (handlePayload payload) world

      -- Update the world
      runSystem (step $ fromIntegral dt) world

      -- Set the background colour and clear the screen
      SDL.rendererDrawColor renderer $= V4 0 0 0 0
      SDL.clear renderer

      -- Render the world
      join $ runSystem (draw renderer newFps) world

      SDL.present renderer
      unless quit $ loop ticks newSecondTick newFpsAcc newFps

-- Begin looping
loop 0 0 0 0
```

You can either put this in a let block inside the `main` function, or you can make it it's own thing. Up to you. What matters is that you understand that we are recursively looping almost indefinitely until the user closes the window (ie, giving SDL the `QuitEvent`). Don't worry if there's a lot of stuff you don't understand, take a moment and research the functions presented to you.

I could probably have named the variables better, but I've essentially made a simple FPS counter for this tutorial. Yes, it could probably be better and yes, there's probably [something like this](https://hackage.haskell.org/package/sdl2-gfx-0.2/docs/SDL-Framerate.html) that does it for you. I just wanted to illustrate how ticks and time work here.

## Ticks

There are 1000 ticks in a second (1 tick is 1 millisecond), and the `ticks` function allows us to see how many ticks have passed since SDL was initialised. We can use this to work out the time since the previous frame (`dt` or [*delta time*](https://en.wikipedia.org/wiki/Delta_timing)). Here, I am using `secondTick` to time seconds, and so `calcFPS` is `True` when a second has passed. The `newFPS` value will either display the fps for the previous second (`prevFps`), or it will display the current running total + 1 stored in `fpsAcc`. Finally, the timer tracking seconds is reset. It's a bit ugly, but this a very easy and primitive way of tracking FPS in the early days, not that we are rendering any text yet. Let's start getting into the basics of Apecs.

# Apecs Components

## Remembering Types

Okay, so there are a couple of lines that won't compile as we haven't implemented them yet. Let me take the time to explain the role of ['runSystem'](https://hackage.haskell.org/package/apecs-0.5.0.0/docs/Apecs.html#v:runSystem). In Apecs, `System` is a monad that most of your code will produce at some point. I'm not going to talk about what a monad really is as that's a hot topic, but I can show what they can do. To put it simply, `runSystem` ***does*** something. It can or *will* execute side effects and can produce multiple side effects at once. The `System` monad is how you will manipulate your game.

Here's a refresher on [creating types in Haskell](https://wiki.haskell.org/Constructor). We will need all of these if we're going to get anywhere with Haskell.

* **`type`:** A *type synonym*. You can replace the occurrence of your type with what it represents, it does nothing else. Functions can accept types in addition to what the type represents, it's just an alias. Example:
```hs
type String = [Char]
type LongThing = (Int, Int, Int, Int, Int)
```
* **`data`:** A *type constructor*. You can craft your own types out of other types (think classes or structs in other languages). Functions that take or return your type will not accept anything but your type, and you will have to deconstruct it to access the contents. Example:
```hs
data Bool = True | False
data Maybe a = Just a | Nothing
data Action = MoveTo (V2 Int) | Attack Entity | Cast Spell | Wait Int
```
* **`newtype`:** A *type constructor* that acts as a wrapper for a single type. Your type will act just like it would if you used `data`, but is more efficient. If you are simply wrapping your type around something, use this. Example:
```hs
newtype PositionComponent = PositionComponent (V2 Double)
```

## Creating Components

Now, let's create some simple components to manipulate. Examine the code below in addition to modifying our `makeWorld` call. Note that `V2` is contained within `SDL.Vect`, which re-exports the package [`Linear`](https://hackage.haskell.org/package/linear) (ie, if you are using SDL, you do not need to install `Linear` as it comes with it. `Linear` comes with lots of nice types like `V2`, `V3` and `V4`, go take a look.)

```hs
-- Global component, exists outside of entities
-- Used for managing the passage of time
newtype Time = Time Double deriving Show
instance Semigroup Time where (<>) = mappend
instance Monoid Time where mempty = Time 0
instance Component Time where type Storage Time = Global Time

-- Unique component, either one or none exists
data Player = Player deriving Show
instance Component Player where type Storage Player = Unique Player

-- Position of game entities
newtype Position = Position (V2 Double) deriving Show
instance Component Position where type Storage Position = Map Position

-- Name of game entities
newtype Name = Name String deriving Show
instance Component Name where type Storage Name = Map Name

-- Uses template Haskell to create the data 'World'
-- also creates initWorld
makeWorld "World" [''Time, ''Player, ''Position, ''Name]
```

So here we have three components. The first, `Time`, is a `Global` component. This means that there's ***only one of it at any one time, and it belongs to no entity***. This is appropriate as time is, well, global. Although, it does actually belong to an entity secretly --- an entity in Apecs (and many other implementations) is just an integer to reference components together. Global components are stored on entity *-2*, which will never be interfered with with normal usage of apecs.

Time is a newtype --- we would have liked to use just a `Double`, but because we need to provide our own instances of typeclasses, we need to `newtype` it as type synonyms don't have their own implementations. We implement the `Component` typeclass so that apecs knows what kind of component it is. You can see the different kinds of entities [here](https://hackage.haskell.org/package/apecs-0.5.0.0/docs/Apecs-Stores.html). It also needs to implement instances of `Semigroup` (usually you can just specify `mappend`, it's worked for me this far!), and `Monoid` (specify what an 'empty' component looks like, as this is the initial state of the component as it exists from the beginning of the game).

Second, we have the `Player` component. This type has only one data constructor, `Player`. The purpose of this component is to simply mark which entity is the player --- ***there can only be unique entity at once, giving it to a new entity removes it from the previous***. Nothing much else to say about this for now, but it will come into play later.

Finally, we have `Position`. Again, this is a newtype so that we can create instances of `Component` --- *every component in Apecs needs this instance, so try to provide these to prevent [Orphan Instances](https://wiki.haskell.org/Orphan_instance).* Even though that the implementation of `V2` is the following..
```hs
data V2 a = V2 a a
```
.. It is still a single piece of data in the same way that `(a,b)` is also a single piece of data (a tuple containing two fields of possibly different types).

As a final note, notice how `makeWorld` now contains components. This piece of template Haskell is going to take what we've done and allow our component types to be used within the Apecs `World`. This means that everything found in [Apecs.System](https://hackage.haskell.org/package/apecs-0.5.0.0/docs/Apecs-System.html) will now be compatible with your types.

Okay, so maybe that was a tough section, maybe it wasn't. Maybe you're learning something or maybe you're upset that I've done something in a beginnerish way. Either way, this code should be close to compilation and should be enough to demonstrate some basics of Apecs.

# Systems

## Side Effects in Monads

In an [Entity Component System](https://en.wikipedia.org/wiki/Entity%E2%80%93component%E2%80%93system) such as Apecs, *systems* are what make the world go round. Normally in C++ or C#, I'd make my ECS seperate from a few things baked into the engine, but with Apecs I have to imagine a lot of things as a global component instead. The game's map, texture caches and other assets will most likely manifest as a global component here, if you wouldn't already have implemented things in this way.

Up to this point in the tutorial, I've given code examples that haven't given much room for flexibility and I assume that you've been following along with me. For this area though, I'm going derail a little bit and just try to show off what apecs can do. I'll provide usable, relevant code, but obviously what you choose to do depends on your game. This section is going to be fairly lengthy and have a lot of code, but if you follow along and understand the methodology I'm sure that you can apply what I give to your own code.

Firstly, some usage. Apecs has a function that I've touched on already called ['runSystem'](https://hackage.haskell.org/package/apecs-0.5.0.0/docs/Apecs.html#v:runSystem). This function will execute side effects contained within the monad and therefore strip away the `System` part. Here's an example of how this may look with the IO monad:
```hs
main :: IO ()
main = do
  -- The following will be executed just like how
  -- runSystem would execute one of your systems
  putStrLn "Here's a basic print function!"

  -- The following will NOT be run..
  let funcThatProducesIOIO = pure $ putStrLn "Here's an IO(IO())!"
  foo <- funcThatProducesIOIO

  -- ..until it is called like so
  foo

  -- That was because 'foo' has type IO (), but this
  -- is simply the storage of the monad, not execution

  -- pure wraps the argument inside the monad, leading to
  -- IO (IO ()), which when bound to foo, creates type IO ()
  -- This means that one layer of IO has been executed but
  -- not the second.

  -- This pattern is dumb, binding something just to call it.
  -- This will work instead:

  join funcThatProducesIOIO

```

Hopefully that code above makes some sense. Because the main thread will execute IO in order to actually make something happen with the program, anything that has type `IO ()` will be executed automatically. The final example was something I wanted to mention because of some confusion with monads that a few people have asked me. When we bind something using `<-`, the monad on the right is executed to produce the thing inside it. `IO (String)` would produce a `String`, for instance (this could be reading in from the keyboard?). This means that if the thing produced is yet another monad, like IO (), the thing on the left is not executed, and `join` is a nice shortcut to save you binding and executing as demonstrated above.

Why am I telling you this? Because no matter whether you use SDL or not, you will want to perform IO to make things appear on screen. If you have a system which produces IO, like a render system, then you will need to use `join` in order to execute the side effects contained within the monad you retrieved.

## Easy System Type Synonym

Next up, a simple type synonym. The following will make using Apecs a lot easier:
```hs
-- Easy type synonym for systems
type System' a = System World a
```
Remember, this is a type synonym, it doesn't actually affect your code in any way. It simply saves you from typing and keeps reading type signatures easier. All `System World a` means is that, assuming your world from your `makeWorld` function was named ``"World"``, that this system operates within your world and can make a change in some way.

## Writing the Initialisation System

If we scroll up to our initialisation code, you will see we do:
```hs
-- Do any initialisation here!
runSystem initialise world
```
It is now time to write our `initialise` system. This system is simply going to act as our kickstart to the game, add our player, do things. I'm sure you can guess what your game will need to do to set things up. One final thing I need to mention is that [a System has the following type](https://hackage.haskell.org/package/apecs-0.5.0.0/docs/Apecs-Core.html#t:SystemT): `newtype SystemT w m a `. This may not mean anything at first, but the documentation clearly states that the monad `m` used in a system is in fact compatible with IO. Anything to do with IO can be performed as a part of a system, which may help with debugging.

Lets go! Here's a sample initialisation system:
```hs
-- The type of this function is System' (), so when we open up
-- a 'do' block, we are allowing us to bind the contents of System' monads
-- as well as executing side effects of any other System' () easily.
initialise :: System' ()
initialise = do

  -- Make sure you use liftIO to get the following to compile,
  -- we are returning System' (), not IO (), hence the lift.
  -- Lift is used to lift a function producing a
  -- monad of one type (IO) into another (System'()).
  liftIO $ putStrLn "Let's do some IO just to prove we can!"

  -- Let's initialise the time monad, even though we don't
  -- really have to as Apecs handles that for us.

  -- 'set' produces a System' (), which means this line
  -- will be executed as part of this system.
  set global $ Time 0

  -- Another two System' ()s, this time we're creating entities
  newEntity (Player, Name "Lola", Position $ V2 0 0)
  newEntity (Name "Harold", Position $ V2 0 0)
```

Remember, we're looking at the functions [on the Apecs Hackage docs](https://hackage.haskell.org/package/apecs-0.5.0.0/docs/Apecs-System.html) for all of our inspiration on what functions to use. It is pretty self explanatory when you get the hang of it I promise. Make sure you're looking at the most up to date version of all your documentation.

Honestly, if you understand how these all work, how we compose monads to execute side effects and how Apecs allows us to run these System's from the main thread to play our game, things will get much easier. If not, fear not, you'll get there. Hopefully some more examples will help.

## Writing the Step System

Recall that we call the following during our main game loop:
```hs
-- Update the world
runSystem (step $ fromIntegral dt) world
```

This is our second system. This will be more important than the previous, with this being a collection of every system you require execution of each frame. Notice how we are calling this `runSystem` --- the system we pass to it has some extra parameters inside. When that parameter is evaluated, `step` will accept `dt` and evaluate into a `System' ()` as you'd expect of any programming language in the same way that passing `(1 + 1)` would be equivalent to an `Int`, as the function:
```hs
(+) :: Num a => a -> a -> a
```
has been fully evaluated.

This means that our new system looks similar to the following:

```hs
-- Okay, so we have access to delta time now.
-- This allows us to animate or simulate stuff.
-- Notice how we can execute other systems inside
-- this system with no problems.
step :: Double -> System' ()
step dT = do
  moveCharacters dT
  rootPlayer

-- The function cmap below finds every entity with a Position
-- and rewrites it with new values - adding dT to the x and y values.
-- 'cmap' is another function producing System' ()
moveCharacters :: Double -> System' ()
moveCharacters dT =
  cmap (\(Position (V2 x y)) -> Position $ V2 (x + dT) (y + dT))

-- Apecs has a lot of magic involved, look at this:
rootPlayer :: Double -> System' ()
  cmap (\(Player, Position _) -> Position $ V2 0 0)

-- This iterates on all entities with both a Player component and
-- Position component, however, Player is unique, so this is only
-- applied to the player.

-- This is actually a bit dumb, as we're operating on the player twice,
-- so let's rewrite that first call:

-- Not is an Apecs Data Constructor used for the exclusion of a component.
-- We have to specify the type of Not as GHC cannot infer this on its own.
-- This will operate on entities without a Player component.
moveNonPlayerCharacters :: Double -> System' ()
moveNonPlayerCharacters dT = do
  cmap (\(Position (V2 x y), Not :: Not Player) -> Position $ V2 (x + dT) (y + dT))
```

## Apecs Magic with Systems

In the above examples, the function `cmap` allowed us to read components and overwrite them. There's a lot more to it, and once you get used to the different approaches you'll have to commend Apecs for the amount of work put into it.

Any of the functions in the Apecs documentation that take or write `Components` (or `c`, as the documentation uses types and magic to make things polymorphic) can take a variety of different forms. As you saw above, you can read multiple components as we did with `(Player, Position _)` (note that in that case we used `_` to ignore the parameter, as we were overwriting the position). You can actually write to multiple components too! As long as the components are produced as a tuple, just like the example just given, then you can return multiple components too. Not that ignoring any input components will leave them alone --- we didn't need to write `Player` again to preserve it on the entity.

I'm going to just throw some examples that I can think of at you now. I'll talk about what they do after each segment. I'm going to be calling functions and using Components that we haven't defined, the idea being that these are just to demonstrate different methods of using Apecs.

```hs
everyoneChasePlayer :: System' ()
everyoneChasePlayer = do
  [(Player, Position p)] <- getAll
  cmap (\(Position p', Not :: Not Player) ->
    Position $ moveTowards p' p)
```

Here, using `do` syntax, we can get components attached to the `player` very easily. When there's only one element in the list we can pattern match on it just like a tuple, but this may crash your program if the `Player` does not exist or if you don't grab a unique component and therefore have more than one entity. By grabbing the player's position, we can pass that to a function called `moveTowards` which is in charge of manipulating the first argument so that it gets closer to the second. This function is pure, as `cmap` is the function producing the `System' ()` monad, so you can forget about Apecs and focus on your logic when writing it.

```hs
debugCharacters :: System' ()
debugCharacters = cmapM_ (\(Name n, Entity e) ->
  liftIO $ putStrLn ("Entity " ++ show e ++ " is named: " ++ n))
```

Here's a simple template you can use for debugging. As previously mentioned, `System' ()` supports `MonadIO`, allowing you to perform `IO` inside it. `cmapM` allows us to read components and iterate over them monadically (basically we can treat each iteration as another `System' ()`), which allows us to perform side effects instead of being locked into a pure environment like with the previous example. If we used `do` here, we could even call another system too!

Also, notice how we found the `Entity` here --- whenever we use `getAll` or `cmap` or anything that gets components (other than `get`), we can allocate bindings for the `Entity` these components belong to. `Entity` is just a newtype for `Int`, so `e :: Int` in this case, hence why we `show` it. The `n` is simply a string which doesn't require the use of `show`. If we want to actually use the `Entity`, be sure to wrap the `Int` back in an `Entity` or simply grab the entity as `e :: Entity` rather than pattern matching as we did here.

Finally, the `cmapM_` function (notice the `_`) that we used is the same as `cmapM`, but it doesn't write anything. It's job is to read and provide the a way of monadically iterating. If we wanted to change the name of the entity, using `cmapM` instead would allow us to return something out of the monadic iteration to write the entity with. Here's an example of writing components with `cmapM`:

```hs
applyGravity :: Double -> System' ()]
applyGravity dT = cmapM (\(Position (V2 x y), e :: Entity) -> do
  let gY = y + (dT * 0.5)
      y' = max gY 0
  liftIO $ putStrLn ("Entity " ++ show e ++ " moved from " ++ show y ++ " to " ++ show y')
  pure $ Position (V2 x y'))
```

So here we have an example of using `let` to make calculations easier, along with some `IO`, getting the `Entity` and finally overwriting `Position` components to apply gravity (in SDL and many other frameworks, the top left is `(0,0)`, so positive `y` moves things down).

```hs
fireWeapon :: V2 Double -> Double -> System' ()
fireWeapon (V2 i j) -> speed = do
  [(Player, p :: Position, Weapon ammo, e :: Entity)] <- getAll
  when (ammo > 0) $ void $ do
    let ammoLeft = max 0 $ ammo - 1
    set e (Weapon ammoLeft)
    newEntity (Velocity (i * speed) (j * speed), p)
```

Here's an example of making a new entity while producing `System' ()`. When the side effects of this system are performed, we begin by grabbing the `Entity` with the `Player` component along with some other components. Remember, we need to make sure beforehand the player has these components otherwise the pattern matching will fail, and we can only pattern match here because we know we are only going to have one element in this list.

We then set the `Weapon` component on the player to a new value. The function `set` can be used regardless of if it already exists on the entity or not, and so can be a better option than `cmap`. Additionally, `set` requires you to provide the entity you're modifying, meaning that if we weren't operating on a unique entity then `cmap` wouldn't be as good --- `cmap` iterates over ***every*** entity with the matching components. You have more control with `set`, just make sure that you're not wasting your time when you could use `cmap` or any other Apecs function.

Lastly, notice our use of `when` and `void`. The function `when` is exported by `Control.Monad`, and is pretty much equivalent to the following:
```hs
when' :: Bool -> m () -> m ()
when bool func =
  if bool
    then func
    else pure ()
```
Basically, `when` will do ***nothing*** when it's logic is false. There's the reverse of when too, called `unless`. The other function, `void` will execute execute the side effects of the monad passed to it, but discard the value returned. When we use `newEntity` to create a new Entity, the Entity created is returned as `System' (Entity)`. Since we want this function to return a blank `System' ()`, we need to just ignore what is returned (don't worry, the entity is not deleted) so that the whole thing type checks.

## Writing the Event Handler System

Okay, this section is going to start combining SDL code with Apecs. There are plenty of guides on how to use SDL, so I'm going to show the code first and mention anything important after. Remember that SDL gives you a list of all the events that have occurred since the last poll and you need to handle them all individually. I have not yet implemented any code to check for modifier keys like `shift` or `ctrl`. This will use many of the techniques shown in the previous section so hopefully you can follow along easily.

```hs
-- Global component used for changing gamestates
data GameMode = Standard | Look deriving (Show, Eq)
data GameState = Game GameMode | Interface deriving (Show, Eq)
instance Semigroup GameState where (<>) = mappend
instance Monoid GameState where mempty = Game Standard
instance Component GameState where type Storage GameState = Global GameState

-- Handle the entire event payload
handlePayload :: [EventPayload] -> System' ()
handlePayload = mapM_ handleEvent

-- The main event handler function for dealing with keypresses
handleEvent :: EventPayload -> System' ()
handleEvent (KeyboardEvent ev) = handleKeyEvent ev
handleEvent _ = pure ()

-- For the handling keyboard events only
handleKeyEvent :: KeyboardEventData -> System' ()
handleKeyEvent ev = do
  (state :: GameState) <- get global
  let code = keysymKeycode $ keyboardEventKeysym ev
  case keyboardEventKeyMotion ev of
    Pressed ->
      case state of
        Game mode -> gameAction mode code
        Interface -> postMessage "Interface state not implemented yet"
    Released -> pure ()

-- For keyboard events that  take place in the game
gameAction :: GameMode -> Keycode -> System' ()
gameAction mode k =
  let intents = lookup k defaultGameIntents in
    case mode of
      Standard ->
        case intents of
          Just (Navigate dir) -> navigate dir
          Just ToggleLook -> toggleLook mode
          Just Wait -> do
            postMessage "You wait.."
            playerActionStep 100
          _ -> pure ()
      Look ->
        case intents of
          Just (Navigate dir) -> moveReticule dir
          Just ToggleLook -> toggleLook mode
          _ -> pure ()

-- Initial bindings for intents
defaultGameIntents :: [(Keycode, GameIntent)]
defaultGameIntents =
  [ (KeycodeUp , Navigate C.Up)
  , (KeycodeLeft , Navigate C.Left)
  , (KeycodeDown , Navigate C.Down)
  , (KeycodeRight , Navigate C.Right)
  , (KeycodeSemicolon, ToggleLook)
  , (KeycodeW, Wait)
  ]
```

I first have created a component called `GameState`, maybe that's not the best name. This `Global` component is basically going to help us work out the context of what's on screen. For instance, our inputs on the arrow keys may move the player in game, but on a menu it will do something else. I haven't done any of the interface stuff yet, but I have two different modes for the actual game. I'm making an Roguelike RPG, and one requirement is that the player can look at things without moving and interacting with anything. This component allows me to firstly see whether we're in game or not and secondly, if we are in game, what we are doing. These things are very likely to change to suit whatever game you're making, but I'm hoping for the purposes of this tutorial you can see what I'm getting at.

The `handlePayload` function itself starts by taking the entire payload and performing `mapM_` on it --- `mapM_` is not an Apecs function but rather one that is found in `Control.Monad`. If you were following what the `cmapM_` function did earlier, this might already be clear. This function is very similar except that instead of iterating over Apecs components, it iterates over whatever `Traversable` class you pass it, in this case, a list. We are using the `mapM_` version of map because a normal `map` function doesn't give us the monadic context we need to do this cleanly, whereas `mapM_` allows us to produce a `System' ()` for each element of the list, and the `_` simply discards any data that isn't a side effect, as we aren't using anything. This takes us into the `handleEvent` function, which now only has to worry about a single `SDL.Event`.

The `handleEvent` system I've written does simple pattern matching on the type of event it is given. If the event is a `KeyboardEvent`, it is opened up to expose the event itself and is then passed on to `handleKeyEvent`. I haven't thought about other `SDL.Event`s yet, as our main loop already handles checking for the `QuitEvent`, which means everything I deem important has been handled. Maybe you want something to happen when the screen is resized or if the mouse is clicked, you should go and check out [the docs](https://hackage.haskell.org/package/sdl2-2.4.1.0/docs/SDL-Event.html#t:EventPayload) to find what kind of events you might need, and what data those events contain.

Our `handleKeyEvent`'s job is to direct the flow of the program to where it needs to go next. This is now getting to the point where different games will do different things. My game isn't interested in any key releases, so if the type of `keyboardEventKeyMotion ev` matches `Released` I don't do anything. Otherwise, I call my own function, `gameAction` along with the current mode and the keycode. Remember that anywhere inside any of these functions you can call as many `System' ()` functions as you like to compose the monad. Apecs has allowed us to keep our code clean and healthy while also giving us room to do whatever we like as long as we construct `System' ()` as the output, which as we know, is really easy when you know how.

## Writing the Draw System

Recall that the draw system interacts with the `IO` monad. I've shown already how you can use `liftIO` to execute `IO` from within a `System' ()`, but I've also shown how returning `System (IO ())` can also be used to draw when the `join` function (found in `Control.Monad`) is used. You should have enough information to get a basic drawing system going.

One of the hardest challenges you'll face early on is managing your textures, fonts and other assets. It's easy to create a `Texture` component and give each `Entity` the texture it's going to render with, however, this means that having five entities that show the same thing will have the same texture loaded five times over.

I've created `Global` components for `Textures` and `Fonts` which implement `HashMap`s (found in `Data.HashMap`). A `HashMap` is like a normal map but it runs a lot faster as it isn't ordered. My `draw` system then passes the assets found inside these components to whatever functions need them. You can of course, just use `IO` in the main loop and pass everything to the `draw` system to avoid loading things multiple times, but this would mean having all of your assets loaded at once. Eventually, I plan on scanning all of my `Sprite` components and loading / unloading assets inside the hashmap depending on what textures are required. Here's the current code:

```hs
-- Turns a list of key value pairs into a hashmap for a resource component
createResourceMap :: [(String, a)] -> HM.Map String a
createResourceMap = foldl (\m (k, v) -> insert k v m) empty

-- Types for creating textures
type TexResource = (String, Texture)
type TextureMap = HM.Map String Texture

-- Create a TextureMap with initial filepaths
loadTextures :: Renderer -> [FilePath] -> IO [TexResource]
loadTextures r = traverse getTex
  where getTex p = do
          tex <- loadTexture r p
          pure (p, tex)

-- Types for creating fonts
type FontResource = (String, Font)
type FontMap = HM.Map String Font

-- Create a FontMap using initial filepaths
loadFonts :: [(FilePath, PointSize)] -> IO [FontResource]
loadFonts = traverse getFont
  where getFont (p, s) = do
          font <- load p s
          pure (p, font)

-- Global store of all textures
newtype Textures = Textures TextureMap
instance Component Textures where type Storage Textures = Global Textures
instance Semigroup Textures where (<>) = mappend
instance Monoid Textures where mempty = Textures HM.empty

-- Global store of all fonts
newtype Fonts = Fonts FontMap
instance Component Fonts where type Storage Fonts = Global Fonts
instance Semigroup Fonts where (<>) = mappend
instance Monoid Fonts where mempty = Fonts HM.empty

-- Create System' (IO ()) for everything depending on item drawn
draw :: SDL.Renderer -> Int -> System' (IO ())
draw renderer fps = do
  Textures texs <- get global
  Fonts fonts <- get global
  let uiFont = HM.lookup "Assets/Roboto-Regular.ttf" fonts
  sequence_ <$> sequence
    [ drawComponents $ renderSprite renderer texs
    , drawComponents $ renderReticule renderer
    , displayFps renderer fps uiFont
    ]

-- Produce a system used for drawing
drawComponents :: Get World c => (c -> Position -> IO ()) -> System' (IO ())
drawComponents f = cfold (\img (p, comp) -> img <> f comp p) mempty

-- Render textures
renderSprite :: SDL.Renderer -> TextureMap -> Sprite -> Position -> IO ()
renderSprite r ts (Sprite fp rect) (Position p) =
  case HM.lookup fp ts of
    Just tex -> SDL.copyEx r tex (Just $ toCIntRect rect) (Just (SDL.Rectangle (P $ toCIntV2 p) tileSize')) 0 Nothing (V2 False False)
    _ -> pure ()

-- Render the target reticule
renderReticule :: SDL.Renderer -> Reticule -> Position -> IO ()
renderReticule r (Reticule on) (Position p)
  | not on = pure ()
  | on = do
    rendererDrawColor r $= V4 255 255 255 20
    fillRect r $ Just $ Rectangle (P $ toCIntV2 p) tileSize'

-- Display FPS
displayFps :: SDL.Renderer -> Int -> Maybe SDL.Font.Font -> System' (IO ())
displayFps r fps Nothing = pure $ pure ()
displayFps r fps (Just f) =
  pure $ renderSolidText r f (V4 255 255 255 255) ("FPS: " ++ show fps) (V2 0 0) False

-- Render solid text
renderSolidText :: SDL.Renderer -> SDL.Font.Font -> SDL.Font.Color -> String -> V2 Double -> Bool -> IO ()
renderSolidText r fo c s p = renderText r fo (SDL.Font.solid fo) c s (toCIntV2 p)

-- Render text to the screen easily
-- renderSolidText calls this
renderText :: SDL.Renderer -> SDL.Font.Font -> (SDL.Font.Color -> Data.Text.Text -> IO SDL.Surface) ->
           SDL.Font.Color -> String -> V2 CInt -> Bool -> IO ()
renderText r fo fu c t (V2 x y) center = do
  let text = Data.Text.pack t
  surface <- fu c text
  texture <- SDL.createTextureFromSurface r surface
  SDL.freeSurface surface
  fontSize <- SDL.Font.size fo text
  let (w, h) = (fromIntegral *** fromIntegral) fontSize
  unless center $
    SDL.copy r texture Nothing (Just (Rectangle (P $ V2 x y) (V2 w h)))
  when center $ do
    let x' = x - fromIntegral (fst fontSize `div` 2)
    SDL.copy r texture Nothing (Just (Rectangle (P $ V2 x' y) (V2 w h)))
  SDL.destroyTexture texture
```

:::{.warning header="Rough Draft"}
This code is just a rough draft just so that I can see things on screen. While I recommend beginners using this so that they can start having fun with Haskell, I ***deeply encourage*** you to make optimisations wherever you can and tailor everything to your game. The `renderText` function is severely inefficient and so I'm going to be trying to batch all of the text onto one surface before creating a texture out of it. This code is ***not good***.
:::

Oh my gosh, what a lot of code. I'm sorry for just dumping so much, but at the same time there's no better way to illustrate how I try to do things. This might be pretty complex and hard to read for the beginners reading this, but all I can say is that you should begin by looking at the function types for each block of code and understanding how the functions use each other. Most importantly, see how we go from producing `IO ()` in `renderText` and `renderSolidText` to producing `System' (IO ())` when bundled with `displayFPS` or `drawComponents`. The `drawComponents` function simply uses `cfold` to combine all of the `IO ()` monads into one, before wrapping `System' ()` around it like all the other Apecs functions do.

# Conclusion

I'm sorry for the huge amount of code pastes, but hopefully there's at least something written in this blog post that will teach someone something. My real intent is to encourage the usage of Haskell for the creation of small games where we no longer need performance as much as we used to. Hopefully this inspires of more programmers and more frameworks to use Haskell so that writing in a functional language becomes less of an alien idea. I want to see indie developers start embracing the power of high-level languages to make development easier on their already-difficult lives. If people can make [cool games in JavaScript](https://store.steampowered.com/app/497800/Golden_Krone_Hotel/), then why not Haskell?
