---
title: Making Notakto in Haskell with Apecs and Raylib
date: 2022-11-13
subtitle: My return to the Apecs, 4 years later
description: My previous post on Apecs was very well received, but I've seen a lot of people using the now-out-of-date code and somewhat struggling. Time to dive back in!
tags:
  - Haskell
  - Functional
  - Raylib
image: /assets/images/logo-splash.png
---

# Previously on Apecs

Roughly 4 years ago, I wrote the post [An introduction to game development to game development in Haskell using Apecs](/blog/making-a-game-with-haskell-and-apecs/). I like to think it was one of my most well-received blog posts [considering it is featured on the Apecs repository itself](https://github.com/jonascarpay/apecs#links), as well as being a common topic across emails and communications I receive.

[Apecs](https://hackage.haskell.org/package/apecs) is a 'fast [Entity-Component-System](https://en.wikipedia.org/wiki/Entity_component_system) library for game programming' written in [Haskell](https://www.haskell.org/). It is by far one of the best ECS implementations I've used and also happens to be my favourite way of structuring games when using Haskell.

However, enough time has passed (4 years!) that some of the tricks and methods used in that blog post are old enough to confuse beginners. Since that was never my intention, I thought I'd make a new blog post about it!

# About this post

This post is going to be a little bit different. Whereas normally I'd finish a project and write a post about it, in this post I want to write it as I go along! Each time I finish implement something substantial, I'm going to add to this post so that I can really capture more of the development process.

This may have a few side-effects, however. Firstly, code we write near the beginning may change towards the end. If I make a simple mistake (such as a syntax improvement), I'll simply swap it out for the superior. But I want the big decisions I make to be written about so that if I later decide against it I can articulate properly why in the post.

My blog has support for both projects and blog posts; this post is a blog post since it is capturing a moment in time, like a photograph. The associated project for this page will be the formal write-up for how the game works, but this will act more as a portfolio piece than as a tutorial. This is a bit of an experiment for me, so please let me know how you think it goes!

# Introduction

## What is the ECS pattern?
As the name suggests, the ECS pattern is divided into three parts: entities, components and systems. These parts work together to form the foundations of your game's architecture.

* **Entity ---** Alone, an entity is just a unique identifier, such as a simple integer value. They aren't very useful by themselves!

* **Component ---** Data, essentially. Anything that has data you need to store between frames, such as position, velocity, health or gold. Each component is attached to an entity in some way --- some games have just a big database where an entity is just an integer used as a look-up key, whereas others (who probably would be using [object oriented programming](https://en.wikipedia.org/wiki/Object-oriented_programming)) might compose component lists within the entity class itself.

* **System ---** The magic part that a lot of people miss out on! A system is simple: it iterates through entities and updates their components in some way. A player movement system might cherry-pick the player's entity and update the velocity component with respect to whatever keys are held on the keyboard; a movement system would then iterate through *all* entities with positions and velocities and update their positions accordingly. If you can break the logic of your game into systems then things become a lot simpler and safer.

It was through ECS that I began to understand how modern game engines work (no thanks to you, university!) since having a greater appreciation for how games can be architected gave me an insight that couldn't be taught through anything other experience. That isn't to say that game engines all use ECS, in fact most of them don't since they want to write their own systems (rendering, physics, scripting etc) that *you* then use and may or may not customise. Also, they probably don't implement entities and components in the same way; both [Unity](https://unity.com/) and [Unreal](https://www.unrealengine.com/en-US) engines allow for logic to be placed in their components whereas the ECS pattern encourages components to be purely data.

## What is Raylib?

One thing we haven't mentioned yet is that this project will be using [Raylib](https://www.raylib.com/) for the rendering side of things. I've always wanted to learn more about Raylib, and my chance came when I found the [Haskell bindings](https://hackage.haskell.org/package/h-raylib) released recently.

We won't spend too much time talking about Raylib, in fact I'm using it purely because it's a really easy way to get things on-screen. The documentation style surprises me a bit, with a preference for documenting the source code rather than an online version of the API. That said, for our purposes the [cheatsheet](https://www.raylib.com/cheatsheet/cheatsheet.html) is an excellent resource for just understanding what functions Raylib provides us with and what arguments they take.

## What is Notakto?

[Notakto](https://en.wikipedia.org/wiki/Notakto) is a variant of tic-tac-toe in which both players play using 'crosses' as their markers across multiple tic-tac-toe boards. Three crosses in a row on any board will 'kill' it, meaning that the board can no longer be played on. When there is only one board remaining, the player who kills it is considered the loser, meaning that to win you have to get yourself in a position where you force the other person to get three-in-a-row on the final board. It is still a solveable game like regular tic-tac-toe, but the stratagies involve enough effort that you will probably not find many people who can figure them out on their own.

Why did I chose to make Notakto? There's a beauty in making a game where the rules are already well-defined since there is already a definition of 'done' --- I know what I'm working towards and the project has scope, a perfect scenario for an experimental tutorial!

And with that, it's 10:00 am, let's begin our journey!

# Getting started

## Initial commit

:::{.gitrepo header="Notakto"}
I will be committing as I go, so even though this blog post may be revised for cleaner reading, the repository will always tell the full story. Each section will have a permalink to the commit I was at so that you can see how the project evolves. I won't be detailing every single change in this blog post, so if you really are following along you may need to check the repository and fill the blanks in yourself.

A link to the repository's first commit can be found [here](https://github.com/Ashe/Notakto/tree/0852350c18071b6332899639055d3c38c1976963).
:::

:::{.figure
  image="https://res.cloudinary.com/aas-sh/image/upload/v1668334893/blog/2022/11/13-11-2022_10_20_31_ywlsij.png"
  caption="The folder structure of the initial commit."
  source="Notakto"
  sourceUrl="https://github.com/Ashe/Notakto/tree/0852350c18071b6332899639055d3c38c1976963"
}
:::

Here we go! I've just pushed an initial commit with my standard Haskell bits and bobs. I always like making a library and separating executable-only code from the game itself, meaning that if we wanted multiple executables that handle the game setup in different ways (i.e. terminal or GUI) then we can. I've also got a testing folder, maybe we'll write some tests as we go, who knows?

You may also notice that I am using [Nix](https://nixos.org/) to build and run this project, so you should just be able to use `nix run` to run the project at any point in time. If you want to work in this repository, you can use `nix develop` to get your development environment set up.

At this point in time, running the project should print `Hello, Notakto`.

## Introducing libraries

:::{.gitrepo header="Notakto"}
A link to the repository's commit for the following section can be found [here](https://github.com/Ashe/Notakto/tree/0852350c18071b6332899639055d3c38c1976963).
:::

After setting up the repository, my next step with any project is getting a motified example running to demonstrate that things are working correctly. Lets begin by updating our cabal file to include the `h-raylib` and `apecs`.

```cabal
library
  exposed-modules:  Lib
  hs-source-dirs:   src/lib
  build-depends:
    base,
    apecs,
    h-raylib
  default-language: Haskell2010
```

## Single file example

### Imports

Now we can try and use our dependencies by modifying an example! Thankfully, `h-raylib` supplies us with a [first-person-camera example](https://github.com/Anut-py/h-raylib/tree/606936336922dea13517abf4d136f17b162efcc1/examples/first-person-camera), and I already have some Apecs examples from the [project](https://github.com/Ashe/HSRogue/) featured in my [previous post](https://aas.sh/blog/making-a-game-with-haskell-and-apecs/)!

```hs
{-# OPTIONS -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Lib (main) where

import Control.Monad (unless)

import Apecs

import qualified Raylib as RL
import qualified Raylib.Colors as RL
import qualified Raylib.Constants as RL
import qualified Raylib.Types as RL
import Raylib.Types (Vector3 (..))
```

Here is how our `Lib` module is setup now. We're still working in one file to keep it simple, so it's okay for things to be messy.

* I use the `-Wall` language option just to make sure we're ironing out warnings we go --- personal prefence.
* The language extensions you see are all used by Apecs (or at least, the example made use of them). I know for a fact that `TemplateHaskell` is used as an easy way of building a `World` type for use throughout the application.
* `Lib` only needs to export `main` for now, but if we ever want to export a configuration class that our client can provide then this is where we'd put it.
* I try to structure my includes with my 'Haskell libraries' at the top (i.e. things that aren't specific to my project), followed by blocks of imports for my dependencies:
  * `Apecs` has quite a nice API; it doesn't get messy if you just import it in its entirety and it almost feels like it's part of the language.
  * Raylib has a *lot* of things going on (again, refer to the [cheatsheet](https://www.raylib.com/cheatsheet/cheatsheet.html), so I've `qualified` it. Normally I'd explicitly write every function I use, however for this tutorial I'm going to use qualifications so that you can easily tell when something is a Raylib thing and when it's something else.
    * I made an exception for `Raylib.Types` --- certain types are quite commonly used throughout the project and so rather than typing `RL.Vector3` constantly I instead explicitly imported it so that we can use it without restraint.

### Creating and initialising a World

Next up is creating our `World`. In Apecs, the `World` is a type built specifically for your components to live in; that's why `TemplateHaskell` is used to automatically write the code that glues your components together. The `initWorld` function is also a result of this template, which is why you might find it hard to find the `World` type and `initWorld' in the [`apecs` documentation](https://hackage.haskell.org/package/apecs).

```hs
-- Define a component, a Camera
newtype Camera = Camera RL.Camera3D

-- Create a world featuring the lonely Camera component
makeWorldAndComponents "World" [''Camera]

-- Initialise our world in the main function, and give it to our game's systems
main :: IO ()
main = initWorld >>= runSystem (initialise >> run >> terminate)
```

So, what are the `initialise`, `run` and `terminate` systems? Well, they are just functions with the type `System World ()`!

* `System` is an Apecs type defining a system, one of the parts of the ECS pattern. Note that `System w a` is mapped to `SystemT w IO a` under the hood; it's just a convenience type.
* `World` is our world type created by template haskell in the above snippet.
* `()` is the absence of type; we aren't expecting these systems to return anything, so these particular functions are only for executing side effects.

Let's first look at `initialise`:

```hs
-- Simple system, doesn't return anything
initialise :: System World ()

-- We're in the 'System' monad, use 'do' to compose side effects
initialise = do

  -- Define a Raylib 3D perspective camera and name it 'camera'
  let camera = RL.Camera3D (Vector3 0 1 0) (Vector3 2 1 1) (Vector3 0 1 0) 70
      RL.cameraProjection'perspective

  -- Create a global entity with our camera component
  -- 'set' is an apecs function for setting a component's state on a given entity
  -- 'global' refers to the singular and unique global entity of the game
  -- 'Camera' refers to the constructor for our component which contains a RL.3DCamera
  set global $ Camera camera

  -- The 'System' monad has IO, remember 'System w a = SystemT w IO a'!
  liftIO $ do

    -- Now we can compose side effects for IO, which is what h-raylib uses
    RL.initWindow 1920 1080 "App"
    RL.setTargetFPS 60
    RL.setCameraMode camera RL.cameraMode'firstPerson
```

The biggest takeaway from the above snippet is to remember that just because we're in Apecs-land doesn't mean that we're constrained to only using Apecs functions. `System` is a `type` constructor for `SystemT`, which `apecs` documentation explains:

> A `SystemT` is a newtype around `ReaderT w m a`, where `w` is the game world variable. Systems serve to:
>
> * Allow type-based lookup of a component's store through `getStore`.
> * Lift side effects into their host Monad.

We can do `IO` 🎉 Before we continue though, let's look into `terminate`, since it's good practice to always write in programming to always write `delete` where there's `new`, or a *destructor* whenever you write a *constructor*. We have made our window, so let's handle closing it before we forget:

```hs
-- When terminate is called, just close the window
terminate :: System World ()
terminate = liftIO RL.closeWindow
```

This is a short and sweet one!

:::{.help header="Why didn't we just manage the window in main?"}
The question on your mind might be *"why did we need to do this in a `System`?"* The answer to that lies in the names: `initialise` and `terminate`. Yes, the Raylib specific stuff could be done purely in `IO` without Apecs getting involved, but this is constraining.

What if we want to store the want to load and save data using a file when the application opens and closes? What if we need to correct the state of the game before we enter the game loop, or after it's concluded? So long as we're in a `System`, we have access to all the components in the `World`; exiting back into `IO` removes this ability and so I prefer to use `liftIO` within `System` to get the best of both worlds.
:::

### Updating and rendering

We aren't quite done with our single-file example since we have one remaining undefined function: `run`. Let's dive in:

```hs
run :: System World ()
run = do
  update
  render
  shouldClose <- liftIO RL.windowShouldClose
  unless shouldClose run
```

Hang on a moment, this is a **game loop**! So in `main`, we had `initialise >> run >> terminate`, you can now see that the reason the program doesn't terminate immediately is because `run` is hogging the thread and infinitely looping until told otherwise!

So what are `update` and `render`? Well, when you are in a monad be it `IO` or `System`, you can easily call functions of the same monadic type to compose side effects. You could pretty much substitute `update` and `render` for their contents and everything will work the same; this is a way of breaking things up. I like updating the game and then rendering the result. We can split these two steps into as many more steps as we need, but for our single file example they are singular systems that just handle the entire game.

```hs
-- Simple system
update :: System World ()
update = do

  -- Retrieve the camera component from the global entity (c is a RL.Camera3D)
  Camera c <- get global

  -- Update the camera and store the updated version in c'
  -- Also note the use of liftIO to dip into the IO monad to allow the use of raylib
  c' <- liftIO $ RL.updateCamera c

  -- Replace the global entity's Camera with a new one containing the updated camera
  set global $ Camera c'
```

I was a bit alarmed at first at how small this function is --- where is the input handling? Where's the movement speed definitions and all the other things we expect to see in a first-person game? Well it looks like Raylib is has some very plug-and-play style functions, which is nice to see when playing around. I'm sure anyone looking to make an FPS will be able to roll their own movement system, but for us we'll be removing this pretty quickly since funnily enough Notakto is not a competitor to [Call of Duty](https://www.callofduty.com/uk/en/).

```hs
render :: System World ()
render = do
  Camera camera <- get global
  liftIO $ do
    RL.beginDrawing
    RL.clearBackground RL.black
    RL.drawFPS 10 20
    RL.beginMode3D camera
    RL.drawGrid 10 1
    RL.drawCircle3D (Vector3 2 1 1) 2 (Vector3 0 1 0) 0 RL.white
    RL.drawLine3D (Vector3 3 0 1) (Vector3 1 2 1) RL.white
    RL.drawLine3D (Vector3 3 2 1) (Vector3 1 0 1) RL.white
    RL.drawCubeWiresV (Vector3 (-2) 1 0) (Vector3 1 2 1) RL.white
    RL.endMode3D
    RL.endDrawing
```

The `render` system is very straightforward. We grab our camera out of our Apecs `World` and then use it in the `IO` monad to help Raylib render the world. I won't go into much detail here.

After what feels like an eternity (it has taken me 2 hours to write this!), here's our single file example up and running! Time for a toilet break and a glass of water!

:::{.figure
  image="https://res.cloudinary.com/aas-sh/image/upload/v1668335749/blog/2022/11/13-11-2022_10_35_33_e1xmdy.png"
  caption="Screenshot of the single file example running"
  source="Notakto"
  sourceUrl="https://github.com/Ashe/Notakto/tree/dd26d756904358cc907a8db11ae66392f45bfa96"
}
:::
