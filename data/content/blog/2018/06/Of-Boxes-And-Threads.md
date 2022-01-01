---
title: "Of Boxes and Threads: Game development in Haskell"
date: 2018-06-09
subtitle: A discussion about games development in Haskell.
description: My experiences of making a game in Haskell and how I think it's is a pioneering process. Despite a wiki and subreddit dedicated to gamedev in Haskell, not many people have actually succeeded making anything close to current games.
categories: 
  - Functional Programming
tags: 
  - Game
  - Haskell
  - Functional
image: https://raw.githubusercontent.com/Ashe/FirstGameHS/master/img/preview.gif
project: hsrogue
status: published
---

# What is this post really about?

Making a game in Haskell is a pioneering process. Despite the fact that there's a page on the [wiki](https://wiki.haskell.org/Game_Development) and a full [subreddit](https://www.reddit.com/r/haskellgamedev/) dedicated to the purpose of making a game in this beautiful language, not many people have actually succeeded making anything close to what current game developers can already achieve. I hope to try and communicate my experiences in this post.

The most exciting thing I saw regarding Haskell in games development was [Wayward Tide](https://blog.chucklefish.org/wayward-tide-technical-details/), a game made by [Chucklefish](https://blog.chucklefish.org/) that was made with Haskell. Unfortunately, the project appears to have been shelved, already giving off bad vibes around the viability of Haskell as a games language. There's also [Keera Studios](http://keera.co.uk/blog/) who make games and solutions in Haskell, however in my honest opinion I don't believe there's enough there to prove anything regarding the power of Haskell in games.

Another thing that was exciting was [John Carmack's keynote at Quakecon 2013](https://www.youtube.com/watch?v=1PhArSujR_A). [John Carmack](https://en.wikipedia.org/wiki/John_Carmack) is the co-founder of [id Software](https://en.wikipedia.org/wiki/Id_Software) and was the lead programmer for games like Doom and Quake. When someone this important starts talking about functional programming in such a positive way, it does fire up. We have all read about the payoffs of using Haskell and so this validation feels great. I've embedded the keynote below just in case you missed the link I provided, it really is insightful.

:::{.caption .w-full
  caption="John Carmack uses Wolfenstein the way normal people use Hello World."
  source="John Carmack's keynote at Quakecon, YouTube"
  sourceUrl="https://www.youtube.com/watch?v=1PhArSujR_A&feature=emb_title"
}
<iframe src="https://www.youtube.com/embed/1PhArSujR_A" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>
:::

So we have a few examples at best of what Haskell can do for the games industry, but the biggest question still remains at large: ***Where is everyone?*** There are lots of libraries out there for games development in Haskell, such as [Apecs](https://github.com/jonascarpay/apecs). Why haven't more people made games? We have bindings to [SDL2](https://www.libsdl.org/download-2.0.php) already available and lots of experienced programmers to help each other out.

# My experience

Unsatisfied with not knowing and understanding the reasons behind the lack of development, I challenged myself to learn through doing. I tried converting my knowledge of how to make a game in using a framework into one compatible with Haskell. It was hard, but I was always convincing myself that the payoff would be fantastic. Naturally, using a high level language will result in less performance than that using something like C++, but once cracked, I believe that development will become significantly easier and more rewarding. That really pushed me to work on my game.

:::{.gitrepo header="FirstGameHS"}
I will be using the commit history from my project [FirstGameHS](https://github.com/ashe/FirstGameHS) for the next few sections. Feel free to go through my commits and laugh at my mistakes as a beginner. Just be sure to remember that the take away here is that these were my thoughts coming from imperative games development, and if we're going to make functional games development a viable thing then we need to stamp out these habits and iron the creases to get everyone thinking correctly when they enter this world.
:::

I started small, I went through [Lazyfoo's SDL tutorials](http://lazyfoo.net/tutorials/SDL/) and tried to recall how every step was different. At this stage, the game loop looked very simple and didn't contain anything interesting, so I immediately sought after something better. [This commit](https://github.com/Crysikrend/FirstGameHS/commit/380789362919b55b024141fbcda935796700bd9c) was when things started becoming clearer. This commit contained a rectangle that could be controlled with the keyboard. Let's dive in and take a look at some things.

```hs
-- This is our game world. It only consists of one lonely guy
-- who has a position and a velocity
data World
    = Guy
    { position :: Point V2 CDouble
    , velocity :: V2 CDouble
    } deriving (Show, Eq)

-- Our initial world starts out with the guy roughly in the middle
initialGuy :: World
initialGuy =
    Guy
    { position = P $ V2 (fromIntegral screenWidth / 2) (fromIntegral $ screenHeight - 100)
    , velocity = V2 0 0
    }
```

Forgive me for not providing complete snippets, but you can guess what exactly `screenWidth` means without me having to show the code. So in this area of our code, we begin to define the `World` of our game. Because there's only one thing changing, instead of `World` being some sort of collection it is more of an alias for our character, `Guy`. When `initialGuy` is used to create a `Guy`, he is placed in the middle of the screen with no velocity, simple.

```hs
processInput :: World -> SDL.EventPayload -> World
processInput world@(Guy _ curVel) (KeyPressed SDL.KeycodeRight) =
    world { velocity = walkingSpeed + curVel  }
processInput world@(Guy _ curVel) (KeyReleased SDL.KeycodeRight) =
    world { velocity = curVel - walkingSpeed  }
processInput w _ = w
```

Here's how we are going to control our character. I have only posted a little portion of the full code which focuses on the right arrow key, but all the directions are covered. All we say here is that when you press a key, you add velocity in that direction, and when you release it the opposite happens. This function is going to be used when iterating through the payload of SDL events, meaning that everything that has happened since the last frame will go through this function, which will then check on whether the current event is relevant. If the player presses both the left and right arrow keys down, the velocity will cancel out, leaving you with either 0 or a constant velocity (`currentVelocity + walkingVelocity - walkingVelocity = currentVelocity`). Nothing out of the ordinary here. Let's look at something more interesting.

```hs
-- This function takes cares of applying things like our entities' velocities
-- to their positions, as well as
updateWorld :: CDouble -> World -> World
updateWorld delta (Guy (P pos) vel) =
    let (V2 newPosX newPosY) = pos + (gravity + vel) * V2 delta delta
        -- Ensure that we stay within bounds
        fixedX = max 0 $ min newPosX (fromIntegral screenWidth - 50)
        fixedY = max 0 $ min (fromIntegral screenHeight - 100) newPosY
    in Guy (P $ V2 fixedX fixedY) vel
```

Okay, here's something more intriguing. This function takes a `CDouble` and our what will evaluate to our `Guy` and it will produce another `Guy`. This part is very important, as it's how games in Haskell are going to work. In C++, state is held whether you like it or not. Every object in an [Object Oriented](https://en.wikipedia.org/wiki/Object-oriented_programming) environment will have a state. In Haskell, however, everything is just a transformation. This function takes ***a*** `Guy` and produced ***a*** `Guy`. I emphasise the ***a*** here as this function could not care less about the fact this is all a game. This function is the thing that links the current frame to the next, and everything that is going to happen to `Guy` happens here. Right now, we simply use the `velocity` value within `Guy` to manipulate his `position`. It then makes sure to keep him inside an arbitrary area.

```hs
let loop last world = do
      events <- SDL.pollEvents

      -- Need to calculate the time delta
      now <- SDL.getPerformanceCounter
      freq <- SDL.getPerformanceFrequency

      let delta = (fromIntegral now - fromIntegral last) * 1000 / fromIntegral freq
          payloads = map SDL.eventPayload events
          quit = SDL.QuitEvent `elem` payloads

      -- Update functions
      let worldAfterInput = foldl' processInput world payloads
          newWorld        = updateWorld delta worldAfterInput

      SDL.clear renderer

      -- Render functions
      SDL.copy renderer texture Nothing Nothing
      -- Draw our world(guy) as a white rectangle
      let drawColor = SDL.rendererDrawColor renderer
      drawColor $= V4 255 255 255 0
      SDL.fillRect renderer . Just $ SDL.Rectangle (truncate <$> position newWorld) (V2 50 100)

      -- My attempt at an FPS limit. I don't write games so it is possible this is incorrect
      let frameDelay = 1000 / fromIntegral frameLimit
      when (delta < frameDelay) $ SDL.delay (truncate $ frameDelay - delta)

      SDL.present renderer
      unless quit $ loop now newWorld

  now <- SDL.getPerformanceCounter
  loop now initialGuy

  SDL.destroyWindow window
  SDL.quit
  ```

Finally, here's the game loop. We get an event payload and we work out how long it has been since the previous frame just like we would anywhere else. Let's look into `newWorld` though. Remember that `loop` isn't a keyword like `for` or `while`, we are simply defining a function called `loop` that takes some timing data in the variable `last` and our `Guy` in the variable `world`. Here, `newWorld` is what we call the thing that `world` transforms into after using our `processInput` and `updateWorld` functions as we have already shown. We then render things as normal below.

Looking at this game loop, lots of things strike me as challenges. Firstly, everything is in one file, and so we will need a reliable way for data such as `Guy` to exist in it's own module and to be rounded up when needed. This is a challenge every game will need to solve in any language. Secondly, the managing of a state becomes very important. Recall that `Guy` is the only thing in our `World` right now, which is why they are the same thing. Later, `World` would contain everything, and so `updateWorld` will have to change to accommodate everything. Going back to the first point, this would mean that `updateWorld` will need to transform all of it's internal data into new data (`Guy` -> `Guy`), and `Guy` would then have to implement his own function `updateGuy`. This sounds okay, as we could export `updateGuy` from the module, and then our `Main.hs` file will import it and call it whenever a `Guy` needs updating every frame. The important thing to remember is that if something changes between frames, it will need to go inside of `World` and it will need to be managed in some way.

# Of Boxes and Threads

So what is with the edgy title? After the final paragraph of the last section, I think it's time to get to the meat of this post. What exactly do I mean by boxes and threads in this context? Beginner programmers tend to imagine a variable as a 'box' with a number inside of it. A number then becomes a piece of data, and data then becomes anything the programming language is capable of evaluating --- functions, classes, pointers, files, days of the week, bools. If you look at the gameloop above though, the value of `newWorld` is only valid for a single frame. While in an imperative language like C++, you could argue that the value of a variable exists in the same way, but I'm trying to describe this a little differently. In C++, if you create a variable, it sticks around until it falls out of scope. This happens when the current code block ends, such as a function or loop. It is true that variables go out of scope in Haskell too, but every function is just a way of transforming data continuously, it doesn't stick around, it ***can't*** (this isn't true, but for simplicity we'll say it is).

To hopefully illustrate what I'm talking about, I made two diagrams. Don't take them literally.

:::{.figure
  image="https://res.cloudinary.com/aas-sh/image/upload/v1617292470/blog/2018/06/traditional_game_structure_o3qxgm.png"
  caption="Typical structure of a game in an imperative language."
  source="I drew this haha"
}
:::

Here's how I'm going to interpret the way traditional, imperative game structures work. In an Object oriented system, you'd probably have a game controller class containing everything in the game in some sort of structure. If you are using an [Entity Component System](https://en.wikipedia.org/wiki/Entity%E2%80%93component%E2%80%93system), you will simply have a list of `GameObjects` or something. These `GameObjects` will contain data for your game which you will manipulate and then render as part of your game loop.

Let's imagine that everything you use inside your game is placed inside of a ***box***. You create the data, you put it into a box. You put that box into a collection of boxes. As long as this controller stays in scope, these boxes will exist and any `GameObjects` can manipulate any other as long as functions are made public blah blah blah. *The object itself doesn't need to know about all possibilities, it only needs to export what it can do, and how it should manipulate the current object*.

This might make more sense if I just insert the next diagram now, so you can compare:

:::{.figure
  image="https://res.cloudinary.com/aas-sh/image/upload/v1617292516/blog/2018/06/functional_game_structure_jxohms.png"
  caption="Sample structure of a game in a functional language."
  source="I drew this haha"
}
:::

Yes, we could have bundled up the data in the first diagram and the two would be extremely similar. What I'm trying to get at with the second diagram, is that the data travels ***with*** the flow of the program, whereas in imperative programming they exist in ***boxes*** irrespective of what happens in the game loop as long as something holds them. I imagine Haskell programs as a bundle of threads --- where we can pack and unpack boxes of data, we can also inter-twine, unravel and tie threads together. These threads run parallel (don't get confused with parallel computing!) with the flow of the game as they travel together, twisting, weaving and separating with each iteration. The boxes exist outside the loop and can interact *and* be interacted with. Threads, however, do not --- any threads that aren't passed to the next iteration are cut off and terminated, ***all*** data must be accounted for in the transition to the next frame of the game.

While it may sound very inefficient passing your entire game around with functions every single frame, Haskell is actually designed to handle this and it isn't as bad as you think. Also, because the data here isn't somewhere in memory but is simply an echo of the initial state twisted continuously, multiple copies are made with every function call and so you can parallelise (yes, [parallel computing](https://en.wikipedia.org/wiki/Parallel_computing)!) very easily. I won't go into this as the information is out there.

# Difficulties

Drawing diagrams is fun and all, but it's time to talk about why I'm exactly writing this. After I got to grips with how state is managed in Haskell, I started separating my code into modules like every good programmer should. [This commit](https://github.com/Ashe/FirstGameHS/commit/7bcb2a0114fba1e2b0a491e3137371a1bdb6a1dc) was the next stepping stone for me, and looking at [`Main.hs`](https://github.com/Crysikrend/FirstGameHS/blob/7bcb2a0114fba1e2b0a491e3137371a1bdb6a1dc/src/Main.hs) will show how the loop has changed. I now have a [`GameState`](https://github.com/Crysikrend/FirstGameHS/blob/7bcb2a0114fba1e2b0a491e3137371a1bdb6a1dc/src/GameState.hs) which actually contains a *list* of [`Guy`](https://github.com/Crysikrend/FirstGameHS/blob/7bcb2a0114fba1e2b0a491e3137371a1bdb6a1dc/src/Guy.hs)s. The `GameState` is also a nice way of streamlining the calculations for `deltaTime` too, as that's another element passed between frames, so why not put it into the state itself?

```hs
-- Updates the game state's entities
updateGameState :: GameState -> CDouble -> GameState
updateGameState state delta =
  state
  { deltaTime = delta
  , elapsedTime = elapsedTime state + delta
  , entities = map (\(Entity _ up _) -> up state) (entities state)
  }

updateGuy :: Guy -> GameState -> Guy
updateGuy g st =
  g
  { position =
    let (P pos) = position g
        res = screenRes $ options $ st
        (V2 newPosX newPosY) = (pos + velocity g) * V2 dt dt
        fixedX = max 0 $ min newPosX (fromIntegral (fst res) - 50)
        fixedY = max 0 $ min (fromIntegral (snd res) - 100) newPosY
     in P $ V2 fixedX fixedY
  , animation = updateAnimationState dt 0.1 (animation g)
  }
  where dt = deltaTime st
```

Every frame, each guy is updated using [`updateGameState`](https://github.com/Crysikrend/FirstGameHS/blob/7bcb2a0114fba1e2b0a491e3137371a1bdb6a1dc/src/GameState.hs#L79) which transforms the `GameState` into a new, updated `GameState` by also fmapping [`updateGuy`](https://github.com/Crysikrend/FirstGameHS/blob/7bcb2a0114fba1e2b0a491e3137371a1bdb6a1dc/src/Guy.hs#L38) onto every element inside the list of `Guy`s. This sounds really tidy, especially when all the code for `Guy` is in his module and out the way (even the function to render him). The problem is that for something to change within our game, our function signature needs to look similar to `ClassToChange -> a -> ClassToChange`, where `ClassToChange` here is a `Guy`. Another object will struggle to manipulate a guy in our collection in some way, as all of these functions are simply *transformations*. Unless you plan on transforming another class into a `Guy`, interacting with things inside the collection is much harder than it was with imperative structures.

If items inside this collection cannot manipulate other items, then how do we get anything done? The solution is to *weave* all of the different things that would change your class into the update function. For instance, our `Guy` takes a `CDouble` that is `deltaTime`. Instead, we could pass the entirety of the `GameState` which contains `deltaTime` anyway. `Guy` can then sample data inside this state and make decisions on how to change based on all of this data. Because data is passed by copy, you can do whatever you like to it to make operating with `Guy`s easier, without any worries of impacting the original data or the performance of your game.

For simple games, this is fine. However, any complex RPG will be tough to code when you have to imagine how to feed in this data. This get's even worse when you want to make exceptions (such as when coding cutscenes or tutorials), as you then have to figure out a way of streamlining these to make things easier. This was definitely my biggest struggle as I couldn't get my head around how I should go about doing this in a sensible way.

:::{.note header="Making a game in Haskell is not impossible"}
Please note that I'm not saying that making a game in Haskell is impossible, I'm simply saying that there are indeed challenges to someone not used to this style of programming. I wasn't able to completely understand everything, and these are the challenges that were presented to me. I am a true believer of Haskell being a great language to work with, and I really want someone to break through these walls.
:::

# Functional Reactive Programming

So I started looking into having dynamic data values to try and solve the challenges above. I found [FRP, or Functional Reactive Programming](https://wiki.haskell.org/Functional_Reactive_Programming) and while I'm still unsure on whether it could be used to overcome this challenge, it was definitely something new for me to learn. I started with [Reactive Banana](https://github.com/HeinrichApfelmus/reactive-banana) in [this commit](https://github.com/Crysikrend/FirstGameHS/commit/c2c39b2060acbde2a9aa842b25ec824a85c71e07), but later I moved on to using [Reflex FRP](https://github.com/reflex-frp/reflex) in [this commit](https://github.com/Crysikrend/FirstGameHS/commit/cbfef5323c8da3c9ec0fc0f9f9ea844db765eda9) --- note that I moved my logic into [`Game.hs`](https://github.com/Crysikrend/FirstGameHS/blob/cbfef5323c8da3c9ec0fc0f9f9ea844db765eda9/src/Game.hs) to keep things tidy.

I'm not going to go into the details of FRP, but the gist is this: In Functional Programming, calling a function with the same arguments will always yield the same output. FRP grants access to a set of data types typically called `Behaviours` and `Events` (and sometimes `Dynamics`) that have values that *vary over time*. To put it simply, imagine these variables to be simply a `Data.Map` with a time value as a key and the value as the type you want. This means that you can use these new types to create functions that *fire* when an `Event` is triggered and that can *sample* `Behaviours` at any point in time to get a value. A good example would be setting the location of the character to be the same position of the mouse whenever the mouse moves, or changing the orientation of an enemy's gaze to face the player whenever the player's position value has changed.

FRP does make things a little more complex though. If you look at [Reflex's quick reference sheet](https://github.com/reflex-frp/reflex/blob/develop/Quickref.md) you will see that most of these FRP types rely on FRP. In terms of threads, you need to already have an existing thread before you can twist and weave it. [Reflex-SDL2](http://hackage.haskell.org/package/reflex-sdl2) is a 'host', which essentially sinks itself into SDL and creates some crucial `Event`s for your to manipulate. When an `Event` fires, it contains information in context to the kind of `Event` you are using. Functions like `getDeltaTickEvent` will get you the number of milliseconds since the last frame tick, which means you can set up a function to render your game every time this event fires (every *tick*) using the information in the event.

FRP does change the process a lot, but the problem I had with regards to having objects in the world react to each other still stood --- I ended up needing to create an `Event` that fires every tick with the entire `GameState`, so all in all it was the same problem as before, just written in a different way. It meant that when I defined the event that is triggered every tick that produces a new `Guy`, I would also need to combine the tick `Event` with any other `Event` that would change my `Guy`.

# Conclusion

The requirement of forward thinking does worry me a little bit, but it does make the game cleaner. It will definitely be easier to iron out bugs if you knew precisely all the conditions in which your character dies, but at the same time it means you can't add new `GameObjects` that call a `kill()` function on a character to kill them without making your character know about specific situations. I will continue to work on [`FirstGameHS`](https://github.com/Crysikrend/FirstGameHS) and post here what I find. Forward thinking is usually a good thing, but I find that the ability to box up data and not have to worry about how they integrate into the system so much is much friendlier for games with a more intertwined world such as an RPG than it is for more simple games such as puzzlers.

That's all for today. Thanks for reading!
