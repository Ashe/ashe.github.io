---
title: What even is Haskell?
date: 2018-03-18
subtitle: An introduction to the functional language known as Haskell.
description: In this post, I'd like to write a beginner-friendly summary in layman's terms with regards to why I have taken such an interest in a language I've only started using. Hopefully this will inspire some people to consider using alternate languages to the ones they're used to.
categories: 
  - Functional Programming
tags: 
  - Haskell
  - Functional
  - Tutorial
image: https://res.cloudinary.com/aas-sh/image/upload/v1617292285/blog/2018/03/haskell_code_umjeh2.png
status: published
---

You may have noticed that I have been talking a lot about Haskell lately. In this post, I'd like to write a beginner-friendly summary in layman's terms with regards to why I have taken such an interest in a language I've only started using. Hopefully this will inspire some people to consider using alternate languages to the ones they're used to.

# What is Haskell?

>Taken from the [wiki](https://wiki.haskell.org/Introduction), *[Haskell](https://haskell.org) is a [polymorphic](https://wiki.haskell.org/Polymorphism), [statically typed](https://wiki.haskell.org/Typing), [lazy](https://wiki.haskell.org/Lazy_evaluation) and [purely-functional](https://wiki.haskell.org/Functional_programming) language*.

Hopefully by the end of this post that sentence will be broken down bit by bit. One of the reasons I started wanting to learn Haskell was because people promised me that learning Haskell will improve your programming skills in general. For reasons we will go into, Haskell has a fairly steep learning curve but a huge payoff. As soon as you can get your head around it not only will you be able to produce elegant code in Haskell but also you will be thinking about new concepts that are important to functional programming that are also applicable to other languages. It's important to note that while Haskell is a purely functional language, ***some languages such as JavaScript are capable of being functional too***, so it would be good to understand FP even if you aren't going to use Haskell, as it may help you in a language you will use later.

# Functional programming
Let's start with what functional programming actually is with a few examples. In essence, functional programming is a mind shift from [Imperative](https://en.wikipedia.org/wiki/Imperative_programming) programming (the one most people are used to), where additional emphasis is placed on the manipulation and transformation of data through functions. Someone who's more of an expert in Haskell or FP in general could give a better answer, but I'm rolling with this explanation for people like my past self. It can be hard to imagine the differences between the two when you haven't really used any functional languages, so just try to keep up and hopefully I can make this relatively painless.

To explain what I mentioned above, I want to start talking about what we mean when we say 'function'. In mathematics, you can imagine the 'number machine' teaching methods they use early on. You place some numbers or some form of data inside the function, and it spits out different numbers or data. However, when you start learning programming you quickly forget this idea and instead imagine functions as a set of instructions --- you are telling the computer to do something in an order in which operations are executed. There are loops, there are if statements, but at the end of the day there is some explicit instruction set that your program is following. The entirety of a game is within a loop that keeps going until the game closes --- you pass in movement and every frame the character moves, the world reacts and finally the world is drawn.

When we talk about imperative vs functional programming, forget concepts of [Object Oriented](https://en.wikipedia.org/wiki/Object-oriented_programming) and [Procedural](https://en.wikipedia.org/wiki/Procedural_programming) programming etc. We aren't talking about the ways of programming in a language, we're talking about the *type* of programming the language itself is capable of doing. They are independent and so you shouldn't dwell on them for this post. Just wanted to clear this up in case you then start questioning the differences between procedural and imperative when you usually do procedural programming inside an imperative language.

Back on topic. Functional programming is an entirely different ball game. It's very difficult to translate imperative code into functional because of the way things are done. There's a wealth of information and discussion on the topic so I won't go into too much detail, but rather try to navigate the simplest aspects so that people who haven't given it a go can still understand it. Below is an example of imperative programming:

```cs
// A class in C# which will represent the player in the game:
public class Player {

    // The player's current location
    Vector2 position = Vector2(0, 0);

    // Function to move the player
    public void MovePlayer(int x, int y) {

        // Ensure that the player will always be in-bounds (0-1000)
        if (this.x + x > 1000)
            this.x = 1000;
        else if (this.x + x < 0)
            this.x = 0;
        else
            this.x += x;

        // Imagine the same for Y:
        // ...
    }
}
```

So in this example, we have a very simple class `Player` which has a single function `MovePlayer`. This function returns absolutely nothing, it is a set of instructions detailing how the player will respond to some form of input. When we call the function like so: `MovePlayer(1, 2);` when the program has just started, we should expect the coordinates of the player to then be `(1, 2)` assuming that the rest of the function is completed. Calling it twice will move the player to `(2, 4)` and so forth. This is a simple example of [Object Oriented Programming](https://en.wikipedia.org/wiki/Object-oriented_programming) and is very effective whilst also being easy to read.

When looking at `MovePlayer`, you start at the top and read it downwards --- why wouldn't you? This feels natural and it actually looks like a list of instructions --- you can role-play as the computer and do everything in your head step by step. When navigating `if` statements it's easy to see the flow of the function and which line is executed next.

Time to bring in some FP. The fact that this function returns nothing (well, `Void`, as you can actually return `Nothing` in Haskell!) shows that this function 'does' something, it doesn't transform your data in any way, it just stores it away and accumulates it. The structure of this function doesn't exactly translate cleanly into a functional language at all. Only a few functions in Haskell 'do' things --- think of it in a sense that one function will be impure that will use all the pure functions to get things done.

With imperative programming, states can be found everywhere. There is a sense of 'state' for every instance of the player class as they each hold data that details where they are at any moment in time. In Haskell, without getting into advanced techniques, there's not really a sense of state anymore. We will go into what purity means in the next chapter, so we will ignore the sense of state (the `Player` class) and instead look at a function that `MovePlayer` function.

```hs
-- A function in Haskell:
calculateNewCoords :: (Int, Int) -> (Int, Int) -> (Int, Int)
calculateNewCoords (x, y) (i, j) = (calc x i, calc y j)
    where calc a b
            | a + b > 1000 = 1000
            | a + b < 0 = 0
            | otherwise = a + b
```

Now for those who are unfamiliar with Haskell may feel a little scared over the code above. This can *definitely* be cleaned up to be more elegant, but I feel this version demonstrates quite a few features of Haskell. Firstly, we have defined almost a 'mini-function' called `calc`. We are no longer reading the function from top to bottom as in order to understand the second line we need to have read `calc` first. Haskell is very mathematics based, and so you need to imagine that you are writing a mathematical function rather than a set of instructions. In this case, we defined a function which investigates two vectors and adds their components together and ensure that they are within 0 and 1000. Obviously, if the bounds were to be any different, we either need to hard code them in (by replacing `1000` and `0`) or by passing them in.

Secondly, let's address what the first line actually means. The first line is the type declaration of the function --- in this case, we are passing in two tuples (pairs in this case) containing two `Int`s each, and then finally returning a tuple. As you can expect, this function takes a vector to represent ***a position*** and a vector in which to translate by. The parameters are split up by `->` where the last section is the return value. By following the rules we established in the previous example, this function should hopefully spit out another tuple of `Int`s where the position will eventually be. The reason I placed emphasis on the fact it is *a position* is because this function does not belong to any class or instance; anything can call it. Because of the fact that we pass in the position we are manipulating and also returning the result, we are able to call this a ***pure*** function that is completely free from side effects and does not depend on anything else to be called. If you are into [Parallel Computing](https://en.wikipedia.org/wiki/Parallel_computing), this is a very good thing as it means that two functions cannot touch the same data at the same time.

Following from the previous point --- the fact that you are always operating and returning data means that another strength of Haskell and other functional languages is that data is [immutable](https://en.wikipedia.org/wiki/Immutable_object); once data is created it cannot be changed. If you use data properly and wisely, this can lead to saving memory. It is also more secure --- unless you overwrite your variable (which is usually done in `do` syntax, a bit out of scope of this blog), your data will always be the same. You don't usually overwrite data a lot in Haskell, but if you did it would be a lot like overwriting a variable passed by value where you are only messing with the copy.

# Pure functions

Let's look into `Pure` functions closely. As described earlier, a pure function has no side effects. By side effect, we mean something which is done as a consequence of calling a function. This could be changing values that aren't a part of the function or simply using them. Here's an imperative example:

```cs
// C# Example 1 -
// This function can be called from anywhere:
public int addem(int a, int b) {
    return a + b;
}
```

That's a very simplistic function written in C#. It doesn't take much to realise that there isn't much of a point to this function when you can use `+` to make it completely redundant. Let's mix it up a bit.

```cs
// C# Example 2 -
// Imagine this is a function in some sort of class to store 'c':
int c = 5;
public int addem(int a, int b) {
    return a + b + c;
}
```

So what's different between these two functions? Well, again, it doesn't require a high IQ to see that there's now an external variable named `c` which is going to be added to whatever we pass to our function. What's important to note here, is that passing in `a = 1` and `b = 2` may not always return `8` with this function so long as `c` is visible outside the function. This is where the word `pure` comes from: a pure function will ***always*** return the same outputs for the same set of given inputs. When doing functional programming, the majority of your functions will follow this pattern. You should be rubbing your head right now if you're a more involved programmer --- how the hell do you ever get anything done?

The answer is to divide your program into pure and impure parts. You begin to look at what computations actually require a sense of state and filter down your functionality into small, focused functions that only focus on manipulating data. When you wrap things up, you will likely have a few or even just one impure function that ties everything together. The key to FP is to really evaluate the purity of every function to ensure that you can have as many pure functions as possible.

# Laziness

Now it's time to look into the other buzzwords that are tagged in Haskell's description. [Lazy Evaluation](https://wiki.haskell.org/Lazy_evaluation) means that you can actually utilise lists of infinite length without crashing the program. Haskell will only evaluate expressions when it needs to, so taking the 32132nd number from an infinite list only requires the list to be calculated to that point. Here's a few examples of some infinite lists written in different ways:

```hs
-- Haskell time:
-- This is a list of one through ten
λ> [1..10]
[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

-- The same list as above, but with a 'step' to make it odd only
λ> [1,3..10]
[1, 3, 5, 7, 9]

-- This is a list like the first but doesn't stop at ten
λ> [1..]

-- Being able to create infinite lists easily like this simply relies
-- on the 'Ord' typeclass. Basically, if you have a type that can be
-- ordered, you can make infinite lists like we've shown here!

-- Note that it doesn't exist until you actually use it
λ> [1..] !! 5
5
λ> take 5 [1..]
[1, 2, 3, 4, 5]

-- A list like the second, but infinite
λ> [1, 3..]

-- Now when we use the lists
λ> [1, 3..] !! 5
9
λ> take 10 [1, 3..]
[1, 3, 5, 7, 9, 11, 13, 15, 17, 19]

-- Going down
λ> take 10 [5, 4..]
[5, 4, 3, 2, 1, 0, -1, -2, -3, -4]

-- Creating an infinite list through recursion
λ> let someList = 1 : map (+1) someList
-- Every iteration prepends '1' and then maps (+1) onto every element in the list as follows:
-- [1]
-- 1 : [2]
-- 1 : [2, 3]
-- 1 : [2, 3, 4]

-- Using the recursive endless loop will not break
λ> take 5 someList
[1, 2, 3, 4, 5]
λ> take 5 [x*2 | x <- someList]
[2, 4, 6, 8, 10]

-- Try to work out how this infinite list is computed, what number is next?
-- This is called 'List Comprehension'
λ> take 12 [x*3 + y | x <- someList, y <- [10,9..7]]
[13, 12, 11, 10, 16, 15, 14, 13, 19, 18, 17, 16]

-- Here is a filter with list comprehension
λ> take 5 [x | x <- [5, 6..], x `mod` 2 == 0]
[6, 8, 10, 12, 14]

-- Ready to get your mind blown?
λ> take 5 [if x `mod` 2 == 0 then "EVEN!" else "ODD!" | x <- [1..]]
["ODD!","EVEN!","ODD!","EVEN!","ODD!"]

-- Note that I've taken out the floating point errors here
λ> take 5 [0, 0.1..]
[0.1, 0.2, 0.3, 0.4, 0.5]

λ> take 5 ['l'..]
['l', 'm', 'n', 'o', 'p']
-- Actually, in Haskell, a string is actually just a list of characters, so..
"lmnop"

-- Note that the above example isn't constrained to the alphabet
-- and will never loop back round to the alpha-numeric ascii characters
-- Not to worry though! A function called cycle can loop a list infinitely!
λ> ['A'..'Z']
"ABCDEFGHIJKLMNOPQRSTUVWXYZ"

λ> let alphaBetty = cycle ['A'..'Z']
-- Now alphaBetty is a list of the alphabet cycling infinitely
λ> take 6 (drop 23 alphaBetty)
"XYZABC"

-- Note that cycle doesn't require Ord at all, as it uses a pre-made list

-- Also note, that [1..] is simply syntactic sugar for the method we used for
-- 'someList'
```

I guess you could say I was pretty lazy in this area as I just demonstrated infinite lists rather than really talking about them, but who cares?

# Static Typing

This subject isn't specific to Haskell, but I'll give you the run down anyway as you may not be clear on it in the first place. Essentially, in [Static Typing](https://wiki.haskell.org/Typing) the type of every variable is known at compile time. Usually, this means that the programmer must explicitly say what type a variable is before you can use it. This is obvious in C++:

```cpp
// C++:
int sum;
int number = 0;
sum = number + 1;
```

In Haskell, there's something called 'type inference' which means that you don't necessarily have to define all of your variable types because the language is very smart. You should still explicitly label your functions though to make sure the compiler is on the same stage as yourself. C++ and Haskell are both statically typed languages, including (but not exclusive to) Java, C and Scala.

So what is *dynamic typing*? Well, it is the opposite of static typing. JavaScript and Python (a language I am unfamiliar with) are both examples of dynamically typed languages. You don't have to write out that your variable is an `Int`, for instance. Remember that some statically typed languages are capable of this, but that's just the compiler being clever and doing the work for you. People say JS is 'untyped', but I'm just going to call it dynamic typing as it is most definitely not static. Now, at the risk of confusing you, I'm going to talk about dynamic typing with regards to [Strong and Weak typing](https://en.wikipedia.org/wiki/Strong_and_weak_typing). These are two different concepts and so you shouldn't get mixed up between them --- just because a language is dynamically typed doesn't necessarily mean it's strong (or weak) typed.

If you are working with a strongly typed language, then once you have bound a variable to a type then you must ensure that it is always that type. For instance, does concatenating (joining) a `String` with an `Int` work? Does `"High " + 5` compile somehow? If it does, is there an explicit method for it, or is it a feature of your language that allows types to mingle like this? A weakly typed language will easily allow it, resulting in `"High 5"`. This is a really simple example and isn't so important, but it's hard to talk about dynamic typing without mentioning it.

# Polymorphism

No, it's not the [spell in World of Warcraft](http://www.wowhead.com/spell=118/polymorph), however, the meaning behind the two go together. Essentially, [Polymorphism](https://wiki.haskell.org/Polymorphism) allows a variable to have more than one type. Once the type is bound then it must remain consistent through every call to said variable or parameter, but until that point it can be anything you like within reason. Haskell has multiple ways to do this, the first is very simple to demonstrate. If you have ever heard of an identity function, then this will quickly make sense:

```hs
-- Haskell's identity function:
id :: a -> a
```

Note that in Haskell, we read `::` as '*has type of*' as in '*The function `id` has a type of `a -> a`*'. This isn't really saying what the function actually does, however, given the type, there's not a lot it *could* do. So what is `a`? Well, we don't know --- it's ***polymorphic***. Since we have no idea what it actually is or what it could be, we can't really pass it to any functions *unless* the function also takes a parameter of `a`, meaning that it accepts anything. `a -> a` means that this function will take a variable of any type and will return a variable *of the same type*. If you aren't sure what an identity function is, it is simply a function that, in context, does nothing to it's arguments. `1` is the identity for multiplication and division because anything `*1` or `/1` equals itself --- `x * 1 = x`. In addition (and subtraction), `0` is the identity because `x + 0 = x`. In this case, `id` is an identity function which does nothing to it's parameters and simply returns them. It might not make sense now, but it does have it's uses.

Anyway, `id` is a polymorphic function because it accepts literally ***any*** type. It doesn't need any information on what the thing it accepts is --- it could be a list or even another function --- that's for another blog post ;). The function will always return something of the same type and will be bound as soon as you pass in a parameter. Here's an example:

```hs
-- Usage of id in Haskell:
λ> id 1
1

-- The type of that first id call was Int -> Int
-- ... or was it? We'll get onto that next!

λ> id "hello"
"hello"

-- The type of id here was [Char] -> [Char], or String -> String
-- as string is essentially an alias of [char]

λ> id ['A', 'B', 'C']
"ABC"

-- This third call is exactly the same as the example above,
-- Haskell prefers to specify that it's a [Char], but then
-- writes it as a string to be friendly to read for us, thanks HS!

λ> id [(1, "Hello"), (2, "there"), (3, "mate!")]
[(1, "Hello"), (2, "there"), (3, "mate!")]

-- The type of id here in the fourth call was
-- [(Int, [Char])] -> [(Int, [Char])]
-- Again, not really Int, but that's coming up next
```

As we went through before, the `->` symbol separates the parameters from each other, where the last one on the line is the return value. The type of `id` binds itself to whatever variable type we pass in. It is also important to remember that `a` is just easy to use as it's the first letter of the alphabet, but the name doesn't matter. Types start with an uppercase letter, such as `Int`, `Bool`, `Char` or `String`. Anything lowercase can be seen as a variable of any type. `a -> b` means we have a function returning something that ***could*** be a different type to the one we passed in. `a -> b -> c` means that we pass in two parameters that aren't necessarily the same type, and get back another type that could be different, or the same as `a` or `b`. Remember, these letters could be *anything*. Just because `a` and `b` indicate that there are uses of two independent types in our function doesn't mean that they *have* to be different, just that they *could* be.

So what was I going on about, surely that first call was `Int -> Int`, I nailed that Ashley! I had it guessed! Well, in this case, you are actually a bit too specific. It's time to introduce `=>` in the context of type declarations.

```hs
--Haskell identity function again:
λ> id 1
1
λ>:t id 1
Num a => a
```

So when we use Haskell in a terminal to evaluate expressions we can use `:t` to query the type of something. This is querying the type of the *result* of `id 1`, so don't get confused. Our answer was `Num a => a`, which is a little different to what I've shown so far. `=>` describes the thing on the right with the definitions on the left --- it's saying that the `a` after the arrow is deriving the typeclass `Num`. As I was saying, we asked for the *result* of the function, and so our compiler is telling us that the result is `a`, where `a is part of the Num` typeclass. Therefore, the specialised type of our function `id` *before the function was applied* became as follows:

```hs
Num a => a -> a
```

You can still see the original `a -> a` type declaration coming through, but now we know that the `a` sent in and `a` retrieved, which are both the same type, are now a `Num`. What is `Num`? The second way Haskell is polymorphic is less generic than the previous. As you get more specific, you get more functionality at a cost of being general --- it makes sense really, as you can't attach functionality to something without defining some basic principles to manipulate.

Enter `Num`, a typeclass for a number. When something derives `Num`, we mean that whatever the type is it has to have some core functions. Put it this way, when you derive `Num`, you have to then fill in some functions for your type showing how they operate --- if you make some whacky type that derives `Num`, you need to work out how things like `+`, `-` and `*` work (you can research why you don't define `/`). [The functions can be found here on Hackage](http://hackage.haskell.org/package/base-4.11.0.0/docs/Prelude.html#t:Num) --- just make sure you let the page load and scroll to the `Num` section or you'll get confused. Let's have a look at the functions from that page:

```hs
-- Haskell Num functions:
λ> :t (+)
(+) :: Num a => a -> a -> a
-- If you have followed everything so far, you should
-- be able to see that addition takes two Nums and returns a Num

λ> :t (*)
(*) :: Num a => a -> a -> a
-- Remember, we are working with Num here, not Int
-- We don't know how this function is implemented, just that types
-- like Int are compatible with these functions.

λ> :t abs
abs :: Num a => a -> a
-- Note that here we only have one parameter and one return value
-- that's because getting the absolute value simply makes the number positive
-- and it doesn't need any other parameters to operate

-- Let's query what typeclasses Int actually derives, so you can see how much Int can do!
λ> :i Int
data Int = GHC.Types.I# GHC.Prim.Int#   -- Defined in `GHC.Types'
instance Bounded Int -- Defined in `GHC.Enum'
instance Enum Int -- Defined in `GHC.Enum'
instance Eq Int -- Defined in `GHC.Classes'
instance Integral Int -- Defined in `GHC.Real'
instance Num Int -- Defined in `GHC.Num'
instance Ord Int -- Defined in `GHC.Classes'
instance Read Int -- Defined in `GHC.Read'
instance Real Int -- Defined in `GHC.Real'
instance Show Int -- Defined in `GHC.Show'
```

As you can see from that last demonstration, `Int` actually has a lot of functionality! If we have a function that has a declaration such as `Eq a => a -> a -> Bool`, we can start thinking about the types of things it could do. `Eq` is the typeclass of equality --- in terms of checking if things are equal. If we have a function which takes two variables of the same type that have the ability to be compared to see if they are equal or not, we can see how we would get a `Bool` out of this (where `Bool` is either `True` or `False`). This function would still be polymorphic as we don't know what type we are being given, all we know is that they can be compared. It isn't 100% generic, but neither is it 100% specific, therefore qualifying as polymorphic.

You may already know how your current language implements polymorphism, whether it is through inheritance of other classes or some other method. This is simply how Haskell does it, and it is very, ***very*** powerful.

# Wrapping things up

I hope this little adventure has been a fun read and that I actually convince some of you to dive into Haskell a little bit. Learning Haskell has been a huge help to my challenge-craving brain and I am in love with how mathematical everything is. If you are interested to start learning, take a look at [Learn you a Haskell for a Great Good (LYAH)](http://learnyouahaskell.com/) which is free, or the [Haskellbook](http://haskellbook.com/) which is more thorough, more complex and more expensive. If you want to get serious, go for the [Haskellbook](http://haskellbook.com/), but I would advise that you have a look at [LYAH](http://learnyouahaskell.com/) first for a quick overview of what you can do. It'll get you up and running to the point of playing around with code, but the more academic of you will probably need something more.

If you want something else too look at, go and take a look at [ComputerPhile on YouTube](https://www.youtube.com/channel/UC9-y-6csu5WGm29I7JiwpnA), they've done some great videos on not just Haskell and not just on Functional Programming, but on computing in general. Definitely check out the Haskell videos though. If you need some help getting started (or even when you're an expert!) or want to talk to some Haskell pros, go to the [#Haskell IRC on Freenode](http://webchat.freenode.net/?channels=haskell) or the [Functional Programming Discord Server](https://discord.me/fp).

Thanks for reading, this was really fun to write!
