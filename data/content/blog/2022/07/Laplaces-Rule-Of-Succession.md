---
title: Laplace's rule of succession
date: 2022-07-20
subtitle: A post on one of my favourite mathematical theorems.
description: I find myself talking to many people about this bit of maths simply because I've actually been able to apply it in my life and work. Hopefully in this post I can explain how!
tags:
  - Maths
image: https://res.cloudinary.com/aas-sh/image/upload/v1658332892/blog/2022/07/sunrise_qo1y8n.jpg
status: published
---

# The sunrise problem

## A scientific certainty

In 1840, [Pierre-Simon Laplace](https://en.wikipedia.org/wiki/Pierre-Simon_Laplace)[@laplace1840essai] posed the following question:

> What is the probability that the sun will rise tomorrow?

This is the *[Sunrise Problem](https://en.wikipedia.org/wiki/Sunrise_problem)*. It's a bit of a peculiar one, and at first glance one might think that either the question is trivial, stupid, or both, but let's dive a little deeper into what it is the question is asking.

Through observation, we can say with confidence that there hasn't ever been a day where the sun hasn't risen. It is a scientific certainty that it'll rise and this very fact is taken as the truth by most if not all of the population --- just look up at the sky and one can indeed see the sun rising each morning. After so much time, there is not one doubt that the sun does indeed rise every day.

But... What if it *didn't*?

## The real question

The reason the sunrise problem is even worth considering can be discovered by simply asking *what if it didn't?* --- what is it we're really asking? If the chance of the sun rising is 100%, then what is the purpose of contemplating the scenario where it didn't?

What we're actually questioning here is not whether the sun rises, but in fact whether the *probability* of the sun rising is actually what we expect --- this is a question of the probability of another probability!

## Laplace's answer to the sunrise problem

:::{.note header="Simplification ahead!"}
I'm going to be the first to say I learned about this stuff online --- while I did further mathematics in sixth form, that is not where I learned about this. Someone with a better education could write this part a lot better, with a full history of how it was deduced and the impact it has had. This post is mostly *my* experience in understanding and using the theorem.
:::

So, what was Laplace's answer, and why was it interesting? Let's go back to the sunrise problem: if human life had existed for 99 days exactly and the sun failed to rise on day 100, then one could argue that the probability is 99/100, as in, it happened 99 days in 100. However, on day 99, the observed probability would have been 99/99, or 1, which isn't too useful in the context of the problem. If only there was a way to think in a similar way that's as simple, while also giving us an answer of worth.

Laplace's rule states that, for any event that has occurred multiple times until now, the probability of it happening again is equal one plus to this number divided by the number of times plus two. Obviously, words aren't the best way of showing this, so here's some fancy math:

$$
P(X_n+1=1|X_1+...+X_n=s)=\frac{s + 1}{n + 2}
$$

Maybe this doesn't help either.. The part to focus on is the last part on the right side of the equation --- take the number of times where the sun has risen and add 1, and divide that number by the number of total days and add 2. What this is essentially doing is 'simulating' what the probability would be *if* there was one more day where the sun had risen, and another where it didn't. In our example, this would be equivalent to:

$$
P(\text{Sun rises on 100th day})=\frac{99 + 1}{99 + 2}=\frac{100}{101}
$$

While simple, and indeed slightly weird, this does provide us a tangible value which we can use, which is a whole lot better than 'always.. I think'.

I can understand why one would think that this piece of knowledge isn't very useful, however it's important to remember what Laplace's rule is telling us: it's giving us a probability of a probability in a fairly quick and easy way: all you need to do is add 1 to the numerator and 2 to the denominator!

Finally, remember that in maths there can be multiple ways of doing things; you can get the average of a bunch of values via mean, median *or* mode, as a quick example. So while this does give us *something*, you should think of it as *an* answer rather than *the* answer.

# My experience with the rule of succession

So I learned about this theorem from [Grant's video on Binomial Distributions](https://www.youtube.com/watch?v=8idr1WZ1A7Q&)[@sanderson2020binomial]. While the main topic of this not Laplace's theorem, it did use it as a quick introduction to the video those 30 seconds truly struck me as something useful. Here's the video if you're curious:

:::{.caption .w-full
  caption="Grant Sanderson talks about Binomial Distributions, mentioning Laplace's rule of succession."
  source="3Blue1Brown, YouTube"
  sourceUrl="https://www.youtube.com/watch?v=8idr1WZ1A7Q&feature=emb_title"
}
<iframe src="https://www.youtube.com/embed/8idr1WZ1A7Q" title="YouTube video player" frameborder="0" allow="accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture" allowfullscreen></iframe>
:::

Ever since I saw that video, I've had the rule of succession flash into my mind whenever I see a situation I might be able to apply it to. In the next section hopefully I can show you how!

## Reviews

### The question

Admittedly I stole this idea from [Grant's video](https://www.youtube.com/watch?v=8idr1WZ1A7Q&?t=98), but it deserves its own section nonetheless. Unfortunately, fake reviews have recently begun to run rampant and so the effectiveness of applying Laplace's rule of succession is diminished in these scenarios.

So, let's imagine that you're wanting to purchase something and you can see two similarly rated products. The question we're trying to answer is the following:

> Given that numerous people have had positive experiences, what is the probability that my own experience is also going to be positive?

This looks nothing like the rising sun problem! Where in this problem is there an element of 'probabilities of probabilities'? Let's start making some assumptions:

1. When we say positive experience, we mean an experience that we could comfortably rate 5/5 stars in the same way that others have done previously.

2. Each review is a binary representation of someone's experience; they either had a 5 star experience, or they didn't.

So with that, we can start viewing this problem in the same way as the sunrise problem, except the probability of someone giving a 5 star review is not 100%.

### The math

Let's say that 87% of 957 people gave a product 5 stars, meaning that 832/957 people had a 5 star experience. What is the probability that the next experience (yours) is also going to be positive? Let's do the math:

$$
P(\text{5 star experience})=\frac{832 + 1}{957 + 2}=\frac{833}{959}=0.86861
$$

We now have a result! `0.86861`! You could do the same maths on another product and then compare these values to see which one is more likely to satisfy you. Now like I said before, this is *an* answer, not *the* answer. There are many other ways you could compare two products, but if you can't be bothered going full math-mode, this is a nice trick.

### Why is this value useful?

Now the question you might have is, 'how is this value any better than just taking the raw 87% value?' The answer lies in the *amount* of reviews. We've always been skeptical of products with 100% 5 star reviews when only 2 people have reviewed it, but why? I believe it's because deep down we simply don't trust these two people to have experienced the product enough to represent our own experience, and so we are filled with distrust.

Laplace's rule of succession handles this gracefully; by adding two extra reviews, one positive and one negative, we can somewhat 'balance' the review scores with the amount of reviews. Let's see an example:

$$
P(\text{5 star experience with } A) = \frac{2 + 1}{2 + 2}=\frac{3}{4}=0.75
$$
$$
P(\text{5 star experience with } B) = \frac{350 + 1}{400 + 2}=\frac{351}{402}=0.87313
$$

In this completely arbitrary example, Laplace's rule of succession tells us to trust product $B$ more than product $A$ despite $B$ only having an 87.5% review score and $A$ having a 100% review score; the low amount of reviews on product $A$ meant that the adjustments made by the succession rule impact the score more significantly than they did in product $B$!

## Challenge nudger

Another time I've used Laplace's rule of succession was while I was at work! I was working on a piece of UI that 'nudged' the player towards content in the game. Players had vast amounts of challenges going on simultaneously, such as 'kill 50 players', 'win 4 matches' and 'walk 1000km'. The problem: determining which challenge has the highest probability of being relevant to the player.

After the review chapter, I'm sure you can figure out why I chose to apply the rule here; how do we compare the player's progress on these challenges when they have completely different requirements in terms of difficulty and time to complete?

> Quiz time! Which challenge would you consider to be closest to completion?
>
> 1. **Win 5 matches ---** Player has won 4.
>
> 2. **Kill 50 players ---** Player has killed 40 players.
>
> 3. **Destroy 100 barrels ---** Player has destroyed 80.
>
> 4. **Walk 1000km ---** Player has walked 800km.

Regardless of which one you *think* is more relevant to players, it may be difficult to pick between them especially when their progress percentages are all 80%! However, once again, Laplace's rule gives us *an* answer to this question (not *the* answer). Applying Laplace's rule here not only allows us to consistently rank challenges by a common metric, but it also allows us to stop caring about details --- we *know* there is not one right answer to this problem, so let's just lean on statistics and move on!

# Wrapping things up

I hope you enjoyed reading this post; this is the first time I've written about something other than programming specifically, but I personally have found if very handy for more things than I've listed. I hope that after giving it some thought, you too might find the answer's that Laplace provides useful!

Make it a good one!
