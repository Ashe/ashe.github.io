---
title: Laplace's rule of succession
date: 2022-07-20
subtitle: A post on one of my favourite mathematical theorems.
description: I find myself talking to many people about this bit of maths simply because I've actually been able to apply it in my life and work. Hopefully in this post I can explain how!
tags:
  - Maths
image: https://res.cloudinary.com/aas-sh/image/upload/v1658332892/blog/2022/07/sunrise_qo1y8n.jpg
---

# The sunrise problem

## A scientific certainty

In 1840, [Pierre-Simon Laplace](https://en.wikipedia.org/wiki/Pierre-Simon_Laplace)[@laplace1840essai] posed the following question :

> What is the probability that the sun will rise tomorrow?

This is the *[Sunrise Problem](https://en.wikipedia.org/wiki/Sunrise_problem)*. It's a bit of a peculiar one, and at first glance one might think that either the question is trivial, stupid, or both, but let's dive a little deeper into what it is the question is asking.

Through observation, we can say with confidence that there hasn't ever been a day where the sun hasn't risen. It is a scientific certainty that it'll rise and this very fact is taken as the truth by most if not all of the population --- just look up at the sky and one can indeed see the sun rising each morning. After so much time, there is not one doubt that the sun does indeed rise every day.

But... What if it *didn't*?

## The real question

The reason the sunrise problem is even worth considering can be discovered by simply asking *what if it didn't?* --- what is it we're really asking? If the chance of the sun rising is 100%, then what is the purpose of contemplating the scenario where it didn't?

What we're actually questionning here is not whether the sun rises, but in fact whether the *probability* of the sun rising is actually what we expect --- this is a question of the probability of another probability!

## Laplace's answer to the sunrise problem

:::{.note header="Simplification ahead!"}
I'm going to be the first to say I learned about this stuff online --- while I did further mathematics in sixth form, that is not where I learned about this. Someone with a better education could write this part a lot better, with a full history of how it was deduced and the impact it has had. This post is mostly *my* experience in understanding and using the theorem.
:::

So, what was Laplace's answer, and why was it interesting? Let's go back to the sunrise problem: if human life had existed for 99 days exactly and the sun failed to rise on day 100, then one could argue that the probability is 99/100, as in, it happened 99 days in 100. However, on day 99, the observed probability would have been 99/99, or 1, which isn't too useful in the context of the problem. If only there was a way to think in a similar way that's as simple, while also giving us an answer of worth.

Laplace's theory states that, for any event that has occurred multiple times until now, the probability of it happening again is equal one plus to this number divided by the number of times plus two. Obviously, words aren't the best way of showing this, so here's some fancy math:

$$
P(X_n+1=1|X_1+...+X_n=s)=\frac{s + 1}{n + 2}
$$

Maybe this doesn't help either.. The part to focus on is the last part on the right side of the equation --- take the number of times where the sun has risen and add 1, and divide that number by the number of total days and add 2. What this is essentially doing is 'simulating' what the probability would be *if* there was one more day where the sun had risen, and another where it didn't. In our example, this would be equivalent to:

$$
P(\text{Sun rises on 100th day})=\frac{99 + 1}{99 + 2}=\frac{100}{101}
$$

While simple, and indeed slightly weird, this does provide us a tangible value which we can use, which is a whole lot better than 'always.. I think'.

I can understand why one would think that this piece of knowledge isn't very useful, however it's important to remember what Laplace's rule is telling us: it's giving us a probability of a probability in a fairly quick and easy way.

Finally, remember that in maths there can be multiple ways of doing things; you can get the average of a bunch of values via mean, median *or* mode, as a quick example. So while this does give us *something*, you should think of it as *an answer* rather than *the* answer.

# My experience with the rule of succession

## Amazon reviews

## Challenge nudger