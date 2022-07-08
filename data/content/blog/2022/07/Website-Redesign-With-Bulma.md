---
title: Website redesign with Bulma CSS
date: 2022-07-07
subtitle: An update about the new look to my website (again).
description: There were a lot of teething issues I had with my website, and so I decided to start from scratch and redesign the appearance of my website using Bulma for common components.
tags:
  - aas.sh
  - Web
  - Slides
image: https://res.cloudinary.com/aas-sh/image/upload/v1657296351/blog/2022/07/08-07-2022_17_03_41_zcwdj2.png
project: aassh
status: published
slides: |

  # Hello!
  (Please click the slideshow and press `Space` to continue!)

  ---

  <!-- .slide: data-background="aquamarine" -->
  Nice job! Here's some cake!
  # 🍰

  ---

  ## Yes we have slides

  I've always enjoyed putting slides on my website, but now they are located conveniently as the same place as their related blog posts. 

  No more having to decide whether slides or blogs are more appropriate!

  ---

  ## Apologies

  The upcoming slides have two uses:
  1. I get to flex a new feature ~~(sorry not sorry)~~
  <!-- .element: class="fragment" data-fragment-index="1" -->

  2. I can use these slides for future reference
  <!-- .element: class="fragment" data-fragment-index="2" -->

  ---

  # Lets look at hello world

  ```cpp [1|2|1-4]
  void HelloWorld() {
    printf("Hello World!");
  }
  // --> By the way, ligature support == true!
  ```

  ---

  ## Oh no! Surprise math!
  `\begin{aligned}
    \dot{x} &amp; = \sigma(y-x) \\
    \dot{y} &amp; = \rho x - y - xz \\
    \dot{z} &amp; = -\beta z + xy \\
  \end{aligned}`

  (These are [The Lorenz Equations](https://en.wikipedia.org/wiki/Lorenz_system))
  <!-- .element: class="fragment" data-fragment-index="1" -->

  ---

  <!-- .slide: data-background-image="https://c.tenor.com/Zrl5ACCRF6cAAAAC/morbius-sense.gif" -->
  > "Hello my name is Morbius"

  Morbius, 2022

  ---

  ## Have fun?

  Go check out [revealjs.com](https://revealjs.com) and go insert slideshows into your website!

  ---

  ## Thanks for watching
  (Now go read the blog for actual changes to the site)

  ---

  <!-- .slide: data-background-image="/assets/images/logo-splash.png" -->
#
---

:::{.gitrepo header="aas.sh"}
GitHub repository for this website can be found [here](https://github.com/Ashe/ashe.github.io).
:::

# Introduction

As stated in my [previous website post](/blog/new-year-new-website/), I have a weird habit of redesigning my website every so often. This is always fueled by nitpicks and ambitions, however this time feels different. The last change was the move to [Nix](https://nixos.org) and [Hakyll](https://jaspervdj.be/hakyll/), and this time I have kept those foundations and focused solely on the appearance.

The result, and I hope you'll agree, is a cleaner, smoother website  with an appearance I have more control over. In my mission to inspire and share, I want to make things as frictionless as possible!

# Changes

## There are now slides

As you may have noticed, there is now a slideshow embedded into the blog post. How cool is that?!

Slides are located as a replacement for the gallery at the top of the page for any blogs with slides enabled. I used to have a section of my website dedicated to slides, but I could never decide if a given post should be in slide format or in blog format. Sometimes I want to go into detail on a specific part of a presentation, whereas other times I want to just do a slideshow. Now I don't need to choose!

If you haven't seen it yet, check the top of this page!

## We now use Bulma!

[Bulma](https://bulma.io) is a CSS framework. I was previously using [Tailwind](https://tailwindcss.com), but unfortunately I leaned on it too much and became controlled by it since I found it very difficult to change the appearance of parts of my site to my liking.

[Bulma](https://bulma.io) is simpler and is purely CSS. My strategy was to use Bulma for the foundations of the website, and then hand-crafting elements I had a particular interest in. It worked out really well and I feel really comfortable in the fact that I've had a hand in every aspect of the website now.

Another advantage of changing framework is that it gave me an opportunity to simplify my [Nix](https://nixos.org) code, speeding up iteration and simplifying dependencies. Bulma, along with all of my personal styling, is compiled into a single `main.css` file via [Sass](https://sass-lang.com/). I'm hoping that this strategy also alleviates pressure on your browser since it's all in a single file, but I don't know enough about web technologies to provide any evidence.

## Refreshed appearance of tags

The old tag appearance was quite ugly to be honest. This was the [default implementation of Hakyll](https://hackage.haskell.org/package/hakyll-4.15.1.1/docs/Hakyll-Web-Tags.html#v:renderTagCloud). I decided to reimplement this function and use the actual number of uses per tag as part of the design. The result is a nice new tag list sorted by popularity!

:::{.figure
  image="https://res.cloudinary.com/aas-sh/image/upload/v1657296459/blog/2022/07/08-07-2022_17_07_25_tpdkvt.png"
  caption="New appearance of tag cloud."
  source="aas.sh"
  sourceUrl="https://aas.sh"
}
:::

## Code cleanup

One of the barriers I had to writing more content was how long Hakyll took to compile my website. As per usual, this was user error. After being spoilt by frameworks like [Hugo](https://gohugo.io/) in the past, I chalked all of the slowness found in building the site up to [Haskell](https://www.haskell.org/)'s compilation times and Hakyll's programming. 

Fortunately, I spotted a lot of places in the code base that could have been optimised and as part of the refresh I fixed them. In the past, I had to recompile every page whenever anything was saved, but this was actually my fault and Hakyll does in fact have the ability to only recompile what's necessary so long as you take the time to understand each step of the building process. That, and making sure your indents are as you expect so that you don't do expensive processes inside loops...

# Wrapping up

It's been a good few weeks working on my website --- if only I put as much love and effort into myself as I do my online presence; perhaps this could be said for lots of people.

I hope the redesign helps provide a more enjoyable experience so that I can receive more comments and questions via email as I really enjoyed [responding to you all in the Q and A!](/blog/qna-1/).

As always, I hope to participate more in the blog-writing space in the coming months, but don't hold me to it. If you have any questions about what you see, feel free to get in touch at [contact@aas.sh](mailto:contact@aas.sh)!
