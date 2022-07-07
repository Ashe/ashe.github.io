---
title: Street Fighter Clone
date: 2019-11-29
subtitle: A fun revisit of the GameMaker engine.
description: A university assignment where the goal was to study and recreate Street Fighter's mechanics in our own clone of the game.
tags:
  - GameMaker
  - Game
  - Uni
image: https://github.com/Ashe/Streetfighter/blob/master/img/streetfighter_preview.gif?raw=true
status: published
---

:::{.gitrepo header="Street Fighter Clone"}
GitHub repository for Street Fighter Clone can be found [here](https://github.com/Ashe/Streetfighter).
:::

# What is this?

This project was made as part of an assignment for Sheffield Hallam University in November, 2019. Since GMS is proprietary software, I've taken the liberty of compiling the project into a `.exe` file that you can find [here](https://github.com/Ashe/Streetfighter/releases/). The point of this project was to try and mimic StreetFighter as closely as possible and understand not only *what* mechanics were programmed but also *why*. 

This assignment was set up as a competition; we all went around the room and played everyone's games (where everyone had chosen a different character to start with) to see which was the most fun and complete. Additionally, we were given the opportunity to guess whose was the best, and if we got it right, we got an extra mark. The person with the most votes also got extra credit.

I was confident with my project, and because I didn't really feel like anyone could beat it, I didn't really want to outright say that someone else's assignment was better than my own (we were allowed to vote for ourselves and a lot of people did) since I wasn't really very enthusiastic about students' marks being dictated by the success of others'. I didn't put anyone's name down, and I actually managed to win the competition by a large margin. Lesson learned --- if you're confident in your work, don't be ashamed!

:::{.caption
  caption="Gameplay of my Street Fighter clone."
  source="Street Fighter clone"
  sourceUrl="https://github.com/Ashe/Streetfighter"
}
<video src="https://res.cloudinary.com/aas-sh/video/upload/v1617294551/projects/street_fighter/street_fighter_rpjtx2.mp4" type="video/mp4" autoplay="autoplay" controls loop muted></video>
:::

# How do I play?

Simply download and extract [the latest release](https://github.com/Ashe/Streetfighter/releases/) and run the executable (`StreetFighter.exe`). You will be presented with this screen:

:::{.figure
  image="https://raw.githubusercontent.com/Ashe/Streetfighter/master/img/opening_screen.png"
  caption="Opening screen of my Street Fighter clone."
  source="Street Fighter clone"
  sourceUrl="https://github.com/Ashe/Streetfighter"
}
:::

I made this game controller-only because we were all using controllers anyway and there were no extra marks for fully implementing KBM. To get around the issue of 'who plays who', I made it so that at the start of the round, both players need to press 'Start' on their gamepads. The characters are then assigned on a first-come, first-served basis. 

:::{.help 
  header="Controls"
  caption="Taken from the [notes file](https://github.com/Ashe/Streetfighter/blob/master/notes/hints.txt) I wrote for the assignment."
}
**Main Controls:**

* Press the start button to gain control of a character each round
* Walk: L-Stick / DPad left or right
* Crouch: L-Stick / DPad down
* Jump: L-Stick / DPad up
* Wall jump: Midair next to wall, L-Stick diagonally up + away from wall
* Block: L-Stick / DPad backwards from opponent when they attack

**Punches:**

* Light punch: Square (PS) / X (Xbox)
* Medium punch: Triangle (PS) / Y (Xbox)
* Heavy punch: Left shoulder / Left trigger

**Kicks:**

* Light kick: X (PS) / A (Xbox)
* Medium kick: Circle (PS) / B (Xbox)
* Heavy kick: Right shoulder / Right trigger

**Special Moves:**

* Sweep: Crouch + Heavy kick
* Flip kick: Move towards opponent, close range, medium kick
* Neck breaker kick: Move towards opponent, close range, heavy kick
* Stomp kick: Midair with a forwards jump (flip), L-Stick down + medium kick

:::

# Credits

Obviously, the artwork used in this game isn't mine. It's taken from various spritesheets I've found on the internet. 
