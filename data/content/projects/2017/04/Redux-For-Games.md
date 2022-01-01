---
title: Redux For Games
date: 2017-04-24
subtitle: A remake of my Alacrity in a homemade engine.
description: Alacrity was really successful, but I didn't like the fact that it wasn't cross platform. To test myself even further I wanted to try porting it to OpenGL.
categories: 
  - Engine Development
tags: 
  - Game
  - C++
  - Uni
  - Engine
image: https://raw.githubusercontent.com/Ashe/ReduxForGames/master/img/preview.gif
status: published
---

:::{.gitrepo header="ReduxForGames"}
A link to the GitHub repository can be found [here](https://github.com/Ashe/ReduxForGames/).
:::

# What is this?

A remake of my [university assignmment, Alacrity](/project/alacrity), made in my own engine!

This is merely a test of my ability. I am simply reusing code from another project to test my SDL+OpenGL engine I have spent the last few nights agonising over. Problems will be fixed depending on whether or not I choose to submit this or my DirectX version. I would heavily consider the build in its current state ***broken*** because of level 4. Also, the UI has not been ported across yet.

# Dependencies

- SDL2
- GLM
- GLEW
- SOIL

# Wanna Play?

Just unzip the `debug` folder and hit that `.exe`!

# Bugs? Yes.

This game is a port from an assignment I worked on with DirectX. I am simply reusing the code to test if it works (spoiler: turns out it does), but it has a long way to go. Anyway, there are a lot of things that aren't jamming --- you will find the 4th level problematic in movement and I have also experienced a crash in the game upon travelling to the back-right edge of the level.
