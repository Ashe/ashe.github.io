---
title: "Hex Voronoi"
date: 2022-04-19
subtitle: An experiment with Haxe and Heaps.
description: "After spending a while wandering the internet in search of something new to learn, I found something I somehow missed: the language, Haxe, and the game framework, Heaps. This was the result of my learning."
categories:
  - Game development
tags:
  - Haxe
  - Heaps
images:
  - https://github.com/Ashe/Hex-Voronoi/blob/master/docs/preview/1.png?raw=true
  - https://github.com/Ashe/Hex-Voronoi/blob/master/docs/preview/2.png?raw=true
  - https://github.com/Ashe/Hex-Voronoi/blob/master/docs/preview/3.png?raw=true
  - https://github.com/Ashe/Hex-Voronoi/blob/master/docs/preview/4.png?raw=true
status: published
---

:::{.live header="Live demo available"}
A live demo of this project can be found [here](https://aas.sh/Hex-Voronoi).
:::

:::{.gitrepo header="aas.sh"}
GitHub repository for this website can be found [here](https://github.com/Ashe/Hex-Voronoi).
:::

# What is this?
I build this as part of my introduction to both [Haxe](https://haxe.org/) and [Heaps](https://heaps.io/). I started off with their [sample projects](https://heaps.io/samples/) and iterated until I had a good handle about what tools were available. The result is something similar to another project of mine, [HexagonalHS](https://aas.sh/project/hexagonalhs/).

You can test this project live at [https://aas.sh/Hex-Voronoi](https://aas.sh/Hex-Voronoi/).

:::{.caption .w-full .h-full .hidden .lg:block
  caption="The live demo of Hex-Voronoi, embedded into this page. It might not work as well as the [full-window version](https://aas.sh/Hex-Voronoi/)."
  source="Hex Voronoi"
  sourceUrl="https://github.com/Ashe/Hex-Voronoi"
}
<canvas id="webgl" style="width:100%;height:100%"></canvas>
<script type="text/javascript" src="https://aas.sh/Hex-Voronoi/voronoi.js"></script>
:::

# How was it made?
This application generates a mesh made up of hexagons. This itself isn't too difficult, especially when you follow the teachings of [Red Blob Games](https://www.redblobgames.com/grids/hexagons/).

Next, I started to transform the grid into a voronoi pattern. In nature, voronoi patterns appear in things like bubbles, where a shape is 'inflated' until its boundaries collide with other boundaries. The trick is to select a point inside a shape and then 'inflate' from there. You can find all the code below [here](https://github.com/Ashe/Hex-Voronoi/blob/master/src/Hexgrid.hx).

To select the location, I separated each hexagon into 3 rhombi around the centre. I then chose a point by randomly traversing the lengths of the rhombus, where each side is equal to the radius of the hexagon to any of its corners.

```haxe
// Split the hexagon into 3 rhombi and choose one
final selectedCorner = random.randomInt(0, 2) * 2;
final corner = corners[selectedCorner];
final nextCorner = corners[selectedCorner + 1];
var toCenter = center.sub(corner);
toCenter.normalize();
var toNext = nextCorner.sub(corner);
toNext.normalize();

// Randomly select a feature point within the rhombus (and hexagon)
final i = toCenter.multiply(random.random() * size);
final j = toNext.multiply(random.random() * size);
featurePoint = corner.add(i).add(j);
```

After selecting these points for each hexagon, I then iterated through the list of hexagons to 'inflate them'. Recall that each corner of a hexagonal tesselation is in contact with three hexagons, meaning that other than the current hexagon, we need to locate these other two hexagons. These hexagons may not exist if the current hexagon is on the border of your grid.

We use these feature points (the name I gave to that randomly selected location we picked earlier) to distort each hexagon in relation to those closest to them. Since we started with pattern of tesselated, regular hexagons, we know that we aren't going to get more than 3 feature points for any corner since the other corners will act as the contact points in the 'inflation' process. It's hard to explain, but if you visualise the process of picking these points and drawing a line that's equidistant between them, you'll see that the way other corners distort results in there only ever being 3 feature points to consider per hexagon.

```haxe
for (tile in tiles) {

  // Hard code directions for iterating around the tile
  final dirs = [
    new Axial( 1, 0),
    new Axial( 0, 1),
    new Axial(-1, 1),
    new Axial(-1, 0),
    new Axial( 0,-1),
    new Axial( 1,-1)
  ];

  // Iterate for each corner and direction
  for (i in 0 ... 6) {

    // Prepare to reposition corner
    final prevDir = dirs[i > 0 ? i - 1: 5];
    final nextDir = dirs[i];
    final prevTile = getTileAt(Axial.add(tile.pos, prevDir));
    final nextTile = getTileAt(Axial.add(tile.pos, nextDir));

    // ...
```

Next, we need to find the midpoint of the three hexagons in contact's feature points. To get the midpoint of three points, simply take the averages of their `x`, `y` and `z` (although I only wanted `x` and `y` here, since I manipulated `z` in other ways to 'smoothen' the map).

```haxe
// Find midpoint between adjacent neighbours
var midpoint = if (prevTile != null) {

  // Both tiles valid - 3 way midpoint
  if (nextTile != null) {
    tile.featurePoint
      .add(prevTile.featurePoint)
      .add(nextTile.featurePoint)
      .multiply(1. / 3.);
  }
  // Just previous tile valid - 2 way midpoint
  else {
      tile.featurePoint
          .add(prevTile.featurePoint)
          .multiply(0.5);
  }
}
else {
  // Just next tile valid - 2 way midpoint
  if (nextTile != null) {
      tile.featurePoint
          .add(nextTile.featurePoint)
          .multiply(0.5);
  }
  // No neighbours - do whatever
  else {
      tile.featurePoint
          .add(tile.corners[i])
          .multiply(0.5);
  }
}

// Set location of corner to resulting midpoint
tile.corners[i].x = midpoint.x;
tile.corners[i].y = midpoint.y;

```
And with that, we have our voronoi pattern! We don't need to worry about the edges, since the corners are where the boundries converge, and so by handling the corners the edges just line into place.

# How do I use it?
By default, the application will rotate around the map. You can disable this orbiting mode in the top right corner. From there, you can control the camera with your mouse and zoom in with the scroll wheel.

In the top left, you'll find various settings for changing how the terrain looks. In the top right, you'll find functions like changing the map's colour, regenerating the map, and resetting all settings to default.

# How do I compile it?
This project was created using [Haxe](https://haxe.org/) and can be compiled in two ways. Before attempting compilation, you'll need to [install Haxe and Heaps](https://heaps.io/documentation/installation.html). Additionally, you'll need to install `seedyrng` and `domkit` via `haxelib install <package>`.

## Visual Studio Code:
[Haxe](https://haxe.org/) works very well with [Visual Studio Code](https://code.visualstudio.com/) when using the [Haxe Extension Pack](https://marketplace.visualstudio.com/items?itemName=vshaxe.haxe-extension-pack). When everything is installed, you should be able to hit `CTRL+SHIFT+B` to build the project. It'll compile to JS and be found in the `docs` folder, which you can then test by opening `index.html` in your web browser.

## Command line:
After installing dependencies, you can run `haxe build.hxml` to compile the project. A JS file will appear in the `docs` folder and you can then test it by opening `index.html` in your web browser.