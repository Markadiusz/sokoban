# sokoban

A project for a Haskell course at MiMUW.

A terminal [sokoban](https://en.wikipedia.org/wiki/Sokoban) game.

## Setup

### GHC instalation

    curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh

### Build

    ghc main.hs
    ./main

## Available actions

Game start / go to the next level: Space

Player movement: WASD

Undo: U

Skip to the next level: N

Game reset: Esc

## Game elements

' ' - blank

'.' - ground

'#' - wall

'x' - storage

'o' - box

'^' / '<' /  'v' / '>' - player

## Example level

    #########
    ###.....#
    ##xxo...#
    ##.x#^..#
    ###.o..##
    ##.#o...#
    ##...o..#
    ##xx..o.#
    #########
