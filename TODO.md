# TODO

## Project Goal

The goal is to create a total haskell implementation of tinywl that uses haskell bindings, 

## Current Approach

The project has been set up to run using wlhs and tinywl. 

Currently a it works but by calling a series of FFI from the c in tinywl, All of this is mainly in Server.
The idea id to move more and more into haskell from the C side. 

Right now the aim is to move all of server_init into a haskell implementation even if this haskell is making FFI calls to the TinyWL.

The ideal to gain increaded access to the TinyWL C code so we can concert only what we need into haskell.
