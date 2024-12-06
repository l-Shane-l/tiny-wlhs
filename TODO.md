# TODO

## Project Goal

A wayland compositor controllable and customizable in Haskell

## Current Approach

The project currently has a feature set similar to tinywl with the addition of keybindings. The long term goal is to approach feature parity with a project like sway.

## Features

The following is a list of features under development

### keybindings

Keybindings exist in a functional state with the option to add more in on the C side or manage them in haskell.

The next steps for keybindings will be investigating adding haskell bindings for xkbcommon

### Window management

At the ability to have multiple screens and cycle through configurations

### App support

Adding the ability to use applications like dmenu and other quality of life apps designed to work with tiling managers.

### Multi Screen support
