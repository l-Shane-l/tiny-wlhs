# TODO

## Project Goal

A wayland compositor controllable and customizable in Haskell

## Current Approach

The project currently has a feature set similar to tinywl with the addition of keybindings. The long term goal is to approach feature parity with a project like sway.

## Features

The following is a list of features under development, they are ordered by complexity from simple to most complex and also the order in which i will develop them

### Copy paste support

Support using a clip board

### Advanced Wayland client support

There is currently client support but clients like waybar expect a lot more protocols to be present

### Multi Screen support

This is already partially implemented

### tiling management

At the ability to have multiple screens and cycle through configurations

### locking support

This is the most complex and will require a secure means to lock the system
