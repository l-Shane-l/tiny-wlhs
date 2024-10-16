# Changelog for tiny-wlhs
All notable changes to this project will be documented in this file.

## Unreleased
### Added
  - Initial project structure
  - Added tinywl from wlroots
  - Basic Haskell project setup
  - Nix shell for project
  - Makefile.shared to a create a shared library instead of a binary
  - Exposed `c_main` with `__attribute__` to be used by Haskell
  - Main.hs and FFI.hs to call the `c_main` FFI function
  - LICENSE, Changelog, TODO, README, and cabal file
  - README.md with project overview and Stage 1 plan
  - TODO list outlining project goals and stages

### Changed
  - Renamed main function in `tiny.c` to `c_main`
  - Renamed original Makefile and appended `.original` to filename
  - Refactored `c_main` function into smaller, modular functions
  - Started exposing key functions to FFI for Haskell integration
  - Refactored c_main

### Work in Progress
  - ~~Refactor `c_main` to be a series of C function calls~~
  - Ongoing refactoring of TinyWL's main function into Haskell
  - Preparation for integrating wlhs bindings

## 0.1.0.0 - 2024-09-21
  - First version. Initial project setup and planning, with basic PoC of a tinywl shared library being called from Haskell.

## 0.1.0.1 - 2024-09-28
  - Refactored `c_main` in `tiny.c` and moved as much as possible to functions.

## 0.1.0.2 - 2024-09-29
  - Removed c_main and replaced with haskell implementation
  - Added wlhs as a submodule and added to cabal and project
  - Successfully imported wlhs log bindings
  - Added wrapper for wlr_log
  - Successfully implemented logging using wlhs bindings

## 0.1.0.3 - 2024-10-16
  - nix niv added to project to pin versions
  - update of readme to include submodules
  
