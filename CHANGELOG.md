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

- ~~Refactor ~~`c_main`~~ to be a series of C function calls~~
- Ongoing refactoring of TinyWL's main function into Haskell
- Preparation for integrating wlhs bindings

## 0.1.1.2 - 2025-02-01

### Changed

- Major refactor of Config implementation
  - Moved to new Config type with improved structure
  - Added State type for managing display and server state
  - Improved key binding configuration with handler functions
  - Restructured startup application configuration
- Updated documentation to reflect new Config system
- Improved error handling in key event system

## 0.1.1.1 - 2025-06-01

- Support for borders
- Wayland client config
- Background images
- Clients now set window size to what is available
- Support for reserved area
- Layers added

## 0.1.1.0 - 2024-12-23

- Wayland client support
- Support for Bemenu
- Support for Mako
- Support for Yambar
- Added layer support
- Support for running outside an X11 session in a TTY
- Better logging
- Better dev tooling for the C code in the form of support for clangd
- Support for seatd

## 0.1.0.5 - 2024-12-07

- Added support for key bindings
- Large refactor
- Aim of project switched to providing features.

## 0.1.0.4 - 2024-10-30

- Moved compositor_init from c to haskell
- Refactored project to be move towards something somebody can use to create compositors instead of just a test for wlhs

## 0.1.0.3 - 2024-10-16

- Nix niv added to project to pin versions
- Update of readme to include submodules

## 0.1.0.2 - 2024-09-29

- Removed c_main and replaced with haskell implementation
- Added wlhs as a submodule and added to cabal and project
- Successfully imported wlhs log bindings
- Added wrapper for wlr_log
- Successfully implemented logging using wlhs bindings

## 0.1.0.1 - 2024-09-28

- Refactored `c_main` in `tiny.c` and moved as much as possible to functions.

## 0.1.0.0 - 2024-09-21

- First version. Initial project setup and planning, with basic PoC of a tinywl shared library being called from Haskell.
