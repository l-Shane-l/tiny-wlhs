# Changelog for tiny-wlhs
All notable changes to this project will be documented in this file.


## Unreleased
### Added
      - Initial project structure
      - Added tinywl from wlroots
      - Basic Haskell project setup
      - Nix shell for project
      - Makefile.shared to a create a shared library istead of binary
      - exposed c_main with __attribute__ to be used by haskell
      - Main.hs and FFI.hs to call the c_main ffi function
      - LICENSE, Changelog, TODO, README, cabal file
    

### Changed
      - Renamed main function in tiny.c to c_main
      - Renamed original make file and appended .original to filename

### Work in Progress
      - Refactor c_main to be a series of c function calls

## 0.1.0.0 - 2024-09-21
  - First version. Initial project setup and planning, with basic PoC of a tinywl shared library being called from haskell
