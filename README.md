# TinyWL Haskell Implementation

## Project Overview

This project aims to create a total Haskell implementation of TinyWL (Tiny Wayland Compositor) using Haskell bindings for wlroots. The ultimate goal is to provide a way to test and validate Haskell bindings for the wlroots library.

## Current Status

Please check [TODO.md](TODO.md)

The project is currently in Stage 1 of development. I am working on refactoring TinyWL's main function into Haskell.

### Stage 1 Goal

Create a Haskell implementation of `c_main` that uses Haskell bindings and implements its logic in Haskell.

### Progress

1. Refactoring TinyWL's `c_main` function
   - In progress: Breaking down the ~200 lines of `c_main` into a series of function calls with minimal logic
   
2. Exposing function calls to FFI
   - Planned: Approximately 25 function calls will be exposed via bindings
   - Planned: Additional helper C functions also to beb exposed
   
3. Replacing `c_main` with Haskell main
   - Planned: Replication of `c_main` functionality in Haskell using FFI function calls
   
4. Testing wlhs bindings
   - Planned: Replacement of local FFI function calls with those defined in wlhs
   
5. Refactoring local C helper functions
   - Planned: Moving remaining C code to Haskell implementation
   
6. Progress assessment and next stage planning
   - Planned: Evaluation of Stage 1 and preparation for subsequent stages

## Getting Started

1. I created a shell.nix, you should be able to simply run `nix-shell` 
1. then `cabal run` 
1. tinywl will create a window you can play around with.
    - alt left click to move the window around and alt right click to resize it.

#### Note

- I ran created and tested this on a Debian laptop with xmonad, please let me know if you run into issues on other configurations

## Contributing

We welcome contributions to this project. This project is this in its infancy however you can find an asperational contrib guide here [CONTRIBUTING.md](CONTRIBUTING.md). At this early stage contributing wont be as strict as outlined the only requirement will be a PR that moves towards the stated goals.

## License

[LICENSE](LICENSE)

## Contact

The best way to reach me is with an email to shane@peregrinum.dev

---

This README will be updated as the project progresses through its stages.
