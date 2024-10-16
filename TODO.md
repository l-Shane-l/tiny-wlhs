# TODO

## Project Goal

The goal is to create a total haskell implementation of tinywl that uses haskell bindings, this then will be come a way to test any haskell bindings for wlroots.

## Stage 1

The first stage is getting something to work with by refactoring tinywl's main function into haskell

### Stage Goal

Create a haskell implementation of c_main that uses haskell bindings and implements its logic in haskell.

### Stage Plan

1. ~~Refactor tinywl's c_main function:~~
    
    - ~~Currently there is about 200 lines in c_main, we need to refactor this to be a series of function calls and no logic~~

2. ~~Expose all function calls in c_main to FFI~~
    
    - There are about 25 function calls we are interested in using via the bindings the rest will simply be local C helper functions
    - We want to expose all the functions in c_main to be called direclty in our haskell.
    - We may need to create header files to keep track of this or we can continue to use attributes
    
3. ~~Replace c_main with the Haskell main.~~
    
    - At this stage we should be able to replicate c_main in haskell by making a series of FFI function calls

4. ~~Begin Testing wlhs bindings~~
    
    - At this stage we should be able to replace our local FFI function calls to those defined in wlhs

5. ~~Refactor the local C helper functions~~
    - We should be able to refactor the rest of the c_main into haskell and remove it
    - At the end of this stage we should have a total haskell implementation of c_main.

6. Assess progress and plan for next stage. 

### Stage Progress Log

Refactoring of c_main wasnt as straight forward I thought, in the end I just simplified it.

Getting wlr_log to work also was a challenge, the wlr_log didn't exist so I had to create it. This is likely because of the challenge with variadic functions. 

However I was successfully in testing the log.hsc in wlhs which I regard as a success.


### Stage Notes

The plan was not followed exactly and the refactor not as clean as I hoped. However we are a stage now in which the wlhs logging functions are being used which was the goal. So I think this stage is complete and the next can begin.

The next steps will be using wlhs where possible, one bindings header at a time.

This will require some planning to chart a course through the various bindings.

Project was updated to use nix niv to pin versions and hopefully make builds more repeatably. This seems to have addressed the issues at hand now. In the future it might make sense to switch to a nix flake and use something like cachix.
