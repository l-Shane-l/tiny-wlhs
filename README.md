# TinyWL Haskell Implementation

## Project Overview
This project aims to create a total Haskell implementation of TinyWL (Tiny Wayland Compositor) using Haskell bindings for wlroots. The ultimate goal is to provide a way to test and validate Haskell bindings for the wlroots library.

## Current Status
Please check [TODO.md](TODO.md)

The project is currently in Stage 1 of development. I am working on refactoring TinyWL's main function into Haskell.

### Stage 1 Goal
Create a Haskell implementation of `c_main` that uses Haskell bindings and implements its logic in Haskell.

## System Architecture

### Component Overview

The following diagram shows the main components of our Wayland compositor and how they relate to each other:

```mermaid
graph TB
    subgraph "Haskell Layer"
        A[Haskell Main]
        B[TinyWL.FFI]
    end

    subgraph "Bindings Layer"
        C[wlhs - Wayland Haskell Bindings]
    end

    subgraph "C Layer"
        D[TinyWL C Server]
    end

    subgraph "System Layer"
        E[wlroots]
        F[Wayland Protocol]
        G[Linux Kernel / Display Server]
    end

    A -->|uses| B
    A -->|uses| C
    B -->|FFI calls| D
    C -->|binds to| E
    D -->|uses| E
    E -->|implements| F
    F -->|interacts with| G

    classDef haskell fill:#f9f,stroke:#333,stroke-width:2px;
    classDef c fill:#9cf,stroke:#333,stroke-width:2px;
    classDef system fill:#fcf,stroke:#333,stroke-width:2px;
    class A,B haskell;
    class C,D c;
    class E,F,G system;
```

### Operational Flow

This sequence diagram illustrates how these components interact during the lifecycle of the compositor:

```mermaid
sequenceDiagram
    participant Haskell as Haskell Main
    participant WLHS as wlhs (Wayland Haskell bindings)
    participant FFI as FFI Layer
    participant C as C TinyWL Server
    participant WLR as wlroots
    participant System as System/OS

    Haskell->>WLHS: Use Wayland Haskell bindings
    WLHS->>WLR: Initialize logging (wlr_log_init)
    Haskell->>FFI: Call c_server_create
    FFI->>C: server_create()
    C-->>FFI: Return server pointer
    FFI-->>Haskell: Return server pointer

    Haskell->>FFI: Call c_server_init
    FFI->>C: server_init()
    C->>WLR: Initialize wlroots
    WLR->>System: Set up Wayland display
    System-->>WLR: Return display info
    WLR-->>C: Return init status
    C-->>FFI: Return init success status
    FFI-->>Haskell: Return init success status

    Haskell->>FFI: Call c_server_start
    FFI->>C: server_start()
    C->>System: Create Wayland socket
    System-->>C: Return socket name
    C-->>FFI: Return socket name
    FFI-->>Haskell: Return socket name

    Haskell->>System: Set WAYLAND_DISPLAY environment variable

    alt Startup command provided
        Haskell->>FFI: Call c_server_set_startup_command
        FFI->>C: server_set_startup_command()
        C->>System: Store startup command
    end

    Haskell->>FFI: Call c_server_run
    FFI->>C: server_run()
    C->>WLR: Start Wayland event loop
    WLR->>System: Handle input/output events

    Note over System: Wayland compositor running

    Haskell->>WLHS: Log messages via wlr_log
    WLHS->>WLR: Pass log messages

    Haskell->>FFI: Call c_server_destroy
    FFI->>C: server_destroy()
    C->>WLR: Cleanup wlroots resources
    C->>System: Close Wayland socket
```

## Getting Started
1. Ensure you have the submodules cloned down `git clone --recurse-submodules https://github.com/l-Shane-l/tiny-wlhs.git`
2. I created a shell.nix, you should be able to simply run `nix-shell` 
3. then `cabal run` 
4. tinywl will create a window you can play around with.
    - alt left click to move the window around and alt right click to resize it.

#### Note
- I ran created and tested this on a Debian laptop with xmonad, please let me know if you run into issues on other configurations

## Contributing
We welcome contributions to this project. This project is in its infancy however you can find an aspirational contrib guide here [CONTRIBUTING.md](CONTRIBUTING.md). At this early stage contributing won't be as strict as outlined; the only requirement will be a PR that moves towards the stated goals.

## License
[LICENSE](LICENSE)

## Contact
The best way to reach me is with an email to shane@peregrinum.dev
