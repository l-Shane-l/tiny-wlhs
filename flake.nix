{
  description = "A Wayland compositor written in Haskell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-24.05";
    flake-utils.url = "github:numtide/flake-utils";
    nix-github-actions.url = "github:nix-community/nix-github-actions";
    nix-github-actions.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = { self, nixpkgs, flake-utils, nix-github-actions }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        # Haskell package set with our dependencies
        haskellPackages = pkgs.haskellPackages.override {
          overrides = self: super: {
            # Add your package overrides here if needed
          };
        };
      in
      {
        githubActions = nix-github-actions.lib.mkGithubMatrix {
          checks = {
            "x86_64-linux" = {
              tiny-wlhs = self.packages.x86_64-linux.default;
            };
          };
        };

        packages.default = haskellPackages.callCabal2nix "tiny-wlhs" ./. { };

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [
            # Wayland dependencies
            wayland
            wayland-protocols
            wayland-scanner
            wlroots
            wlr-protocols
            pixman
            libxkbcommon
            libffi
            libdrm
            egl-wayland
            libGL
            mesa
            vulkan-loader
            pkg-config
            libudev-zero
            systemdLibs
            libdisplay-info
            libliftoff
            libinput
            xorg.libxcb
            xorg.libXau
            xorg.libXdmcp
            xorg.xcbutilrenderutil
            xorg.xcbutilwm
            xorg.xcbutilerrors

            # Development tools
            bear
            gdb
            haskell-language-server
            haskellPackages.fourmolu
            clang-tools

            # Example Wayland Clients
            yambar
            mako
            bemenu
            wbg

            # Build tools
            gcc
            gnumake
            libseat
            ghc
            cabal-install
          ];

          shellHook = ''
            export WAYLAND_PROTOCOLS=${pkgs.wayland-protocols}/share/wayland-protocols
            export WLR_PROTOCOLS=${pkgs.wlr-protocols}/share/wlr-protocols/
            export WAYLAND_SCANNER=${pkgs.wayland-scanner}/bin/wayland-scanner
            export WLR_RENDERER=pixman
            export BEMENU_OPTS="-i -l 10 --fn 'monospace 12' --tb '#1d1f21' --tf '#c5c8c6' --fb '#1d1f21' --ff '#c5c8c6' --nb '#1d1f21' --nf '#c5c8c6' --hb '#1d1f21' --hf '#81a2be'"
            export BEMENU_BACKEND=wayland

            unset GHC_PACKAGE_PATH

            # Build tinywl
            echo "Building tinywl..."
            cd tinywl
            make -f Makefile.shared clean
            make -f Makefile.shared
            cd ..
            
            # Add the tinywl directory to LD_LIBRARY_PATH
            export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$PWD/tinywl
          '';
        };
      }
    );
}
