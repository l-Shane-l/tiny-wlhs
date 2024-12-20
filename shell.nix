{ system ? builtins.currentSystem }:
let
  pkgs = import ./nix/nixpkgs.nix { inherit system; };
in
pkgs.mkShell {
  buildInputs = with pkgs; [
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
    seatd
    bear
    gcc
    gnumake
    gdb
    ghc
    cabal-install
    haskell-language-server
  ];
  shellHook = ''
    export WAYLAND_PROTOCOLS=${pkgs.wayland-protocols}/share/wayland-protocols
    export WLR_PROTOCOLS=${pkgs.wlr-protocols}/share/wlr-protocols/
    export WAYLAND_SCANNER=${pkgs.wayland-scanner}/bin/wayland-scanner
    export WLR_RENDERER=pixman
    unset GHC_PACKAGE_PATH

    # Build tinywl
    echo "Building tinywl..."
    cd tinywl
    make -f Makefile.shared clean
    make -f Makefile.shared
    cd ..
    
    # Add the tinywl directory to LD_LIBRARY_PATH
    export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:$PWD/tinywl
    
    echo "Wayland development environment"
    echo "wayland-scanner version: $(wayland-scanner --version)"
    echo "Wayland protocols path: $WAYLAND_PROTOCOLS"
    echo "LD_LIBRARY_PATH includes: $LD_LIBRARY_PATH"
  '';
}
