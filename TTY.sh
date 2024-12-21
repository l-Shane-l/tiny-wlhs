# Make sure we have clean environment
unset WAYLAND_DISPLAY
unset DISPLAY

# Set required environment variables
export XDG_RUNTIME_DIR=/run/user/$(id -u)
export WLR_RENDERER=pixman # Using pixman renderer as shown in your logs

# Ensure the runtime directory exists with proper permissions
mkdir -p $XDG_RUNTIME_DIR
chmod 0700 $XDG_RUNTIME_DIR

# Run your compositor
cabal run
