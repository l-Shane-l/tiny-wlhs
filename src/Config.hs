module Config where

import Control.Monad (when)
import Foreign
import Foreign.C (CUInt)
import Foreign.C.String
import System.Environment (getArgs, setEnv)
import System.Process (spawnProcess)

import qualified LibTinyWLHS.Compositor.Compositor as Compositor
import LibTinyWLHS.Compositor.Types
import LibTinyWLHS.KeyBinding.KeyBindings
import LibTinyWLHS.KeyBinding.KeySyms
import qualified LibTinyWLHS.Server.FFI as FFI
import qualified LibTinyWLHS.Server.Server as Server
import LibTinyWLHS.Server.Types (TinyWLServer)

import Setup
import WLR.Util.Log


data Config = Config
    { logLevel :: WLR_log_importance
    , startupApplication :: String
    , modKey :: Modifier
    , terminalEmulator :: String
    }

data State = State
    { display :: Ptr WlDisplay
    , server :: Ptr TinyWLServer
    }

-- Process to run on startup with the program name and an array of arguments to pass to it
startingApps :: IO ()
startingApps = do
    startUpProcess
        [ -- [ ("kitty", [])
          -- ,
          ("yambar", [])
        , -- , ("wbg", ["~/.wallpapers/haskell.png"])
          ("swaybg", ["-i", "./images/haskell.png", "-m", "fill"])
        ]

cycleWindows :: State -> IO Bool
cycleWindows state = FFI.c_cycle_windows $ server state

terminate :: State -> IO ()
terminate state = FFI.c_wl_display_terminate $ display state

customKeybindings
    :: State -> Config -> IO (FunPtr (CUInt -> IO ()))
customKeybindings state config = do
    let
        handler :: CUInt -> IO ()
        handler sym = do
            -- Add your custom key event handler heres
            wlr_log WLR_INFO $ "Handler called with sym: " ++ show sym
            when (sym == keySymToInt KEY_s) $ do
                -- simple match to key events defined in LibTinyWL.KeyBinding.KeySyms
                wlr_log WLR_INFO "Mod + s pressed, spawning a terminal emulator"
                -- for this key event a process is spawned in Haskell
                _ <- spawnProcess (terminalEmulator config) []
                -- _ <- spawnProcess "swaybg" ["-i", "~/.wallpapers/haskell.png", "-m", "fill"]
                pure ()

            when (sym == keySymToInt KEY_a) $ do
                -- simple match to key events defined in LibTinyWL.KeyBinding.KeySyms
                wlr_log WLR_INFO "Mod + a pressed, running beMenu"
                _ <-
                    spawnProcess
                        "bemenu-run"
                        [ "-i" -- case insensitive
                        , "-l"
                        , "10" -- show 10 lines
                        , "-p"
                        , "run:" -- prompt
                        , "--tb"
                        , "#285577" -- title background
                        , "--tf"
                        , "#ffffff" -- title foreground
                        , "--fb"
                        , "#222222" -- filter background
                        , "--ff"
                        , "#ffffff" -- filter foreground
                        , "--nb"
                        , "#222222" -- normal background
                        , "--nf"
                        , "#888888" -- normal foreground
                        , "--hb"
                        , "#285577" -- highlighted background
                        , "--hf"
                        , "#ffffff" -- highlighted foreground
                        , "--fn"
                        , "monospace 12" -- font
                        , "-W"
                        , terminalEmulator config ++ " -e"
                        ] -- for this key event a process is spawned in Haskell
                pure ()

            when (sym == keySymToInt KEY_c) $ do
                -- the key Events just show up here as ints so you can also match against a raw int
                wlr_log WLR_INFO "Mod + c pressed closing server"
                -- for this event we call a Wayland FFI function
                terminate state
                pure ()
            when
                ( sym == keySymToInt KEY_d || sym == keySymToInt KEY_v || sym == keySymToInt KEY_l
                )
                $ do
                    -- You can also use logical OR
                    wlr_log WLR_INFO "Mod + d pressed, cycling windows"
                    result <- cycleWindows state
                    ( if result
                            then wlr_log WLR_INFO "window cycled"
                            else wlr_log WLR_INFO "Window cycling failed, Only one window"
                        )

                    pure ()

    mkKeybindingHandler handler

runWLHS :: Config -> IO ()
runWLHS config = do
    wlr_log_init (logLevel config) nullFunPtr
    args <- getArgs
    commandLineArguments args
    setup <- setupServer
    wlr_log WLR_INFO "Server destroyed, shutting down"
    case setup of
        Nothing -> wlr_log WLR_ERROR "Failed to Setup Server"
        Just (myServer, renderer) -> do
            initSuccess <- FFI.c_server_init myServer
            if initSuccess
                then do
                    wlr_log WLR_INFO "Server initialized successfully"
                    wlDisplay <- Server.getWlDisplay myServer
                    _ <- Compositor.initialize_compositor wlDisplay 5 renderer
                    socket <- FFI.c_server_start myServer
                    if socket /= nullPtr
                        then do
                            socketStr <- peekCString socket
                            setEnv "WAYLAND_DISPLAY" socketStr
                            wlr_log WLR_INFO $ "WAYLAND_DISPLAY set to " ++ socketStr
                            withCString (startupApplication config) FFI.c_server_set_startup_command
                            setModKey $ modKey config
                            handlerPtr <- customKeybindings (State { display = wlDisplay, server = myServer} ) config
                            wlr_log WLR_DEBUG $ "Handler created: " ++ show handlerPtr
                            FFI.c_set_keybinding_handler myServer handlerPtr
                            startingApps
                            wlr_log WLR_INFO "TinyWLHS started"
                            FFI.c_server_run myServer
                        else wlr_log WLR_ERROR "Failed to start server"
                else wlr_log WLR_ERROR "Failed to initialize server"
            FFI.c_server_destroy myServer
            wlr_log WLR_INFO "Server destroyed, shutting down"

