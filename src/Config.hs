module Config where

import Control.Monad (when)
import Foreign
import Foreign.C (CUInt)
import LibTinyWLHS.Compositor.Types
import LibTinyWLHS.KeyBinding.KeyBindings
import LibTinyWLHS.KeyBinding.KeySyms
import qualified LibTinyWLHS.Server.FFI as FFI
import LibTinyWLHS.Server.Types (TinyWLServer)
import Setup
import System.Process (spawnProcess)
import WLR.Util.Log

data Config = Config
    { logLevel :: WLR_log_importance
    , startupApplication :: String
    , modKey :: Modifier
    , terminalEmulator :: String
    }

-- Customize your app here, to help I placed the options in the comments
appConfig :: Config
appConfig =
    Config
        { logLevel = WLR_DEBUG -- WLR_INFO | WLR_DEBUG | WLR_SILENT | WLR_ERROR
        , startupApplication = "" -- can be any app that works with wayland, Leave blank for no startup app
        , modKey = ModAlt -- ModAlt | ModCtrl | ModLogo | ModShift
        , terminalEmulator = "kitty" -- I use kitty as my emulator, alacritty is also a popular choice
        }

-- Process to run on startup with the program name and an array of arguments to pass to it
startingApps :: IO ()
startingApps = do
    startUpProcess
        [ -- [ ("kitty", [])
          -- ,
          ("yambar", [])
          -- , ("wbg", ["~/.wallpapers/haskell.png"])
        ]

customKeybindings
    :: Ptr WlDisplay -> Ptr TinyWLServer -> IO (FunPtr (CUInt -> IO ()))
customKeybindings display server = do
    let
        handler :: CUInt -> IO ()
        handler sym = do
            -- Add your custom key event handler heres
            wlr_log WLR_INFO $ "Handler called with sym: " ++ show sym
            when (sym == keySymToInt KEY_s) $ do
                -- simple match to key events defined in LibTinyWL.KeyBinding.KeySyms
                wlr_log WLR_INFO "Mod + s pressed, spawning a terminal emulator"
                -- for this key event a process is spawned in Haskell
                _ <- spawnProcess (terminalEmulator appConfig) []
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
                        , terminalEmulator appConfig ++ " -e"
                        ] -- for this key event a process is spawned in Haskell
                pure ()

            when (sym == keySymToInt KEY_c) $ do
                -- the key Events just show up here as ints so you can also match against a raw int
                wlr_log WLR_INFO "Mod + c pressed closing server"
                -- for this event we call a Wayland FFI function
                FFI.c_wl_display_terminate display
                pure ()
            when
                ( sym == keySymToInt KEY_d || sym == keySymToInt KEY_v || sym == keySymToInt KEY_l
                )
                $ do
                    -- You can also use logical OR
                    wlr_log WLR_INFO "Mod + d pressed, cycling windows"
                    result <- FFI.c_cycle_windows server
                    ( if result
                            then wlr_log WLR_INFO "window cycled"
                            else wlr_log WLR_INFO "Window cycling failed, Only one window"
                        )

                    pure ()

    mkKeybindingHandler handler
