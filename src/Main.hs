module Main where

import Config
import Control.Monad (when)
import LibTinyWLHS.KeyBinding.KeyBindings
import LibTinyWLHS.KeyBinding.KeySyms
import Setup
import System.Process (spawnProcess)
import WLR.Util.Log

-- Customize your app here, to help I placed the options in the comments
appConfig :: Config
appConfig =
    Config
        { logLevel = WLR_DEBUG -- WLR_INFO | WLR_DEBUG | WLR_SILENT | WLR_ERROR
        , startupApplication = "" -- can be any app that works with wayland, Leave blank for no startup app
        , onStartup = handleStartup
        , onKeyPress = handleKeyPress
        , modKey = ModAlt -- ModAlt | ModCtrl | ModLogo | ModShift
        }
    where
        terminalEmulator = "kitty" -- I use kitty as my emulator, alacritty is also a popular choice
        handleStartup = do
            startUpProcess
                [ ("yambar", [])
                , ("swaybg", ["-i", "./images/haskell.png", "-m", "fill"])
                ]
        handleKeyPress state sym = do
            -- Add your custom key event handler heres
            wlr_log WLR_INFO $ "Handler called with sym: " ++ show sym
            when (sym == keySymToInt KEY_s) $ do
                -- simple match to key events defined in LibTinyWL.KeyBinding.KeySyms
                wlr_log WLR_INFO "Mod + s pressed, spawning a terminal emulator"
                -- for this key event a process is spawned in Haskell
                _ <- spawnProcess terminalEmulator []
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
                        , terminalEmulator ++ " -e"
                        ] -- for this key event a process is spawned in Haskell
                pure ()

            when (sym == keySymToInt KEY_q) $ do
                -- the key Events just show up here as ints so you can also match against a raw int
                wlr_log WLR_INFO "Mod + q pressed closing server"
                -- for this event we call a Wayland FFI function
                terminate state
                pure ()

            when
                ( sym == keySymToInt KEY_d || sym == keySymToInt KEY_l
                )
                $ do
                    -- You can also use logical OR
                    wlr_log WLR_INFO "Mod + d pressed, cycling windows"
                    result <- cycleWindows state
                    if result
                        then wlr_log WLR_INFO "window cycled"
                        else wlr_log WLR_INFO "Window cycling failed, Only one window"
                    pure ()

main :: IO ()
main = runWLHS appConfig
