module Main where

import Config
import LibTinyWLHS.KeyBinding.KeyBindings
import WLR.Util.Log

-- Customize your app here, to help I placed the options in the comments
appConfig :: Config
appConfig = Config
        { logLevel = WLR_DEBUG -- WLR_INFO | WLR_DEBUG | WLR_SILENT | WLR_ERROR
        , startupApplication = "" -- can be any app that works with wayland, Leave blank for no startup app
        , modKey = ModAlt -- ModAlt | ModCtrl | ModLogo | ModShift
        , terminalEmulator = "kitty" -- I use kitty as my emulator, alacritty is also a popular choice
        }

main :: IO ()
main = runWLHS appConfig
