module Config where

import Control.Monad (when)
import Foreign
import Foreign.C (CUInt)
import LibTinyWLHS.Compositor.Types
import LibTinyWLHS.KeyBinding.KeyBindings
import LibTinyWLHS.KeyBinding.KeySyms
import qualified LibTinyWLHS.Server.FFI as FFI
import System.Process (spawnProcess)
import WLR.Util.Log

data Config = Config
    { logLevel :: WLR_log_importance
    , startupApplication :: String
    , modKey :: Modifier
    , terminalEmulator :: String
    }

appConfig :: Config -- Customize your app here, to help I placed the options in the comments
appConfig =
    Config
        { logLevel = WLR_DEBUG -- WLR_INFO | WLR_DEBUG | WLR_SILENT | WLR_ERROR
        , startupApplication = "" -- can be any app that works with wayland, Leave blank for no startup app
        , modKey = ModAlt -- ModAlt | ModCtrl | ModLogo | ModShift
        , terminalEmulator = "kitty" -- I use kitty as my emulator, alacritty is also a popular choice
        }

customKeybindings :: Ptr WlDisplay -> IO (FunPtr (CUInt -> IO ()))
customKeybindings display = do
    let handler :: CUInt -> IO ()
        handler sym = do
            -- Add your custom key event handler heres
            wlr_log WLR_INFO $ "Handler called with sym: " ++ show sym -- This will long as an int and key pressed while the mod key is held down
            when (sym == keySymToInt KEY_s) $ do
                -- simple match to key events defined in LibTinyWL.KeyBinding.KeySyms
                wlr_log WLR_INFO "Alt+s pressed, spawning a terminal emulator"
                _ <- spawnProcess (terminalEmulator appConfig) [] -- for this key event a process is spawned in Haskell
                pure ()
            when (sym == keySymToInt KEY_c) $ do
                -- the key Events just show up here as ints so you can also match against a raw int
                wlr_log WLR_INFO "Alt + c pressed closing server"
                FFI.c_wl_display_terminate display -- for this event we call a Wayland FFI function
                pure ()
    mkKeybindingHandler handler