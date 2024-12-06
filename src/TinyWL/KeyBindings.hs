module TinyWL.KeyBindings where

import Foreign.C.Types (CUInt (..)) -- Note the (..) to import the constructor
import Foreign.Ptr (FunPtr)

foreign import ccall "wrapper" mkKeybindingHandler :: (CUInt -> IO ()) -> IO (FunPtr (CUInt -> IO ()))

data Modifier
    = ModLogo -- WLR_MODIFIER_LOGO
    | ModAlt -- WLR_MODIFIER_ALT
    | ModShift -- WLR_MODIFIER_SHIFT
    | ModCtrl -- WLR_MODIFIER_CTRL
    deriving (Eq, Show)

foreign import ccall "set_modifier_key"
    setModifierKey :: CUInt -> IO ()

setModKey :: Modifier -> IO ()
setModKey ModLogo = setModifierKey 0x40
setModKey ModAlt = setModifierKey 0x8
