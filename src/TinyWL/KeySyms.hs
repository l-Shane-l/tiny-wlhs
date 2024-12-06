{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}

module TinyWL.KeySyms (
    KeySym (..),
    keySymToInt,
) where

import Foreign.C.Types (CUInt)

data KeySym where
    KEY_a :: KeySym
    KEY_b :: KeySym
    KEY_c :: KeySym
    KEY_d :: KeySym
    KEY_e :: KeySym
    KEY_f :: KeySym
    KEY_s :: KeySym
    KEY_Return :: KeySym
    KEY_space :: KeySym
    KEY_Escape :: KeySym
    KEY_Tab :: KeySym
    KEY_BackSpace :: KeySym
    KEY_q :: KeySym
    KEY_w :: KeySym
    KEY_1 :: KeySym
    KEY_2 :: KeySym
    KEY_3 :: KeySym
    deriving (Eq, Show)

keySymToInt :: KeySym -> CUInt
keySymToInt = \case
    KEY_a -> 97
    KEY_b -> 98
    KEY_c -> 99
    KEY_d -> 100
    KEY_e -> 101
    KEY_f -> 102
    KEY_s -> 115
    KEY_Return -> 65293
    KEY_space -> 32
    KEY_Escape -> 65307
    KEY_Tab -> 65289
    KEY_BackSpace -> 65288
    KEY_q -> 113
    KEY_w -> 119
    KEY_1 -> 49
    KEY_2 -> 50
    KEY_3 -> 51
