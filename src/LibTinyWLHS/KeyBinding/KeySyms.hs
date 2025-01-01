{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use camelCase" #-}
module LibTinyWLHS.KeyBinding.KeySyms
    ( KeySym (..)
    , keySymToInt
    )
where

import Foreign.C.Types (CUInt)

data KeySym where
    -- Numbers
    KEY_0 :: KeySym
    KEY_1 :: KeySym
    KEY_2 :: KeySym
    KEY_3 :: KeySym
    KEY_4 :: KeySym
    KEY_5 :: KeySym
    KEY_6 :: KeySym
    KEY_7 :: KeySym
    KEY_8 :: KeySym
    KEY_9 :: KeySym
    -- Lowercase Letters
    KEY_a :: KeySym
    KEY_b :: KeySym
    KEY_c :: KeySym
    KEY_d :: KeySym
    KEY_e :: KeySym
    KEY_f :: KeySym
    KEY_g :: KeySym
    KEY_h :: KeySym
    KEY_i :: KeySym
    KEY_j :: KeySym
    KEY_k :: KeySym
    KEY_l :: KeySym
    KEY_m :: KeySym
    KEY_n :: KeySym
    KEY_o :: KeySym
    KEY_p :: KeySym
    KEY_q :: KeySym
    KEY_r :: KeySym
    KEY_s :: KeySym
    KEY_t :: KeySym
    KEY_u :: KeySym
    KEY_v :: KeySym
    KEY_w :: KeySym
    KEY_x :: KeySym
    KEY_y :: KeySym
    KEY_z :: KeySym
    -- Uppercase Letters
    KEY_A :: KeySym
    KEY_B :: KeySym
    KEY_C :: KeySym
    KEY_D :: KeySym
    KEY_E :: KeySym
    KEY_F :: KeySym
    KEY_G :: KeySym
    KEY_H :: KeySym
    KEY_I :: KeySym
    KEY_J :: KeySym
    KEY_K :: KeySym
    KEY_L :: KeySym
    KEY_M :: KeySym
    KEY_N :: KeySym
    KEY_O :: KeySym
    KEY_P :: KeySym
    KEY_Q :: KeySym
    KEY_R :: KeySym
    KEY_S :: KeySym
    KEY_T :: KeySym
    KEY_U :: KeySym
    KEY_V :: KeySym
    KEY_W :: KeySym
    KEY_X :: KeySym
    KEY_Y :: KeySym
    KEY_Z :: KeySym
    -- Special Keys
    KEY_Return :: KeySym
    KEY_space :: KeySym
    KEY_Escape :: KeySym
    KEY_Tab :: KeySym
    KEY_BackSpace :: KeySym
    KEY_Logo :: KeySym
    deriving (Eq, Show)

keySymToInt :: KeySym -> CUInt
keySymToInt = \case
    -- Numbers (ASCII values)
    KEY_0 -> 48
    KEY_1 -> 49
    KEY_2 -> 50
    KEY_3 -> 51
    KEY_4 -> 52
    KEY_5 -> 53
    KEY_6 -> 54
    KEY_7 -> 55
    KEY_8 -> 56
    KEY_9 -> 57
    -- Lowercase Letters
    KEY_a -> 97
    KEY_b -> 98
    KEY_c -> 99
    KEY_d -> 100
    KEY_e -> 101
    KEY_f -> 102
    KEY_g -> 103
    KEY_h -> 104
    KEY_i -> 105
    KEY_j -> 106
    KEY_k -> 107
    KEY_l -> 108
    KEY_m -> 109
    KEY_n -> 110
    KEY_o -> 111
    KEY_p -> 112
    KEY_q -> 113
    KEY_r -> 114
    KEY_s -> 115
    KEY_t -> 116
    KEY_u -> 117
    KEY_v -> 118
    KEY_w -> 119
    KEY_x -> 120
    KEY_y -> 121
    KEY_z -> 122
    -- Uppercase Letters
    KEY_A -> 65
    KEY_B -> 66
    KEY_C -> 67
    KEY_D -> 68
    KEY_E -> 69
    KEY_F -> 70
    KEY_G -> 71
    KEY_H -> 72
    KEY_I -> 73
    KEY_J -> 74
    KEY_K -> 75
    KEY_L -> 76
    KEY_M -> 77
    KEY_N -> 78
    KEY_O -> 79
    KEY_P -> 80
    KEY_Q -> 81
    KEY_R -> 82
    KEY_S -> 83
    KEY_T -> 84
    KEY_U -> 85
    KEY_V -> 86
    KEY_W -> 87
    KEY_X -> 88
    KEY_Y -> 89
    KEY_Z -> 90
    -- Special Keys (X11 keysyms)
    KEY_Return -> 65293
    KEY_space -> 32
    KEY_Escape -> 65307
    KEY_Tab -> 65289
    KEY_BackSpace -> 65288
    KEY_Logo -> 65515 -- Super_L in X11
