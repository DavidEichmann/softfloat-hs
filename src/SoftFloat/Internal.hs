{-|
Module      : SoftFloat.Internal
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : BSD-3
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

This module provides the underlying impure FFI calls to softfloat.
-}

module SoftFloat.Internal
  ( -- * Integer to float conversion
    ui32_to_f16
  , ui32_to_f32
  , ui32_to_f64

  , ui64_to_f16
  , ui64_to_f32
  , ui64_to_f64

  , i32_to_f16
  , i32_to_f32
  , i32_to_f64

  , i64_to_f16
  , i64_to_f32
  , i64_to_f64

  -- * Float to integer conversions

  , f16_to_ui32
  , f16_to_ui64
  , f16_to_i32
  , f16_to_i64

  , f32_to_ui32
  , f32_to_ui64
  , f32_to_i32
  , f32_to_i64

  , f64_to_ui32
  , f64_to_ui64
  , f64_to_i32
  , f64_to_i64

  -- * Float to float conversions

  , f16_to_f32
  , f16_to_f64
  , f32_to_f16
  , f32_to_f64
  , f64_to_f16
  , f64_to_f32

  -- * 16-bit floating point operations
  , f16_roundToInt
  , f16_add
  , f16_sub
  , f16_mul
  , f16_mulAdd
  , f16_div
  , f16_rem
  , f16_sqrt
  , f16_eq
  , f16_le
  , f16_lt
  , f16_eq_signaling
  , f16_le_quiet
  , f16_lt_quiet
  , f16_isSignalingNaN

  -- * 32-bit floating point operations
  , f32_roundToInt
  , f32_add
  , f32_sub
  , f32_mul
  , f32_mulAdd
  , f32_div
  , f32_rem
  , f32_sqrt
  , f32_eq
  , f32_le
  , f32_lt
  , f32_eq_signaling
  , f32_le_quiet
  , f32_lt_quiet
  , f32_isSignalingNaN

  -- * 64-bit floating point operations
  , f64_roundToInt
  , f64_add
  , f64_sub
  , f64_mul
  , f64_mulAdd
  , f64_div
  , f64_rem
  , f64_sqrt
  , f64_eq
  , f64_le
  , f64_lt
  , f64_eq_signaling
  , f64_le_quiet
  , f64_lt_quiet
  , f64_isSignalingNaN
  ) where

import Data.Int
import Data.Word
import Foreign.C.Types

type CRoundingMode = Word8

-- Integer to float conversion routines

foreign import ccall unsafe "softfloat_wrappers.h hs_ui32_to_f16" ui32_to_f16 :: CRoundingMode -> Word32 -> Word16
foreign import ccall unsafe "softfloat_wrappers.h hs_ui32_to_f32" ui32_to_f32 :: CRoundingMode -> Word32 -> Word32
foreign import ccall unsafe "softfloat_wrappers.h hs_ui32_to_f64" ui32_to_f64 :: CRoundingMode -> Word32 -> Word64

foreign import ccall unsafe "softfloat_wrappers.h hs_ui64_to_f16" ui64_to_f16 :: CRoundingMode -> Word64 -> Word16
foreign import ccall unsafe "softfloat_wrappers.h hs_ui64_to_f32" ui64_to_f32 :: CRoundingMode -> Word64 -> Word32
foreign import ccall unsafe "softfloat_wrappers.h hs_ui64_to_f64" ui64_to_f64 :: CRoundingMode -> Word64 -> Word64

foreign import ccall unsafe "softfloat_wrappers.h hs_i32_to_f16" i32_to_f16 :: CRoundingMode -> Int32 -> Word16
foreign import ccall unsafe "softfloat_wrappers.h hs_i32_to_f32" i32_to_f32 :: CRoundingMode -> Int32 -> Word32
foreign import ccall unsafe "softfloat_wrappers.h hs_i32_to_f64" i32_to_f64 :: CRoundingMode -> Int32 -> Word64

foreign import ccall unsafe "softfloat_wrappers.h hs_i64_to_f16" i64_to_f16 :: CRoundingMode -> Int64 -> Word16
foreign import ccall unsafe "softfloat_wrappers.h hs_i64_to_f32" i64_to_f32 :: CRoundingMode -> Int64 -> Word32
foreign import ccall unsafe "softfloat_wrappers.h hs_i64_to_f64" i64_to_f64 :: CRoundingMode -> Int64 -> Word64

-- Float to integer conversion routines

foreign import ccall unsafe "softfloat_wrappers.h hs_f16_to_ui32" f16_to_ui32 :: CRoundingMode -> Word16 -> CBool -> Word32
foreign import ccall unsafe "softfloat_wrappers.h hs_f16_to_ui64" f16_to_ui64 :: CRoundingMode -> Word16 -> CBool -> Word64
foreign import ccall unsafe "softfloat_wrappers.h hs_f16_to_i32"  f16_to_i32  :: CRoundingMode -> Word16 -> CBool -> Int32
foreign import ccall unsafe "softfloat_wrappers.h hs_f16_to_i64"  f16_to_i64  :: CRoundingMode -> Word16 -> CBool -> Int64

foreign import ccall unsafe "softfloat_wrappers.h hs_f32_to_ui32" f32_to_ui32 :: CRoundingMode -> Word32 -> CBool -> Word32
foreign import ccall unsafe "softfloat_wrappers.h hs_f32_to_ui64" f32_to_ui64 :: CRoundingMode -> Word32 -> CBool -> Word64
foreign import ccall unsafe "softfloat_wrappers.h hs_f32_to_i32"  f32_to_i32  :: CRoundingMode -> Word32 -> CBool -> Int32
foreign import ccall unsafe "softfloat_wrappers.h hs_f32_to_i64"  f32_to_i64  :: CRoundingMode -> Word32 -> CBool -> Int64

foreign import ccall unsafe "softfloat_wrappers.h hs_f64_to_ui32" f64_to_ui32 :: CRoundingMode -> Word64 -> CBool -> Word32
foreign import ccall unsafe "softfloat_wrappers.h hs_f64_to_ui64" f64_to_ui64 :: CRoundingMode -> Word64 -> CBool -> Word64
foreign import ccall unsafe "softfloat_wrappers.h hs_f64_to_i32"  f64_to_i32  :: CRoundingMode -> Word64 -> CBool -> Int32
foreign import ccall unsafe "softfloat_wrappers.h hs_f64_to_i64"  f64_to_i64  :: CRoundingMode -> Word64 -> CBool -> Int64

-- Float to float conversion routines

foreign import ccall unsafe "softfloat_wrappers.h hs_f16_to_f32" f16_to_f32 :: CRoundingMode -> Word16 -> Word32
foreign import ccall unsafe "softfloat_wrappers.h hs_f16_to_f64" f16_to_f64 :: CRoundingMode -> Word16 -> Word64
foreign import ccall unsafe "softfloat_wrappers.h hs_f32_to_f16" f32_to_f16 :: CRoundingMode -> Word32 -> Word16
foreign import ccall unsafe "softfloat_wrappers.h hs_f32_to_f64" f32_to_f64 :: CRoundingMode -> Word32 -> Word64
foreign import ccall unsafe "softfloat_wrappers.h hs_f64_to_f16" f64_to_f16 :: CRoundingMode -> Word64 -> Word16
foreign import ccall unsafe "softfloat_wrappers.h hs_f64_to_f32" f64_to_f32 :: CRoundingMode -> Word64 -> Word32


-- 16-bit operations
foreign import ccall unsafe "softfloat_wrappers.h hs_f16_roundToInt" f16_roundToInt :: CRoundingMode -> Word16 -> CBool -> Word16
foreign import ccall unsafe "softfloat_wrappers.h hs_f16_add"  f16_add  :: CRoundingMode -> Word16 -> Word16 -> Word16
foreign import ccall unsafe "softfloat_wrappers.h hs_f16_sub"  f16_sub  :: CRoundingMode -> Word16 -> Word16 -> Word16
foreign import ccall unsafe "softfloat_wrappers.h hs_f16_mul"  f16_mul  :: CRoundingMode -> Word16 -> Word16 -> Word16
foreign import ccall unsafe "softfloat_wrappers.h hs_f16_mulAdd"  f16_mulAdd  :: CRoundingMode -> Word16 -> Word16 -> Word16 -> Word16
foreign import ccall unsafe "softfloat_wrappers.h hs_f16_div"  f16_div  :: CRoundingMode -> Word16 -> Word16 -> Word16
foreign import ccall unsafe "softfloat_wrappers.h hs_f16_rem"  f16_rem  :: CRoundingMode -> Word16 -> Word16 -> Word16
foreign import ccall unsafe "softfloat_wrappers.h hs_f16_sqrt" f16_sqrt :: CRoundingMode -> Word16 -> Word16
foreign import ccall unsafe "softfloat_wrappers.h hs_f16_eq"   f16_eq   :: CRoundingMode -> Word16 -> Word16 -> CBool
foreign import ccall unsafe "softfloat_wrappers.h hs_f16_le"   f16_le   :: CRoundingMode -> Word16 -> Word16 -> CBool
foreign import ccall unsafe "softfloat_wrappers.h hs_f16_lt"   f16_lt   :: CRoundingMode -> Word16 -> Word16 -> CBool
foreign import ccall unsafe "softfloat_wrappers.h hs_f16_eq_signaling" f16_eq_signaling :: CRoundingMode -> Word16 -> Word16 -> CBool
foreign import ccall unsafe "softfloat_wrappers.h hs_f16_le_quiet"     f16_le_quiet     :: CRoundingMode -> Word16 -> Word16 -> CBool
foreign import ccall unsafe "softfloat_wrappers.h hs_f16_lt_quiet"     f16_lt_quiet     :: CRoundingMode -> Word16 -> Word16 -> CBool
foreign import ccall unsafe "softfloat_wrappers.h hs_f16_isSignalingNaN" f16_isSignalingNaN :: CRoundingMode -> Word16 -> CBool

foreign import ccall unsafe "softfloat_wrappers.h hs_f32_roundToInt" f32_roundToInt :: CRoundingMode -> Word32 -> CBool -> Word32
foreign import ccall unsafe "softfloat_wrappers.h hs_f32_add"  f32_add  :: CRoundingMode -> Word32 -> Word32 -> Word32
foreign import ccall unsafe "softfloat_wrappers.h hs_f32_sub"  f32_sub  :: CRoundingMode -> Word32 -> Word32 -> Word32
foreign import ccall unsafe "softfloat_wrappers.h hs_f32_mul"  f32_mul  :: CRoundingMode -> Word32 -> Word32 -> Word32
foreign import ccall unsafe "softfloat_wrappers.h hs_f32_mulAdd"  f32_mulAdd  :: CRoundingMode -> Word32 -> Word32 -> Word32 -> Word32
foreign import ccall unsafe "softfloat_wrappers.h hs_f32_div"  f32_div  :: CRoundingMode -> Word32 -> Word32 -> Word32
foreign import ccall unsafe "softfloat_wrappers.h hs_f32_rem"  f32_rem  :: CRoundingMode -> Word32 -> Word32 -> Word32
foreign import ccall unsafe "softfloat_wrappers.h hs_f32_sqrt" f32_sqrt :: CRoundingMode -> Word32 -> Word32
foreign import ccall unsafe "softfloat_wrappers.h hs_f32_eq"   f32_eq   :: CRoundingMode -> Word32 -> Word32 -> CBool
foreign import ccall unsafe "softfloat_wrappers.h hs_f32_le"   f32_le   :: CRoundingMode -> Word32 -> Word32 -> CBool
foreign import ccall unsafe "softfloat_wrappers.h hs_f32_lt"   f32_lt   :: CRoundingMode -> Word32 -> Word32 -> CBool
foreign import ccall unsafe "softfloat_wrappers.h hs_f32_eq_signaling" f32_eq_signaling :: CRoundingMode -> Word32 -> Word32 -> CBool
foreign import ccall unsafe "softfloat_wrappers.h hs_f32_le_quiet"     f32_le_quiet     :: CRoundingMode -> Word32 -> Word32 -> CBool
foreign import ccall unsafe "softfloat_wrappers.h hs_f32_lt_quiet"     f32_lt_quiet     :: CRoundingMode -> Word32 -> Word32 -> CBool
foreign import ccall unsafe "softfloat_wrappers.h hs_f32_isSignalingNaN" f32_isSignalingNaN :: CRoundingMode -> Word32 -> CBool

foreign import ccall unsafe "softfloat_wrappers.h hs_f64_roundToInt" f64_roundToInt :: CRoundingMode -> Word64 -> CBool -> Word64
foreign import ccall unsafe "softfloat_wrappers.h hs_f64_add"  f64_add  :: CRoundingMode -> Word64 -> Word64 -> Word64
foreign import ccall unsafe "softfloat_wrappers.h hs_f64_sub"  f64_sub  :: CRoundingMode -> Word64 -> Word64 -> Word64
foreign import ccall unsafe "softfloat_wrappers.h hs_f64_mul"  f64_mul  :: CRoundingMode -> Word64 -> Word64 -> Word64
foreign import ccall unsafe "softfloat_wrappers.h hs_f64_mulAdd"  f64_mulAdd  :: CRoundingMode -> Word64 -> Word64 -> Word64 -> Word64
foreign import ccall unsafe "softfloat_wrappers.h hs_f64_div"  f64_div  :: CRoundingMode -> Word64 -> Word64 -> Word64
foreign import ccall unsafe "softfloat_wrappers.h hs_f64_rem"  f64_rem  :: CRoundingMode -> Word64 -> Word64 -> Word64
foreign import ccall unsafe "softfloat_wrappers.h hs_f64_sqrt" f64_sqrt :: CRoundingMode -> Word64 -> Word64
foreign import ccall unsafe "softfloat_wrappers.h hs_f64_eq"   f64_eq   :: CRoundingMode -> Word64 -> Word64 -> CBool
foreign import ccall unsafe "softfloat_wrappers.h hs_f64_le"   f64_le   :: CRoundingMode -> Word64 -> Word64 -> CBool
foreign import ccall unsafe "softfloat_wrappers.h hs_f64_lt"   f64_lt   :: CRoundingMode -> Word64 -> Word64 -> CBool
foreign import ccall unsafe "softfloat_wrappers.h hs_f64_eq_signaling" f64_eq_signaling :: CRoundingMode -> Word64 -> Word64 -> CBool
foreign import ccall unsafe "softfloat_wrappers.h hs_f64_le_quiet"     f64_le_quiet     :: CRoundingMode -> Word64 -> Word64 -> CBool
foreign import ccall unsafe "softfloat_wrappers.h hs_f64_lt_quiet"     f64_lt_quiet     :: CRoundingMode -> Word64 -> Word64 -> CBool
foreign import ccall unsafe "softfloat_wrappers.h hs_f64_isSignalingNaN" f64_isSignalingNaN :: CRoundingMode -> Word64 -> CBool
