{-|
Module      : SoftFloat
Copyright   : (c) Benjamin Selfridge, 2018
                  Galois Inc.
License     : BSD-3
Maintainer  : benselfridge@galois.com
Stability   : experimental
Portability : portable

This library provides a pure function interface to John Hauser's softfloat C library,
supported by underlying impure FFI calls. We support all 16-, 32-, and 64-bit
floating point operations that the original softfloat provides.

We represent floating point data as 'Word16', 'Word32', and 'Word64' unsigned
integers. We represent signed integers as 'Int32' and 'Int64'. We represent unsigned
integers as 'Word32' and 'Word64'.

Floating point functions fall into two major categories: type conversions, and
floating point operations. All functions take a 'RoundingMode' as an initial
argument, and return one of the above-mentioned data types, along with the resulting
'ExceptionFlags' that would be set by the operation. The softfloat library reads and
sets these variables implicitly. The point of this module is to provide the illusion
that these variables are pure functions, by carefully ensuring they are reset to 0
before every single function used in this library.

The data types involved should be clear from the function names: In general, 'f16',
'f32', and 'f64' refer to 16-, 32-, and 64-bit floats. 'i32' and 'i64' refer to
signed 32- and 64-bit integers. 'ui32' and 'ui64' refer to unsigned 32- and 64-bit
integers. We do not provide documentation for particular functions; instead, we
classify them in broad categories and document those categories. The user of this
module should be able to easily discern exactly what each individual function does
from its name and category description.
-}
{-# LANGUAGE DeriveFunctor #-}
module SoftFloat
  (
    -- * Result of floating-point computations
    F16Result
  , F32Result
  , F64Result
  , Ui32Result
  , Ui64Result
  , I32Result
  , I64Result
  , BoolResult
  , ExceptionFlags(..)

  -- * Rounding
  , RoundingMode(..)

    -- * Fixed-width integer to floating point conversions
    -- | Conversions from unsigned and signed 32- and 64-bit integers to all three
    -- supported floating point types.
  , ui32ToF16
  , ui32ToF32
  , ui32ToF64

  , i32ToF16
  , i32ToF32
  , i32ToF64

  , ui64ToF16
  , ui64ToF32
  , ui64ToF64

  , i64ToF16
  , i64ToF32
  , i64ToF64

  -- * Floating point to fixed-width integer conversions
  -- | Conversions from all three supported floating point types to unsigned and
  -- signed 32- and 64-bit integers.
  , f16ToUi32
  , f16ToUi64
  , f16ToI32
  , f16ToI64

  , f32ToUi32
  , f32ToUi64
  , f32ToI32
  , f32ToI64

  , f64ToUi32
  , f64ToUi64
  , f64ToI32
  , f64ToI64

  -- * Floating point to floating point conversions
  -- | Conversions from one floating point format to another.
  , f16ToF32
  , f16ToF64
  , f32ToF16
  , f32ToF64
  , f64ToF16
  , f64ToF32

  -- * 16-bit Floating point operations
  -- | All 16-bit floating point unary, binary, and comparison operations.
  , f16RoundToInt
  , f16Add
  , f16Sub
  , f16Mul
  , f16MulAdd
  , f16Div
  , f16Rem
  , f16Sqrt
  , f16Eq
  , f16Le
  , f16Lt
  , f16EqSignaling
  , f16LeQuiet
  , f16LtQuiet
  , f16IsSignalingNaN

  -- * 32-bit Floating point operations
  -- | All 32-bit floating point unary, binary, and comparison operations.
  , f32RoundToInt
  , f32Add
  , f32Sub
  , f32Mul
  , f32MulAdd
  , f32Div
  , f32Rem
  , f32Sqrt
  , f32Eq
  , f32Le
  , f32Lt
  , f32EqSignaling
  , f32LeQuiet
  , f32LtQuiet
  , f32IsSignalingNaN

  -- * 64-bit Floating point operations
  -- | All 64-bit floating point unary, binary, and comparison operations.
  , f64RoundToInt
  , f64Add
  , f64Sub
  , f64Mul
  , f64MulAdd
  , f64Div
  , f64Rem
  , f64Sqrt
  , f64Eq
  , f64Le
  , f64Lt
  , f64EqSignaling
  , f64LeQuiet
  , f64LtQuiet
  , f64IsSignalingNaN

  ) where

import Data.Int
import Data.Word

import SoftFloat.Internal

type Ui32Result = Word32
type Ui64Result = Word64
type I32Result = Int32
type I64Result = Int64
type F16Result = Word16
type F32Result = Word32
type F64Result = Word64
type BoolResult = Bool

-- | Data type for specifying rounding mode to a floating point computation.
data RoundingMode = RoundNearEven
                  | RoundMinMag
                  | RoundMin
                  | RoundMax
                  | RoundNearMaxMag
                  | RoundOdd
  deriving (Show, Eq)

roundingModeToInt :: (Integral a) => RoundingMode -> a
roundingModeToInt RoundNearEven = 0
roundingModeToInt RoundMinMag = 1
roundingModeToInt RoundMin = 2
roundingModeToInt RoundMax = 3
roundingModeToInt RoundNearMaxMag = 4
roundingModeToInt RoundOdd = 6

-- | Exception flags returned by a floating point computation.
data ExceptionFlags = ExceptionFlags
  { inexact   :: Bool -- x
  , underflow :: Bool -- u
  , overflow  :: Bool -- o
  , infinite  :: Bool -- z (division by zero)
  , invalid   :: Bool -- i
  } deriving (Eq, Show)


-- TODO remove?
-- doSoftFloatBool :: RoundingMode -> IO CBool -> BoolResult
-- doSoftFloatBool rm ioRes = fmap (/=0) $ doSoftFloat rm ioRes

----------------------------------------------------------------------
-- Integer to float conversions

ui32ToF16 :: RoundingMode -> Word32 -> F16Result
ui32ToF16 = ui32_to_f16 . roundingModeToInt

ui32ToF32 :: RoundingMode -> Word32 -> F32Result
ui32ToF32 = ui32_to_f32 . roundingModeToInt

ui32ToF64 :: RoundingMode -> Word32 -> F64Result
ui32ToF64 = ui32_to_f64 . roundingModeToInt

i32ToF16 :: RoundingMode -> Int32 -> F16Result
i32ToF16 = i32_to_f16 . roundingModeToInt

i32ToF32 :: RoundingMode -> Int32 -> F32Result
i32ToF32 = i32_to_f32 . roundingModeToInt

i32ToF64 :: RoundingMode -> Int32 -> F64Result
i32ToF64 = i32_to_f64 . roundingModeToInt

ui64ToF16 :: RoundingMode -> Word64 -> F16Result
ui64ToF16 = ui64_to_f16 . roundingModeToInt

ui64ToF32 :: RoundingMode -> Word64 -> F32Result
ui64ToF32 = ui64_to_f32 . roundingModeToInt

ui64ToF64 :: RoundingMode -> Word64 -> F64Result
ui64ToF64 = ui64_to_f64 . roundingModeToInt

i64ToF16 :: RoundingMode -> Int64 -> F16Result
i64ToF16 = i64_to_f16 . roundingModeToInt

i64ToF32 :: RoundingMode -> Int64 -> F32Result
i64ToF32 = i64_to_f32 . roundingModeToInt

i64ToF64 :: RoundingMode -> Int64 -> F64Result
i64ToF64 = i64_to_f64 . roundingModeToInt

----------------------------------------------------------------------
-- Float to integer conversions
f16ToUi32 :: RoundingMode -> Word16 -> Ui32Result
f16ToUi32 rm fa = f16_to_ui32 (roundingModeToInt rm) fa 0x1

f16ToUi64 :: RoundingMode -> Word16 -> Ui64Result
f16ToUi64 rm fa = f16_to_ui64 (roundingModeToInt rm) fa 0x1

f16ToI32 :: RoundingMode -> Word16 -> I32Result
f16ToI32 rm fa = f16_to_i32 (roundingModeToInt rm) fa 0x1

f16ToI64 :: RoundingMode -> Word16 -> I64Result
f16ToI64 rm fa = f16_to_i64 (roundingModeToInt rm) fa 0x1

f32ToUi32 :: RoundingMode -> Word32 -> Ui32Result
f32ToUi32 rm fa = f32_to_ui32 (roundingModeToInt rm) fa 0x1

f32ToUi64 :: RoundingMode -> Word32 -> Ui64Result
f32ToUi64 rm fa = f32_to_ui64 (roundingModeToInt rm) fa 0x1

f32ToI32 :: RoundingMode -> Word32 -> I32Result
f32ToI32 rm fa = f32_to_i32 (roundingModeToInt rm) fa 0x1

f32ToI64 :: RoundingMode -> Word32 -> I64Result
f32ToI64 rm fa = f32_to_i64 (roundingModeToInt rm) fa 0x1

f64ToUi32 :: RoundingMode -> Word64 -> Ui32Result
f64ToUi32 rm fa = f64_to_ui32 (roundingModeToInt rm) fa 0x1

f64ToUi64 :: RoundingMode -> Word64 -> Ui64Result
f64ToUi64 rm fa = f64_to_ui64 (roundingModeToInt rm) fa 0x1

f64ToI32 :: RoundingMode -> Word64 -> I32Result
f64ToI32 rm fa = f64_to_i32 (roundingModeToInt rm) fa 0x1

f64ToI64 :: RoundingMode -> Word64 -> I64Result
f64ToI64 rm fa = f64_to_i64 (roundingModeToInt rm) fa 0x1

----------------------------------------------------------------------
-- Float to float conversions

f16ToF32 :: RoundingMode -> Word16 -> F32Result
f16ToF32 = f16_to_f32 . roundingModeToInt

f16ToF64 :: RoundingMode -> Word16 -> F64Result
f16ToF64 = f16_to_f64 . roundingModeToInt

f32ToF16 :: RoundingMode -> Word32 -> F16Result
f32ToF16 = f32_to_f16 . roundingModeToInt

f32ToF64 :: RoundingMode -> Word32 -> F64Result
f32ToF64 = f32_to_f64 . roundingModeToInt

f64ToF16 :: RoundingMode -> Word64 -> F16Result
f64ToF16 = f64_to_f16 . roundingModeToInt

f64ToF32 :: RoundingMode -> Word64 -> F32Result
f64ToF32 = f64_to_f32 . roundingModeToInt

----------------------------------------------------------------------
-- 16-bit operations

f16RoundToInt :: RoundingMode -> Word16 -> F16Result
f16RoundToInt rm fa = f16_roundToInt (roundingModeToInt rm) fa 0x1

f16Add :: RoundingMode -> Word16 -> Word16 -> F16Result
f16Add = f16_add . roundingModeToInt

f16Sub :: RoundingMode -> Word16 -> Word16 -> F16Result
f16Sub = f16_sub . roundingModeToInt

f16Mul :: RoundingMode -> Word16 -> Word16 -> F16Result
f16Mul = f16_mul . roundingModeToInt

f16MulAdd :: RoundingMode -> Word16 -> Word16 -> Word16 -> F16Result
f16MulAdd = f16_mulAdd . roundingModeToInt

f16Div :: RoundingMode -> Word16 -> Word16 -> F16Result
f16Div = f16_div . roundingModeToInt

f16Rem :: RoundingMode -> Word16 -> Word16 -> F16Result
f16Rem = f16_rem . roundingModeToInt

f16Sqrt :: RoundingMode -> Word16 -> F16Result
f16Sqrt = f16_sqrt . roundingModeToInt

f16Eq :: Word16 -> Word16 -> BoolResult
f16Eq fa fb = f16_eq (roundingModeToInt RoundNearEven) fa fb /= 0

f16Le :: Word16 -> Word16 -> BoolResult
f16Le fa fb = f16_le (roundingModeToInt RoundNearEven) fa fb /= 0

f16Lt :: Word16 -> Word16 -> BoolResult
f16Lt fa fb = f16_lt (roundingModeToInt RoundNearEven) fa fb /= 0

f16EqSignaling :: Word16 -> Word16 -> BoolResult
f16EqSignaling fa fb = f16_eq_signaling (roundingModeToInt RoundNearEven) fa fb /= 0

f16LeQuiet :: Word16 -> Word16 -> BoolResult
f16LeQuiet fa fb = f16_le_quiet (roundingModeToInt RoundNearEven) fa fb /= 0

f16LtQuiet :: Word16 -> Word16 -> BoolResult
f16LtQuiet fa fb = f16_lt_quiet (roundingModeToInt RoundNearEven) fa fb /= 0

f16IsSignalingNaN :: Word16 -> BoolResult
f16IsSignalingNaN fa = f16_isSignalingNaN (roundingModeToInt RoundNearEven) fa /= 0

----------------------------------------------------------------------
-- 32-bit operations

f32RoundToInt :: RoundingMode -> Word32 -> F32Result
f32RoundToInt rm fa = f32_roundToInt (roundingModeToInt rm) fa 0x1

f32Add :: RoundingMode -> Word32 -> Word32 -> F32Result
f32Add = f32_add . roundingModeToInt

f32Sub :: RoundingMode -> Word32 -> Word32 -> F32Result
f32Sub = f32_sub . roundingModeToInt

f32Mul :: RoundingMode -> Word32 -> Word32 -> F32Result
f32Mul = f32_mul . roundingModeToInt

f32MulAdd :: RoundingMode -> Word32 -> Word32 -> Word32 -> F32Result
f32MulAdd = f32_mulAdd . roundingModeToInt

f32Div :: RoundingMode -> Word32 -> Word32 -> F32Result
f32Div = f32_div . roundingModeToInt

f32Rem :: RoundingMode -> Word32 -> Word32 -> F32Result
f32Rem = f32_rem . roundingModeToInt

f32Sqrt :: RoundingMode -> Word32 -> F32Result
f32Sqrt = f32_sqrt . roundingModeToInt

f32Eq :: Word32 -> Word32 -> BoolResult
f32Eq fa fb = f32_eq (roundingModeToInt RoundNearEven) fa fb /= 0

f32Le :: Word32 -> Word32 -> BoolResult
f32Le fa fb = f32_le (roundingModeToInt RoundNearEven) fa fb /= 0

f32Lt :: Word32 -> Word32 -> BoolResult
f32Lt fa fb = f32_lt (roundingModeToInt RoundNearEven) fa fb /= 0

f32EqSignaling :: Word32 -> Word32 -> BoolResult
f32EqSignaling fa fb = f32_eq_signaling (roundingModeToInt RoundNearEven) fa fb /= 0

f32LeQuiet :: Word32 -> Word32 -> BoolResult
f32LeQuiet fa fb = f32_le_quiet (roundingModeToInt RoundNearEven) fa fb /= 0

f32LtQuiet :: Word32 -> Word32 -> BoolResult
f32LtQuiet fa fb = f32_lt_quiet (roundingModeToInt RoundNearEven) fa fb /= 0

f32IsSignalingNaN :: Word32 -> BoolResult
f32IsSignalingNaN fa = f32_isSignalingNaN (roundingModeToInt RoundNearEven) fa /= 0

----------------------------------------------------------------------
-- 64-bit operations

f64RoundToInt :: RoundingMode -> Word64 -> F64Result
f64RoundToInt rm fa = f64_roundToInt (roundingModeToInt rm) fa 0x1

f64Add :: RoundingMode -> Word64 -> Word64 -> F64Result
f64Add = f64_add . roundingModeToInt

f64Sub :: RoundingMode -> Word64 -> Word64 -> F64Result
f64Sub = f64_sub . roundingModeToInt

f64Mul :: RoundingMode -> Word64 -> Word64 -> F64Result
f64Mul = f64_mul . roundingModeToInt

f64MulAdd :: RoundingMode -> Word64 -> Word64 -> Word64 -> F64Result
f64MulAdd = f64_mulAdd . roundingModeToInt

f64Div :: RoundingMode -> Word64 -> Word64 -> F64Result
f64Div = f64_div . roundingModeToInt

f64Rem :: RoundingMode -> Word64 -> Word64 -> F64Result
f64Rem = f64_rem . roundingModeToInt

f64Sqrt :: RoundingMode -> Word64 -> F64Result
f64Sqrt = f64_sqrt . roundingModeToInt

f64Eq :: Word64 -> Word64 -> BoolResult
f64Eq fa fb = f64_eq (roundingModeToInt RoundNearEven) fa fb /= 0

f64Le :: Word64 -> Word64 -> BoolResult
f64Le fa fb = f64_le (roundingModeToInt RoundNearEven) fa fb /= 0

f64Lt :: Word64 -> Word64 -> BoolResult
f64Lt fa fb = f64_lt (roundingModeToInt RoundNearEven) fa fb /= 0

f64EqSignaling :: Word64 -> Word64 -> BoolResult
f64EqSignaling fa fb = f64_eq_signaling (roundingModeToInt RoundNearEven) fa fb /= 0

f64LeQuiet :: Word64 -> Word64 -> BoolResult
f64LeQuiet fa fb = f64_le_quiet (roundingModeToInt RoundNearEven) fa fb /= 0

f64LtQuiet :: Word64 -> Word64 -> BoolResult
f64LtQuiet fa fb = f64_lt_quiet (roundingModeToInt RoundNearEven) fa fb /= 0

f64IsSignalingNaN :: Word64 -> BoolResult
f64IsSignalingNaN fa = f64_isSignalingNaN (roundingModeToInt RoundNearEven) fa /= 0

