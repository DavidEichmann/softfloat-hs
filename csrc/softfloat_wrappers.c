/*
 * Wrappers for softfloat functions. All functions beginning with the "hs_" prefix
 * have the same signature as their softfloat counterparts, except that all floatXX_t
 * types are changed to uintXX_t. This makes them more immediately compatible with
 * the Haskell FFI.
 */

#include "softfloat_wrappers.h"

/****************************************
 * Integer to float conversions
 ****************************************/

uint16_t hs_ui32_to_f16( uint_fast8_t rm, uint32_t x ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float16_t f = ui32_to_f16(x);
  return f.v;
}

uint32_t hs_ui32_to_f32( uint_fast8_t rm, uint32_t x ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float32_t f = ui32_to_f32(x);
  return f.v;
}

uint64_t hs_ui32_to_f64( uint_fast8_t rm, uint32_t x ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float64_t f = ui32_to_f64(x);
  return f.v;
}

uint16_t hs_ui64_to_f16( uint_fast8_t rm, uint64_t x ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float16_t f = ui64_to_f16(x);
  return f.v;
}

uint32_t hs_ui64_to_f32( uint_fast8_t rm, uint64_t x ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float32_t f = ui64_to_f32(x);
  return f.v;
}

uint64_t hs_ui64_to_f64( uint_fast8_t rm, uint64_t x ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float64_t f = ui64_to_f64(x);
  return f.v;
}

uint16_t hs_i32_to_f16( uint_fast8_t rm, int32_t x ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float16_t f = i32_to_f16(x);
  return f.v;
}

uint32_t hs_i32_to_f32( uint_fast8_t rm, int32_t x ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float32_t f = i32_to_f32(x);
  return f.v;
}

uint64_t hs_i32_to_f64( uint_fast8_t rm, int32_t x ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float64_t f = i32_to_f64(x);
  return f.v;
}

uint16_t hs_i64_to_f16( uint_fast8_t rm, int64_t x ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float16_t f = i64_to_f16(x);
  return f.v;
}

uint32_t hs_i64_to_f32( uint_fast8_t rm, int64_t x ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float32_t f = i64_to_f32(x);
  return f.v;
}

uint64_t hs_i64_to_f64( uint_fast8_t rm, int64_t x ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float64_t f = i64_to_f64(x);
  return f.v;
}

/****************************************
 * 16-bit floating point operations
 ****************************************/

uint32_t hs_f16_to_ui32( uint_fast8_t rm, uint16_t f_int, bool inexact ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float16_t f;
  f.v = f_int;
  uint_fast32_t i = f16_to_ui32(f, rm, inexact);
  return (uint32_t) i;
}

uint64_t hs_f16_to_ui64( uint_fast8_t rm, uint16_t f_int, bool inexact ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float16_t f;
  f.v = f_int;
  uint_fast64_t i = f16_to_ui64(f, rm, inexact);
  return (uint64_t) i;
}

int32_t hs_f16_to_i32( uint_fast8_t rm, uint16_t f_int, bool inexact ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float16_t f;
  f.v = f_int;
  int_fast32_t i = f16_to_i32(f, rm, inexact);
  return (int32_t) i;
}

int64_t hs_f16_to_i64( uint_fast8_t rm, uint16_t f_int, bool inexact ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float16_t f;
  f.v = f_int;
  int_fast64_t i = f16_to_i64(f, rm, inexact);
  return (int64_t) i;
}

uint32_t hs_f16_to_f32( uint_fast8_t rm, uint16_t f_int ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float16_t f;
  f.v = f_int;
  float32_t g = f16_to_f32(f);
  return g.v;
}

uint64_t hs_f16_to_f64( uint_fast8_t rm, uint16_t f_int ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float16_t f;
  f.v = f_int;
  float64_t g = f16_to_f64(f);
  return g.v;
}

uint16_t hs_f16_roundToInt( uint_fast8_t rm, uint16_t f_int, bool inexact ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float16_t f;
  f.v = f_int;
  float16_t g = f16_roundToInt(f, rm, inexact);
  return g.v;
}

uint16_t hs_f16_add( uint_fast8_t rm, uint16_t f_int, uint16_t g_int ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float16_t f, g;
  f.v = f_int;
  g.v = g_int;
  float16_t h = f16_add(f, g);
  return h.v;
}
uint16_t hs_f16_sub( uint_fast8_t rm, uint16_t f_int, uint16_t g_int ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float16_t f, g;
  f.v = f_int;
  g.v = g_int;
  float16_t h = f16_sub(f, g);
  return h.v;
}
uint16_t hs_f16_mul( uint_fast8_t rm, uint16_t f_int, uint16_t g_int ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float16_t f, g;
  f.v = f_int;
  g.v = g_int;
  float16_t h = f16_mul(f, g);
  return h.v;
}
uint16_t hs_f16_mulAdd( uint_fast8_t rm, uint16_t f_int, uint16_t g_int, uint16_t h_int ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float16_t f, g, h;
  f.v = f_int;
  g.v = g_int;
  h.v = h_int;
  float16_t i = f16_mulAdd(f, g, h);
  return i.v;
}
uint16_t hs_f16_div( uint_fast8_t rm, uint16_t f_int, uint16_t g_int ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float16_t f, g;
  f.v = f_int;
  g.v = g_int;
  float16_t h = f16_div(f, g);
  return h.v;
}
uint16_t hs_f16_rem( uint_fast8_t rm, uint16_t f_int, uint16_t g_int ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float16_t f, g;
  f.v = f_int;
  g.v = g_int;
  float16_t h = f16_rem(f, g);
  return h.v;
}
uint16_t hs_f16_sqrt( uint_fast8_t rm, uint16_t f_int ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float16_t f;
  f.v = f_int;
  float16_t g = f16_sqrt(f);
  return g.v;
}
bool hs_f16_eq( uint_fast8_t rm, uint16_t f_int, uint16_t g_int ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float16_t f, g;
  f.v = f_int;
  g.v = g_int;
  bool res = f16_eq(f, g);
  return res;
}
bool hs_f16_le( uint_fast8_t rm, uint16_t f_int, uint16_t g_int ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float16_t f, g;
  f.v = f_int;
  g.v = g_int;
  bool res = f16_le(f, g);
  return res;
}
bool hs_f16_lt( uint_fast8_t rm, uint16_t f_int, uint16_t g_int ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float16_t f, g;
  f.v = f_int;
  g.v = g_int;
  bool res = f16_lt(f, g);
  return res;
}
bool hs_f16_eq_signaling( uint_fast8_t rm, uint16_t f_int, uint16_t g_int ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float16_t f, g;
  f.v = f_int;
  g.v = g_int;
  bool res = f16_eq_signaling(f, g);
  return res;
}
bool hs_f16_le_quiet( uint_fast8_t rm, uint16_t f_int, uint16_t g_int ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float16_t f, g;
  f.v = f_int;
  g.v = g_int;
  bool res = f16_le_quiet(f, g);
  return res;
}
bool hs_f16_lt_quiet( uint_fast8_t rm, uint16_t f_int, uint16_t g_int ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float16_t f, g;
  f.v = f_int;
  g.v = g_int;
  bool res = f16_lt_quiet(f, g);
  return res;
}
bool hs_f16_isSignalingNaN( uint_fast8_t rm, uint16_t f_int ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float16_t f;
  f.v = f_int;
  bool res = f16_isSignalingNaN(f);
  return res;
}

/****************************************
 * 32-bit floating point operations
 ****************************************/

uint32_t hs_f32_to_ui32( uint_fast8_t rm, uint32_t f_int, bool inexact ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float32_t f;
  f.v = f_int;
  uint_fast32_t i = f32_to_ui32(f, rm, inexact);
  return (uint32_t) i;
}

uint64_t hs_f32_to_ui64( uint_fast8_t rm, uint32_t f_int, bool inexact ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float32_t f;
  f.v = f_int;
  uint_fast64_t i = f32_to_ui64(f, rm, inexact);
  return (uint64_t) i;
}

int32_t hs_f32_to_i32( uint_fast8_t rm, uint32_t f_int, bool inexact ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float32_t f;
  f.v = f_int;
  int_fast32_t i = f32_to_i32(f, rm, inexact);
  return (int32_t) i;
}

int64_t hs_f32_to_i64( uint_fast8_t rm, uint32_t f_int, bool inexact ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float32_t f;
  f.v = f_int;
  int_fast64_t i = f32_to_i64(f, rm, inexact);
  return (int64_t) i;
}

uint32_t hs_f32_to_f16( uint_fast8_t rm, uint32_t f_int ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float32_t f;
  f.v = f_int;
  float16_t g = f32_to_f16(f);
  return g.v;
}

uint64_t hs_f32_to_f64( uint_fast8_t rm, uint32_t f_int ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float32_t f;
  f.v = f_int;
  float64_t g = f32_to_f64(f);
  return g.v;
}

uint32_t hs_f32_roundToInt( uint_fast8_t rm, uint32_t f_int, bool inexact ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float32_t f;
  f.v = f_int;
  float32_t g = f32_roundToInt(f, rm, inexact);
  return g.v;
}

uint32_t hs_f32_add( uint_fast8_t rm, uint32_t f_int, uint32_t g_int ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float32_t f, g;
  f.v = f_int;
  g.v = g_int;
  float32_t h = f32_add(f, g);
  return h.v;
}
uint32_t hs_f32_sub( uint_fast8_t rm, uint32_t f_int, uint32_t g_int ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float32_t f, g;
  f.v = f_int;
  g.v = g_int;
  float32_t h = f32_sub(f, g);
  return h.v;
}
uint32_t hs_f32_mul( uint_fast8_t rm, uint32_t f_int, uint32_t g_int ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float32_t f, g;
  f.v = f_int;
  g.v = g_int;
  float32_t h = f32_mul(f, g);
  return h.v;
}
uint32_t hs_f32_mulAdd( uint_fast8_t rm, uint32_t f_int, uint32_t g_int, uint32_t h_int ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float32_t f, g, h;
  f.v = f_int;
  g.v = g_int;
  h.v = h_int;
  float32_t i = f32_mulAdd(f, g, h);
  return i.v;
}
uint32_t hs_f32_div( uint_fast8_t rm, uint32_t f_int, uint32_t g_int ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float32_t f, g;
  f.v = f_int;
  g.v = g_int;
  float32_t h = f32_div(f, g);
  return h.v;
}
uint32_t hs_f32_rem( uint_fast8_t rm, uint32_t f_int, uint32_t g_int ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float32_t f, g;
  f.v = f_int;
  g.v = g_int;
  float32_t h = f32_rem(f, g);
  return h.v;
}
uint32_t hs_f32_sqrt( uint_fast8_t rm, uint32_t f_int ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float32_t f;
  f.v = f_int;
  float32_t g = f32_sqrt(f);
  return g.v;
}
bool hs_f32_eq( uint_fast8_t rm, uint32_t f_int, uint32_t g_int ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float32_t f, g;
  f.v = f_int;
  g.v = g_int;
  bool res = f32_eq(f, g);
  return res;
}
bool hs_f32_le( uint_fast8_t rm, uint32_t f_int, uint32_t g_int ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float32_t f, g;
  f.v = f_int;
  g.v = g_int;
  bool res = f32_le(f, g);
  return res;
}
bool hs_f32_lt( uint_fast8_t rm, uint32_t f_int, uint32_t g_int ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float32_t f, g;
  f.v = f_int;
  g.v = g_int;
  bool res = f32_lt(f, g);
  return res;
}
bool hs_f32_eq_signaling( uint_fast8_t rm, uint32_t f_int, uint32_t g_int ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float32_t f, g;
  f.v = f_int;
  g.v = g_int;
  bool res = f32_eq_signaling(f, g);
  return res;
}
bool hs_f32_le_quiet( uint_fast8_t rm, uint32_t f_int, uint32_t g_int ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float32_t f, g;
  f.v = f_int;
  g.v = g_int;
  bool res = f32_le_quiet(f, g);
  return res;
}
bool hs_f32_lt_quiet( uint_fast8_t rm, uint32_t f_int, uint32_t g_int ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float32_t f, g;
  f.v = f_int;
  g.v = g_int;
  bool res = f32_lt_quiet(f, g);
  return res;
}
bool hs_f32_isSignalingNaN( uint_fast8_t rm, uint32_t f_int ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float32_t f;
  f.v = f_int;
  bool res = f32_isSignalingNaN(f);
  return res;
}

/****************************************
 * 64-bit floating point operations
 ****************************************/

uint32_t hs_f64_to_ui32( uint_fast8_t rm, uint64_t f_int, bool inexact ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float64_t f;
  f.v = f_int;
  uint_fast32_t i = f64_to_ui32(f, rm, inexact);
  return (uint32_t) i;
}

uint64_t hs_f64_to_ui64( uint_fast8_t rm, uint64_t f_int, bool inexact ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float64_t f;
  f.v = f_int;
  uint_fast64_t i = f64_to_ui64(f, rm, inexact);
  return (uint64_t) i;
}

int32_t hs_f64_to_i32( uint_fast8_t rm, uint64_t f_int, bool inexact ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float64_t f;
  f.v = f_int;
  int_fast32_t i = f64_to_i32(f, rm, inexact);
  return (int32_t) i;
}

int64_t hs_f64_to_i64( uint_fast8_t rm, uint64_t f_int, bool inexact ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float64_t f;
  f.v = f_int;
  int_fast64_t i = f64_to_i64(f, rm, inexact);
  return (int64_t) i;
}

uint64_t hs_f64_to_f16( uint_fast8_t rm, uint64_t f_int ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float64_t f;
  f.v = f_int;
  float16_t g = f64_to_f16(f);
  return g.v;
}

uint32_t hs_f64_to_f32( uint_fast8_t rm, uint64_t f_int ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float64_t f;
  f.v = f_int;
  float32_t g = f64_to_f32(f);
  return g.v;
}

uint64_t hs_f64_roundToInt( uint_fast8_t rm, uint64_t f_int, bool inexact ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float64_t f;
  f.v = f_int;
  float64_t g = f64_roundToInt(f, rm, inexact);
  return g.v;
}

uint64_t hs_f64_add( uint_fast8_t rm, uint64_t f_int, uint64_t g_int ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float64_t f, g;
  f.v = f_int;
  g.v = g_int;
  float64_t h = f64_add(f, g);
  return h.v;
}
uint64_t hs_f64_sub( uint_fast8_t rm, uint64_t f_int, uint64_t g_int ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float64_t f, g;
  f.v = f_int;
  g.v = g_int;
  float64_t h = f64_sub(f, g);
  return h.v;
}
uint64_t hs_f64_mul( uint_fast8_t rm, uint64_t f_int, uint64_t g_int ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float64_t f, g;
  f.v = f_int;
  g.v = g_int;
  float64_t h = f64_mul(f, g);
  return h.v;
}
uint64_t hs_f64_mulAdd( uint_fast8_t rm, uint64_t f_int, uint64_t g_int, uint64_t h_int ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float64_t f, g, h;
  f.v = f_int;
  g.v = g_int;
  h.v = h_int;
  float64_t i = f64_mulAdd(f, g, h);
  return i.v;
}
uint64_t hs_f64_div( uint_fast8_t rm, uint64_t f_int, uint64_t g_int ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float64_t f, g;
  f.v = f_int;
  g.v = g_int;
  float64_t h = f64_div(f, g);
  return h.v;
}
uint64_t hs_f64_rem( uint_fast8_t rm, uint64_t f_int, uint64_t g_int ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float64_t f, g;
  f.v = f_int;
  g.v = g_int;
  float64_t h = f64_rem(f, g);
  return h.v;
}
uint64_t hs_f64_sqrt( uint_fast8_t rm, uint64_t f_int ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float64_t f;
  f.v = f_int;
  float64_t g = f64_sqrt(f);
  return g.v;
}
bool hs_f64_eq( uint_fast8_t rm, uint64_t f_int, uint64_t g_int ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float64_t f, g;
  f.v = f_int;
  g.v = g_int;
  bool res = f64_eq(f, g);
  return res;
}
bool hs_f64_le( uint_fast8_t rm, uint64_t f_int, uint64_t g_int ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float64_t f, g;
  f.v = f_int;
  g.v = g_int;
  bool res = f64_le(f, g);
  return res;
}
bool hs_f64_lt( uint_fast8_t rm, uint64_t f_int, uint64_t g_int ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float64_t f, g;
  f.v = f_int;
  g.v = g_int;
  bool res = f64_lt(f, g);
  return res;
}
bool hs_f64_eq_signaling( uint_fast8_t rm, uint64_t f_int, uint64_t g_int ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float64_t f, g;
  f.v = f_int;
  g.v = g_int;
  bool res = f64_eq_signaling(f, g);
  return res;
}
bool hs_f64_le_quiet( uint_fast8_t rm, uint64_t f_int, uint64_t g_int ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float64_t f, g;
  f.v = f_int;
  g.v = g_int;
  bool res = f64_le_quiet(f, g);
  return res;
}
bool hs_f64_lt_quiet( uint_fast8_t rm, uint64_t f_int, uint64_t g_int ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float64_t f, g;
  f.v = f_int;
  g.v = g_int;
  bool res = f64_lt_quiet(f, g);
  return res;
}
bool hs_f64_isSignalingNaN( uint_fast8_t rm, uint64_t f_int ) {
  softfloat_detectTininess = 0;
  softfloat_exceptionFlags = 0;
  softfloat_roundingMode = rm;
  extF80_roundingPrecision = 80;
  float64_t f;
  f.v = f_int;
  bool res = f64_isSignalingNaN(f);
  return res;
}
