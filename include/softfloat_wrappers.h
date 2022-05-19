/*
 * Wrappers for softfloat functions. All functions beginning with the "hs_" prefix
 * have the same signature as their softfloat counterparts, except that all floatXX_t
 * types are changed to uintXX_t. This makes them more immediately compatible with
 * the Haskell FFI.
 */
#include <softfloat.h>

uint16_t hs_ui32_to_f16( uint8_t, uint32_t );
uint32_t hs_ui32_to_f32( uint8_t, uint32_t );
uint64_t hs_ui32_to_f64( uint8_t, uint32_t );
uint16_t hs_ui64_to_f16( uint8_t, uint64_t );
uint32_t hs_ui64_to_f32( uint8_t, uint64_t );
uint64_t hs_ui64_to_f64( uint8_t, uint64_t );
uint16_t hs_i32_to_f16( uint8_t, int32_t );
uint32_t hs_i32_to_f32( uint8_t, int32_t );
uint64_t hs_i32_to_f64( uint8_t, int32_t );
uint16_t hs_i64_to_f16( uint8_t, int64_t );
uint32_t hs_i64_to_f32( uint8_t, int64_t );
uint64_t hs_i64_to_f64( uint8_t, int64_t );

uint32_t hs_f16_to_ui32( uint8_t, uint16_t, bool );
uint64_t hs_f16_to_ui64( uint8_t, uint16_t, bool );
int32_t hs_f16_to_i32( uint8_t, uint16_t, bool );
int64_t hs_f16_to_i64( uint8_t, uint16_t, bool );
uint32_t hs_f16_to_f32( uint8_t, uint16_t );
uint64_t hs_f16_to_f64( uint8_t, uint16_t );

uint16_t hs_f16_roundToInt( uint8_t, uint16_t, bool );
uint16_t hs_f16_add( uint8_t, uint16_t, uint16_t );
uint16_t hs_f16_sub( uint8_t, uint16_t, uint16_t );
uint16_t hs_f16_mul( uint8_t, uint16_t, uint16_t );
uint16_t hs_f16_mulAdd( uint8_t, uint16_t, uint16_t, uint16_t );
uint16_t hs_f16_div( uint8_t, uint16_t, uint16_t );
uint16_t hs_f16_rem( uint8_t, uint16_t, uint16_t );
uint16_t hs_f16_sqrt( uint8_t, uint16_t );
bool hs_f16_eq( uint8_t, uint16_t, uint16_t );
bool hs_f16_le( uint8_t, uint16_t, uint16_t );
bool hs_f16_lt( uint8_t, uint16_t, uint16_t );
bool hs_f16_eq_signaling( uint8_t, uint16_t, uint16_t );
bool hs_f16_le_quiet( uint8_t, uint16_t, uint16_t );
bool hs_f16_lt_quiet( uint8_t, uint16_t, uint16_t );
bool hs_f16_isSignalingNaN( uint8_t, uint16_t );

uint32_t hs_f32_to_ui32( uint8_t, uint32_t, bool );
uint64_t hs_f32_to_ui64( uint8_t, uint32_t, bool );
int32_t hs_f32_to_i32( uint8_t, uint32_t, bool );
int64_t hs_f32_to_i64( uint8_t, uint32_t, bool );
uint32_t hs_f32_to_f16( uint8_t, uint32_t );
uint64_t hs_f32_to_f64( uint8_t, uint32_t );

uint32_t hs_f32_roundToInt( uint8_t, uint32_t, bool );
uint32_t hs_f32_add( uint8_t, uint32_t, uint32_t );
uint32_t hs_f32_sub( uint8_t, uint32_t, uint32_t );
uint32_t hs_f32_mul( uint8_t, uint32_t, uint32_t );
uint32_t hs_f32_mulAdd( uint8_t, uint32_t, uint32_t, uint32_t );
uint32_t hs_f32_div( uint8_t, uint32_t, uint32_t );
uint32_t hs_f32_rem( uint8_t, uint32_t, uint32_t );
uint32_t hs_f32_sqrt( uint8_t, uint32_t );
bool hs_f32_eq( uint8_t, uint32_t, uint32_t );
bool hs_f32_le( uint8_t, uint32_t, uint32_t );
bool hs_f32_lt( uint8_t, uint32_t, uint32_t );
bool hs_f32_eq_signaling( uint8_t, uint32_t, uint32_t );
bool hs_f32_le_quiet( uint8_t, uint32_t, uint32_t );
bool hs_f32_lt_quiet( uint8_t, uint32_t, uint32_t );
bool hs_f32_isSignalingNaN( uint8_t, uint32_t );


uint32_t hs_f64_to_ui32( uint8_t, uint64_t, bool );
uint64_t hs_f64_to_ui64( uint8_t, uint64_t, bool );
int32_t hs_f64_to_i32( uint8_t, uint64_t, bool );
int64_t hs_f64_to_i64( uint8_t, uint64_t, bool );
uint64_t hs_f64_to_f16( uint8_t, uint64_t );
uint32_t hs_f64_to_f32( uint8_t, uint64_t );

uint64_t hs_f64_roundToInt( uint8_t, uint64_t, bool );
uint64_t hs_f64_add( uint8_t, uint64_t, uint64_t );
uint64_t hs_f64_sub( uint8_t, uint64_t, uint64_t );
uint64_t hs_f64_mul( uint8_t, uint64_t, uint64_t );
uint64_t hs_f64_mulAdd( uint8_t, uint64_t, uint64_t, uint64_t );
uint64_t hs_f64_div( uint8_t, uint64_t, uint64_t );
uint64_t hs_f64_rem( uint8_t, uint64_t, uint64_t );
uint64_t hs_f64_sqrt( uint8_t, uint64_t );
bool hs_f64_eq( uint8_t, uint64_t, uint64_t );
bool hs_f64_le( uint8_t, uint64_t, uint64_t );
bool hs_f64_lt( uint8_t, uint64_t, uint64_t );
bool hs_f64_eq_signaling( uint8_t, uint64_t, uint64_t );
bool hs_f64_le_quiet( uint8_t, uint64_t, uint64_t );
bool hs_f64_lt_quiet( uint8_t, uint64_t, uint64_t );
bool hs_f64_isSignalingNaN( uint8_t, uint64_t );
