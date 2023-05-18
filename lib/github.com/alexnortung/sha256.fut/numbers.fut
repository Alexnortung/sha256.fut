def rotate_right (x: u32) (n: u32) =
  -- let num_bits = popc x.int_t.highest
  let num_bits = 32
  let actual_n = n & (num_bits - 1)
  in (x >> actual_n) | (x << (num_bits - actual_n))

-- This is unsafe because it doesn't check that n is less than 32
def rotate_right_unsafe (x: u32) (n: u32) =
  (x >> n) | (x << (32 - n))

def i64_to_u8_array (n: i64): [8]u8 =
  let shifts = reverse (iota 8)
  let shifted = map (\i -> u8.i64 (n >> (i * 8))) shifts
  in shifted

-- TODO: make an optimized flattened version of this
def u8_array_to_u32 (a: [4]u8): u32 =
  let shifts = iota 4 |> reverse
  let shifted = map2 (\i byte -> (u32.u8 byte) << ((u32.i64 i) * 8u32)) shifts a
  in reduce (\x y -> x | y) 0u32 shifted

-- Tests

entry rotate_right_test (x: u32) (n: u32) : u32 =
  rotate_right x n

entry rotate_right_unsafe_test (x: u32) (n: u32) : u32 =
  rotate_right_unsafe x n

-- Test rotate right
-- ==
-- entry: rotate_right_test
-- input {0xf0f0u32 4u32}
-- output {0x0f0fu32}

-- Test rotate right unsafe
-- ==
-- entry: rotate_right_unsafe_test
-- input {0xf0f0u32 4u32}
-- output {0x0f0fu32}


entry i64_to_u8_array_test (n: i64): []u8 =
  i64_to_u8_array n
-- Test of generating the last 64 bits of a message
-- ==
-- entry: i64_to_u8_array_test
-- input {3i64}
-- output {[ 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 3u8]}
-- input {257i64}
-- output {[ 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 1u8, 1u8]}
-- input {1025i64}
-- output {[ 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 4u8, 1u8]}

entry u8_array_to_u32_test (a: [4]u8): u32 =
  u8_array_to_u32 a

-- Test make u8 array to u32
-- ==
-- entry: u8_array_to_u32_test
-- input {[ 0u8, 0u8, 0u8, 0u8]}
-- output {0u32}
-- input {[ 0u8, 0u8, 0u8, 1u8]}
-- output {1u32}
-- input {[ 0x0au8, 0xf1u8, 0x10u8, 0xffu8]}
-- output {0x0af110ffu32}
