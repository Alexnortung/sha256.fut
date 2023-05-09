def rotate_right (x: i32) (n: i32) =
  -- let num_bits = popc x.int_t.highest
  let num_bits = 32
  let actual_n = n % (num_bits)
  in (x >> actual_n) | (x << (num_bits - actual_n))

entry rotate_right_test (x: i32) (n: i32) : i32 =
  rotate_right x n

-- Test rotate right
-- ==
-- entry: rotate_right_test
-- input {0xf0f0i32 4i32}
-- output {0x0f0fi32}
