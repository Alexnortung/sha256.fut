import "./numbers"

def get_last_64_bits (n: i64) = i64_to_u8_array (n * 8)

def pad_message [n] (message: [n]u8) (m: i64): [m]u8 =
  let appended_one = 0x80u8
  let new_arr = replicate m 0u8
  let indexes = iota n
  -- the 64 last bits of the message should be the length of the original message
  let last_number = get_last_64_bits n
  let last_number_indexes = map (\i -> i + (m - 8)) (iota 8)
  let padded = scatter new_arr indexes message
  let padded_with_one = scatter padded [n] [appended_one]
  let with_last_number = scatter padded_with_one last_number_indexes last_number
  in with_last_number

def get_padding_for_message [n] (message: [n]u8): i64 =
  let block_size = 512 / 8 -- 512 bit blocks
  let end_size = 64 / 8 -- 64 bit size at the end
  -- let blocks = ((n + 1) + block_size - 1) / block_size
  let blocks = (n + 1 + end_size + block_size - 1) / block_size
  in blocks * block_size


entry pad_message_entry [n] (message: [n]u8): []u8 =
  pad_message message (get_padding_for_message message)

-- Test for padding of messages for sha256
-- ==
-- entry: pad_message_entry
-- input {[50u8, 51u8, 52u8]}
-- output {[50u8, 51u8, 52u8, 0x80u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 24u8 ]}
-- input {[100u8, 20u8, 50u8, 51u8, 52u8]}
-- output {[100u8, 20u8, 50u8, 51u8, 52u8, 0x80u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 40u8 ]}
-- input {[117u8, 27u8, 98u8, 108u8, 35u8, 87u8, 7u8, 43u8, 124u8, 58u8, 26u8, 59u8, 17u8, 74u8, 106u8, 40u8, 109u8, 92u8, 117u8, 3u8, 126u8, 63u8, 28u8, 125u8, 90u8, 99u8, 78u8, 48u8, 75u8, 80u8, 89u8, 117u8, 8u8, 99u8, 113u8, 49u8, 92u8, 65u8, 118u8, 89u8, 77u8, 26u8, 12u8, 85u8, 96u8, 74u8, 63u8, 64u8, 114u8, 56u8, 97u8, 11u8, 45u8, 37u8, 115u8, 17u8, 109u8, 12u8, 93u8, 72u8, 41u8, 0u8, 20u8]}
-- output {[117u8, 27u8, 98u8, 108u8, 35u8, 87u8, 7u8, 43u8, 124u8, 58u8, 26u8, 59u8, 17u8, 74u8, 106u8, 40u8, 109u8, 92u8, 117u8, 3u8, 126u8, 63u8, 28u8, 125u8, 90u8, 99u8, 78u8, 48u8, 75u8, 80u8, 89u8, 117u8, 8u8, 99u8, 113u8, 49u8, 92u8, 65u8, 118u8, 89u8, 77u8, 26u8, 12u8, 85u8, 96u8, 74u8, 63u8, 64u8, 114u8, 56u8, 97u8, 11u8, 45u8, 37u8, 115u8, 17u8, 109u8, 12u8, 93u8, 72u8, 41u8, 0u8, 20u8, 0x80u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0x01u8, 0xf8u8]}
-- input {[10u8, 117u8, 27u8, 98u8, 108u8, 35u8, 87u8, 7u8, 43u8, 124u8, 58u8, 26u8, 59u8, 17u8, 74u8, 106u8, 40u8, 109u8, 92u8, 117u8, 3u8, 126u8, 63u8, 28u8, 125u8, 90u8, 99u8, 78u8, 48u8, 75u8, 80u8, 89u8, 117u8, 8u8, 99u8, 113u8, 49u8, 92u8, 65u8, 118u8, 89u8, 77u8, 26u8, 12u8, 85u8, 96u8, 74u8, 63u8, 64u8, 114u8, 56u8, 97u8, 11u8, 45u8, 37u8, 115u8, 17u8, 109u8, 12u8, 93u8, 72u8, 41u8, 0u8, 20u8, 121u8]}
-- output {[10u8, 117u8, 27u8, 98u8, 108u8, 35u8, 87u8, 7u8, 43u8, 124u8, 58u8, 26u8, 59u8, 17u8, 74u8, 106u8, 40u8, 109u8, 92u8, 117u8, 3u8, 126u8, 63u8, 28u8, 125u8, 90u8, 99u8, 78u8, 48u8, 75u8, 80u8, 89u8, 117u8, 8u8, 99u8, 113u8, 49u8, 92u8, 65u8, 118u8, 89u8, 77u8, 26u8, 12u8, 85u8, 96u8, 74u8, 63u8, 64u8, 114u8, 56u8, 97u8, 11u8, 45u8, 37u8, 115u8, 17u8, 109u8, 12u8, 93u8, 72u8, 41u8, 0u8, 20u8, 121u8, 0x80u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0u8, 0x02u8, 0x08u8 ]}
