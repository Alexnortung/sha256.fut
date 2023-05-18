import "./padding"
import "./hashing"

entry sha256_long (string: []u8): Digest =
  -- pad the string
  let padded = pad_message_simple string
  -- create message schedules
  let schedules = create_message_schedules padded
  -- compress message schedules
  let compressed = compress schedules
  in compressed

entry sha256_fast (string: []u8): Digest =
  -- pad the string
  let padded = pad_message_simple string
  let n = length padded
  let hash_values = copy initial_hash_values
  let num_message_words = (n + 3) / 4
  let num_chunks = (num_message_words + 15) / 16
  -- let message_words = unflatten num_message_words 4 message
  -- let u32_message = map (u8_array_to_u32) message_words
  let digest = loop hash_values = hash_values for i in 0..<num_chunks do
    let start = i * 64
    let chunk = iota 64 |> map (+start) |> map (\j -> padded[j])
    let schedule = create_message_schedule chunk
    let hash_values = compress_single schedule hash_values
    in hash_values
  in digest

entry multi_sha256 [n_strings] [m_lengths] (strings: [n_strings][m_lengths]u8): [n_strings]Digest =
  map (sha256_fast) strings

entry multi_sha256_long [n_strings] [m_lengths] (strings: [n_strings][m_lengths]u8): [n_strings]Digest =
  map (sha256_long) strings

-- End to end test
-- ==
-- entry: sha256_long sha256_fast
-- nobench input {[ 0x48u8, 0x65u8, 0x6cu8, 0x6cu8, 0x6fu8, 0x2cu8, 0x20u8, 0x77u8, 0x6fu8, 0x72u8, 0x6cu8, 0x64u8, 0x21u8 ]}
-- output {[ 0x315f5bdbu32, 0x76d078c4u32, 0x3b8ac006u32, 0x4e4a0164u32, 0x612b1fceu32, 0x77c86934u32, 0x5bfc94c7u32, 0x5894edd3u32 ]}
-- nobench input {[ 0x4du8, 0x79u8, 0x20u8, 0x76u8, 0x65u8, 0x72u8, 0x79u8, 0x20u8, 0x6cu8, 0x6fu8, 0x6eu8, 0x67u8, 0x20u8, 0x6du8, 0x65u8, 0x73u8, 0x73u8, 0x61u8, 0x67u8, 0x65u8, 0x20u8, 0x74u8, 0x68u8, 0x61u8, 0x74u8, 0x20u8, 0x61u8, 0x72u8, 0x65u8, 0x20u8, 0x74u8, 0x77u8, 0x6fu8, 0x20u8, 0x62u8, 0x6cu8, 0x6fu8, 0x63u8, 0x6bu8, 0x73u8, 0x20u8, 0x6cu8, 0x6fu8, 0x6eu8, 0x67u8, 0x21u8, 0x2eu8, 0x2eu8, 0x2eu8, 0x2eu8, 0x2eu8, 0x2eu8, 0x2eu8, 0x2eu8, 0x2eu8, 0x2eu8, 0x2eu8, 0x2eu8, 0x2eu8, 0x2eu8, 0x2eu8, 0x2eu8, 0x2eu8, 0x2eu8 ]}
-- output {[ 0x138c93b3u32, 0x65c9c9dcu32, 0x774a1930u32, 0x275d294eu32, 0x80454b7au32, 0xfb7ba187u32, 0x5f8f8acdu32, 0xe9937a84u32 ]}

-- End to end test for multiple strings at a time
-- This requires sha256 function to pass
-- ==
-- entry: multi_sha256
-- random input { [100][30]u8 }

-- End to end benchmarks
-- ==
-- tags { disable }
-- entry: multi_sha256
-- compiled random input { [500][30]u8 }
-- compiled random input { [1000][30]u8 }
-- compiled random input { [2000][30]u8 }
-- compiled random input { [3000][30]u8 }
-- compiled random input { [4000][30]u8 }
-- compiled random input { [10000][30]u8 }
-- compiled random input { [1000000][30]u8 }
-- compiled random input { [2000000][30]u8 }
-- compiled random input { [4000000][30]u8 }
-- compiled random input { [8000000][30]u8 }
-- compiled random input { [16000000][30]u8 }
-- compiled random input { [24000000][30]u8 }
-- compiled random input { [10][2000000]u8 }
