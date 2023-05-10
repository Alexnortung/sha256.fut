import "./numbers"
import "./padding"

-- message should already be padded to a multiple of 512 bits
def create_message_schedules [n] (message: [n]u8) : [][64]u32 =
  let num_message_words = (n + 3) / 4
  let num_chunks = (num_message_words + 15) / 16
  let message_words = unflatten num_message_words 4 message
  let u32_message = map (u8_array_to_u32) message_words
  let schedule_messages = unflatten num_chunks 16 u32_message
  -- put the message into the schedule
  let schedules = map (\m -> 
    let schedule = replicate 64 0u32
    in scatter schedule (iota 16) m
  ) schedule_messages
  let schedules = loop newSchedules = schedules for i in 16..<64 do
    map (\schedule ->
      let s0 = (rotate_right schedule[i-15] 7) ^ (rotate_right schedule[i-15] 18) ^ (schedule[i-15] >> 3)
      let s1 = (rotate_right schedule[i-2] 17) ^ (rotate_right schedule[i-2] 19) ^ (schedule[i-2] >> 10)
      let newValue = schedule[i-16] + s0 + schedule[i-7] + s1
      -- TODO: avoid copying - maybe use map to get the new values, then scatter the values with scatter2d
      in (copy schedule) with [i] = newValue
    ) newSchedules

  in schedules

-- def initialize [n] (message: [n]u8) =
--   let hash_values = [
--     0x6a09e667,
--     0xbb67ae85,
--     0x3c6ef372,
--     0xa54ff53a,
--     0x510e527f,
--     0x9b05688c,
--     0x1f83d9ab,
--     0x5be0cd19
--   ]
--   let round_constants = [
--     0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5,
--     0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174,
--     0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da,
--     0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967,
--     0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85,
--     0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070,
--     0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
--     0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2
--   ]
--   in 0
  

entry create_message_schedules_test [n] (message: [n]u8): [][64]u32 =
  let padded_message = pad_message message (get_padding_for_message message)
  in create_message_schedules padded_message


-- test creation of message schedule from message: "my message"
-- ==
-- entry: create_message_schedules_test
-- input {[0x6du8, 0x79u8, 0x20u8, 0x6du8, 0x65u8, 0x73u8, 0x73u8, 0x61u8, 0x67u8, 0x65u8]}
-- output{[
--        [0x6d79206du32,0x65737361u32,0x67658000u32,0x0u32,0x0u32,0x0u32,0x0u32,0x0u32,0x0u32,0x0u32,0x0u32,0x0u32,0x0u32,0x0u32,0x0u32,0x50u32,0x8035f243u32,0xd1b7d63au32,0xaeaf3d60u32,0x11ee5f18u32,0x7937e94du32,0xe46b715du32,0x9915e97u32,0x56f246a7u32,0x5652d7e7u32,0x1a423a96u32,0x4309346eu32,0xd3574834u32,0xa1159d05u32,0x56b984aau32,0x74106880u32,0x34beddc2u32,0x6de07e6fu32,0xe5e610a4u32,0x58bfbc0du32,0xed0b609eu32,0x74955871u32,0xad688ee4u32,0x1b5667ccu32,0x8cf78917u32,0xdd2b899au32,0x4288a4d1u32,0x879f8821u32,0x6f0bcc0u32,0xc30416f9u32,0xd053dcc4u32,0xc02935aau32,0x74b56279u32,0x3e178ce8u32,0x899a312fu32,0x8ff3d787u32,0xc907a7a4u32,0xd548ebc2u32,0x9755e7bbu32,0xd57b2989u32,0xe69813a2u32,0xdc669f12u32,0x900db5e6u32,0x9ce7533au32,0x36fe3c1fu32,0x2d30b24u32,0x80408d50u32,0x30991ae3u32,0xdcc31297u32]
--        ]}
