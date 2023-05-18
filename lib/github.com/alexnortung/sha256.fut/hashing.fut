import "./numbers"
import "./padding"

type MessageSchedule = [64]u32
type Digest = [8]u32

-- message should already be padded to a multiple of 512 bits
def create_message_schedules [n] (message: [n]u8) : []MessageSchedule =
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
  let schedules = loop new_schedules = schedules for i in 16..<64 do
    let newValues = map (\schedule ->
      let s0 = (rotate_right_unsafe schedule[i-15] 7) ^ (rotate_right_unsafe schedule[i-15] 18) ^ (schedule[i-15] >> 3)
      let s1 = (rotate_right_unsafe schedule[i-2] 17) ^ (rotate_right_unsafe schedule[i-2] 19) ^ (schedule[i-2] >> 10)
      let newValue = schedule[i-16] + s0 + schedule[i-7] + s1
      in newValue
    ) new_schedules
    let indexes = iota num_chunks |> map (\y -> (y, i))
    in scatter_2d new_schedules indexes newValues

  in schedules

def compress (message_schedules: []MessageSchedule): Digest =
  let hash_values = {
    a = 0x6a09e667u32,
    b = 0xbb67ae85u32,
    c = 0x3c6ef372u32,
    d = 0xa54ff53au32,
    e = 0x510e527fu32,
    f = 0x9b05688cu32,
    g = 0x1f83d9abu32,
    h = 0x5be0cd19u32
  }

  -- round constants k
  let k = [
    0x428a2f98u32, 0x71374491u32, 0xb5c0fbcfu32, 0xe9b5dba5u32, 0x3956c25bu32, 0x59f111f1u32, 0x923f82a4u32, 0xab1c5ed5u32,
    0xd807aa98u32, 0x12835b01u32, 0x243185beu32, 0x550c7dc3u32, 0x72be5d74u32, 0x80deb1feu32, 0x9bdc06a7u32, 0xc19bf174u32,
    0xe49b69c1u32, 0xefbe4786u32, 0x0fc19dc6u32, 0x240ca1ccu32, 0x2de92c6fu32, 0x4a7484aau32, 0x5cb0a9dcu32, 0x76f988dau32,
    0x983e5152u32, 0xa831c66du32, 0xb00327c8u32, 0xbf597fc7u32, 0xc6e00bf3u32, 0xd5a79147u32, 0x06ca6351u32, 0x14292967u32,
    0x27b70a85u32, 0x2e1b2138u32, 0x4d2c6dfcu32, 0x53380d13u32, 0x650a7354u32, 0x766a0abbu32, 0x81c2c92eu32, 0x92722c85u32,
    0xa2bfe8a1u32, 0xa81a664bu32, 0xc24b8b70u32, 0xc76c51a3u32, 0xd192e819u32, 0xd6990624u32, 0xf40e3585u32, 0x106aa070u32,
    0x19a4c116u32, 0x1e376c08u32, 0x2748774cu32, 0x34b0bcb5u32, 0x391c0cb3u32, 0x4ed8aa4au32, 0x5b9cca4fu32, 0x682e6ff3u32,
    0x748f82eeu32, 0x78a5636fu32, 0x84c87814u32, 0x8cc70208u32, 0x90befffau32, 0xa4506cebu32, 0xbef9a3f7u32, 0xc67178f2u32
  ]
  let hash_values = loop hash_values = hash_values for schedule in message_schedules do
    let initial_hash_values = [
      hash_values.a,
      hash_values.b,
      hash_values.c,
      hash_values.d,
      hash_values.e,
      hash_values.f,
      hash_values.g,
      hash_values.h
    ]
    let hash_values = loop hash_values = hash_values for i in 0..<64 do
      let {a, b, c, d, e, f, g, h} = hash_values
      let s1 = (rotate_right_unsafe e 6) ^ (rotate_right_unsafe e 11) ^ (rotate_right_unsafe e 25)
      let choice = (e & f) ^ ((!e) & g)
      let temp1 = h + s1 + choice + k[i] + schedule[i]
      let s0 = (rotate_right_unsafe a 2) ^ (rotate_right_unsafe a 13) ^ (rotate_right_unsafe a 22)
      let majority = (a & b) ^ (a & c) ^ (b & c)
      let temp2 = s0 + majority
      in {
        a = temp1 + temp2,
        b = a,
        c = b,
        d = c,
        e = d + temp1,
        f = e,
        g = f,
        h = g
      }
    let {a, b, c, d, e, f, g, h} = hash_values
    in {
      a = a + initial_hash_values[0],
      b = b + initial_hash_values[1],
      c = c + initial_hash_values[2],
      d = d + initial_hash_values[3],
      e = e + initial_hash_values[4],
      f = f + initial_hash_values[5],
      g = g + initial_hash_values[6],
      h = h + initial_hash_values[7]
    }
  in [
    hash_values.a,
    hash_values.b,
    hash_values.c,
    hash_values.d,
    hash_values.e,
    hash_values.f,
    hash_values.g,
    hash_values.h
  ]


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

entry compress_message_schedule_test (message_schedules: []MessageSchedule) =
  compress message_schedules

-- test compression of message schedule for message: "my message" (input used from test above)
-- ==
-- entry: compress_message_schedule_test
-- input{[
--        [0x6d79206du32,0x65737361u32,0x67658000u32,0x0u32,0x0u32,0x0u32,0x0u32,0x0u32,0x0u32,0x0u32,0x0u32,0x0u32,0x0u32,0x0u32,0x0u32,0x50u32,0x8035f243u32,0xd1b7d63au32,0xaeaf3d60u32,0x11ee5f18u32,0x7937e94du32,0xe46b715du32,0x9915e97u32,0x56f246a7u32,0x5652d7e7u32,0x1a423a96u32,0x4309346eu32,0xd3574834u32,0xa1159d05u32,0x56b984aau32,0x74106880u32,0x34beddc2u32,0x6de07e6fu32,0xe5e610a4u32,0x58bfbc0du32,0xed0b609eu32,0x74955871u32,0xad688ee4u32,0x1b5667ccu32,0x8cf78917u32,0xdd2b899au32,0x4288a4d1u32,0x879f8821u32,0x6f0bcc0u32,0xc30416f9u32,0xd053dcc4u32,0xc02935aau32,0x74b56279u32,0x3e178ce8u32,0x899a312fu32,0x8ff3d787u32,0xc907a7a4u32,0xd548ebc2u32,0x9755e7bbu32,0xd57b2989u32,0xe69813a2u32,0xdc669f12u32,0x900db5e6u32,0x9ce7533au32,0x36fe3c1fu32,0x2d30b24u32,0x80408d50u32,0x30991ae3u32,0xdcc31297u32]
--        ]}
-- output{[
--         0xea38e30fu32,
--         0x75767d7eu32,
--         0x6c21eba8u32,
--         0x5b140166u32,
--         0x46a3b60au32,
--         0xde426ca9u32,
--         0x66dac940u32,
--         0xa5db1babu32
--        ]}
