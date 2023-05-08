def pad_message [n] (message: [n]i8) (m: i64): [m]i8 =
  let new_arr = replicate m 0i8
  let message_start = m - n
  let indexes = iota n |> map (+message_start)
  in scatter new_arr indexes message

def get_padding_for_message [n] (message: [n]i8): i64 =
  let block_size = 512 / 8 -- 512 bit blocks
  let blocks = (n + block_size - 1) / block_size
  in blocks * block_size

entry pad_message_entry [n] (message: [n]i8): []i8 =
  pad_message message (get_padding_for_message message)

-- Test for padding of messages for sha256
-- ==
-- entry: pad_message_entry
-- input {[50i8, 51i8, 52i8]}
-- output {[0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 50i8, 51i8, 52i8 ]}
-- input {[100i8, 20i8, 50i8, 51i8, 52i8]}
-- output {[0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 100i8, 20i8, 50i8, 51i8, 52i8 ]}
-- input {[117i8, 27i8, 98i8, 108i8, 35i8, 87i8, 7i8, 43i8, 124i8, 58i8, 26i8, 59i8, 17i8, 74i8, 106i8, 40i8, 109i8, 92i8, 117i8, 3i8, 126i8, 63i8, 28i8, 125i8, 90i8, 99i8, 78i8, 48i8, 75i8, 80i8, 89i8, 117i8, 8i8, 99i8, 113i8, 49i8, 92i8, 65i8, 118i8, 89i8, 77i8, 26i8, 12i8, 85i8, 96i8, 74i8, 63i8, 64i8, 114i8, 56i8, 97i8, 11i8, 45i8, 37i8, 115i8, 17i8, 109i8, 12i8, 93i8, 72i8, 41i8, 0i8, 20i8, 121i8]}
-- output {[117i8, 27i8, 98i8, 108i8, 35i8, 87i8, 7i8, 43i8, 124i8, 58i8, 26i8, 59i8, 17i8, 74i8, 106i8, 40i8, 109i8, 92i8, 117i8, 3i8, 126i8, 63i8, 28i8, 125i8, 90i8, 99i8, 78i8, 48i8, 75i8, 80i8, 89i8, 117i8, 8i8, 99i8, 113i8, 49i8, 92i8, 65i8, 118i8, 89i8, 77i8, 26i8, 12i8, 85i8, 96i8, 74i8, 63i8, 64i8, 114i8, 56i8, 97i8, 11i8, 45i8, 37i8, 115i8, 17i8, 109i8, 12i8, 93i8, 72i8, 41i8, 0i8, 20i8, 121i8]}
-- input {[10i8, 117i8, 27i8, 98i8, 108i8, 35i8, 87i8, 7i8, 43i8, 124i8, 58i8, 26i8, 59i8, 17i8, 74i8, 106i8, 40i8, 109i8, 92i8, 117i8, 3i8, 126i8, 63i8, 28i8, 125i8, 90i8, 99i8, 78i8, 48i8, 75i8, 80i8, 89i8, 117i8, 8i8, 99i8, 113i8, 49i8, 92i8, 65i8, 118i8, 89i8, 77i8, 26i8, 12i8, 85i8, 96i8, 74i8, 63i8, 64i8, 114i8, 56i8, 97i8, 11i8, 45i8, 37i8, 115i8, 17i8, 109i8, 12i8, 93i8, 72i8, 41i8, 0i8, 20i8, 121i8]}
-- output {[0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 0i8, 10i8, 117i8, 27i8, 98i8, 108i8, 35i8, 87i8, 7i8, 43i8, 124i8, 58i8, 26i8, 59i8, 17i8, 74i8, 106i8, 40i8, 109i8, 92i8, 117i8, 3i8, 126i8, 63i8, 28i8, 125i8, 90i8, 99i8, 78i8, 48i8, 75i8, 80i8, 89i8, 117i8, 8i8, 99i8, 113i8, 49i8, 92i8, 65i8, 118i8, 89i8, 77i8, 26i8, 12i8, 85i8, 96i8, 74i8, 63i8, 64i8, 114i8, 56i8, 97i8, 11i8, 45i8, 37i8, 115i8, 17i8, 109i8, 12i8, 93i8, 72i8, 41i8, 0i8, 20i8, 121i8]}
