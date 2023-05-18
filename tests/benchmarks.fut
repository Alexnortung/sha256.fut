import "../lib/github.com/alexnortung/sha256.fut/main";

-- End to end benchmarks
-- ==
-- tags { no_test }
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
