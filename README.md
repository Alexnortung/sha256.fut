# SHA256 library for futhark

This library provides functions for running the sha256 algorithm in futhark.
The performance is very low compared to other programs (see benchmarks)

## Getting started

Install the package

```bash
$ futhark pkg add github.com/alexnortung/sha256.fut
```

## Benchmarks

These benchmarks has been run on an AMD Radeon 6700 RX XT

```bash
$ futhark bench --backend=opencl lib/github.com/alexnortung/sha256.fut/main.fut
```

| Number of hashes | Time (µ) | Hashes per second |
| ---------------- | -------- | ----------------- |
| 100              | 97       | 1,030,927.84      |
| 500              | 98       | 5,102,040.82      |
| 1000             | 100      | 10,000,000.00     |
| 2000             | 102      | 19,607,843.14     |
| 3000             | 109      | 27,522,935.78     |
| 4000             | 114      | 35,087,719.30     |
| 10000            | 151      | 66,225,165.56     |
| 1000000          | 9539     | 104,832,791.70    |
| 2000000          | 18782    | 106,484,932.38    |
| 4000000          | 37451    | 106,806,226.80    |
| 8000000          | 75230    | 106,340,555.63    |
| 16000000         | 148484   | 107,755,717.79    |
| 24000000         | 1016341  | 23,614,121.64     |

The best performance was when doing 16 million hashes in a batch with a performance of 107 MH/s (mega hashes) per second

Benchmarks has also been obtained from hashcat with the sha256 algorithm, on the same system.
It had a performance of 5154.0 MH/s, which is significantly higher than this library

```bash
$ hashcat -b -m 1470
```
