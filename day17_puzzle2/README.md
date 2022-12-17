day17_puzzle2
=====
rebar3 escriptize && ./_build/default/bin/day17_puzzle2 ./data/puzzle_17_input

rebuild board every | time of running test simulation 2022 steps [ms]  | time of running input data simulation 2022 steps [ms]

every 10  |  34.331 | Returns incorrect result
every 20  |  37.648 | 242.602
every 30  |  40.381 | 245.995
every 50  |  45.621 | 250.649
every 100 |  58.742 | 264.154
every 250 |  94.561 | 302.862
every 500 | 145.940 | 352.438
every 1000| 215.547 | 427.081
without   | 212.832 | 426.029