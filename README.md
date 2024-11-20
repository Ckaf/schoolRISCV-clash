So, I decided to play with Clash. The [schoolRISCV project](https://github.com/zhelnio/schoolRISCV) was taken as a basis.
I couldn't figure out how to use `mealy` correctly for a long time, so I described 2 SR_CPU blocks (with and without `mealy`).

To launch it, you can write something like: `stack run clash -- SM_TOP --verilog`
