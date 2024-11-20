{-# LANGUAGE NumericUnderscores #-}

module SM_TOP where

import Clash.Prelude
import SR_CPU2
import SM_ROM
import TypesCPU

-- {-# ANN topEntity
--   (Synthesize
--     { t_name   = "SM_TOP"
--     , t_inputs = [ PortName "clkIn"
--                  , PortName "rst_n"
--                  , PortName "clkEnable"
--                  , PortName "clkDivide"
--                  , PortName "regAddr"
--                  ]
--     , t_output = PortName "regData"
--     })
--   #-}

topEntity ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System Bool -> -- clkEnable
  Signal System (BitVector 4) -> -- clkDivide
  Signal System (BitVector 5) -> -- regAddr
  Signal System (BitVector 32) -- regData
topEntity clk rst en clkEnable clkDivide regAddr =
  withClockResetEnable clk rst en $
    smTop clkEnable clkDivide regAddr


smTop ::
  HiddenClockResetEnable System =>
  Signal System Bool -> -- clkEnable
  Signal System (BitVector 4) -> -- clkDivide
  Signal System (BitVector 5) -> -- regAddr
  Signal System (BitVector 32) -- regData
smTop clkEnable clkDivide regAddr = regData
  where
    clkOut = mealy smClkDividerMealy (0, False) (bundle (clkDivide, clkEnable))
    imData = smRom romContent imAddr
    (regData, imAddr) = srCPU (regAddr, imData)

    romContent :: Vec 64 Instruction
    romContent = repeat defaultInstr -- replace with program content

smClkDividerMealy ::
  (Unsigned 32, Bool) ->
  (BitVector 4, Bool) ->
  ((Unsigned 32, Bool), Bool)
smClkDividerMealy (counter, clkOut) (divide, enable) =
  let nextCounter = if enable then counter + 1 else counter
      shifted = shiftR nextCounter (fromIntegral (unpack divide :: Unsigned 4))
      newClkOut = testBit shifted 0
  in ((nextCounter, newClkOut), newClkOut)
