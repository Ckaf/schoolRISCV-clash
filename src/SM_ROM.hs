{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}

module SM_ROM where

import Clash.Prelude
import TypesCPU

smRom ::
  HiddenClockResetEnable dom =>
  Vec 64 (Instruction) ->
  Signal dom (BitVector 32) ->
  Signal dom (Instruction)
smRom romContent addr = asyncRom romContent <$> addr

topEntity ::
  Clock System ->
  Reset System ->
  Enable System ->
  Signal System (BitVector 32) ->
  Signal System (Instruction)
topEntity clk rst en addr =
  withClockResetEnable clk rst en $
    smRom romContent addr
  where
    romContent = repeat defaultInstr -- Предзагруженные данные
