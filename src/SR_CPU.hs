-- @createDomain@ below generates a warning about orphan instances, but we like
-- our code to be warning-free.
{-# OPTIONS_GHC -Wno-orphans #-}

module SR_CPU where

import Clash.Prelude
import TypesCPU


{-# ANN topEntity
  (Synthesize
    { t_name   = "SR_CPU"
    , t_inputs = [ PortName "clk"
                 , PortName "rst"
                 , PortName "en"
                 , PortProduct ""
                     [ PortName "regAddr"
                     , PortName "imData"
                     ]
                 ]
    , t_output = PortProduct ""
                  [ PortName "regData"
                  , PortName "imAddr"
                  ]
    })
  #-}

topEntity
    :: Clock System
    -> Reset System
    -> Enable System
    -> (Signal System (BitVector 5), -- regAddr
       Signal System (Instruction)) -- imData
    -> (Signal System (BitVector 32), -- regData
        Signal System (BitVector 32)) -- imAddr
topEntity clk rst en (regAddr, imData) = exposeClockResetEnable sr_cpu clk rst en (regAddr, imData)

smRegister :: HiddenClockResetEnable System => Signal System (BitVector 32) -> Signal System (BitVector 32)
smRegister d = regEn 0 (pure True) d

smRegisterWe :: HiddenClockResetEnable System => Signal System (Bool) -> Signal System (BitVector 32) -> Signal System (BitVector 32)
smRegisterWe we d = regEn 0 we d

fmapB f = unbundle . fmap f . bundle

sr_cpu :: SystemClockResetEnable
       => (Signal System (BitVector 5),   -- regAddr
           Signal System (Instruction))   -- imData
       -> (Signal System (BitVector 32),  -- regData
           Signal System (BitVector 32))  -- imAddr
sr_cpu (regAddr, imData) = (regData, imAddr)
  where
    pcNext = mux pcSrc pcBranch pcPlus4
    pcBranch = pc + immB
    pcPlus4 = pc + 4
    pc = smRegister pcNext
    regData = mux (regAddr .==. 0) pc rd0
    imAddr = shiftR <$> pc <*> pure 2
    srcB = mux aluSrc immI rd2
    wd3 = mux wdSrc immU aluResult
    (op, rd, f3, rs1, rs2, f7, immI, immB, immU) = srDecode imData
    (pcSrc, regWrite, aluSrc, wdSrc, aluControl) = srControl op f3 f7 aluZero
    (aluZero, aluResult) = srAlu rd1 srcB aluControl

    rf = register (repeat 0) newRf
    (newRf, (rd0, rd1, rd2)) = srRegisterFile (register (repeat 0) rf)
        (fmap (toEnum . fromEnum . (unpack :: BitVector 5 -> Unsigned 5)) regAddr, rs1, rs2, rd, wd3, regWrite)

srControl :: Signal System Opcode -> Signal System Funct3 -> Signal System Funct7 -> Signal System Bool
  -> (Signal System Bool, Signal System Bool, Signal System Bool, Signal System Bool, Signal System AluControl)

srControl op f3 f7 aluZero = (pcSrc, regWrite, aluSrc, wdSrc, aluControl) where
  pcSrc = branch .&&. (aluZero .==. condZero)

  inps::Signal System (Funct7, Funct3, Opcode)
  inps = (bundle (f7, f3, op))
  (branch, condZero, regWrite, aluSrc, wdSrc, aluControl) = unbundle (liftA3 getRes f7 f3 op)

  unpackRes (op, f3, f7) = getRes f7 f3 op

  getRes :: Funct7 -> Funct3 -> Opcode -> (Bool, Bool, Bool, Bool, Bool, AluControl)

  getRes RVF7_ADD   RVF3_ADD  RVOP_ADD =  ( False, False, True, False, False, ALU_ADD)
  getRes RVF7_OR    RVF3_OR   RVOP_OR =   ( False, False, True, False, False, ALU_OR)
  getRes RVF7_SRL   RVF3_SRL  RVOP_SRL =  ( False, False, True, False, False, ALU_SRL)
  getRes RVF7_SLTU  RVF3_SLTU RVOP_SLTU = ( False, False, True, False, False, ALU_SLTU)
  getRes RVF7_SUB   RVF3_SUB  RVOP_SUB =  ( False, False, True, False, False, ALU_SUB)

  getRes RVF7_ANY  RVF3_ADDI RVOP_ADDI = (False, False, True, True, False, ALU_ADD)
  getRes RVF7_ANY  RVF3_ANY  RVOP_LUI =  (False, False, True, False, True, ALU_ADD)

  getRes RVF7_ANY  RVF3_BEQ  RVOP_BEQ =  (True, True,  False, False, False, ALU_SUB)
  getRes RVF7_ANY  RVF3_BNE  RVOP_BNE =  (True, False, False, False, False, ALU_SUB)

  getRes _ _ _ = (False, False, False, False, False, ALU_ADD)

srDecode :: Signal System Instruction
         -> ( Signal System Opcode
            , Signal System Reg
            , Signal System Funct3
            , Signal System Reg
            , Signal System Reg
            , Signal System Funct7
            , Signal System (Imm 32)
            , Signal System (Imm 32)
            , Signal System (Imm 32)
            )
srDecode instr = (op, rd, f3, rs1, rs2, f7, immI, immB, immU)
  where
    bitInstr = bitPatternInstruction <$> instr
    op = fmap (\b -> case slice d6 d0 b of
                    x | x == bitPatternOpcode RVOP_ADDI -> RVOP_ADDI
                      | x == bitPatternOpcode RVOP_BEQ  -> RVOP_BEQ
                      | x == bitPatternOpcode RVOP_LUI  -> RVOP_LUI
                      | x == bitPatternOpcode RVOP_ADD  -> RVOP_ADD
                      | otherwise -> errorX "Unknown Opcode"
               ) bitInstr

    rd :: Signal System Reg
    rd = fmap (\b -> toEnum $ fromEnum (unpack (slice d11 d7 b ) :: Signed 5)) bitInstr

    f3 = fmap (\b -> case slice d14 d12 b of
                  x | x == bitPatternFunct3 RVF3_ADD -> RVF3_ADD
                    | x == bitPatternFunct3 RVF3_OR ->  RVF3_OR
                    | x == bitPatternFunct3 RVF3_SRL -> RVF3_SRL
                    | otherwise -> RVF3_ANY
              ) bitInstr

    rs1, rs2 :: Signal System Reg
    rs1 = fmap (\b -> toEnum $ fromEnum (unpack (slice d19 d15 b ) :: Signed 5)) bitInstr
    rs2 = fmap (\b -> toEnum $ fromEnum (unpack (slice d24 d20 b ) :: Signed 5)) bitInstr

    f7 = fmap (\b -> case slice d31 d25 b of
              x | x == bitPatternFunct7 RVF7_ADD -> RVF7_ADD
                | x == bitPatternFunct7 RVF7_SUB -> RVF7_SUB
                | otherwise -> RVF7_ANY
              ) bitInstr

    immI = fmap (\b -> pack (h b, 1 :: Signed 1, slice d30 d20 b)) bitInstr
    immB = fmap (\b -> pack (h b, boolToBV (bitToBool (b ! 7)), slice d30 d25 b, slice d11 d8 b, 0 :: Signed 1)) bitInstr
    immU = fmap (\b -> pack (h b, setSlice d11 d0 0 (0 :: Signed 12))) bitInstr

    h b = setSlice d19 d0 (boolToBV (bitToBool (b ! 31))) (0 :: Signed 20)

srAlu :: Signal System (BitVector 32)
      -> Signal System (BitVector 32)
      -> Signal System AluControl
      -> (Signal System Bool, Signal System (BitVector 32))
srAlu srcA srcB op = (zero, result)
  where
    result = mux (op .==. pure ALU_ADD)  (srcA + srcB) $
             mux (op .==. pure ALU_OR)   (liftA2 (.|.) srcA srcB) $
             mux (op .==. pure ALU_SRL)  (shiftR <$> srcA  <*> (fmap (\b -> fromEnum (unpack (slice d4 d0 b):: Signed 5) ) srcB)) $
             mux (op .==. pure ALU_SLTU) (mux ((<) <$> srcA <*> srcB) (pure 1) (pure 0)) $
             mux (op .==. pure ALU_SUB)  (srcA - srcB) $
             srcA  -- Default case

    zero = result .==. pure 0


srRegisterFile
  :: HiddenClockResetEnable System
  => Signal System (Vec 32 (BitVector 32)) -- register file (current state)
  -> ( Signal System Reg  -- a0
     , Signal System Reg -- a1
     , Signal System Reg  -- a2
     , Signal System Reg  -- a3
     , Signal System (Imm 32) -- wd3
     , Signal System Bool -- we3
     )
  -> ( Signal System (Vec 32 (BitVector 32)) -- new register file (new state)
     , ( Signal System (BitVector 32) -- rd0
       , Signal System (BitVector 32) -- rd1
       , Signal System (BitVector 32) -- rd2
       )
     )
srRegisterFile rf (a0, a1, a2, a3, wd3, we3) = (nrf, (rd0, rd1, rd2)) where

  rd0 = mux (a0 .==. pure ZERO) (pure 0) (fmap (\(x, y) -> x !! (fromEnum y)) (bundle (rf, a0)))
  rd1 = mux (a1 .==. pure ZERO) (pure 0) (fmap (\(x, y) -> x !! (fromEnum y)) (bundle (rf, a1)))
  rd2 = mux (a2 .==. pure ZERO) (pure 0) (fmap (\(x, y) -> x !! (fromEnum y)) (bundle (rf, a2)))
  nrf = register (repeat 0) $ sequenceA $ zipWith
        (\idx reg -> mux (we3 .&&. (a3 .==. pure idx)) wd3 reg)
        (fmap (toEnum . fromEnum) indicesI)
        (sequenceA rf)
