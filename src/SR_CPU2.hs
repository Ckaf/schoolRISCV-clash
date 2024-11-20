-- @createDomain@ below generates a warning about orphan instances, but we like
-- our code to be warning-free.
{-# OPTIONS_GHC -Wno-orphans #-}

module SR_CPU2 where

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
       Signal System (Instruction))   -- imData
    -> (Signal System (BitVector 32), -- regData
        Signal System (BitVector 32)) -- imAddr
topEntity clk rst en (regAddr, imData) = exposeClockResetEnable srCPU clk rst en (regAddr, imData)

smRegister :: HiddenClockResetEnable System => Signal System (BitVector 32) -> Signal System (BitVector 32)
smRegister d = regEn 0 (pure True) d

smRegisterWe :: HiddenClockResetEnable System => Signal System (Bool) -> Signal System (BitVector 32) -> Signal System (BitVector 32)
smRegisterWe we d = regEn 0 we d

fmapB f = unbundle . fmap f . bundle

srCPU :: SystemClockResetEnable
       => (Signal System (BitVector 5),   -- regAddr
           Signal System (Instruction))   -- imData
       -> (Signal System (BitVector 32),  -- regData
           Signal System (BitVector 32))  -- imAddr
srCPU (regAddr, imData) = (regData, imAddr)
  where
    pcNext = mux pcSrc pcBranch pcPlus4
    pcBranch = pc + immB
    pcPlus4 = pc + 4
    pc = smRegister pcNext
    regData = mux (regAddr .==. 0) pc rd0
    imAddr = shiftR <$> pc <*> pure 2
    srcB = mux aluSrc immI rd2
    wd3 = mux wdSrc immU aluResult
    (op, rd, f3, rs1, rs2, f7, immI, immB, immU) = unbundle $ mealy srDecodeMealy () imData
    (pcSrc, regWrite, aluSrc, wdSrc, aluControl) = unbundle $ mealy srControlMealy () (bundle (op, f3, f7, aluZero))
    (aluZero, aluResult) = unbundle $ mealy srAluMealy () (bundle (rd1, srcB, aluControl))


    (rd0, rd1, rd2) = unbundle $
        mealy srRegisterFileMealy (repeat 0) (bundle (fmap (toEnum . fromEnum) regAddr, rs1, rs2, rd, wd3, regWrite))


srControlMealy :: () -> (Opcode, Funct3, Funct7, Bool) ->
  ((), (Bool, Bool, Bool, Bool, AluControl))
srControlMealy () (op, f3, f7, aluZero) = ((), (pcSrc, regWrite, aluSrc, wdSrc, aluControl))
  where
    pcSrc = branch && (aluZero == condZero)

    (branch, condZero, regWrite, aluSrc, wdSrc, aluControl) = getRes f7 f3 op

    getRes :: Funct7 -> Funct3 -> Opcode -> (Bool, Bool, Bool, Bool, Bool, AluControl)
    getRes RVF7_ADD   RVF3_ADD  RVOP_ADD  = (False, False, True, False, False, ALU_ADD)
    getRes RVF7_OR    RVF3_OR   RVOP_OR   = (False, False, True, False, False, ALU_OR)
    getRes RVF7_SRL   RVF3_SRL  RVOP_SRL  = (False, False, True, False, False, ALU_SRL)
    getRes RVF7_SLTU  RVF3_SLTU RVOP_SLTU = (False, False, True, False, False, ALU_SLTU)
    getRes RVF7_SUB   RVF3_SUB  RVOP_SUB  = (False, False, True, False, False, ALU_SUB)
    getRes RVF7_ANY   RVF3_ADDI RVOP_ADDI = (False, False, True, True,  False, ALU_ADD)
    getRes RVF7_ANY   RVF3_ANY  RVOP_LUI  = (False, False, True, False, True,  ALU_ADD)
    getRes RVF7_ANY   RVF3_BEQ  RVOP_BEQ  = (True,  True,  False, False, False, ALU_SUB)
    getRes RVF7_ANY   RVF3_BNE  RVOP_BNE  = (True,  False, False, False, False, ALU_SUB)
    getRes _          _         _         = (False, False, False, False, False, ALU_ADD)


srDecodeMealy :: () -> Instruction ->
  ((), (Opcode, Reg, Funct3, Reg, Reg, Funct7, Imm 32, Imm 32, Imm 32))
srDecodeMealy () instr = ((), (op, rd, f3, rs1, rs2, f7, immI, immB, immU))
  where
    bitInstr = bitPatternInstruction instr

    op = case slice d6 d0 bitInstr of
      x | x == bitPatternOpcode RVOP_ADDI -> RVOP_ADDI
        | x == bitPatternOpcode RVOP_BEQ  -> RVOP_BEQ
        | x == bitPatternOpcode RVOP_LUI  -> RVOP_LUI
        | x == bitPatternOpcode RVOP_ADD  -> RVOP_ADD
        | otherwise -> errorX "Unknown Opcode"

    f3 = case slice d14 d12 bitInstr of
      x | x == bitPatternFunct3 RVF3_ADD -> RVF3_ADD
        | x == bitPatternFunct3 RVF3_OR  -> RVF3_OR
        | x == bitPatternFunct3 RVF3_SRL -> RVF3_SRL
        | otherwise -> RVF3_ANY

    rd = toEnum $ fromEnum (unpack (slice d11 d7 bitInstr :: BitVector 5) :: Unsigned 5)
    rs1 = toEnum $ fromEnum (unpack (slice d19 d15 bitInstr :: BitVector 5) :: Unsigned 5)
    rs2 = toEnum $ fromEnum (unpack (slice d24 d20 bitInstr :: BitVector 5) :: Unsigned 5)

    f7 = case slice d31 d25 bitInstr of
      x | x == bitPatternFunct7 RVF7_ADD -> RVF7_ADD
        | x == bitPatternFunct7 RVF7_SUB -> RVF7_SUB
        | otherwise -> RVF7_ANY

    immI = pack (h bitInstr, 1 :: Signed 1, slice d30 d20 bitInstr)
    immB = pack (h bitInstr, boolToBV (bitToBool (bitInstr ! 7)), slice d30 d25 bitInstr, slice d11 d8 bitInstr, 0 :: Signed 1)
    immU = pack (h bitInstr, setSlice d11 d0 0 (0 :: Signed 12))

    h b = setSlice d19 d0 (boolToBV (bitToBool (b ! 31))) (0 :: Signed 20)


srAluMealy :: () ->
  (BitVector 32, BitVector 32, AluControl) ->
  ((), (Bool, BitVector 32))
srAluMealy () (srcA, srcB, op) =
  let result = case op of
        ALU_ADD -> srcA + srcB
        ALU_OR -> srcA .|. srcB
        ALU_SRL -> shiftR srcA (fromEnum (slice d4 d0 srcB))
        ALU_SLTU -> if srcA < srcB then 1 else 0
        ALU_SUB -> srcA - srcB
        _ -> srcA
      zero = result == 0
   in ((), (zero, result))

srRegisterFileMealy ::
  Vec 32 (BitVector 32) ->
  (Reg, Reg, Reg, Reg, Imm 32, Bool) ->
  (Vec 32 (BitVector 32), (BitVector 32, BitVector 32, BitVector 32))
srRegisterFileMealy rf (a0, a1, a2, a3, wd3, we3) = (newRf, (rd0, rd1, rd2))
  where
    rd0 = if a0 == ZERO then 0 else rf !! fromEnum a0
    rd1 = if a1 == ZERO then 0 else rf !! fromEnum a1
    rd2 = if a2 == ZERO then 0 else rf !! fromEnum a2
    newRf = replace a3 (if we3 then wd3 else rf !! fromEnum a3) rf

