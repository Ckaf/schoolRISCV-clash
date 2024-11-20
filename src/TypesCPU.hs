module TypesCPU where
import Clash.Prelude

data Instruction = RType Funct7 Reg Reg Funct3 Reg Opcode
                 | IType (Imm 12) Reg Funct3 Reg Opcode
                 | SType (Imm 7) Reg Reg Funct3 (Imm 5) Opcode
                 | BType (Imm 1) (Imm 6) Reg Reg Funct3 (Imm 4) (Imm 1) Opcode
                 | UType (Imm 20) Reg Opcode
                 | JType (Imm 1) (Imm 10) (Imm 1) (Imm 8) Reg Opcode
                 deriving (Eq, Show, Generic, NFDataX)

data AluControl = ALU_ADD | ALU_OR | ALU_SRL | ALU_SLTU | ALU_SUB deriving (Eq, Show, Generic, Enum, NFDataX)
data Opcode = RVOP_ADDI | RVOP_BEQ | RVOP_LUI | RVOP_BNE | RVOP_ADD | RVOP_OR | RVOP_SRL | RVOP_SLTU | RVOP_SUB deriving (Eq, Show, Generic, Enum, NFDataX)
data Funct3 = RVF3_ADDI | RVF3_BEQ | RVF3_BNE | RVF3_ADD | RVF3_OR | RVF3_SRL | RVF3_SLTU | RVF3_SUB | RVF3_ANY deriving (Eq, Show, Generic, Enum, NFDataX)
data Funct7 = RVF7_ADD | RVF7_OR | RVF7_SRL | RVF7_SLTU | RVF7_SUB | RVF7_ANY deriving (Eq, Show, Generic, Enum, NFDataX)
data Reg = ZERO | RA | SP | GP | TP | T0 | T1 | T2 | S0 | S1 | A0 | A1 | A2 | A3 | A4 | A5 | A6 | A7 | S2 | S3
          | S4 | S5 | S6 | S7 | S9 | S10 | S11 | T3 | T4 | T5 | T6 deriving (Eq, Show, Generic, Enum, NFDataX)
type Imm (n :: Nat) = BitVector n

defaultInstr = RType RVF7_ADD RA GP RVF3_ADDI T0 RVOP_ADDI

bitPatternOpcode :: Opcode -> BitVector 7
bitPatternOpcode RVOP_ADDI = 0b0010011
bitPatternOpcode RVOP_BEQ  = 0b1100011
bitPatternOpcode RVOP_LUI  = 0b0110111
bitPatternOpcode RVOP_BNE  = 0b1100011
bitPatternOpcode RVOP_ADD  = 0b0110011
bitPatternOpcode RVOP_OR   = 0b0110011
bitPatternOpcode RVOP_SRL  = 0b0110011
bitPatternOpcode RVOP_SLTU = 0b0110011
bitPatternOpcode RVOP_SUB  = 0b0110011

bitPatternFunct3 :: Funct3 -> BitVector 3
bitPatternFunct3 RVF3_ADDI = 0b000
bitPatternFunct3 RVF3_BEQ  = 0b000
bitPatternFunct3 RVF3_BNE  = 0b001
bitPatternFunct3 RVF3_ADD  = 0b000
bitPatternFunct3 RVF3_OR   = 0b110
bitPatternFunct3 RVF3_SRL  = 0b101
bitPatternFunct3 RVF3_SLTU = 0b011
bitPatternFunct3 RVF3_SUB  = 0b000
bitPatternFunct3 RVF3_ANY  = 0b000

bitPatternFunct7 :: Funct7 -> BitVector 7
bitPatternFunct7 RVF7_ADD  = 0b0000000
bitPatternFunct7 RVF7_OR   = 0b0000000
bitPatternFunct7 RVF7_SRL  = 0b0000000
bitPatternFunct7 RVF7_SLTU = 0b0000000
bitPatternFunct7 RVF7_SUB  = 0b0100000
bitPatternFunct7 RVF7_ANY  = 0b0000000

bitPatternReg :: Reg -> BitVector 5
bitPatternReg r = resize (fromIntegral (fromEnum r) :: BitVector 5)

bitPatternImm :: KnownNat n => Imm n -> BitVector n
bitPatternImm bv = bv

bitPatternInstruction :: Instruction -> BitVector 32
bitPatternInstruction (RType f7 rs2 rs1 f3 rd op) =
  pack (bitPatternFunct7 f7, bitPatternReg rs2, bitPatternReg rs1, bitPatternFunct3 f3, bitPatternReg rd, bitPatternOpcode op)
bitPatternInstruction (IType imm rs1 f3 rd op) =
  pack (bitPatternImm imm, bitPatternReg rs1, bitPatternFunct3 f3, bitPatternReg rd, bitPatternOpcode op)
bitPatternInstruction (SType imm1 rs2 rs1 f3 imm2 op) =
  pack (bitPatternImm imm1, bitPatternReg rs2, bitPatternReg rs1, bitPatternFunct3 f3, bitPatternImm imm2, bitPatternOpcode op)
bitPatternInstruction (BType imm1 imm2 rs2 rs1 f3 imm3 imm4 op) =
  pack (bitPatternImm imm1, bitPatternImm imm2, bitPatternReg rs2, bitPatternReg rs1, bitPatternFunct3 f3, bitPatternImm imm3, bitPatternImm imm4, bitPatternOpcode op)
bitPatternInstruction (UType imm rd op) =
  pack (bitPatternImm imm, bitPatternReg rd, bitPatternOpcode op)
bitPatternInstruction (JType imm1 imm2 imm3 imm4 rd op) =
  pack (bitPatternImm imm1, bitPatternImm imm2, bitPatternImm imm3, bitPatternImm imm4, bitPatternReg rd, bitPatternOpcode op)
