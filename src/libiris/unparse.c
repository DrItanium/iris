#include <stdlib.h>
#include <stdio.h>
#include "iris.h"
enum {
   RegisterStringLength = 5,
};
enum {
   twoOperandRegister,
   twoOperandImmediate,
   threeOperandRegister,
   threeOperandImmediate,
   fourOperandRegister,
   fourOperandImmediate,
};
const char* operandTypes[] = {
   "%s %s",
   "%s %d",
   "%s %s %s",
   "%s %s %d",
   "%s %s %s %s",
   "%s %s %s %d",
};
void iris_unparse(char* unparsed, instruction* insn) {
   switch(get_group(insn)) {
      case InstructionGroupArithmetic:
         iris_unparse_arithmetic(unparsed, insn);
         break;
      case InstructionGroupMove:
         iris_unparse_move(unparsed, insn);
         break;
      case InstructionGroupJump:
         iris_unparse_jump(unparsed, insn);
         break;
      case InstructionGroupCompare:
         iris_unparse_compare(unparsed, insn);
         break;
      case InstructionGroupMisc:
         iris_unparse_misc(unparsed, insn);
         break;
      default:
         sprintf(unparsed, "UNASSIGNED INSTRUCTION GROUP");
         break;
   }
}

void iris_unparse_register(char* unparsed, byte index) {
   if(index < RegisterCount) {
      sprintf(unparsed, "r%d", index);
   } else {
      sprintf(unparsed, "INVALID_REGISTER");
   }
}

void iris_unparse_arithmetic(char* unparsed, instruction* insn) {
   const char* op;
   char dest[RegisterStringLength];
   char source0[RegisterStringLength];
   char source1[RegisterStringLength];

   op = iris_arithmetic_mnemonic(insn);
   iris_unparse_register(dest, get_arithmetic_dest(insn));
   iris_unparse_register(source0, get_arithmetic_source0(insn));
   iris_unparse_register(source1, get_arithmetic_source1(insn));

   switch(get_arithmetic_op(insn)) {
      case ArithmeticOpBinaryNot:
         sprintf(unparsed, operandTypes[threeOperandRegister], op, dest, source0);
         break;
      case ArithmeticOpAddImmediate:
      case ArithmeticOpSubImmediate:
      case ArithmeticOpMulImmediate:
      case ArithmeticOpDivImmediate:
      case ArithmeticOpRemImmediate:
      case ArithmeticOpShiftLeftImmediate:
      case ArithmeticOpShiftRightImmediate:
         sprintf(unparsed, operandTypes[fourOperandImmediate], op, source0, get_arithmetic_immediate(insn));
      default:
         sprintf(unparsed, operandTypes[fourOperandRegister], op, dest, source0, source1);
         break;
   }
}

void iris_unparse_move(char* unparsed, instruction* insn) {
   const char* op;
   char reg0[RegisterStringLength];
   char reg1[RegisterStringLength];

   op = iris_move_mnemonic(insn);
   switch(get_move_op(insn)) {
      case MoveOpMove: /* move r? r? */
      case MoveOpSwap: /* swap r? r? */
      case MoveOpSwapRegAddr: /* swap.reg.addr r? r? */
      case MoveOpSwapAddrAddr: /* swap.addr.addr r? r? */
      case MoveOpLoad: /* load r? r? */
      case MoveOpStore: /* store r? r? */
      case MoveOpStoreAddr: /* store.addr r? r? */
         iris_unparse_register(reg0, get_move_reg0(insn));
         iris_unparse_register(reg1, get_move_reg1(insn));
         sprintf(unparsed, operandTypes[threeOperandRegister], op, reg0, reg1);
         break;
      case MoveOpSwapRegMem: /* swap.reg.mem r? imm */
      case MoveOpSwapAddrMem: /* swap.addr.mem r? imm */
      case MoveOpSet: /* set r? imm */
      case MoveOpLoadMem: /* load.mem r? imm */
      case MoveOpStoreMem: /* memcopy r? imm */
      case MoveOpStoreImm: /* memset r? imm */
         iris_unparse_register(reg0, get_move_reg0(insn));
         sprintf(unparsed, operandTypes[threeOperandImmediate], op, reg0, get_move_immediate(insn));
         break;
      case MoveOpPush:
      case MoveOpPop:
         iris_unparse_register(reg0, get_move_reg0(insn));
         sprintf(unparsed, operandTypes[twoOperandRegister], op, reg0);
         break;
      case MoveOpPushImmediate:
         sprintf(unparsed, operandTypes[twoOperandImmediate], op, get_move_immediate(insn));
         break;
      default:
         sprintf(unparsed, "%s", "INVALID MOVE");
         break;
   }
}
void iris_unparse_jump(char* unparsed, instruction* insn) {
   const char* op;
   char reg0[RegisterStringLength];
   char reg1[RegisterStringLength];
   char reg2[RegisterStringLength];
   op = iris_jump_mnemonic(insn);
   switch(get_jump_op(insn)) {
      case JumpOpUnconditionalImmediate:
         sprintf(unparsed, operandTypes[twoOperandImmediate], op, get_jump_immediate(insn));
         break;
      case JumpOpUnconditionalImmediateLink:
      case JumpOpConditionalTrueImmediate:
      case JumpOpConditionalFalseImmediate:
      // remember that for the next two cases the predicate is implied
      case JumpOpConditionalTrueImmediateLink:
      case JumpOpConditionalFalseImmediateLink:
         iris_unparse_register(reg0, get_jump_reg0(insn));
         sprintf(unparsed, operandTypes[threeOperandImmediate], op, reg0, get_jump_immediate(insn));
         break;
      case JumpOpUnconditionalRegister:
         iris_unparse_register(reg0, get_jump_reg0(insn));
         sprintf(unparsed, operandTypes[twoOperandRegister], op, reg0);
         break;
      case JumpOpUnconditionalRegisterLink:
      case JumpOpConditionalTrueRegister:
      case JumpOpConditionalFalseRegister:
         iris_unparse_register(reg0, get_jump_reg0(insn));
         iris_unparse_register(reg1, get_jump_reg1(insn));
         sprintf(unparsed, operandTypes[threeOperandRegister], op, reg0, reg1);
         break;
      case JumpOpConditionalTrueRegisterLink:
      case JumpOpConditionalFalseRegisterLink:
      case JumpOpIfThenElseNormalPredTrue:
      case JumpOpIfThenElseNormalPredFalse:
      case JumpOpIfThenElseLinkPredTrue: /* implied predicate register */
      case JumpOpIfThenElseLinkPredFalse: /* implied predicate register */
         iris_unparse_register(reg0, get_jump_reg0(insn));
         iris_unparse_register(reg1, get_jump_reg1(insn));
         iris_unparse_register(reg2, get_jump_reg2(insn));
         sprintf(unparsed, operandTypes[fourOperandRegister], op, reg0, reg1, reg2);
         break;
      default: 
         sprintf(unparsed, "%s", "INVALID JUMP");
         break;
   }
}


void iris_unparse_compare(char* unparsed, instruction* insn) {
   const char* op;
   char reg0[RegisterStringLength];
   char reg1[RegisterStringLength];
   char reg2[RegisterStringLength];

   op = iris_compare_mnemonic(insn);
   iris_unparse_register(reg0, get_compare_reg0(insn));
   iris_unparse_register(reg1, get_compare_reg1(insn));
   iris_unparse_register(reg2, get_compare_reg2(insn));

   sprintf(unparsed, operandTypes[fourOperandRegister], op, reg0, reg1, reg2);
}
void iris_unparse_misc(char* unparsed, instruction* insn) {
   const char* op;
   char reg0[RegisterStringLength];
   char reg1[RegisterStringLength];
   op = iris_misc_mnemonic(insn);
   switch(get_misc_op(insn)) {
      case MiscOpSystemCall:
         iris_unparse_register(reg0, get_misc_reg0(insn));
         iris_unparse_register(reg1, get_misc_reg1(insn));
         sprintf(unparsed, "%s %d %s %s", op, get_misc_index(insn), reg0, reg1);
         break;
      default:
         sprintf(unparsed, "%s", "INVALID MISC");
         break;

   }

}

void iris_unparse_bitstring(char* unparsed, instruction* insn) {
   int bit;
   dword data;
   unparsed[32] = '\0';
   data = *insn;
   for (bit = 31; bit >= 0; bit -= 1) {
      unparsed[bit] = (data & 1) + '0';
      data >>= 1;
   }
}

/* vim: set expandtab tabstop=3 shiftwidth=3: */
