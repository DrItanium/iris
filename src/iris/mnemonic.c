#include <string.h>
#include "mnemonic.h"

char* arithmetic_mnemonic(instruction i) {
   switch(i.arithmetic.op) {
      case ArithmeticOpAdd:        return "add";
      case ArithmeticOpSub:        return "sub";
      case ArithmeticOpMul:        return "mul";
      case ArithmeticOpDiv:        return "div";
      case ArithmeticOpRem:        return "rem";
      case ArithmeticOpShiftLeft:  return "shl";
      case ArithmeticOpShiftRight: return "shr";
      case ArithmeticOpBinaryAnd:  return "and";
      case ArithmeticOpBinaryOr:   return "or";
      case ArithmeticOpBinaryNot:  return "not";
      case ArithmeticOpBinaryXor:  return "xor";
      default:                     return "UNASSIGNED_ARITHMETIC";
   }
}

char* move_mnemonic(instruction i) {
   switch(i.move.op) {
      case MoveOpRegToReg:       return "move";
      case MoveOpImmediateToReg: return "move";
      case MoveOpRegToAddress:
         switch(i.move.addressmode.accessmode) {
            case AccessModeMoveOpLoad:  return "load";
            case AccessModeMoveOpStore: return "store";
         }
      default:                   return "UNKNOWN_MOVE";
   }
}

char* jump_mnemonic(instruction i) {
   switch(i.jump.conditional) {
      case JumpOpUnconditional: return "goto";
      case JumpOpIfTrue:        return "iftrue";
      case JumpOpIfFalse:       return "iffalse";
      case JumpOpIfThenElse:    return "if";
      default:                  return "UNKNOWN_JUMP";
   }
}

char* compare_mnemonic(instruction i) {
   char combinebits[5];

   switch(i.compare.combinebits) {
      case CombineBitsOpNil: strcpy(combinebits, "");     break;
      case CombineBitsOpAnd: strcpy(combinebits, "and_"); break;
      case CombineBitsOpOr:  strcpy(combinebits, "or_");  break;
      case CombineBitsOpXor: strcpy(combinebits, "xor_"); break;
      default:               return "UNKNOWN_COMPARE";
   }

   switch(i.compare.op) {
      case CompareOpEq:                   return strcat(combinebits, "eq");
      case CompareOpNeq:                  return strcat(combinebits, "ne");
      case CompareOpLessThan:             return strcat(combinebits, "lt");
      case CompareOpGreaterThan:          return strcat(combinebits, "gt");
      case CompareOpLessThanOrEqualTo:    return strcat(combinebits, "le");
      case CompareOpGreaterThanOrEqualTo: return strcat(combinebits, "ge");
      default:                            return "UNKNOWN_COMPARE";
   }
}

/* vim: set expandtab tabstop=3 shiftwidth=3: */
