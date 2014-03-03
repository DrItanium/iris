#include <u.h>
#include <libc.h>
#include <stdio.h>
#include "processor.h"

void main() {
   processor proc;

   exits(0);
}
void putregister(processor* proc, byte index, byte value) {
   if(index >= 0 && index < 4) {
      proc->gpr[index] = value;
   } else if(index >= 4 && index < 8) {
      proc->memory[proc->gpr[index % 4]] = value;
   } else {
      sysfatal("panic: attempted to store to a register out of range");
      exits("registerputoutofrange");
   }
}
byte getregister(processor* proc, byte index) {
   if(index >= 0 && index < 4) {
      return proc->gpr[index];
   } else if(index >= 4 && index < 8) {
      return proc->memory[proc->gpr[index % 4]];
   } else {
      sysfatal("panic: attempted to retrieve a value from a register out of range");
      exits("registergetoutofrange");
      return 0;
   }
}
void arithmetic(processor* proc, instruction inst) {
   
}
void move(processor* proc, instruction inst) {

}

void jump(processor* proc, instruction inst) {

   if(inst.jump.distance == JumpDistanceShort) {
         
   } else {
     /* long form */ 
   }


}
void compare(processor* proc, instruction inst) {
   byte value;
   switch(inst.compare.op) {
      case CompareOpEq:
         value = (getregister(proc, inst.compare.reg0) ==
                  getregister(proc, inst.compare.reg1));
         break;
      case CompareOpNeq:
         value = (getregister(proc, inst.compare.reg0) !=
                  getregister(proc, inst.compare.reg1));
         break;
      case CompareOpLessThan:
         value = (getregister(proc, inst.compare.reg0) < 
                  getregister(proc, inst.compare.reg1));
         break;
      case CompareOpGreaterThan:
         value = (getregister(proc, inst.compare.reg0) > 
                  getregister(proc, inst.compare.reg1));
         break;
      case CompareOpLessThanOrEqualTo:
         value = (getregister(proc, inst.compare.reg0) <= 
                  getregister(proc, inst.compare.reg1));
         break;
      case CompareOpGreaterThanOrEqualTo:
         value = (getregister(proc, inst.compare.reg0) >= 
                  getregister(proc, inst.compare.reg1));
         break;
      default:
         sysfatal("panic: invalid compare operation");
         exits("invalidcompareopcode");
   }

   switch(inst.compare.combinebits) {
      case CombineBitsOpNil:
         proc->predicateregister = value;
         break;
      case CombineBitsOpAnd:
         proc->predicateregister &= value;
         break;
      case CombineBitsOpOr:
         proc->predicateregister |= value;
         break;
      case CombineBitsOpXor:
         proc->predicateregister ^= value;
         break;
      default:
         sysfatal("panic: invalid compare combine bits");
         exits("invalidcomparecombinebits");
   }
}


