/**
 * @copyright 
 * iris
 * Copyright (c) 2013-2019, Joshua Scoggins and Contributors
 * All rights reserved.
 * 
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions are met:
 *     * Redistributions of source code must retain the above copyright
 *       notice, this list of conditions and the following disclaimer.
 *     * Redistributions in binary form must reproduce the above copyright
 *       notice, this list of conditions and the following disclaimer in the
 *       documentation and/or other materials provided with the distribution.
 * 
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
 * DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
 * ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 * (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */
#include "types.h"
#include "iris.h"
#include "opcodes.h"
#include "execution_code.h"

namespace iris {
void 
Core::invoke(DoubleWord ibits) {
    Instruction inst(ibits);
    if (auto uinst = microcode::eeprom[inst.getOpcodeIndex()]; microcode::bypassesMicrocode(uinst)) {
        if (auto operation = inst.decode(); operation) {
            std::visit([this](auto&& op) { invoke(op); }, *operation);
        } else {
            throw BadOperationException();
        }
    } else if (microcode::isErrorGroup(uinst)) {
        // throw error
    } else if (microcode::isUnimplemented(uinst)) {
        // throw error
    } else {
        // a giant state machine
        if (microcode::isMemoryGroup(uinst)) {
            // the address in question is always in arg0
           auto arg0 = inst.getArg0<RegisterIndex>();
           auto address = getRegisterValue<Address>(arg0);
           bool updateArg0 = false;
           if (microcode::manipulateArg0Before(uinst)) {
               updateArg0 = true;
               if (microcode::incrArg0Action(uinst)) {
                    ++address;
               } else if (microcode::decrArg0Action(uinst)) {
                    --address;
               } else {
                   // raise error
               }
           }
           auto arg1 = inst.getArg1<RegisterIndex>();
           if (microcode::isStoreOp(uinst)) {
               // the value to write to some memory space is always in arg1
               Word lower = getRegisterValue<Word>(arg1);
               auto temporaryAddress = address;
               if (microcode::shouldTreatArg1AsImm16(uinst)) {
                   // overwrite what we did previously
                   lower = inst.getArg1<Address>();
               } else if (microcode::shouldTreatArg2AsImm8(uinst)) {
                   // we have to compute an offset here but make sure we
                   // don't corrupt the address to potentially be
                   // reinstalled into arg0 
                   auto offset = inst.getArg2<Byte>();
                   temporaryAddress += offset;
               }
               if (microcode::targetsCodeMemory(uinst)) {
                    auto arg2 = RegisterIndex{static_cast<uint8_t>(1 + static_cast<uint8_t>(arg1)) };
                   // need to make a double word using the third register as
                   // the upper 16-bits
                   auto upperHalf = static_cast<DoubleWord>(getRegisterValue<Word>(arg2)) << 16;
                   _code[temporaryAddress] = upperHalf | static_cast<DoubleWord>(lower);
               } else if (microcode::targetsStackMemory(uinst)) {
                   _stack[temporaryAddress] = lower;
               } else if (microcode::targetsDataMemory(uinst)) {
                   _data[temporaryAddress] = lower;
               } else if (microcode::targetsIOMemory(uinst)) {
                   // IO Space is special
                   /// @todo implement IO space dispatch
               } else {
                   // raise error
               }
           } else if (microcode::isLoadOp(uinst)) {
               // it is always going to be a load to arg1
               Word lowerHalf = 0;
               auto temporaryAddress = address;
               if (microcode::shouldTreatArg2AsImm8(uinst)) {
                   auto offset = inst.getArg2<Byte>();
                   temporaryAddress += offset;
               }
               if (microcode::targetsCodeMemory(uinst)) {
                    auto arg2 = RegisterIndex{static_cast<uint8_t>(1 + static_cast<uint8_t>(arg1)) };
                    auto intermediate = _code[address];
                    lowerHalf = static_cast<Word>(intermediate);
                    setRegisterValue<Word>(arg2, static_cast<Word>(intermediate >> 16));
                    // special case where we have to use the third register

               } else if (microcode::targetsStackMemory(uinst)) {
                   lowerHalf = _stack[address];
               } else if (microcode::targetsDataMemory(uinst)) {
                   lowerHalf = _data[address];
               } else if (microcode::targetsIOMemory(uinst)) {
                   /// @todo implement IO space dispatch
               } else {
                   // raise error
               }
               setRegisterValue<Word>(arg1, lowerHalf);
           } else {
               // raise error
           }
           if (microcode::manipulateArg0After(uinst)) {
               updateArg0 = true;
               if (microcode::incrArg0Action(uinst)) {
                   ++address;
               } else if (microcode::decrArg0Action(uinst)) {
                   --address;
               } else {
                   // raise error
               }
           }
           if (updateArg0) {
               setRegisterValue<Address>(inst.getArg0<RegisterIndex>(), address);
           }
        } else {

        }
    }
}
} // end namespace iris
