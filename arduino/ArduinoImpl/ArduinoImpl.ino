#include <libbitmanip.h>

/**
iris
Copyright (c) 2013-2018, Joshua Scoggins and Contributors
All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:
    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.
    * Redistributions in binary form must reproduce the above copyright
      notice, this list of conditions and the following disclaimer in the
      documentation and/or other materials provided with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR
ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
(INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

 */

using Integer = int16_t;
using Ordinal = uint16_t;
using LongOrdinal = uint32_t;
using LongInteger = int32_t;
using Word = Ordinal;
using DoubleWord = LongOrdinal;
using RawInstruction = DoubleWord;
using Address = Word;
constexpr auto RegisterCount = 64;
class Register {
  public:
    explicit constexpr Register(Ordinal value = 0) : _ordinal(value) { }
    explicit constexpr Register(Integer value) : _integer(value) { }
    constexpr Ordinal getOrdinal() const noexcept { return _ordinal; }
    constexpr Integer getInteger() const noexcept { return _integer; }
    void setValue(Ordinal v) noexcept {
      _ordinal = v;
    }
    void setValue(Integer v) noexcept {
      _integer = v;
    }
  private:    

union {
  Ordinal _ordinal;
  Integer _integer;
};
};

constexpr byte getMajorOpcode(RawInstruction inst) noexcept {
  return bitmanip::decode<byte, RawInstruction, LongOrdinal(0xFF000000), 24>(inst);
}

Register _registers[RegisterCount];
Word _instructionPointer;


void setup() {
  // put your setup code here, to run once:
  for (auto &a : _registers) {
    a.setValue(0u);
  }
}

void loop() {
  // put your main code here, to run repeatedly:

}
