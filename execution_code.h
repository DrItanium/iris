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
#ifndef IRIS_MICROCODE_H__
#define IRIS_MICROCODE_H__
#include <array>
#include <type_traits>
namespace iris::microcode {

using RawMicroInstruction = int32_t;
constexpr bool shouldBypassMicrocodeDispatch(RawMicroInstruction inst) noexcept {
    return inst < 0;
}
enum class MicroGroup : RawMicroInstruction {
    Error = 0x0000'0000,
    Memory = 0x1000'0000,
    Branch = 0x2000'0000,
    Compare = 0x3000'0000,
};

// in a memory instruction, the microinstruction kind is four bits after the group
// 0b0001'xxxx'....

enum class MemoryOpcodes : RawMicroInstruction {
    Load = 0x0000'0000,
    Store = 0x0100'0000,
};
// the two bits following the opcodes when we have a Load or Store describe the 
// memory space to operate on. 
// 0b0001'000{0|1}'xx??...
// 0b0001'000?'0000... - Code
// 0b0001'000?'0100... - Data
// 0b0001'000?'1000... - Stack
// 0b0001'000?'1100... - IO
enum class MemorySpace : RawMicroInstruction {
    Code = 0x0000'0000,
    Data = 0x0040'0000,
    Stack = 0x0040'0000,
    IO = 0x00C0'0000,
};


constexpr RawMicroInstruction Unimplemented() noexcept { return 0; }
constexpr RawMicroInstruction BypassMicrocode() noexcept { return 0x8000'0000; }
template<typename T>
constexpr std::underlying_type_t<std::enable_if_t<std::is_enum_v<T>, T>> unpack(T value) noexcept {
    using K = std::underlying_type_t<T>;
    return static_cast<K>(value);
}
constexpr auto ErrorGroup() noexcept { return unpack(MicroGroup::Error); }
constexpr auto MemoryGroup() noexcept { return unpack(MicroGroup::Memory); }
constexpr auto BranchGroup() noexcept { return unpack(MicroGroup::Branch); }
constexpr auto CompareGroup() noexcept { return unpack(MicroGroup::Compare); }
constexpr auto CodeMemory() noexcept { return unpack(MemorySpace::Code); }
constexpr auto DataMemory() noexcept { return unpack(MemorySpace::Data); }
constexpr auto StackMemory() noexcept { return unpack(MemorySpace::Stack); }
constexpr auto IOMemory() noexcept { return unpack(MemorySpace::IO); }
/// @todo figure out how to make incorrect flag usage a compile time error
#if 0
enum class MemoryFlags : RawMicroInstruction { 
    
};

enum class BranchFlags : RawMicroInstruction {

};
enum class CompareFlags : RawMicroInstruction {

};
enum class ErrorFlags : RawMicroInstruction { };
template<MicroGroup group>
using TargetFlags = std::conditional<group == MicroGroup::Memory,
      MemoryFlags,
      std::conditional<group == MicroGroup::Branch,
      BranchFlags,
      std::conditional<group == MicroGroup::Compare,
      CompareFlags,
      ErrorFlags>>>;
template<MicroGroup group, typename ... Args>
constexpr RawMicroInstruction makeInstruction(Args&& ... flags) {
    
}
#endif

    // increment is adding one to register
    // decrement is subtracting one from the register
    // double is multiply by two or shift left by two
    // halve is divide by two or shift right by two imm
    // double and triple can be defined in terms of this operation

constexpr inline std::array<RawMicroInstruction, 256> eeprom {
    ErrorGroup(), // 0
    BypassMicrocode(), // 1 addi
    BypassMicrocode(), // 2 addo
    BypassMicrocode(), // 3 subi
    BypassMicrocode(), // 4 subo
    BypassMicrocode(), // 5 muli
    BypassMicrocode(), // 6 mulo
    BypassMicrocode(), // 7 divi
    BypassMicrocode(), // 8 divo
    BypassMicrocode(), // 9 remi
    BypassMicrocode(), // 10 remo
    BypassMicrocode(), // 11 shli
    BypassMicrocode(), // 12 shlo
    BypassMicrocode(), // 13 shri
    BypassMicrocode(), // 14 shro
    BypassMicrocode(), // 15 maxi
    BypassMicrocode(), // 16 maxo
    BypassMicrocode(), // 17 not
    BypassMicrocode(), // 18 and
    BypassMicrocode(), // 19 or
    BypassMicrocode(), // 20 xor
    BypassMicrocode(), // 21 nor
    BypassMicrocode(), // 22 nand
    BypassMicrocode(), // 23 addi.imm8
    BypassMicrocode(), // 24 addo.imm8
    BypassMicrocode(), // 25 subi.imm8
    BypassMicrocode(), // 26 subo.imm8
    BypassMicrocode(), // 27 muli.imm8
    BypassMicrocode(), // 28 mulo.imm8
    BypassMicrocode(), // 29 divi.imm8
    BypassMicrocode(), // 30 divo.imm8
    BypassMicrocode(), // 31 remi.imm8
    BypassMicrocode(), // 32 remo.imm8
    BypassMicrocode(), // 33 shli.imm8
    BypassMicrocode(), // 34 shlo.imm8
    BypassMicrocode(), // 35 shri.imm8
    BypassMicrocode(), // 36 shro.imm8
    // memory style operations begin
    BypassMicrocode(), // 37 move
    BypassMicrocode(), // 38 swap
    BypassMicrocode(), // 39 set
    Unimplemented(), // 40
    Unimplemented(), // 41
    Unimplemented(), // 42 stack.push
    Unimplemented(), // 43 stack.push.imm
    Unimplemented(), // 44 stack.pop
    Unimplemented(), // 45 load.data (with offset)
    Unimplemented(), // 46 store.data (with offset)
    Unimplemented(), // 47 store.data.imm16
    Unimplemented(), // 48 load.code (with offset)
    Unimplemented(), // 49 store.code (with offset)
    Unimplemented(), // 50 
    Unimplemented(), // 51
    Unimplemented(), // 52
    Unimplemented(), // 53
    Unimplemented(), // 54
    Unimplemented(), // 55
    Unimplemented(), // 56
    Unimplemented(), // 57
    Unimplemented(), // 58
    Unimplemented(), // 59
    Unimplemented(), // 60
    Unimplemented(), // 61
    Unimplemented(), // 62
    Unimplemented(), // 63
    Unimplemented(), // 64
    Unimplemented(), // 65
    Unimplemented(), // 66
    Unimplemented(), // 67
    Unimplemented(), // 68
    Unimplemented(), // 69
    Unimplemented(), // 70
    Unimplemented(), // 71
    Unimplemented(), // 72
    Unimplemented(), // 73
    Unimplemented(), // 74
    Unimplemented(), // 75
    Unimplemented(), // 76
    Unimplemented(), // 77
    Unimplemented(), // 78
    Unimplemented(), // 79
    Unimplemented(), // 80
    Unimplemented(), // 81
    Unimplemented(), // 82
    Unimplemented(), // 83
    Unimplemented(), // 84
    Unimplemented(), // 85
    Unimplemented(), // 86
    Unimplemented(), // 87
    Unimplemented(), // 88
    Unimplemented(), // 89
    Unimplemented(), // 90
    Unimplemented(), // 91
    Unimplemented(), // 92
    Unimplemented(), // 93
    Unimplemented(), // 94
    Unimplemented(), // 95
    Unimplemented(), // 96
    Unimplemented(), // 97
    Unimplemented(), // 98
    Unimplemented(), // 99
    Unimplemented(), // 100
    Unimplemented(), // 101
    Unimplemented(), // 102
    Unimplemented(), // 103
    Unimplemented(), // 104
    Unimplemented(), // 105
    Unimplemented(), // 106
    Unimplemented(), // 107
    Unimplemented(), // 108
    Unimplemented(), // 109
    Unimplemented(), // 110
    Unimplemented(), // 111
    Unimplemented(), // 112
    Unimplemented(), // 113
    Unimplemented(), // 114
    Unimplemented(), // 115
    Unimplemented(), // 116
    Unimplemented(), // 117
    Unimplemented(), // 118
    Unimplemented(), // 119
    Unimplemented(), // 120
    Unimplemented(), // 121
    Unimplemented(), // 122
    Unimplemented(), // 123
    Unimplemented(), // 124
    Unimplemented(), // 125
    Unimplemented(), // 126
    Unimplemented(), // 127
    Unimplemented(), // 128
    Unimplemented(), // 129
    Unimplemented(), // 130
    Unimplemented(), // 131
    Unimplemented(), // 132
    Unimplemented(), // 133
    Unimplemented(), // 134
    Unimplemented(), // 135
    Unimplemented(), // 136
    Unimplemented(), // 137
    Unimplemented(), // 138
    Unimplemented(), // 139
    Unimplemented(), // 140
    Unimplemented(), // 141
    Unimplemented(), // 142
    Unimplemented(), // 143
    Unimplemented(), // 144
    Unimplemented(), // 145
    Unimplemented(), // 146
    Unimplemented(), // 147
    Unimplemented(), // 148
    Unimplemented(), // 149
    Unimplemented(), // 150
    Unimplemented(), // 151
    Unimplemented(), // 152
    Unimplemented(), // 153
    Unimplemented(), // 154
    Unimplemented(), // 155
    Unimplemented(), // 156
    Unimplemented(), // 157
    Unimplemented(), // 158
    Unimplemented(), // 159
    Unimplemented(), // 160
    Unimplemented(), // 161
    Unimplemented(), // 162
    Unimplemented(), // 163
    Unimplemented(), // 164
    Unimplemented(), // 165
    Unimplemented(), // 166
    Unimplemented(), // 167
    Unimplemented(), // 168
    Unimplemented(), // 169
    Unimplemented(), // 170
    Unimplemented(), // 171
    Unimplemented(), // 172
    Unimplemented(), // 173
    Unimplemented(), // 174
    Unimplemented(), // 175
    Unimplemented(), // 176
    Unimplemented(), // 177
    Unimplemented(), // 178
    Unimplemented(), // 179
    Unimplemented(), // 180
    Unimplemented(), // 181
    Unimplemented(), // 182
    Unimplemented(), // 183
    Unimplemented(), // 184
    Unimplemented(), // 185
    Unimplemented(), // 186
    Unimplemented(), // 187
    Unimplemented(), // 188
    Unimplemented(), // 189
    Unimplemented(), // 190
    Unimplemented(), // 191
    Unimplemented(), // 192
    Unimplemented(), // 193
    Unimplemented(), // 194
    Unimplemented(), // 195
    Unimplemented(), // 196
    Unimplemented(), // 197
    Unimplemented(), // 198
    Unimplemented(), // 199
    Unimplemented(), // 200
    Unimplemented(), // 201
    Unimplemented(), // 202
    Unimplemented(), // 203
    Unimplemented(), // 204
    Unimplemented(), // 205
    Unimplemented(), // 206
    Unimplemented(), // 207
    Unimplemented(), // 208
    Unimplemented(), // 209
    Unimplemented(), // 210
    Unimplemented(), // 211
    Unimplemented(), // 212
    Unimplemented(), // 213
    Unimplemented(), // 214
    Unimplemented(), // 215
    Unimplemented(), // 216
    Unimplemented(), // 217
    Unimplemented(), // 218
    Unimplemented(), // 219
    Unimplemented(), // 220
    Unimplemented(), // 221
    Unimplemented(), // 222
    Unimplemented(), // 223
    Unimplemented(), // 224
    Unimplemented(), // 225
    Unimplemented(), // 226
    Unimplemented(), // 227
    Unimplemented(), // 228
    Unimplemented(), // 229
    Unimplemented(), // 230
    Unimplemented(), // 231
    Unimplemented(), // 232
    Unimplemented(), // 233
    Unimplemented(), // 234
    Unimplemented(), // 235
    Unimplemented(), // 236
    Unimplemented(), // 237
    Unimplemented(), // 238
    Unimplemented(), // 239
    Unimplemented(), // 240
    Unimplemented(), // 241
    Unimplemented(), // 242
    Unimplemented(), // 243
    Unimplemented(), // 244
    Unimplemented(), // 245
    Unimplemented(), // 246
    Unimplemented(), // 247
    Unimplemented(), // 248
    Unimplemented(), // 249
    Unimplemented(), // 250
    Unimplemented(), // 251
    Unimplemented(), // 252
    Unimplemented(), // 253
    Unimplemented(), // 254
    Unimplemented(), // 255
};


} // end namespace iris::microcode

#endif // end IRIS_MICROCODE_H__
