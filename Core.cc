/*
 * iris
 * Copyright (c) 2013-2018, Joshua Scoggins and Contributors
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


#include "Core.h"
#include <iostream>
#include <functional>
#include <sstream>
#include <vector>
#include <fstream>


namespace iris {
    constexpr RegisterIndex getDestinationIndex(RawInstruction i) noexcept {
        return decodeBits<decltype(i), RegisterIndex, 0x0000'FF00, 8>(i);
    }
    constexpr RegisterIndex getSourceIndex(RawInstruction i) noexcept {
        return decodeBits<decltype(i), RegisterIndex, 0x00FF'0000, 16>(i);
    }
    constexpr RegisterIndex getSource2Index(RawInstruction i) noexcept {
        return decodeBits<decltype(i), RegisterIndex, 0xFF00'0000, 24>(i);
    }
    constexpr Address getImmediate16(RawInstruction i) noexcept {
        return decodeBits<decltype(i), Address, 0xFFFF'0000, 16>(i);
    }
    constexpr byte getImmediate8(RawInstruction i) noexcept {
        if constexpr (std::is_same_v<byte, RegisterIndex>) {
            return getSource2Index(i);
        } else {
            return decodeBits<decltype(i), byte, 0xFF00'0000, 24>(i);
        }
    }
    constexpr RawInstruction setDestinationIndex(RegisterIndex dest, RawInstruction i = 0) noexcept {
        return encodeBits<decltype(i), decltype(dest), 0x0000'FF00, 8>(i, dest);
    }
    constexpr RawInstruction setSourceIndex(RegisterIndex src, RawInstruction i = 0) noexcept {
        return encodeBits<decltype(i), decltype(src), 0x00FF'0000, 16>(i, src);
    }
    constexpr RawInstruction setSource2Index(RegisterIndex src2, RawInstruction i = 0) noexcept {
        return encodeBits<decltype(i), decltype(src2), 0xFF00'0000, 24>(i, src2);
    }
    constexpr RawInstruction setImmediate16(Address imm16, RawInstruction i = 0) noexcept {
        return encodeBits<decltype(i), decltype(imm16), 0xFFFF'0000, 16>(i, imm16);
    }
    constexpr RawInstruction setImmediate8(byte imm8, RawInstruction i = 0) noexcept {
        if constexpr (std::is_same_v<decltype(imm8), RegisterIndex>) {
            return setSource2Index(imm8, i);
        } else {
            return encodeBits<decltype(i), decltype(imm8), 0xFF00'0000, 24>(i, imm8);
        }
    }
    constexpr RawInstruction setRegisterPair(RegisterIndex dest, RegisterIndex src, RawInstruction i = 0) noexcept {
        return setDestinationIndex(dest, setSourceIndex(src, i));
    }
    Register::Register() : Register(0) { }
    Register::Register(Number v) : _value(v) { }
    Register::~Register() {
        _value = 0;
    }
    void Register::setValue(Number v) noexcept {
        if (!_noWrites) {
            _value = v;
        }
    }

    Core::Core() : _pc(0) { 
        _code = std::make_unique<RawInstruction[]>(addressSize);
        _data = std::make_unique<Number[]>(addressSize);
        _stack = std::make_unique<Number[]>(addressSize);
        _registers = std::make_unique<Register[]>(registerCount);
    }
    void Core::decodeArguments(RawInstruction, Core::NoArguments&) noexcept { }
    void Core::decodeArguments(RawInstruction i, Core::OneRegister& a) noexcept {
        a.dest = getDestinationIndex(i);
    }
    void Core::decodeArguments(RawInstruction i, Core::TwoRegister& a) noexcept {
        a.dest = getDestinationIndex(i);
        a.src = getSourceIndex(i);
    }
    void Core::decodeArguments(RawInstruction i, Core::ThreeRegister& a) noexcept {
        a.dest = getDestinationIndex(i);
        a.src = getSourceIndex(i);
        a.src2 = getSource2Index(i);
    }
    void Core::decodeArguments(RawInstruction i, Core::Immediate16& a) noexcept {
        a.imm = getImmediate16(i);
    }
    void Core::decodeArguments(RawInstruction i, Core::OneRegisterWithImmediate& a) noexcept {
        a.imm = getImmediate16(i);
        a.dest = getDestinationIndex(i);
    }
    void Core::decodeArguments(RawInstruction i, Core::TwoRegisterWithImmediate& a) noexcept {
        a.dest = getDestinationIndex(i);
        a.src = getSourceIndex(i);
        a.src2 = getImmediate8(i);
    }
    Core::DecodedInstruction Core::decodeInstruction(RawInstruction i) {
        Core::DecodedInstruction tmp;
        switch (decodeBits<RawInstruction, Opcode, 0x0000'00FF, 0>(i)) {
#define X(title, style, z) \
            case Opcode :: title : \
                                   tmp = Core::title () ; \
            break;
#define FirstX(title, style, z) X(title, style, z)
#include "Opcodes.def"
#undef X
#undef FirstX
            default:
                throw Problem("Illegal Opcode!");
        }
        std::visit([this, i](auto&& value) { decodeArguments(i, value._args); }, tmp);
        return tmp;
    }

    RawInstruction Core::encodeArguments(const Core::NoArguments&) noexcept { return 0; }
    RawInstruction Core::encodeArguments(const Core::OneRegister& reg) noexcept { 
        return setDestinationIndex(reg.dest);
    }
    RawInstruction Core::encodeArguments(const Core::TwoRegister& reg) noexcept {
        return setRegisterPair(reg.dest, reg.src);
    }
    RawInstruction Core::encodeArguments(const Core::ThreeRegister& reg) noexcept {
        return setRegisterPair(reg.dest, reg.src, setSource2Index(reg.src2));
    }
    RawInstruction Core::encodeArguments(const Core::Immediate16& a) noexcept {
        return setImmediate16(a.imm);
    }
    RawInstruction Core::encodeArguments(const Core::OneRegisterWithImmediate& a) noexcept {
        return setImmediate16(a.imm, setDestinationIndex(a.dest));
    }
    RawInstruction Core::encodeArguments(const Core::TwoRegisterWithImmediate& a) noexcept {
        return setRegisterPair(a.dest, a.src, setImmediate8(a.src2));
    }
    RawInstruction Core::encodeInstruction(const Core::DecodedInstruction& i) noexcept {
        return std::visit([this](auto&& v) { return encodeBits<RawInstruction, byte, 0x0000'00FF, 0>(encodeArguments(v._args), byte(v.opcode())); }, i);
    }
    void Core::dispatchInstruction(const Core::DecodedInstruction& di) {
        std::visit([this](auto&& v) { perform(v); }, di);
    }
    void Core::setRegister(RegisterIndex index, Number value) noexcept {
        _registers[index].setValue(value);
    }
    const Register& Core::getRegister(RegisterIndex index) const noexcept {
        return _registers[index];
    }
#define DefExec(title) \
    void Core::perform ( const Core:: title & op ) 
    DefExec(Add) { setDestination(op, getSource(op).integer + getSource2(op).integer); }
    DefExec(Sub) { 
        auto a = getSource(op).integer;
        auto b = getSource2(op).integer;
        auto c = a - b;
        setDestination(op, c);
    }
    DefExec(Mul) { setDestination(op, getSource(op).integer * getSource2(op).integer); }
    DefExec(Div) { setDestination(op, getSource(op).integer / getSource2(op).integer); }
    DefExec(Rem) { setDestination(op, getSource(op).integer % getSource2(op).integer); }
    DefExec(ShiftLeft) { setDestination(op, getSource(op).integer << getSource2(op).integer); }
    DefExec(ShiftRight) { setDestination(op, getSource(op).integer >> getSource2(op).integer); }
    DefExec(And) { setDestination(op, getSource(op).integer & getSource2(op).integer); }
    DefExec(Or) { setDestination(op, getSource(op).integer | getSource2(op).integer); }
    DefExec(Not) { setDestination(op, ~getSource(op).integer); }
    DefExec(Xor) { setDestination(op, getSource(op).integer ^ getSource2(op).integer); }
    DefExec(Nand) { setDestination(op, binaryNand(getSource(op).integer, getSource2(op).integer)); }
    DefExec(Nor) { setDestination(op, binaryNor(getSource(op).integer, getSource2(op).integer)); }
    DefExec(AddImmediate) { setDestination(op, getSource(op).integer + getSource2(op).integer); }
    DefExec(SubImmediate) { setDestination(op, getSource(op).integer - getSource2(op).integer); }
    DefExec(MulImmediate) { setDestination(op, getSource(op).integer * getSource2(op).integer); }
    DefExec(DivImmediate) { setDestination(op, getSource(op).integer / getSource2(op).integer); }
    DefExec(RemImmediate) { setDestination(op, getSource(op).integer % getSource2(op).integer); }
    DefExec(ShiftLeftImmediate) { setDestination(op, getSource(op).integer << getSource2(op).integer); }
    DefExec(ShiftRightImmediate) { setDestination(op, getSource(op).integer >> getSource2(op).integer); }
    DefExec(Min) {
        auto a = getSource(op).integer;
        auto b = getSource2(op).integer;
        setDestination(op, a < b ? a : b);
    }
    DefExec(Max) {
        auto a = getSource(op).integer;
        auto b = getSource2(op).integer;
        setDestination(op, a > b ? a : b);
    }
    DefExec(LogicalXor) {
        auto a = getSource(op).getTruth();
        auto b = getSource2(op).getTruth();
        setDestination(op, a != b);
    }
    DefExec(LogicalNot) { setDestination(op, !getSource(op).getTruth()); }
    DefExec(LogicalAnd) {
        auto a = getSource(op).getTruth();
        auto b = getSource2(op).getTruth();
        setDestination(op, a && b);
    }
    DefExec(LogicalOr) {
        auto a = getSource(op).getTruth();
        auto b = getSource2(op).getTruth();
        setDestination(op, a || b);
    }
    DefExec(LogicalNand) { setDestination(op, binaryNand<bool>(getSource(op).getTruth(), getSource2(op).getTruth())); }
    DefExec(LogicalNor) { setDestination(op, binaryNor<bool>(getSource(op).getTruth(), getSource2(op).getTruth())); }
    DefExec(Eq) { setDestination(op, getSource(op).integer == getSource2(op).integer); }
    DefExec(EqImmediate) { setDestination(op, getSource(op).integer == getSource2(op).integer); }
    DefExec(Neq) { setDestination(op, getSource(op).integer != getSource2(op).integer); }
    DefExec(NeqImmediate) { setDestination(op, getSource(op).integer != getSource2(op).integer); }
    DefExec(LessThan) { setDestination(op, getSource(op).integer < getSource2(op).integer); }
    DefExec(LessThanImmediate) { setDestination(op, getSource(op).integer < getSource2(op).integer); }
    DefExec(LessThanOrEqualTo) { setDestination(op, getSource(op).integer <= getSource2(op).integer); }
    DefExec(LessThanOrEqualToImmediate) { setDestination(op, getSource(op).integer <= getSource2(op).integer); }
    DefExec(GreaterThan) { setDestination(op, getSource(op).integer > getSource2(op).integer); }
    DefExec(GreaterThanImmediate) { setDestination(op, getSource(op).integer > getSource2(op).integer); }
    DefExec(GreaterThanOrEqualTo) { setDestination(op, getSource(op).integer >= getSource2(op).integer); }
    DefExec(GreaterThanOrEqualToImmediate) { setDestination(op, getSource(op).integer >= getSource2(op).integer); }
    DefExec(Move) { setDestination(op, getSource(op)); }
    DefExec(Swap) {
        if (op._args.dest != op._args.src) {
            auto v0 = getSource(op);
            setRegister(op._args.src, getRegisterValue(op._args.dest));
            setRegister(op._args.dest, v0);
        }
    }
    DefExec(Set) { setDestination(op, op._args.imm); }
    DefExec(Load) { 
        auto addr = getRegisterValue(op._args.src).address;
        auto value = _data[addr];
        setDestination(op, value);
    }
    DefExec(LoadImmediate) { setDestination(op, _data[op._args.imm]); }
    DefExec(Store) { _data[getRegisterValue(op._args.dest).address] = getRegisterValue(op._args.src); }
    DefExec(StoreImmediate) { _data[getRegisterValue(op._args.dest).address] = op._args.imm; }
    DefExec(Push) {
        Address stackAddress = getRegisterValue(op._args.dest).address - 1;
        Address value = getSource(op).address;
        _stack[stackAddress] = value;
        setRegister(op._args.dest, stackAddress);
    }
    DefExec(PushImmediate) {
        Address stackAddress = getRegisterValue(op._args.dest).address - 1;
        Address value = op._args.imm;
        _stack[stackAddress] = value;
        setRegister(op._args.dest, stackAddress);
    }
    DefExec(Pop) {
        setDestination(op, _stack[getRegisterValue(op._args.src).address]);
        setRegister(op._args.src, getRegisterValue(op._args.src).address + 1);
    }
    DefExec(LoadCode) {
        auto inst = _code[getSource2(op).address];
        setRegister(op._args.dest, decodeBits<RawInstruction, Address, 0x0000'FFFF, 0>(inst));
        setRegister(op._args.src, decodeBits<RawInstruction, Address, 0xFFFF'0000, 16>(inst));
    }

    DefExec(StoreCode) {
        auto lower = getSource(op).address;
        auto upper = getSource2(op).address;
        _code[getRegisterValue(op._args.dest).address] = encodeBits<RawInstruction, Address, 0xFFFF'0000, 16>(encodeBits<RawInstruction, Address, 0x0000'FFFF, 0>(0, lower), upper);
    }
    DefExec(Branch) { _pc = op._args.imm; }
    DefExec(BranchAndLink) {
        setDestination(op, _pc);
        _pc = op._args.imm;
    }
    DefExec(BranchIndirect) {
        _pc = getRegisterValue(op._args.dest).address;
    }
    DefExec(BranchIndirectLink) {
        setRegister(op._args.src, _pc);
        _pc = getRegisterValue(op._args.dest).address;
    }
    DefExec(BranchConditional) {
        if (getRegisterValue(op._args.dest).getTruth()) {
            _pc = op._args.imm;
        }
    }
    DefExec(BranchConditionalIndirect) {
        if (getSource(op).getTruth()) {
            _pc = getRegisterValue(op._args.dest).address;
        }
    }
    DefExec(BranchConditionalIndirectLink) {
        if (getSource(op).getTruth()) {
            setRegister(op._args.src2, _pc);
            _pc = getRegisterValue(op._args.dest).address;
        }
    }

    DefExec(TerminateExecution) {
        _keepExecuting = getRegisterValue(op._args.dest).getTruth();
    }

    DefExec(LoadIO) {
        auto addr = getSource(op).address;
        for (auto& a : _io) {
            if (a.respondsTo(addr)) {
                setDestination(op, a.read(addr));
            }
        }
    }
    DefExec(StoreIO) {
        auto addr = getRegisterValue(op._args.dest).address;
        auto value = getSource(op).address;
        for (auto& a : _io) {
            if (a.respondsTo(addr)) {
                a.write(addr, value);
            }
        }
    }
    DefExec(SaveGroupOfRegisters) {
        // extract the bits from the immediate and use it when calling the 
        // push operation
        Core::Push tmp;
        tmp._args.dest = op._args.dest;
        // shift by three to get the correct offset
        auto base = (op._args.imm & 0b11111) << 3;
        auto mask = (op._args.imm & 0b1111111100000000) >> 8;
        if (mask == 0) {
            return;
        }
        for (auto index = base; index < (base + 8); ++index) {
            // start at the smallest value and work your way up
            if ((mask & 0b1) != 0) {
                tmp._args.src = SourceRegister(index);
                perform(tmp);
            }
            mask = mask >> 1;
        }
    }
    DefExec(RestoreGroupOfRegisters) {
        // extract the bits from the immediate and use it when calling the pop
        // operation
        // 
        // unlike the pushg operation, this one will do things a tad 
        // differently
        Core::Pop tmp;
        tmp._args.src = op._args.dest; // the destination becomes the source
        auto base = ((op._args.imm & 0b11111) << 3); // go over by one
        auto mask = (op._args.imm & 0b1111111100000000);
        if (mask == 0) {
            return;
        }
        static constexpr decltype(mask) upperMostMask = 0b1000'0000'0000'0000;
        for (auto index = base + 7; index >= base; --index) {
            // we shift to the left instead of right and use the mask to determine
            // if we should pop a value off
            if ((upperMostMask & mask) != 0) {
                tmp._args.dest = DestinationRegister(index);
                perform(tmp);
            }
            mask = mask << 1;
        }
    }
    DefExec(GetUpperByte) {
        setDestination(op, decodeBits<Address, Address, 0xFF00, 8>(getSource(op).address));
    }
    DefExec(GetLowerByte) {
        setDestination(op, decodeBits<Address, Address, 0x00FF, 0>(getSource(op).address));
    }
    DefExec(UnpackHalves) {
        Core::GetLowerByte lower;
        lower._args.dest = op._args.dest;
        lower._args.src = op._args.src2;
        Core::GetUpperByte upper;
        upper._args.dest = op._args.src;
        upper._args.src = op._args.src2;
        perform(lower);
        perform(upper);
    }
    DefExec(UnsignedEq) { setDestination(op, getSource(op).address == getSource2(op).address); }
    DefExec(UnsignedEqImmediate) { setDestination(op, getSource(op).address == getSource2(op).address); }
    DefExec(UnsignedNeq) { setDestination(op, getSource(op).address != getSource2(op).address); }
    DefExec(UnsignedNeqImmediate) { setDestination(op, getSource(op).address != getSource2(op).address); }
    DefExec(UnsignedLessThan) { setDestination(op, getSource(op).address < getSource2(op).address); }
    DefExec(UnsignedLessThanImmediate) { setDestination(op, getSource(op).address < getSource2(op).address); }
    DefExec(UnsignedLessThanOrEqualTo) { setDestination(op, getSource(op).address <= getSource2(op).address); }
    DefExec(UnsignedLessThanOrEqualToImmediate) { setDestination(op, getSource(op).address <= getSource2(op).address); }
    DefExec(UnsignedGreaterThan) { setDestination(op, getSource(op).address > getSource2(op).address); }
    DefExec(UnsignedGreaterThanImmediate) { setDestination(op, getSource(op).address > getSource2(op).address); }
    DefExec(UnsignedGreaterThanOrEqualTo) { setDestination(op, getSource(op).address >= getSource2(op).address); }
    DefExec(UnsignedGreaterThanOrEqualToImmediate) { setDestination(op, getSource(op).address >= getSource2(op).address); }
    DefExec(UnsignedAnd) { setDestination(op, getSource(op).address & getSource2(op).address); }
    DefExec(UnsignedOr) { setDestination(op, getSource(op).address | getSource2(op).address); }
    DefExec(UnsignedNot) { setDestination(op, ~getSource(op).address); }
    DefExec(UnsignedXor) { setDestination(op, getSource(op).address ^ getSource2(op).address); }
    DefExec(UnsignedNand) { setDestination(op, binaryNand(getSource(op).address, getSource2(op).address)); }
    DefExec(UnsignedNor) { setDestination(op, binaryNor(getSource(op).address, getSource2(op).address)); }
    DefExec(UnsignedMin) {
        auto a = getSource(op).address;
        auto b = getSource2(op).address;
        setDestination(op, a < b ? a : b);
    }
    DefExec(UnsignedMax) {
        auto a = getSource(op).address;
        auto b = getSource2(op).address;
        setDestination(op, a > b ? a : b);
    }
    DefExec(ReadToken) {
        if (op._args.dest == op._args.src) {
            throw Problem("Destination and source must be different!");
        }
        // taken from the flow graph on pg 90 of threaded interpretive languages
        //
        // three register
        // src - line buffer
        // dest - dictionary pointer / destination
        auto dp = getRegister(op._args.dest).get<Address>();
        auto lbp = getSource(op).address;
        //auto separator = getSource2(op).get<byte>();
        static constexpr char separator = 0x20;
        // ignore any whitespace before token itself
        for (auto front = (signed char)(_code[lbp]); front == separator; ++lbp, front = (signed char)(_code[lbp]));
        auto start = lbp; // save the start of the token
        auto count = 0;
        while (true) {
            ++lbp;
            ++count;
            auto curr = (signed char)(_code[lbp]);
            if (curr == separator) {
                break;
            }
            // the terminator is specially encoded to have the upper most bit 
            // set to one
            if (curr < 0) {
                // line terminator is one bit too far
                --lbp;
                break;
            }
        }
        // stash the start of the next token into the lbp (src) register
        ++lbp;
        setRegister(op._args.src, lbp);
        // go back to the starting token position
        lbp = start;
        // stash the length into the destination
        _code[dp] = count;
        // go to the next cell
        ++dp;
        do {
            // start copying the token contents over
          _code[dp] = _code[lbp];
          // advance both
          ++lbp;
          ++dp;
          --count;
          // keep doing this until we get back to the place we stopped
        } while (count > 0);
        // update the dictionary pointer as well
        setRegister(op._args.dest, dp);
    }
    DefExec(WriteRangeToIOAddress) {
        // destination is io address
        // src the starting point in the code space
        // src2 the length
        // only the lower half of each number is used
        auto starting = getSource(op).address;
        auto ioStorage = getRegister(op._args.dest).get<Address>();
        auto end = starting + getSource2(op).address;
        if (end < starting) {
            throw Problem("Memory wrap around");
        } else {
            for (auto& a : _io) {
                if (a.respondsTo(ioStorage)) {
                    for (auto loc = starting; loc < end; ++loc) {
                        // load the lower half of it and store it
                        a.write(ioStorage, static_cast<Address>(0x0000FFFF & _code[loc]));
                    }
                    break;
                }
            }
        }
    }
    DefExec(SetBase) {
        // only update if it is in range
        if (auto value = getRegister(op._args.dest).get<byte>(); value >= 2 && value < 37) {
            _base = value;
        } 
    }
    DefExec(GetBase) {
        setDestination(op, Number(_base));
    }
    DefExec(NumberRoutine) {
        // src2 is the base address
        // src is the resultant number
        // dest is the success code
        auto flag = false;
        auto baseAddress = getSource2(op).address;
        auto p1 = Integer(_code[baseAddress] & 0xFFFF);
        auto count = _code[p1];
        ++p1;
        auto sgn = _code[p1] == '-';
        if (sgn) {
            ++p1;
        }
        auto result = 0;
        auto terminatedEarly = false;
        while (count != 0) {
            auto num = Integer(_code[p1] & 0xFF) - 0x30;
            if (num < 0) {
                flag = false;
                terminatedEarly = true;
                break;
            }
            if (num > 9) {
                if (num > 0x11) {
                    num = num - 7;
                } else {
                    flag = false;
                    terminatedEarly = true;
                    break;
                }
            }
            if (num < (_base - 1)) {
                result = (result * _base) + num;
                --count;
                ++p1;
            } else {
                flag = false;
                terminatedEarly = true;
                break;
            }
        }
        if (!terminatedEarly) {
            if (sgn) {
                result = -result;
            }
            flag = true;
            setRegister(op._args.src, result);
        }
        setRegister(op._args.dest, flag);
    }
#undef DefExec
    void Core::installIODevice(Core::IODevice dev) {
        _io.emplace_back(dev);
    }
    RawInstruction Core::extractInstruction() noexcept {
        // extract the current instruction and then go next
        auto result = _code[_pc];
        ++_pc;
        return result;
    }

    void Core::cycle() {
        // load the current instruction and go to the next address
        auto inst = extractInstruction(); // automatically increment PC
        auto op = decodeInstruction(inst);
        dispatchInstruction(op);
    }
    void Core::execute() {
        while(_keepExecuting) {
            cycle();
        }
    }

    void Core::dump(std::ostream& out) {
        // start with registers
        auto putAddress = [&out](Address v) {
            out.put(decodeBits<Address, char, 0x00FF, 0>(v));
            out.put(decodeBits<Address, char, 0xFF00, 8>(v));
        };
        auto putDoubleAddress = [putAddress](RawInstruction v) {
            putAddress(decodeBits<RawInstruction, Address, 0x0000'FFFF, 0>(v));
            putAddress(decodeBits<RawInstruction, Address, 0xFFFF'0000, 16>(v));
        };
        auto putRegister = [putAddress](const Register& reg) { putAddress(reg.getValue().address); };
        auto walkThroughSection = [](auto fn) {
            for (int i = 0; i < Core::maxAddress; ++i) {
                fn(i);
            }
        };
        for (int i = 0; i < Core::registerCount; ++i) {
            putRegister(_registers[i]);
        }
        walkThroughSection([this, putDoubleAddress](int i) { putDoubleAddress(_code[i]); });
        walkThroughSection([this, putAddress](int i) { putAddress(_data[i].get<Address>()); });
        walkThroughSection([this, putAddress](int i) { putAddress(_stack[i].get<Address>()); });
    }
    void Core::install(std::istream& in) {
        auto getByte = [&in]() {
            union {
                char c;
                byte b;
            } tmp;
            tmp.c = in.get();
            return tmp.b;
        };
        auto getAddress = [getByte]() {
            auto lower = Address(getByte());
            auto upper = Address(getByte()) << 8;
            return lower | upper;
        };
        auto getDoubleAddress = [getAddress]() {
            auto lower = RawInstruction(getAddress());
            auto upper = RawInstruction(getAddress()) << 16;
            return lower | upper;
        };
        for (int i = 0; i < Core::registerCount; ++i) {
            _registers[i].setValue(Number(getAddress()));
        }
        auto walkThroughSection = [](auto fn) {
            for (int i = 0; i < Core::maxAddress; ++i) {
                fn(i);
            }
        };
        walkThroughSection([this, getDoubleAddress](int i) { _code[i] = getDoubleAddress(); });
        walkThroughSection([this, getAddress](int i) { _data[i].address = getAddress(); });
        walkThroughSection([this, getAddress](int i) { _stack[i].address = getAddress(); });
    }
    Address Core::IODevice::read(Address addr) {
        if (_read) {
            return _read(addr - _begin);
        } else {
            return 0;
        }
    }
    void Core::IODevice::write(Address addr, Address value) {
        if (_write) {
            _write(addr - _begin, value);
        }
    }
    bool Core::IODevice::respondsTo(Address addr) const noexcept {
        return (addr >= _begin) && (addr < _end);
    }
    Address readFromStandardIn(Address index) {
        if (index == 0) {
            return Address(byte(char(std::cin.get())));
        } else {
            // doesn't make sense to do this
            return 0;
        }
    }
    void writeToStandardOut(Address index, Address value) {
        if (index == 0) {
            std::cout << char(value);
        } else if (index == 1) {
            // control setup / extra actions
            switch (value) {
                case 0: // perform a flush
                    std::cout.flush();
                    break;
                default:
                    break;
            }
        } else {
            throw Problem("Unimplemented address!");
        }
    }
    void Core::init() {
        // disable writing to register 0
        _registers[0].setValue(0);
        _registers[0].disableWrites();
        // setup the basic IO device for console input output
        IODevice sink(0);
        IODevice console(1, 2, readFromStandardIn, writeToStandardOut);
        auto selectCore = [this](auto index, auto value) {
            // we use the index to determine what action to perform
            std::stringstream fileName;
            fileName << value; // the number becomes the filename to dump to disk
            std::string path = fileName.str();
            if (index == 0) {
                // perform a dump to disk
                std::ofstream outputFile(path);
                // dump as plain text instead of binary encoding to prevent 
                // encoding shenanigans and increase portability. 
                //
                // Also make git not view the file as a binary file
                if (outputFile.is_open()) {
                    for (int i = 0; i < 0x10000 ; ++i) {
                        outputFile << std::hex << _data[i].address << std::endl;
                    }
                }
                outputFile.close();
            } else if (index == 1) {
                // load core from disk
                std::ifstream inputFile(path);
                if (inputFile.is_open()) {
                    for (int i = 0; i < 0x10000; ++i) {
                        inputFile >> std::hex >> _data[i].address;
                    }
                } else {
                    for (int i = 0; i < 0x10000; ++i) {
                        _data[i].address = 0;
                    }
                }
                inputFile.close();
            } else {
                throw Problem("Unimplemented address!");
            }
        };
        auto dumpVM = [this](auto index, auto value) {
            if (index == 0) {
                std::stringstream fileName;
                fileName << "iris_" << std::hex << value << ".core";
                auto path = fileName.str();
                std::ofstream file(path, std::ofstream::trunc);
                if (file.is_open()) {
                    dump(file);
                    file.close();
                } else {
                    file.close();
                    std::stringstream msg;
                    msg << "Could not dump memory to " << path;
                    auto errmsg = msg.str();
                    throw Problem(errmsg);
                }
            } else {
                throw Problem("Unimplemented address!");
            }
        };
        IODevice coreManipulator(3, 2, nullptr, selectCore);
        IODevice vmDumper(5, 1, nullptr, dumpVM);

        installIODevice(sink);
        installIODevice(console);
        installIODevice(coreManipulator);
        installIODevice(vmDumper);
    }
    void Core::shutdown() {

    }
} // end namespace iris
