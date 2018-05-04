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
#include <cctype>


namespace iris {
    constexpr Opcode getOpcode(RawInstruction i) noexcept {
        return decodeBits<decltype(i), Opcode, 0x000000FF, 0>(i);
    }
    constexpr RegisterIndex getDestinationIndex(RawInstruction i) noexcept {
        return decodeBits<decltype(i), RegisterIndex, 0x1F << 8, 8>(i);
    }
    constexpr RegisterIndex getSourceIndex(RawInstruction i) noexcept {
        return decodeBits<decltype(i), RegisterIndex, 0x1F << 14, 14>(i);
    }
    constexpr RegisterIndex getSource2Index(RawInstruction i) noexcept {
        return decodeBits<decltype(i), RegisterIndex, 0x1F << 20, 20>(i);
    }
    constexpr RegisterIndex getSource3Index(RawInstruction i) noexcept {
        return decodeBits<decltype(i), RegisterIndex, 0x1F << 26, 26>(i);
    }
    constexpr Address getImmediate16(RawInstruction i) noexcept {
        return decodeBits<decltype(i), Address, 0xFFFF'0000, 16>(i);
    }
    constexpr byte getImmediate6(RawInstruction i) noexcept {
        if constexpr (std::is_same_v<byte, RegisterIndex>) {
            return getSource3Index(i);
        } else {
            return decodeBits<decltype(i), byte, 0x1F << 26, 26>(i);
        }
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
        _memory = std::make_unique<Number[]>(addressSize);
        _core = std::make_unique<Number[]>(addressSize);
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
    void Core::decodeArguments(RawInstruction i, Core::FourRegister& a) noexcept {
        a.dest = getDestinationIndex(i);
        a.src = getSourceIndex(i);
        a.src2 = getSource2Index(i);
        a.src3 = getSource3Index(i);
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
        a.src2 = getImmediate6(i);
    }
    void Core::decodeArguments(RawInstruction i, Core::ThreeRegisterWithImmediate& a) noexcept {
        a.dest = getDestinationIndex(i);
        a.src = getSourceIndex(i);
        a.src2 = getSource2Index(i);
        a.src3 = getImmediate6(i);
    }
    Core::DecodedInstruction Core::decodeInstruction(RawInstruction i) {
        Core::DecodedInstruction tmp;
        switch (getOpcode(i)) {
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
        auto value = _memory[addr];
        setDestination(op, value);
    }
    DefExec(LoadImmediate) { setDestination(op, _memory[op._args.imm]); }
    DefExec(Store) { _memory[getRegisterValue(op._args.dest).address] = getRegisterValue(op._args.src); }
    DefExec(StoreImmediate) { _memory[getRegisterValue(op._args.dest).address] = op._args.imm; }
    DefExec(Push) {
        Address stackAddress = getRegisterValue(op._args.dest).address - 1;
        Address value = getSource(op).address;
        _memory[stackAddress] = value;
        setRegister(op._args.dest, stackAddress);
    }
    DefExec(PushImmediate) {
        Address stackAddress = getRegisterValue(op._args.dest).address - 1;
        Address value = op._args.imm;
        _memory[stackAddress] = value;
        setRegister(op._args.dest, stackAddress);
    }
    DefExec(Pop) {
        setDestination(op, _memory[getRegisterValue(op._args.src).address]);
        setRegister(op._args.src, getRegisterValue(op._args.src).address + 1);
    }
    DefExec(LoadCore) {
        auto inst = _core[getSource(op).address];
        setDestination(op, inst);
    }

    DefExec(StoreCore) {
        auto lower = getSource(op);
        auto addr = getRegisterValue(op._args.dest).address;
        _core[addr] = lower;
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
    void Core::onIODeviceFound(Address addr, IODeviceOp fn) {
        for (auto& a : _io) {
            if (a.respondsTo(addr)) {
                fn(a);
            }
        }
    }
    DefExec(LoadIO) {
        auto addr = getSource(op).address;
        onIODeviceFound(addr, [this, &op, addr](auto& a) { setDestination(op, a.read(addr));});
    }
    DefExec(StoreIO) {
        auto addr = getRegisterValue(op._args.dest).address;
        onIODeviceFound(addr, [this, &op, addr](auto& a) { a.write(addr, getSource(op).address); });
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
        for (auto front = _memory[lbp].get<signed char>(); front == separator; ++lbp, front = _memory[lbp].get<signed char>()) { }
        auto start = lbp; // save the start of the token
        auto count = 0;
        while (true) {
            ++lbp;
            ++count;
            auto curr = _memory[lbp].get<signed char>();
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
        _memory[dp] = count;
        // go to the next cell
        ++dp;
        do {
            // start copying the token contents over
          _memory[dp] = _memory[lbp];
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
        auto ioStorage = getRegister(op._args.dest).get<Address>();
        onIODeviceFound(ioStorage, [this, ioStorage, &op](auto& a) {
                    auto start = getSource(op).address;
                    auto end = getSource2(op).address;
                    for (auto loc = 0; loc < end; ++loc) {
                        a.write(ioStorage, _memory[start + loc].get<Address>());
                    }
                });
    }
    DefExec(NumberRoutine) {
        // src2 is the base address
        // src is the resultant number
        // dest is the success code
        auto flag = false;
        auto baseAddress = getSource2(op).address;
        auto p1 = _memory[baseAddress].get<Integer>();
        auto count = _memory[p1].get<Address>();
        ++p1;
        auto sgn = _memory[p1].get<char>() == '-';
        if (sgn) {
            ++p1;
        }
        auto result = 0;
        auto terminatedEarly = false;
        auto _base = _registers[registerNumericBase].get<Integer>();
        while (count != 0) {
            auto num = Integer(_memory[p1].get<signed char>()) - 0x30;
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
    DefExec(ReadRangeFromIOAddress) {
        // destination is the place to write the length followed by
        // the string itself
        // src is the io address to read from
        // src2 is the number of elements
        auto count = getSource2(op).get<Address>();
        auto ioAddr = getSource(op).address;
        auto dest = getRegister(op._args.dest).get<Address>();
        onIODeviceFound(ioAddr, [this, count, ioAddr, dest](auto& dev) {
                    // keep reading from address until we get the term value or count is exhausted
                    auto pos = 0;
                    auto loc = dest;
                    auto terminator = _registers[registerTerminator].get<Address>();
                    for (; pos < count; ++pos, ++loc) {
                        auto value = dev.read(ioAddr);
                        _memory[loc] = value;
                        if (value == terminator) {
                            break;
                        }
                    }
                });
    }
#undef DefExec
    void Core::installIODevice(Core::IODevice dev) {
        _io.emplace_back(dev);
    }
    RawInstruction Core::extractInstruction() noexcept {
        // extract the current instruction and then go next
        auto lower = static_cast<RawInstruction>(_memory[_pc].address);
        auto upper = static_cast<RawInstruction>(_memory[_pc + 1].address) << 16;
        _pc += 2;
        return lower | upper;
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
        auto putRegister = [putAddress](const Register& reg) { putAddress(reg.getValue().address); };
        auto walkThroughSection = [](auto fn) {
            for (int i = 0; i < Core::maxAddress; ++i) {
                fn(i);
            }
        };
        for (int i = 0; i < Core::registerCount; ++i) {
            putRegister(_registers[i]);
        }
        walkThroughSection([this, putAddress](int i) { putAddress(_memory[i].get<Address>()); });
        walkThroughSection([this, putAddress](int i) { putAddress(_core[i].get<Address>()); });
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
        for (int i = 0; i < Core::registerCount; ++i) {
            _registers[i].setValue(Number(getAddress()));
        }
        auto walkThroughSection = [](auto fn) {
            for (int i = 0; i < Core::maxAddress; ++i) {
                fn(i);
            }
        };
        walkThroughSection([this, getAddress](int i) { _memory[i].address = getAddress(); });
        walkThroughSection([this, getAddress](int i) { _core[i].address = getAddress(); });
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
        _registers[registerZero].setValue(0);
        _registers[registerZero].disableWrites();
        _registers[registerErrorCode].setValue(0);
        _registers[registerTerminator].setValue(0);
        _registers[registerNumericBase].setValue(16);
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
                        outputFile << std::hex << _memory[i].address << std::endl;
                    }
                }
                outputFile.close();
            } else if (index == 1) {
                // load core from disk
                std::ifstream inputFile(path);
                if (inputFile.is_open()) {
                    for (int i = 0; i < 0x10000; ++i) {
                        inputFile >> std::hex >> _memory[i].address;
                    }
                } else {
                    for (int i = 0; i < 0x10000; ++i) {
                        _memory[i].address = 0;
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
