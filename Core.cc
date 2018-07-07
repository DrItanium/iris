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
#include <iomanip>
#include <functional>
#include <sstream>
#include <vector>
#include <fstream>
#include <cctype>


namespace iris {
    constexpr RegisterIndex getLowerIndex(byte value) noexcept {
        return decodeBits<decltype(value), RegisterIndex, 0x0F, 0>(value);
    }
    constexpr RegisterIndex getUpperIndex(byte value) noexcept {
        return decodeBits<decltype(value), RegisterIndex, 0xF0, 4>(value);
    }
    constexpr Address makeImmediate16(byte lower, byte upper) noexcept {
        return ((Address)lower) | (((Address)upper) << 8);
    }
    constexpr DoubleWideInteger makeDoubleWideInteger(Integer lower, Integer upper) noexcept {
        return ((static_cast<DoubleWideInteger>(upper) << 16) & 0xFFFF0000) | 
               ((static_cast<DoubleWideInteger>(lower) & 0x0000FFFF));
    }
    constexpr DoubleWideAddress makeDoubleWideAddress(Address lower, Address upper) noexcept {
        return ((static_cast<DoubleWideAddress>(upper) << 16) & 0xFFFF0000) | 
               ((static_cast<DoubleWideAddress>(lower) & 0x0000FFFF));
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
        _memory = std::make_unique<byte[]>(addressSize);
        _registers = std::make_unique<Register[]>(registerCount);
    }
    void Core::decodeArguments(const Core::NoArguments&) noexcept {
        // go to the next instruction
    }
    void Core::decodeArguments(const Core::OneRegister&) noexcept {
        auto i = load(_pc);
        _dest = getRegister(getLowerIndex(i));
    }
    void Core::decodeArguments(const Core::TwoRegister&) noexcept {
        auto i = load(_pc);
        _dest = getRegister(getLowerIndex(i));
        _src = getRegister(getUpperIndex(i));
    }
    void Core::decodeArguments(const Core::ThreeRegister&) noexcept {
        auto i = load(_pc);
        _dest = getRegister(getLowerIndex(i));
        _src = getRegister(getUpperIndex(i));
        ++_pc;
        i = load(_pc);
        _src2 = getRegister(getLowerIndex(i));
    }
    void Core::decodeArguments(const Core::FourRegister&) noexcept {
        auto i = load(_pc);
        _dest = getRegister(getLowerIndex(i));
        _src = getRegister(getUpperIndex(i));
        ++_pc;
        i = load(_pc);
        _src2 = getRegister(getLowerIndex(i));
        _src3 = getRegister(getUpperIndex(i));
    }
    void Core::decodeArguments(const Core::OneRegisterWithImmediate&) noexcept {
        auto i = load(_pc);
        _dest = getRegister(getLowerIndex(i));
        ++_pc;
        auto lower = load(_pc);
        ++_pc;
        auto upper = load(_pc);
        _imm = makeImmediate16(lower, upper);
    }
    void Core::decodeArguments(const Core::TwoRegisterWithImmediate&) noexcept {
        auto i = load(_pc);
        _dest = getRegister(getLowerIndex(i));
        _src = getRegister(getUpperIndex(i));
        ++_pc;
        auto lower = load(_pc);
        ++_pc;
        auto upper = load(_pc);
        _imm = makeImmediate16(lower, upper);
    }
    Core::DecodedInstruction Core::decodeInstruction() {
        auto control = static_cast<Opcode>(load(_pc));
        Core::DecodedInstruction tmp;
        ++_pc;
        switch (control) {
#define X(title, style) case Opcode :: title : \
            tmp = Core:: title () ; \
            decodeArguments(Core::title () ); \
            ++_pc; \
            break;
#define FirstX(title, style) X(title, style)
#include "Opcodes.def"
#undef X
#undef FirstX
            default:
                std::cout << "@ " << std::hex << (_pc - 1) << std::endl;
                std::cout << "bad opcode " << int(control) << std::endl;
                throw Problem("Illegal Opcode!");
        }
        // std::visit([this](auto&& value) { decodeArguments(value); ++_pc; }, tmp);
        return tmp;
    }

    void Core::dispatchInstruction(const Core::DecodedInstruction& di) {
        std::visit([this](auto&& v) { perform(v); }, di);
    }
    Register& Core::getRegister(RegisterIndex index) noexcept {
        return _registers[index];
    }
    void Core::push(Register& reg, byte value) noexcept {
        auto addr = reg.get<Address>() - 1;
        store(addr, value);
        reg.setValue(addr);
    }
	void Core::pushNumber(Register& index, Number value) noexcept {
        auto addr = index.get<Address>() - 2;
        store(addr + 1, byte(value.address >> 8));
        store(addr, byte(value.address));
        index.setValue(addr);
	}
    byte Core::pop(Register& index) noexcept {
        auto addr = index.get<Address>();
        auto result = load(addr);
        index.setValue(addr + 1);
        return result;
    }
	Number Core::popNumber(Register& index) noexcept {
        auto addr = index.get<Address>();
        auto lower = load(addr);
        auto upper = load(addr + 1);
        index.setValue(addr + 2);
		return makeImmediate16(lower, upper);
	}
#define DefExec(title) \
    void Core::perform ( const Core:: title & op ) 
	DefExec(Illegal) {
		throw Problem("Illegal Instruction Invoked!");
	}
    DefExec(Div) { 
        auto denom = _src2.get<Integer>();
        _dest.setValue(denom == 0 ? 0u : _src.get<Integer>() / denom);
	}
    DefExec(Rem) { 
        auto denom = _src2.get<Integer>();
        _dest.setValue(denom == 0 ? 0u : _src.get<Integer>() % denom);
	}
    DefExec(UnsignedDiv) { 
        auto denom = _src2.get<Address>();
        _dest.setValue(denom == 0 ? 0u : _src.get<Address>() / denom);
	}
    DefExec(UnsignedRem) { 
        auto denom = _src2.get<Address>();
        _dest.setValue(denom == 0 ? 0u : _src.get<Address>() % denom);
	}
#define DefBinaryOp(opcode, action, x) \
    DefExec(opcode) { \
        _dest.setValue(_src.get<x>() action _src2.get<x>()); \
    }
#define DefBinaryOpInteger(opc, action) DefBinaryOp(opc, action, Integer)
#define DefBinaryOpUnsigned(opc, action) DefBinaryOp(opc, action, Address)
    DefBinaryOpInteger(Add, +);
    DefBinaryOpInteger(Sub, -);
    DefBinaryOpInteger(Mul, *);
    DefBinaryOpInteger(ShiftLeft, <<);
    DefBinaryOpInteger(ShiftRight, >>);
    DefBinaryOpInteger(And, &);
    DefBinaryOpInteger(Or, |);
    DefBinaryOpInteger(Xor, ^);
    DefBinaryOpInteger(Eq, == );
    DefBinaryOpInteger(Neq, != );
    DefBinaryOpInteger(LessThan, < );
    DefBinaryOpInteger(LessThanOrEqualTo, <= );
    DefBinaryOpInteger(GreaterThan, >);
    DefBinaryOpInteger(GreaterThanOrEqualTo, >=);
    DefBinaryOpUnsigned(UnsignedEq, == );
    DefBinaryOpUnsigned(UnsignedNeq, != );
    DefBinaryOpUnsigned(UnsignedLessThan, < );
    DefBinaryOpUnsigned(UnsignedLessThanOrEqualTo, <= );
    DefBinaryOpUnsigned(UnsignedGreaterThan, >);
    DefBinaryOpUnsigned(UnsignedGreaterThanOrEqualTo, >=);
    DefBinaryOpUnsigned(UnsignedAnd, &);
    DefBinaryOpUnsigned(UnsignedOr, |);
    DefBinaryOpUnsigned(UnsignedXor, ^);
    DefBinaryOpUnsigned(UnsignedAdd, +);
    DefBinaryOpUnsigned(UnsignedSub, -);
    DefBinaryOpUnsigned(UnsignedMul, *);
    DefBinaryOpUnsigned(UnsignedShiftLeft, <<);
    DefBinaryOpUnsigned(UnsignedShiftRight, >>);
#undef DefBinaryOpInteger
#undef DefBinaryOpUnsigned

    DefExec(Negate) { _dest.setValue(~_src.get<Integer>()); }
    DefExec(UnsignedNegate) { _dest.setValue(~_src.get<Address>()); }
    DefExec(Min) {
        auto a = _src.get<Integer>();
        auto b = _src2.get<Integer>();
        _dest.setValue(a < b ? a : b );
    }
    DefExec(Max) {
        auto a = _src.get<Integer>();
        auto b = _src2.get<Integer>();
        _dest.setValue( a > b ? a : b );
    }
    DefExec(UnsignedMin) {
        auto a = _src.get<Address>();
        auto b = _src2.get<Address>();
        _dest.setValue( a < b ? a : b );
    }
    DefExec(UnsignedMax) {
        auto a = _src.get<Address>();
        auto b = _src2.get<Address>();
        _dest.setValue( a > b ? a : b );
    }
    DefExec(Set) { 
        _dest.setValue(_imm);
    }
    // DefExec(Load) { 
    //     op.setDestination(loadNumber(op.getSource<Address>()));
    // }
    // DefExec(Store) { 
    //     storeNumber(op.dest.get<Address>(), op.getSource<Address>());
	// }
    // DefExec(Push) {
	// 	push(op.dest, getSource(op));
    // }
    // DefExec(Pop) {
	// 	setDestination(op, pop(op.src));
    // }
    // DefExec(BranchRegister) {
	// 	_pc = getDestination(op).address;
    // }
    // DefExec(BranchRegisterAndLink) {
	// 	push(op.src, _pc);
    //     _pc = getDestination(op).address;
    // }
    // DefExec(BranchConditionalRegister) {
    //     if (getSource(op).getTruth()) {
	// 		_pc = getDestination(op).address;
    //     }
    // }
    // DefExec(BranchConditionalRegisterLink) {
    //     if (getSource(op).getTruth()) {
	// 		push(op.src2, _pc);
	// 		_pc = getDestination(op).address;
    //     }
    // }

    void Core::onIODeviceFound(Address addr, IODeviceOp fn) {
        for (auto& a : _io) {
            if (a.respondsTo(addr)) {
                fn(a);
            }
        }
    }
	void Core::storeNumber(Address addr, Number value, bool unmapIO) noexcept {
        store(addr, byte(value.address), unmapIO);
        store(addr + 1, byte(value.address >> 8), unmapIO);
	}
    void Core::store(Address addr, byte value, bool unmapIO) noexcept {
        if (!unmapIO && addr >= ioSpaceStart) {
			onIODeviceFound(addr, [addr, value](auto& a) { a.write(addr, value); });
        } else {
            _memory[addr] = value;
        }
    }
    byte Core::load(Address addr, bool unmapIO) noexcept {
        if (!unmapIO && addr >= ioSpaceStart) {
			byte outcome = 0;
        	onIODeviceFound(addr, [addr, &outcome](auto& a) { outcome = a.read(addr); });
			return outcome;
        } else {
            return _memory[addr];
        }
    }
	Number Core::loadNumber(Address addr, bool unmapIO) noexcept {
        auto lower = load(addr, unmapIO);
        auto upper = load(addr + 1, unmapIO);
        return makeImmediate16(lower, upper);
	}
    DefExec(Increment) { _dest.setValue(_src.get<Integer>() + 1); }
    DefExec(Decrement) { _dest.setValue(_src.get<Integer>() - 1); }
    DefExec(UnsignedIncrement) { _dest.setValue(_src.get<Address>() + 1); }
    DefExec(UnsignedDecrement) { _dest.setValue(_src.get<Address>() - 1); }
    DefExec(Call) { 
        push(_dest, _pc);
        _pc = _addr;
    }
    DefExec(ConditionalBranch) {
        if (_dest.get<bool>()) {
            _pc = _addr;
        }
    }
#define DefBinaryOpImmediate(opc, action, x, ifield) \
    DefExec(opc) { \
        _dest.setValue(_src.get<x>() action ifield); \
    }
#define DefBinaryOpImmediateInteger(opc, action) DefBinaryOpImmediate(opc, action, Integer, _imm)
    DefBinaryOpImmediateInteger(AddImmediate, +);
    DefBinaryOpImmediateInteger(SubImmediate, -);
    DefBinaryOpImmediateInteger(RightShiftImmediate, >>);
    DefBinaryOpImmediateInteger(LeftShiftImmediate, <<);
    DefBinaryOpImmediateInteger(LessThanImmediate, <);
    DefExec(Move) { _dest.setValue(_src.get<Address>()); }
    DefExec(Return) {
        // the destination is the return stack pointer to extract from
        _pc = popNumber(_dest).address;
    }
    DefExec(ConditionalReturn) {
        if (_src.get<bool>()) {
            _pc = popNumber(_dest).address;
        }
    }
    DefExec(LoadThenIncrement) {
        auto addr = _src.get<Address>();
        _dest.setValue(loadNumber(addr));
        _src.setValue(addr + 1);
    }
    DefExec(StoreThenIncrement) {
        auto addr = _dest.get<Address>();
        auto val = _src.get<Address>();
        storeNumber(addr, val);
        _dest.setValue(addr + 1);
    }
    // DefExec(WideAdd) {
    //     // the design means that x255 will couple with zero
    //     auto src = makeDoubleWideInteger(getSource(op).integer, getRegister(op.src + 1).getValue().integer);
    //     auto src2 = makeDoubleWideInteger(getSource2(op).integer, getRegister(op.src2 + 1).getValue().integer);
    //     auto result = src + src2;
    //     setDestination(op, Integer(result));
    //     setRegister(op.dest + 1, Integer(result >> 16));
    // }
    // DefExec(WideSubtract) {
    //     // the design means that x255 will couple with zero
    //     auto src = makeDoubleWideInteger(getSource(op).integer, getRegister(op.src + 1).getValue().integer);
    //     auto src2 = makeDoubleWideInteger(getSource2(op).integer, getRegister(op.src2 + 1).getValue().integer);
    //     auto result = src - src2;
    //     setDestination(op, Integer(result));
    //     setRegister(op.dest + 1, Integer(result >> 16));
    // }
    // DefExec(WidePush) {
    //     // L SP -> ( -- L H ) 
    //     auto lower = getSource(op);
    //     auto upper = getRegister(op.src + 1).getValue();
    //     push(op.dest, lower);
    //     push(op.dest, upper);
    // }
    // DefExec(WidePop) {
    //     // SP X ( L H -- ) L -> X, H -> (X + 1 | Y)
    //     auto upper = this->pop(op.src);
    //     auto lower = this->pop(op.src);
    //     setDestination(op, lower);
    //     setRegister(op.dest + 1, upper);
    // }
    // DefExec(WideNegate) {
    //     auto src = makeDoubleWideInteger(getSource(op).integer, getRegister(op.src + 1).getValue().integer);
    //     src = ~src;
    //     setDestination(op, Integer(src));
    //     setRegister(op.dest + 1, Integer(src >> 16));
    // }

    // DefExec(UMSMOD) {
    //     // unsigned divide of a double by a single. Return mod and quotient
    //     auto src = makeDoubleWideAddress(getSource(op).address, getRegister(op.src + 1).getValue().address);
    //     auto src2 = DoubleWideAddress(getSource2(op).address);
    //     DoubleWideAddress quotient = src2 == 0 ? 0 : src / src2;
    //     DoubleWideAddress remainder = src2 == 0 ? 0 : src % src2;
    //     setDestination(op, Address(quotient));
    //     setRegister(op.dest + 1, Address(remainder));
    // }
    // DefExec(MSMOD) {
    //     // signed floored divide of a double by double. Return mod and quotient
    //     auto src = makeDoubleWideInteger(getSource(op).integer, getRegister(op.src + 1).getValue().integer);
    //     auto src2 = DoubleWideInteger(getSource2(op).integer);
    //     DoubleWideInteger quotient = src2 == 0 ? 0 : src / src2;
    //     DoubleWideInteger remainder = src2 == 0 ? 0 : src % src2;
    //     setDestination(op, Integer(quotient));
    //     setRegister(op.dest + 1, Integer(remainder));
    // }
    // DefExec(UMSTAR) {
    //     auto src = DoubleWideAddress(getSource(op).address);
    //     auto src2 = DoubleWideAddress(getSource2(op).address);
    //     auto result = src * src2;
    //     setDestination(op, Address(result));
    //     setRegister(op.dest + 1, Address(result >> 16));
    // }
    // DefExec(MSTAR) {
    //     auto src = DoubleWideInteger(getSource(op).integer);
    //     auto src2 = DoubleWideInteger(getSource2(op).integer);
    //     auto result = src * src2;
    //     setDestination(op, Integer(result));
    //     setRegister(op.dest + 1, Integer(result >> 16));
    // }
    // 
    // DefExec(WideStore) {
    //     auto dest = getDestination(op).address;
    //     auto src = makeDoubleWideInteger(getSource(op).integer, getRegister(op.src + 1).getValue().integer);
    //     store(dest, Number(Address(src)));
    //     store(dest+1, Number(Address(src >> 16)));
    // }
    // DefExec(WideLoad) {
    //     auto src = getSource(op).address;
    //     setDestination(op, load(src));
    //     setRegister(op.dest + 1, load(src + 1));
    // }
#undef DefExec
    void Core::installIODevice(Core::IODevice dev) {
        _io.emplace_back(dev);
    }
    // RawInstruction Core::extractInstruction() noexcept {
    //     // extract the current instruction and then go next
    //     auto lower = static_cast<RawInstruction>(_memory[_pc].address);
    //     auto upper = static_cast<RawInstruction>(_memory[_pc + 1].address) << 16;
    //     _pc += 2;
    //     return lower | upper;
    // }

    void Core::cycle() {
        // load the current instruction and go to the next address
        auto op = decodeInstruction();
        dispatchInstruction(op);
    }
    void Core::execute() {
        while(_keepExecuting) {
            cycle();
        }
    }

    void Core::dump(std::ostream& out) {
		for (int i = 0; i < (Core::maxAddress + 1); ++i) {
			auto value = load(i, true);
			out.put(decodeBits<Address, char, 0x00FF, 0>(value));
			out.put(decodeBits<Address, char, 0xFF00, 8>(value));
		}
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
		for (int i = 0 ; i < (Core::maxAddress + 1); ++i) {
			store(i, getAddress(), true);
		}
    }
    byte Core::IODevice::read(Address addr) {
        if (_read) {
            return _read(addr - _begin);
        } else {
            return 0;
        }
    }
    void Core::IODevice::write(Address addr, byte value) {
        if (_write) {
            _write(addr - _begin, value);
        }
    }
    bool Core::IODevice::respondsTo(Address addr) const noexcept {
        return (addr >= _begin) && (addr < _end);
    }
    Address readFromStandardIn(Address index) {
        if (index == 0) {
			auto result = std::cin.get();
            return Address(byte(char(result)));
        } else {
            // doesn't make sense to do this
            return 0;
        }
    }
    void writeToStandardOut(Address index, Address value) {
        if (index == 0) {
            // print as char code
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
        } else if (index == 2) {
            // print numeric representation
            std::cout << value;
        } else {
            throw Problem("Unimplemented address!");
        }
    }
    void Core::init() {
        // disable writing to register 0
        _registers[registerZero].setValue(0);
        _registers[registerZero].disableWrites();
        // setup the basic IO device for console input output
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
                    for (int i = coreCacheStart; i < coreCacheEnd ; ++i) {
                        outputFile << std::hex << _memory[i] << std::endl;
                    }
                }
                outputFile.close();
            } else if (index == 1) {
                // load core from disk
                std::ifstream inputFile(path);
                if (inputFile.is_open()) {
                    for (int i = coreCacheStart; i < coreCacheEnd; ++i) {
                        if (inputFile.eof()) {
                            _memory[i] = 0;
                        } else {
                            inputFile >> std::hex >> _memory[i];
                        }
                    }
                } else {
                    for (int i = coreCacheStart; i < coreCacheEnd; ++i) {
                        _memory[i] = 0;
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
		auto terminateVM = [this](auto index, auto value) {
			if (index == 0) {
				_keepExecuting = value != 0;
			} else {
				throw Problem("Unimplemented address!");
			}
		};
        IODevice sink(ioSpaceStart);
        IODevice console(sink.getEnd(), 3, readFromStandardIn, writeToStandardOut);
        IODevice coreManipulator(console.getEnd(), 2, nullptr, selectCore);
        IODevice vmDumper(coreManipulator.getEnd(), 1, nullptr, dumpVM);
		IODevice vmTerminator(vmDumper.getEnd(),1, nullptr, terminateVM);
        IODevice registerInspector(vmTerminator.getEnd(),1, nullptr, [this](auto index, auto value) {
                        auto maskedIndex = byte(0b111111 & value);
                        std::cout << "r" << std::setfill('0') << std::setw(2) << std::dec << int(maskedIndex) << ": " << std::setfill('0') << std::setw(4) << std::hex << getRegister(maskedIndex).get<Address>();
                    });
		IODevice hexPrinter(registerInspector.getEnd(),1, nullptr, [](auto index, auto value) { std::cout << std::hex << value; });
		IODevice decPrinter(hexPrinter.getEnd(), 1, nullptr, [](auto index, auto value) { std::cout << std::dec << value; });
		IODevice octPrinter(decPrinter.getEnd(), 1, nullptr, [](auto index, auto value) { std::cout << std::oct << value; });
        installIODevice(sink);
        installIODevice(console);
        installIODevice(coreManipulator);
        installIODevice(vmDumper);
		installIODevice(vmTerminator);
        installIODevice(registerInspector);
		installIODevice(hexPrinter);
		installIODevice(decPrinter);
		installIODevice(octPrinter);
    }
    void Core::shutdown() {

    }
} // end namespace iris
