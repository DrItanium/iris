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
        ++_pc;
        _dest = getRegister(getLowerIndex(load(_pc)));
    }
    void Core::decodeArguments(const Core::TwoRegister&) noexcept {
        ++_pc;
        auto i = load(_pc);
        _dest = getRegister(getLowerIndex(i));
        _src = getRegister(getUpperIndex(i));
    }
    void Core::decodeArguments(const Core::ThreeRegister&) noexcept {
        ++_pc;
        auto i = load(_pc);
        _dest = getRegister(getLowerIndex(i));
        _src = getRegister(getUpperIndex(i));
        ++_pc;
        i = load(_pc);
        _src2 = getRegister(getLowerIndex(i));
    }
    void Core::decodeArguments(const Core::FourRegister&) noexcept {
        ++_pc;
        auto i = load(_pc);
        _dest = getRegister(getLowerIndex(i));
        _src = getRegister(getUpperIndex(i));
        ++_pc;
        i = load(_pc);
        _src2 = getRegister(getLowerIndex(i));
        _src3 = getRegister(getUpperIndex(i));
    }
    void Core::decodeArguments(const Core::OneRegisterWithImmediate&) noexcept {
        ++_pc;
        auto i = load(_pc);
        _dest = getRegister(getLowerIndex(i));
        ++_pc;
        auto lower = load(_pc);
        ++_pc;
        auto upper = load(_pc);
        _imm = makeImmediate16(lower, upper);
    }
    void Core::decodeArguments(const Core::TwoRegisterWithImmediate&) noexcept {
        ++_pc;
        auto i = load(_pc);
        _dest = getRegister(getLowerIndex(i));
        _src = getRegister(getUpperIndex(i));
        ++_pc;
        auto lower = load(_pc);
        ++_pc;
        auto upper = load(_pc);
        _imm = makeImmediate16(lower, upper);
    }
	void Core::decodeArguments(const Core::WideTwoRegister&) noexcept {
        ++_pc;
        auto i = load(_pc);
		auto lower = getLowerIndex(i);
		auto upper = getUpperIndex(i);
        _dest = getRegister(lower);
		_destNext = getRegister(lower + 1);
        _src = getRegister(upper);
        _srcNext = getRegister(upper + 1);
	}
    void Core::decodeArguments(const Core::WideThreeRegister&) noexcept {
        ++_pc;
        auto i = load(_pc);
		auto lower = getLowerIndex(i);
		auto upper = getUpperIndex(i);
        _dest = getRegister(lower);
		_destNext = getRegister(lower + 1);
        _src = getRegister(upper);
        _srcNext = getRegister(upper + 1);
        ++_pc;
        i = load(_pc);
		lower = getLowerIndex(i);
        _src2 = getRegister(lower);
		_src2Next = getRegister(lower + 1);
    }
	void Core::decodeArguments(const Core::OneRegisterWithByte&) noexcept {
		++_pc;
		_dest = getRegister(getLowerIndex(load(_pc)));
		++_pc;
		_half = load(_pc);
	}
	void Core::decodeArguments(const Core::ImmediateOnly&) noexcept {
		++_pc;
		auto lower = load(_pc);
		++_pc;
		auto upper = load(_pc);
		_addr = makeImmediate16(lower, upper);
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
	DefExec(Load) {
		_dest.setValue(loadNumber(_src.get<Address>()));
	}
	DefExec(Store) {
		storeNumber(_dest.get<Address>(), _src.get<Address>());
	}
	DefExec(Push) {
		pushNumber(_dest, _src.get<Address>());
	}
	DefExec(Pop) {
		_dest.setValue(popNumber(_src));
	}
	DefExec(BranchRegister) {
		_pc = _dest.get<Address>();
	}
	DefExec(BranchRegisterAndLink) {
		pushNumber(_src, _pc);
		_pc = _dest.get<Address>();
	}
	DefExec(BranchConditionalRegister) {
		if (_src.getTruth()) {
			_pc = _dest.get<Address>();
		}
	}
	DefExec(BranchConditionalRegisterLink) {
		if (_src.getTruth()) {
			pushNumber(_src2, _pc);
			_pc = _dest.get<Address>();
		}
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
        _src.setValue(addr + 2);
    }
    DefExec(StoreThenIncrement) {
        auto addr = _dest.get<Address>();
        auto val = _src.get<Address>();
        storeNumber(addr, val);
        _dest.setValue(addr + 2);
    }
    DefExec(WideAdd) {
        auto src = makeDoubleWideInteger(_src.get<Integer>(), _srcNext.get<Integer>());
        auto src2 = makeDoubleWideInteger(_src2.get<Integer>(), _src2Next.get<Integer>());
        auto result = src + src2;
		_dest.setValue(Integer(result));
		_destNext.setValue(Integer(result >> 16));
    }
    DefExec(WideSubtract) {
        // the design means that x255 will couple with zero
        auto src = makeDoubleWideInteger(_src.get<Integer>(), _srcNext.get<Integer>());
        auto src2 = makeDoubleWideInteger(_src2.get<Integer>(), _src2Next.get<Integer>());
        auto result = src - src2;
		_dest.setValue(Integer(result));
		_destNext.setValue(Integer(result >> 16));
    }
    DefExec(WidePush) {
        // L SP -> ( -- L H ) 
        pushNumber(_dest, _src.getValue());
        pushNumber(_dest, _srcNext.getValue());
    }
    DefExec(WidePop) {
        // SP X ( L H -- ) L -> X, H -> (X + 1 | Y)
		_destNext.setValue(popNumber(_src));
		_dest.setValue(popNumber(_src));
    }
    DefExec(WideNegate) {
        auto src = makeDoubleWideInteger(_src.get<Integer>(), _srcNext.get<Integer>());
        src = ~src;
		_dest.setValue(Integer(src));
		_destNext.setValue(Integer(src >> 16));
    }

    DefExec(UMSMOD) {
        // unsigned divide of a double by a single. Return mod and quotient
        auto src = makeDoubleWideAddress(_src.get<Address>(), _srcNext.get<Address>());
        auto src2 = DoubleWideAddress(_src2.get<Address>());
		_dest.setValue(Address(src2 == 0 ? 0 : src / src2)); // quotient
		_destNext.setValue(Address(src2 == 0 ? 0 : src % src2)); // remainder
    }
    DefExec(MSMOD) {
        // signed floored divide of a double by double. Return mod and quotient
        auto src = makeDoubleWideInteger(_src.get<Integer>(), _srcNext.get<Integer>());
        auto src2 = DoubleWideInteger(_src2.get<Integer>());
		_dest.setValue(Integer(src2 == 0 ? 0 : src / src2)); // quotient
		_destNext.setValue(Integer(src2 == 0 ? 0 : src % src2)); // remainder
    }
    DefExec(UMSTAR) {
        auto src = DoubleWideAddress(_src.get<Address>());
        auto src2 = DoubleWideAddress(_src2.get<Address>());
        auto result = src * src2;
		_dest.setValue(Address(result));
		_destNext.setValue(Address(result >> 16));
    }
    DefExec(MSTAR) {
        auto src = DoubleWideInteger(_src.get<Integer>());
        auto src2 = DoubleWideInteger(_src2.get<Integer>());
        auto result = src * src2;
		_dest.setValue(Integer(result));
		_destNext.setValue(Integer(result >> 16));
    }
    DefExec(WideStore) {
		auto dest = _dest.get<Address>();
        auto src = makeDoubleWideInteger(_src.get<Integer>(), _srcNext.get<Integer>());
        storeNumber(dest, Address(src));
        storeNumber(dest+1, Address(src >> 16));
    }
	DefExec(WideLoad) {
		auto src = _src.get<Address>();
		_dest.setValue(loadNumber(src));
		_destNext.setValue(loadNumber(src + 1));
	}
	DefExec(LoadByte_Upper) {
		// load a byte address and stash it into the upper half of the
		// destination register. Overwrites the contents
		_dest.setValue((Address(load(_src.get<Address>())) << 8) & 0xFF00);
	}
	DefExec(LoadByte_Lower) {
		// load a byte address and stash it into the lower half of the
		// destination register. Overwrites the contents
		_dest.setValue(Address(load(_src.get<Address>())) & 0x00FF);
	}
	DefExec(StoreByte_Lower) {
		// store the lower half of a register to a given address
		store(_dest.get<Address>(), _src.get<byte>());
	}
	DefExec(StoreByte_Upper) {
		// store the upper half of a register to the given address
		store(_dest.get<Address>(), byte(_src.get<Address>() >> 8));
	}
	DefExec(Nop) { }
	DefExec(SetByte) { _dest.setValue(_half); }
	DefExec(Branch) { _pc = _addr; }
	DefExec(EqualZero) { _dest.setValue(_src.get<Integer>() == 0); }
	DefExec(NotEqualZero) { _dest.setValue(_src.get<Integer>() != 0); }
	DefExec(LessThanZero) { _dest.setValue(_src.get<Integer>() < 0); }
	DefExec(GreaterThanZero) { _dest.setValue(_src.get<Integer>() > 0); }
	DefExec(LessThanOrEqualToZero) { _dest.setValue(_src.get<Integer>() <= 0); }
	DefExec(GreaterThanOrEqualToZero) { _dest.setValue(_src.get<Integer>() >= 0); }
	DefExec(AndImmediate) { _dest.setValue(_src.get<Integer>() & _imm); }
	DefExec(UnsignedAndImmediate) { _dest.setValue(_src.get<Address>() & _addr); }
    DefExec(MultiplyImmediate) { _dest.setValue(_src.get<Integer>() * _imm); }
    DefExec(DivideImmediate) { _dest.setValue(_imm == 0 ? 0 : _src.get<Integer>() / _imm); }
    DefExec(PushImmediate) { pushNumber(_dest, _addr); }
#undef DefExec
    void Core::installIODevice(Core::IODevice dev) {
        _io.emplace_back(dev);
    }
	void Core::cycle() {
		auto control = static_cast<Opcode>(load(_pc));
		switch (control) {
#define X(title, style) case Opcode :: title : \
			decodeArguments(Core::title () ); \
			++_pc; \
			perform(Core::title()); \
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
	}
    void Core::execute() {
        while(_keepExecuting) {
			cycle();
        }
    }

    void Core::dump(std::ostream& out) {
		for (int i = 0; i < (Core::maxAddress + 1); ++i) {
			out.put(load(i, true));
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
		for (int i = 0 ; i < (Core::maxAddress + 1); ++i) {
			store(i, getByte(), true);
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
    byte readFromStandardIn(Address index) {
        if (index == 0) {
			auto result = std::cin.get();
            return Address(byte(char(result)));
        } else {
            // doesn't make sense to do this
            return 0;
        }
    }
    void writeToStandardOut(Address index, byte value) {
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
		auto terminateVM = [this](auto index, auto value) {
			if (index == 0) {
				_keepExecuting = value != 0;
			} else {
				throw Problem("Unimplemented address!");
			}
		};
        IODevice sink(ioSpaceStart);
        IODevice console(sink.getEnd(), 3, readFromStandardIn, writeToStandardOut);
		IODevice vmTerminator(console.getEnd(),1, nullptr, terminateVM);
        installIODevice(sink);
        installIODevice(console);
		installIODevice(vmTerminator);
    }
    void Core::shutdown() { }
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
	Register Core::nullReg;
} // end namespace iris
