/*
 * iris
 * Copyright (c) 2013-2017, Joshua Scoggins and Contributors
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
#include <functional>
#include <sstream>
#include <vector>


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
	Register::Register(Number v) : _value(v) { }
	Register::~Register() {
		_value = 0;
	}
	void Register::setValue(Number v) noexcept {
		_value = v;
	}
	void HardwiredRegister::setValue(Number) noexcept {
		// do nothing, ignore writes
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
#define X(title, style) \
			case Opcode :: title : \
				tmp = Core::title () ; \
				break;
#define FirstX(title, style) X(title, style)
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
	DefExec(Sub) { setDestination(op, getSource(op).integer - getSource2(op).integer); }
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
	DefExec(Eq) { setDestination(op, getSource(op).getTruth() == getSource2(op).getTruth()); }
	DefExec(EqImmediate) { setDestination(op, getSource(op).getTruth() == getSource2(op).getTruth()); }
	DefExec(Neq) { setDestination(op, getSource(op).getTruth() != getSource2(op).getTruth()); }
	DefExec(NeqImmediate) { setDestination(op, getSource(op).getTruth() != getSource2(op).getTruth()); }
	DefExec(LessThan) { setDestination(op, getSource(op).getTruth() < getSource2(op).getTruth()); }
	DefExec(LessThanImmediate) { setDestination(op, getSource(op).getTruth() < getSource2(op).getTruth()); }
	DefExec(LessThanOrEqualTo) { setDestination(op, getSource(op).getTruth() <= getSource2(op).getTruth()); }
	DefExec(LessThanOrEqualToImmediate) { setDestination(op, getSource(op).getTruth() <= getSource2(op).getTruth()); }
	DefExec(GreaterThan) { setDestination(op, getSource(op).getTruth() > getSource2(op).getTruth()); }
	DefExec(GreaterThanImmediate) { setDestination(op, getSource(op).getTruth() > getSource2(op).getTruth()); }
	DefExec(GreaterThanOrEqualTo) { setDestination(op, getSource(op).getTruth() >= getSource2(op).getTruth()); }
	DefExec(GreaterThanOrEqualToImmediate) { setDestination(op, getSource(op).getTruth() >= getSource2(op).getTruth()); }
	DefExec(Move) { setDestination(op, getSource(op)); }
	DefExec(Swap) {
		if (op._args.dest != op._args.src) {
			auto v0 = getSource(op);
			setRegister(op._args.src, getRegisterValue(op._args.dest));
			setRegister(op._args.dest, v0);
		}
	}
	DefExec(Set) { setDestination(op, op._args.imm); }
	DefExec(Load) { setDestination(op, _data[getRegisterValue(op._args.src).address]); }
	DefExec(LoadImmediate) { setDestination(op, _data[op._args.imm]); }
	DefExec(LoadWithOffset) { setDestination(op, _data[getSource(op).address + getSource2(op).address]); }
	DefExec(Store) { _data[getRegisterValue(op._args.dest).address] = getRegisterValue(op._args.src); }
	DefExec(StoreImmediate) { _data[getRegisterValue(op._args.dest).address] = op._args.imm; }
	DefExec(StoreWithOffset) { 
		auto base = getRegisterValue(op._args.dest).address;
		auto offset = getSource2(op).address;
		_data[base + offset] = getSource(op);
	}
	DefExec(Push) {
		auto stackAddress = getRegisterValue(op._args.dest).address - 1;
		auto value = getSource(op).address;
		_stack[stackAddress] = value;
		setRegister(op._args.dest, stackAddress);
	}
	DefExec(PushImmediate) {
		auto stackAddress = getRegisterValue(op._args.dest).address - 1;
		auto value = op._args.imm;
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

#undef DefExec
	RawInstruction Core::extractInstruction() noexcept {
		// extract the current instruction and then go next
		auto result = _code[_pc];
		++_pc;
		return result;
	}
} // end namespace iris
