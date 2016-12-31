#include "cisc0.h"
#include <functional>
#include <sstream>
#include "Problem.h"
#include <utility>
#include <map>

namespace cisc0 {
	/*
	 * Iris18 is a variable length encoding 16 bit architecture.
	 * It has a 24 bit memory space across 256 16-bit sections. The variable length
	 * encoding comes from different register choices. The reserved registers are
	 * used to compress the encoding.
	 */
	Core* newCore() noexcept {
		return new Core();
	}
	constexpr RegisterValue encodeRegisterValue(byte a, byte b, byte c, byte d) noexcept {
		return stdiris::encodeUint32LE(a, b, c, d);
	}
	constexpr Word encodeWord(byte a, byte b) noexcept {
		return stdiris::encodeUint16LE(a, b);
	}
	void decodeWord(Word value, byte* storage) noexcept {
		return stdiris::decodeUint32LE(value, storage);
	}
	void decodeWord(RegisterValue value, byte* storage) noexcept {
		return stdiris::decodeInt32LE(value, storage);
	}

	Word decodeUpperHalf(RegisterValue value) noexcept {
		return stdiris::decodeBits<RegisterValue, Word, upper16Mask, 16>(value);
	}
	Word decodeLowerHalf(RegisterValue value) noexcept {
		return stdiris::decodeBits<RegisterValue, Word, lower16Mask, 16>(value);
	}

	constexpr RegisterValue encodeUpperHalf(RegisterValue value, Word upperHalf) noexcept {
		return stdiris::encodeBits<RegisterValue, Word, upper16Mask, 16>(value, upperHalf);
	}
	constexpr RegisterValue encodeLowerHalf(RegisterValue value, Word lowerHalf) noexcept {
		return stdiris::encodeBits<RegisterValue, Word, lower16Mask, 0>(value, lowerHalf);
	}

	constexpr RegisterValue encodeRegisterValue(Word upper, Word lower) noexcept {
		return encodeUpperHalf(encodeLowerHalf(0, lower), upper);
	}

	RegisterValue Core::retrieveImmediate(byte bitmask) noexcept {
		switch(bitmask) {
#define X(value) case value : return retrieveImmediate<value>();
#include "def/cisc0/bitmask4bit.def"
#undef X
			default:
				throw stdiris::Problem("Illegal bitmask defined!");
		}
	}

	Core::Core() noexcept { }
	Core::~Core() noexcept { }

	void Core::initialize() {
		// setup the default system handlers
		for (auto i = 0; i < ArchitectureConstants::MaxSystemCalls; ++i) {
			installSystemHandler(i, Core::defaultSystemHandler);
		}
		installSystemHandler(Core::DefaultHandlers::Terminate, Core::terminate);
		installSystemHandler(Core::DefaultHandlers::GetC, Core::getc);
		installSystemHandler(Core::DefaultHandlers::PutC, Core::putc);
	}

	void Core::defaultSystemHandler(Core* core, DecodedInstruction&& inst) {
		throw stdiris::Problem("Unimplemented system call!");
	}


	void Core::shutdown() { }

	void Core::installprogram(std::istream& stream) {
		gpr.install(stream, [](char* buf) { return stdiris::encodeUint32LE((byte*)buf); });
		memory.install(stream, [](char* buf) { return stdiris::encodeUint16LE((byte*)buf); });
	}

	void Core::dump(std::ostream& stream) {
		gpr.dump(stream, [](RegisterValue value, char* buf) { stdiris::decodeUint32LE(value, (byte*)buf); });
		memory.dump(stream, [](Word value, char* buf) { stdiris::decodeUint16LE(value, (byte*)buf); });
	}

	bool Core::cycle() {
		if (debugEnabled()) {
			std::cout << "Current Instruction Location: " << std::hex << getInstructionPointer() << std::endl;
			std::cout << "\tCurrent word value: " << std::hex << getCurrentCodeWord() << std::endl;
		}
		DecodedInstruction di(getCurrentCodeWord());
		dispatch(std::move(di));
		if (advanceIp) {
			incrementInstructionPointer();
		} else {
			// just re-enable it
			advanceIp = true;
		}
		return execute;
	}
    inline void mask24(RegisterValue& ref) noexcept {
        ref &= bitmask24;
    }
	void Core::incrementAddress(RegisterValue& ptr) noexcept {
		++ptr;
		mask24(ptr);
	}
	void Core::decrementAddress(RegisterValue& ptr) noexcept {
		--ptr;
		mask24(ptr);
	}
	void Core::incrementInstructionPointer() noexcept {
		incrementAddress(getInstructionPointer());
	}
	void Core::incrementStackPointer() noexcept {
		incrementStackPointer(getStackPointer());
	}
	void Core::incrementStackPointer(RegisterValue& ptr) noexcept {
		incrementAddress(ptr);
	}

	void Core::decrementStackPointer() noexcept {
		decrementStackPointer(getStackPointer());
	}
	void Core::decrementStackPointer(RegisterValue& ptr) noexcept {
		decrementAddress(ptr);
	}

	void Core::dispatch(DecodedInstruction&& current) {
		auto tControl = current.getControl();
		auto throwIfNotFound = [](auto result, auto& table, const std::string& msg) {
			if (result == table.end()) {
				throw stdiris::Problem(msg);
			}
		};
		if (tControl == Operation::Shift) {
			auto &destination = registerValue(current.getShiftRegister0());
			auto source = (current.getShiftFlagImmediate() ? static_cast<RegisterValue>(current.getShiftImmediate()) : registerValue(current.getShiftRegister1()));
			destination = _shifter.performOperation( current.getShiftFlagLeft() ? ALU::Operation::ShiftLeft : ALU::Operation::ShiftRight, destination, source);
		} else if (tControl == Operation::Swap) {
			if (current.getSwapDestination() != current.getSwapSource()) {
				gpr.swap(current.getSwapDestination(), current.getSwapSource());
			}
		} else if (tControl == Operation::Arithmetic) {
			static std::map<ArithmeticOps, ALU::Operation> translationTable = {
				{ ArithmeticOps::Add, ALU::Operation::Add },
				{ ArithmeticOps::Sub, ALU::Operation::Subtract },
				{ ArithmeticOps::Mul, ALU::Operation::Multiply },
				{ ArithmeticOps::Div, ALU::Operation::Divide},
				{ ArithmeticOps::Rem, ALU::Operation::Remainder},
			};
			auto result = translationTable.find(current.getArithmeticFlagType());
			throwIfNotFound(result, translationTable, "Illegal arithmetic operation!");
			auto op = result->second;
			auto src = current.getArithmeticImmediate() ? current.getArithmeticImmediate() : registerValue(current.getArithmeticSource());
			auto& dest = registerValue(current.getArithmeticDestination());
			dest = _alu.performOperation(op, dest, src);
		} else if (tControl == Operation::Logical) {
			ALU::Operation op;
			RegisterValue source1 = 0;
			byte source0 = 0u;
			if (current.getLogicalFlagImmediate()) {
				static std::map<ImmediateLogicalOps, ALU::Operation> immediateTranslation = {
					{ ImmediateLogicalOps::Or, ALU::Operation::BinaryOr },
					{ ImmediateLogicalOps::And, ALU::Operation::BinaryAnd },
					{ ImmediateLogicalOps::Xor, ALU::Operation::BinaryXor },
					{ ImmediateLogicalOps::Nand, ALU::Operation::BinaryNand },
				};
				auto result = immediateTranslation.find(current.getLogicalFlagImmediateType());
				throwIfNotFound(result, immediateTranslation, "Illegal immediate logical flag type");
				op = result->second;
				source1 = retrieveImmediate(current.getLogicalFlagImmediateMask());
				source0 = current.getLogicalImmediateDestination();
			} else {
				static std::map<LogicalOps, ALU::Operation> nonImmediateTranslation = {
					{ LogicalOps::Not, ALU::Operation::UnaryNot },
					{ LogicalOps::Or, ALU::Operation::BinaryOr },
					{ LogicalOps::And, ALU::Operation::BinaryAnd },
					{ LogicalOps::Xor, ALU::Operation::BinaryXor },
					{ LogicalOps::Nand, ALU::Operation::BinaryNand },
				};
				auto result = nonImmediateTranslation.find(current.getLogicalFlagType());
				throwIfNotFound(result, nonImmediateTranslation, "Illegal indirect logical operation!");
				op = result->second;
				source0 = current.getLogicalRegister0();
				source1 = registerValue(current.getLogicalRegister1());
			}
			auto& dest = registerValue(source0);
			dest = _logicalOps.performOperation(op, dest, source1);
		} else if (tControl == Operation::Move) {
			registerValue(current.getMoveRegister0()) = stdiris::decodeBits<RegisterValue, RegisterValue>( registerValue(current.getMoveRegister1()), mask(current.getMoveBitmask()), 0);
		} else if (tControl == Operation::Set) {
			registerValue(current.getSetDestination()) = retrieveImmediate(current.getSetBitmask());
		} else if (tControl == Operation::Memory) {
			auto indirect = current.getMemoryFlagIndirect();
			auto readNext = current.getMemoryFlagReadNextWord();
			auto rawMask = current.getMemoryFlagBitmask();
			auto useLower = readLower(rawMask);
			auto useUpper = readUpper(rawMask);
			auto fullMask = mask(rawMask);
			auto rawType = current.getMemoryFlagType();
			auto offset = current.getMemoryOffset();
			auto lmask = lowerMask(rawMask);
			auto umask = upperMask(rawMask);
			auto upper = 0u;
			auto lower = 0u;
			DecodedInstruction next(tryReadNext(readNext));
			if (rawType == MemoryOperation::Load) {
				auto addr = readNext ? registerValue(next.getMemoryAddress()) : getAddressRegister();
				auto& value = readNext ? registerValue(next.getMemoryValue()) : getValueRegister();
				if (!useLower && !useUpper) {
					value = 0;
				} else {
					auto address = addr + offset;
					if (indirect) {
						address = encodeRegisterValue(loadWord(address + 1), loadWord(address)) & bitmask24;
					}
					lower = useLower ? encodeLowerHalf(0, loadWord(address)) : 0u;
					upper = useUpper ? encodeUpperHalf(0, loadWord(address + 1)) : 0u;
					value = stdiris::encodeBits<RegisterValue, RegisterValue>(0u, lower | upper, fullMask, 0);
				}
			} else if (rawType == MemoryOperation::Store) {
				static constexpr auto maskCheck = 0x0000FFFF;
				auto addr = readNext ? registerValue(next.getMemoryAddress()) : getAddressRegister();
				auto value = readNext ? registerValue(next.getMemoryValue()) : getValueRegister();
				auto address = addr + offset;
				if (indirect) {
					address = encodeRegisterValue(loadWord(address + 1), loadWord(address)) & bitmask24;
				}
				if (useLower) {
					if (lmask == maskCheck) {
						storeWord(address, value);
					} else {
						storeWord(address, (lmask & value) | (loadWord(address) & ~lmask));
					}
				}
				if (useUpper) {
					if (umask == maskCheck) {
						storeWord(address + 1, value);
					} else {
						storeWord(address + 1, (umask & value) | (loadWord(address + 1) & ~umask));
					}
				}
			} else if (rawType == MemoryOperation::Push) {
				if (indirect) {
					throw stdiris::Problem("Indirect bit not supported in push operations!");
				} else {
					// update the target stack to something different
					auto pushToStack = registerValue(offset);
					auto &stackPointer = readNext ? registerValue(next.getMemoryAddress()) : getStackPointer();
					// read backwards because the stack grows upward towards zero
					if (useUpper) {
						pushWord(umask & decodeUpperHalf(pushToStack), stackPointer);
					}
					if (useLower) {
						pushWord(lmask & decodeLowerHalf(pushToStack), stackPointer);
					}
				}
			} else if (rawType == MemoryOperation::Pop) {
				if (indirect) {
					throw stdiris::Problem("Indirect bit not supported in pop operations!");
				} else {
					auto &stackPointer = readNext ? registerValue(next.getMemoryAddress()) : getStackPointer();
					if (useLower) {
						lower = lmask & popWord(stackPointer);
					}
					if (useUpper) {
						upper = umask & popWord(stackPointer);
					}
					registerValue(offset) = encodeRegisterValue(upper, lower);
					// can't think of a case where we should
					// restore the instruction pointer and then
					// immediate advance so just don't do it
					advanceIp = offset != ArchitectureConstants::InstructionPointer;
				}
			} else {
				throw stdiris::Problem("Illegal memory operation!");
			}
		} else if (tControl == Operation::Branch) {
			auto isIf = current.getBranchFlagIsIfForm();
			auto isCall = current.getBranchFlagIsCallForm();
			auto isImm = current.getBranchFlagIsImmediate();
			auto isCond = current.getBranchFlagIsConditional();
			advanceIp = true;
			auto choice = getConditionRegister() != 0;
			if (isIf) {
				advanceIp = false;
				if (isCall) {
					// push the instruction pointer onto the stack
					pushDword((getInstructionPointer() + 1) & bitmask24);
				}
				if (choice) {
					getInstructionPointer() = registerValue(current.getBranchIfOnTrue());
				} else {
					getInstructionPointer() = registerValue(current.getBranchIfOnFalse());
				}
			} else if (isCall) {
				// call instruction
				advanceIp = false;
				// determine next
				auto length = isImm ? 2 : 1;
				pushDword((getInstructionPointer() + length) & bitmask24);

				auto address = 0u;
				if (isImm) {
					// make a 24 bit number
					auto upper16 = static_cast<RegisterValue>(tryReadNext(isImm)) << 8;
					auto lower8 = static_cast<RegisterValue>(current.getUpper());
					address = upper16 | lower8;
				} else {
					address = registerValue(current.getBranchIndirectDestination());
				}
				getInstructionPointer() = bitmask24 & address;
			} else {
				// jump instruction
				if (isImm) {
					incrementInstructionPointer();
					if ((isCond && choice) || !isCond) {
						advanceIp = false;
						getInstructionPointer() = bitmask24 & (current.getUpper() | static_cast<RegisterValue>(getCurrentCodeWord()) << 8);
					} 
				} else {
					if ((isCond && choice) || !isCond) {
						advanceIp = false;
						getInstructionPointer() = bitmask24 & registerValue(current.getBranchIndirectDestination());
					}
				}
			}
		} else if (tControl == Operation::Compare) {
			static std::map<CompareStyle, CompareUnit::Operation> translationTable = {
				{ CompareStyle::Equals, CompareUnit::Operation::Eq },
				{ CompareStyle::NotEquals, CompareUnit::Operation::Neq },
				{ CompareStyle::LessThan, CompareUnit::Operation::LessThan },
				{ CompareStyle::LessThanOrEqualTo, CompareUnit::Operation::LessThanOrEqualTo },
				{ CompareStyle::GreaterThan, CompareUnit::Operation::GreaterThan },
				{ CompareStyle::GreaterThanOrEqualTo, CompareUnit::Operation::GreaterThanOrEqualTo },
			};
			using CombineOp = stdiris::BooleanCombineUnit::Operation;
			static std::map<CompareCombine, CombineOp> combineTranslation = {
				{ CompareCombine::None, CombineOp::None },
				{ CompareCombine::Xor, CombineOp::Xor },
				{ CompareCombine::Or, CombineOp::Or },
				{ CompareCombine::And, CombineOp::And },
			};
			DecodedInstruction next(tryReadNext<true>());
			auto first = registerValue(next.getCompareRegister0());
			auto second = current.getCompareImmediateFlag() ? next.getUpper() : registerValue(next.getCompareRegister1());
			auto compareResult = translationTable.find(current.getCompareType());
			throwIfNotFound(compareResult, translationTable, "Illegal compare type!");
			auto combineResult = combineTranslation.find(current.getCompareCombineFlag());
			throwIfNotFound(combineResult, combineTranslation, "Illegal compare combine operation!");
			getConditionRegister() = static_cast<RegisterValue>(_bCombine.performOperation(combineResult->second, 
						_compare.performOperation(compareResult->second, first, second),
						getConditionRegister() != 0));
		} else if (tControl == Operation::SystemCall) {
			if (getAddressRegister() >= ArchitectureConstants::MaxSystemCalls) {
				throw stdiris::Problem("ERROR: system call index out of range!");
			} else {
				systemHandlers[current.getSystemAction()](this, std::move(current));
			}
        } else if (tControl == Operation::Complex) {
            complexOperation(std::move(current));
		} else {
			std::stringstream str;
			str << "Illegal instruction " << std::hex << static_cast<int>(current.getControl()) << std::endl;
			str << "Location: " << std::hex << getInstructionPointer() << std::endl;
			execute = false;
			throw stdiris::Problem(str.str());
		}
	}

	void Core::complexOperation(DecodedInstruction&& inst) {
		auto type = inst.getComplexSubClass();
		if (type == ComplexSubTypes::Encoding) {
			encodingOperation(std::move(inst));
		} else {
			throw stdiris::Problem("Undefined complex subtype!");
		}
	}
	RegisterValue notOperation(ALU & unit, RegisterValue v0) {
		return unit.performOperation(ALU::Operation::UnaryNot, v0, 0);
	}
	RegisterValue shiftLeftOp(ALU& unit, RegisterValue v0, RegisterValue v1) {
		return unit.performOperation(ALU::Operation::ShiftLeft, v0, v1);
	}
    void Core::encodingOperation(DecodedInstruction&& inst) {
		auto type = inst.getComplexClassEncoding_Type();
		if (type == EncodingOperation::Decode) {
			// connect the result of the logical operations alu to the
			// shifter alu then store the result in the value register
			getValueRegister() = stdiris::decodeBits<RegisterValue, RegisterValue>(getAddressRegister(), getMaskRegister(), getShiftRegister());
		} else if (type == EncodingOperation::Encode) {
			getAddressRegister() = stdiris::encodeBits<RegisterValue, RegisterValue>(getAddressRegister(), getValueRegister(), getMaskRegister(), getShiftRegister());

		} else if (type == EncodingOperation::BitSet) {
			getConditionRegister() = _compare.performOperation(CompareUnit::Operation::Eq, 
					_logicalOps.performOperation(ALU::Operation::BinaryAnd,
						_shifter.performOperation(ALU::Operation::ShiftRight, getAddressRegister(),
							getFieldRegister()), 0x1), 1);

		} else if (type == EncodingOperation::BitUnset) {
			getConditionRegister() = _compare.performOperation(CompareUnit::Operation::Neq,
					_logicalOps.performOperation(ALU::Operation::BinaryAnd,
						_shifter.performOperation(ALU::Operation::ShiftRight, 
							getAddressRegister(),
							getFieldRegister()), 0x1), 1);
		} else {
			throw stdiris::Problem("Illegal complex encoding operation defined!");
		}
    }

	void Core::terminate(Core* core, DecodedInstruction&& inst) {
		core->execute = false;
		core->advanceIp = false;
	}

	void Core::putc(Core* core, DecodedInstruction&& current) {
		std::cout.put(static_cast<char>(core->registerValue(current.getSystemArg0())));
	}
	void Core::getc(Core* core, DecodedInstruction&& current) {
		byte value = 0;
		std::cin >> std::noskipws >> value;
		core->registerValue(current.getSystemArg0()) = static_cast<Word>(value);
	}


	void Core::link(std::istream& input) {
		// we have some more data to read through
		// two address system, 1 RegisterValue -> address, 1 Word -> value
		static constexpr int bufSize = 8;
		char buf[bufSize] = { 0 };
		for(int lineNumber = 0; input.good(); ++lineNumber) {
			input.read(buf, bufSize);
			if (input.gcount() == 0) {
				break;
			} else if (input.gcount() != bufSize) {
				throw stdiris::Problem("unaligned object file found");
			} else {
				// use the first byte to determine what sort of installation
				// should occur
				switch (buf[0]) {
					case 0: // memory value
						storeWord(encodeRegisterValue(buf[2], buf[3], buf[4], buf[5]), encodeWord(buf[6], buf[7]));
						break;
					case 1: // register value
						gpr[static_cast<byte>(buf[1])] = encodeRegisterValue(buf[2], buf[3], buf[4], buf[5]);
						break;
					default:
						throw stdiris::Problem("undefined link class!");
				}
			}
		}
	}
	RegisterValue& Core::registerValue(byte index) {
		return gpr[index];
	}
	Word Core::getCurrentCodeWord() noexcept {
		return memory[getInstructionPointer()];
	}
	void Core::storeWord(RegisterValue address, Word value) {
			memory[address] = value;
	}
	Word Core::loadWord(RegisterValue address) {
		return memory[address];
	}
	RegisterValue Core::loadRegisterValue(RegisterValue address) {
		return stdiris::encodeBits<RegisterValue, Word, bitmask32, 16>(static_cast<RegisterValue>(loadWord(address)), loadWord(address + 1));
	}
	void Core::storeRegisterValue(RegisterValue address, RegisterValue value) {
		storeWord(address, stdiris::decodeBits<RegisterValue, Word, lower16Mask, 0>(value));
		storeWord(address + 1, stdiris::decodeBits<RegisterValue, Word, upper16Mask, 16>(value));
	}

	void Core::installSystemHandler(byte index, Core::SystemFunction func) {
		if (index >= ArchitectureConstants::MaxSystemCalls) {
			throw stdiris::Problem("Can't install to out of range system handler index!");
		} else {
			systemHandlers[index] = func;
		}
	}
	void Core::pushWord(Word value) {
		pushWord(value, getStackPointer());
	}
	void Core::pushWord(Word value, RegisterValue& ptr) {
		decrementStackPointer(ptr);
		storeWord(ptr, value);
	}
	void Core::pushDword(DWord value, RegisterValue& ptr) {
		pushWord(decodeUpperHalf(value), ptr);
		pushWord(decodeLowerHalf(value), ptr);
	}
	void Core::pushDword(DWord value) {
		return pushDword(value, getStackPointer());
	}

	Word Core::popWord() {
		return popWord(getStackPointer());
	}
	Word Core::popWord(RegisterValue& ptr) {
		auto result = loadWord(ptr);
		incrementStackPointer(ptr);
		return result;
	}

	InstructionEncoder::Encoding InstructionEncoder::encodeArithmetic() {
		auto first = encodeControl(0, type);
		first = encodeArithmeticFlagImmediate(first, immediate);
		first = encodeArithmeticFlagType(first, static_cast<ArithmeticOps>(subType));
		first = encodeArithmeticDestination(first, arg0);
		first = immediate ? encodeArithmeticImmediate(first, arg1) : encodeArithmeticSource(first, arg1);
		return std::make_tuple(1, first, 0, 0);
	}

	InstructionEncoder::Encoding InstructionEncoder::encodeMove() {
		auto first = encodeControl(0, type);
		first = encodeMoveBitmask(first, bitmask);
		first = encodeMoveRegister0(first, arg0);
		first = encodeMoveRegister1(first, arg1);
		return std::make_tuple(1, first, 0, 0);
	}

	InstructionEncoder::Encoding InstructionEncoder::encodeSwap() {
		return std::make_tuple(1, encodeSwapSource( encodeSwapDestination( encodeControl(0, type), arg0), arg1), 0, 0);
	}

	InstructionEncoder::Encoding InstructionEncoder::encodeShift() {
		auto first = encodeControl(0, type);
		first = encodeShiftFlagImmediate(first, immediate);
		first = encodeShiftFlagLeft(first, shiftLeft);
		first = encodeShiftRegister0(first, arg0);
		first = immediate ? encodeShiftImmediate(first, arg1) : encodeShiftRegister1(first, arg1);
		return std::make_tuple(1, first, 0, 0);
	}

	InstructionEncoder::Encoding InstructionEncoder::encodeSystemCall() {
        auto first = encodeControl(0, type);
        first = encodeSystemAction(first, arg0);
        first = encodeSystemArg0(first, arg1);
        return std::make_tuple(1, first, 0, 0);
	}

	InstructionEncoder::Encoding InstructionEncoder::encodeCompare() {
		auto first = encodeControl(0, type);
		first = encodeCompareType(first, static_cast<CompareStyle>(subType));
		first = encodeCompareCombineFlag(first, combineType);
		first = encodeCompareImmediateFlag(first, immediate);
		auto second = encodeCompareRegister0(0, arg0);
		second = immediate ? encodeCompareImmediate(second, arg1) : encodeCompareRegister1(second, arg1);
		return std::make_tuple(2, first, second, 0);
	}

	InstructionEncoder::Encoding InstructionEncoder::encodeSet() {
		int count = instructionSizeFromImmediateMask(bitmask);
		auto first = encodeControl(0, type);
		first = encodeSetBitmask(first, bitmask);
		first = encodeSetDestination(first, arg0);
		// use the mask during encoding since we know how many Words the
		// instruction is made up of
		auto maskedValue = getMask(bitmask) & fullImmediate;
		auto second = static_cast<Word>(maskedValue);
		auto third = static_cast<Word>(maskedValue >> 16);
		return std::make_tuple(count, first, second, third);
	}

	InstructionEncoder::Encoding InstructionEncoder::encodeMemory() {
		auto first = encodeControl(0, type);
		first = encodeMemoryFlagType(first, static_cast<MemoryOperation>(subType));
		first = encodeMemoryFlagBitmask(first, bitmask);
		first = encodeMemoryFlagIndirect(first, indirect);
		first = cisc0::encodeMemoryFlagReadNextWord(first, readNextWord);
		// the register and offset occupy the same space
		first = encodeMemoryOffset(first, arg0);
		// be lazy and set up the second word even if it isn't used. Reduces
		// the amount of branching and special cases :)
		auto second = encodeMemoryAddress(0, arg1);
		second = encodeMemoryValue(0, arg2);
		return std::make_tuple(readNextWord ? 2 : 1, first, second, 0);
	}

	InstructionEncoder::Encoding InstructionEncoder::encodeLogical() {
		auto first = encodeControl(0, type);
		first = encodeLogicalFlagImmediate(first, immediate);
		if (immediate) {
			first = encodeLogicalFlagImmediateType(first, static_cast<ImmediateLogicalOps>(subType));
			first = encodeLogicalFlagImmediateMask(first, bitmask);
			first = encodeLogicalImmediateDestination(first, arg0);
			auto maskedImmediate = getMask(bitmask) & fullImmediate;
			auto second = static_cast<Word>(maskedImmediate);
			auto third = static_cast<Word>(maskedImmediate >> 16);
			return std::make_tuple(instructionSizeFromImmediateMask(bitmask), first, second, third);
		} else {
			first = encodeLogicalFlagType(first, static_cast<LogicalOps>(subType));
			first = encodeLogicalRegister0(first, arg0);
			first = encodeLogicalRegister1(first, arg1);
			return std::make_tuple(1, first, 0, 0);
		}
	}

	InstructionEncoder::Encoding InstructionEncoder::encodeBranch() {
		auto first = encodeControl(0, type);
		first = encodeBranchFlagIsConditional(first, isConditional);
		first = encodeBranchFlagIsIfForm(first, isIf);
		first = encodeBranchFlagIsImmediate(first, immediate);
		first = encodeBranchFlagIsCallForm(first, isCall);
		if (isIf) {
			first = encodeBranchIfOnTrue(first, arg0);
			first = encodeBranchIfOnFalse(first, arg1);
			return std::make_tuple(1, first, 0, 0);
		} else {
			if (immediate) {
				// encode the 24-bit number
				first = encodeUpper(first, static_cast<byte>(fullImmediate));
				auto second = static_cast<Word>(fullImmediate >> 8);
				return std::make_tuple(2, first, second, 0);
			} else {
				first = encodeBranchIndirectDestination(first, arg0);
				return std::make_tuple(1, first, 0, 0);
			}
		}
	}
    InstructionEncoder::Encoding InstructionEncoder::encodeComplex() {
        auto sType = static_cast<ComplexSubTypes>(subType);
        auto first = encodeControl(0, type);
        first = encodeComplexSubClass(first, sType);
        if (sType == ComplexSubTypes::Encoding) {
            // right now it is a single word
            first = encodeComplexClassEncoding_Type(first, static_cast<EncodingOperation>(bitmask));
            return std::make_tuple(1, first, 0, 0);
        } else {
            throw stdiris::Problem("Attempted to encode an unsupported value as a complex type!");
        }
    }

	InstructionEncoder::Encoding InstructionEncoder::encode() {
		// always encode the type
		switch (type) {
#define DefEnum(a, b)
#define EndDefEnum(a, b, c)
#define EnumEntry(compareType) case Operation:: compareType : return encode ## compareType () ; 
#include "def/cisc0/ops.def"
#undef DefEnum
#undef EndDefEnum
#undef EnumEntry
			default:
				throw stdiris::Problem("Illegal type to encode!");
		}
	}

	int instructionSizeFromImmediateMask(byte bitmask) {
		switch(bitmask) {
#define X(bits) case bits : return instructionSizeFromImmediateMask< bits > () ; 
#include "def/cisc0/bitmask4bit.def"
#undef X
			default:
		throw stdiris::Problem("Illegal bitmask provided!");
		}
	}
	RegisterValue getMask(byte bitmask) {
		switch(bitmask) {
#define X(bits) case bits : return SetBitmaskToWordMask< bits > :: mask ; 
#include "def/cisc0/bitmask4bit.def"
#undef X
			default:
		throw stdiris::Problem("Illegal bitmask provided!");
		}
	}
	int InstructionEncoder::numWords() {
		return std::get<0>(encode());
	}
	void InstructionEncoder::clear() {
		currentLine = 0;
		address = 0;
		type = Operation::Memory;
		immediate = false;
		shiftLeft = false;
		isIf = false;
		isCall = false;
		isConditional = false;
		bitmask = 0b0000;
		arg0 = 0;
		arg1 = 0;
		arg2 = 0;
		isLabel = false;
		labelValue.clear();
		subType = 0;
		combineType = CompareCombine::Xor;
		fullImmediate = 0;
		indirect = false;
		readNextWord = false;
	}
}