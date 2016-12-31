#include "iris20.h"
#include <functional>
#include <sstream>
#include <vector>

namespace iris20 {


	//full 64bit addr space
	Core::Core() noexcept : _controller(0, 0x7FFFFFFFFFFFFFFF) { }

	Core::~Core() { }

	void Core::installprogram(std::istream& stream) {
		auto encodeWord = [](char* buf) { return iris20::encodeWord(buf[0], buf[1], buf[2], buf[3], buf[4], buf[5], buf[6], buf[7]); };
		gpr.install(stream, encodeWord);
		//memory.install(stream, encodeWord);
	}

	void Core::dump(std::ostream& stream) {

		auto decodeWord = [](word value, char* buf) { stdiris::decodeInt64LE(value, (byte*)buf); };
		gpr.dump(stream, decodeWord);
		//memory.dump(stream, decodeWord);
		
	}
	void Core::run() {
		while(execute) {
			execute = cycle();
		}
	}
	bool Core::cycle() {
		advanceIp = true;
		dispatch();
		if (advanceIp) {
			++getInstructionPointer();
		}
		return execute;
	}
	void Core::dispatch() {
		current = _controller.read(getInstructionPointer());
		if (decodeMoleculeContainsOneInstruction(current)) {
			executeMolecule();
		} else {
			executeAtom(getFirstAtom(current));
			executeAtom(getSecondAtom(current));
		}
	}
	enum class ExecutionUnitTarget {
		ALU,
		CompareUnit,
		BranchUnit,
		MoveUnit,
	};
	// target, subcommand, immediate?
	using DispatchTableEntry = std::tuple<ExecutionUnitTarget, byte, bool>;
    constexpr DispatchTableEntry makeDispatchEntry(ExecutionUnitTarget target, byte value, bool immediate) {
        return std::make_tuple(target, value, immediate);
    }
    template<typename T>
    constexpr DispatchTableEntry makeDispatchEntry(ExecutionUnitTarget target, T value, bool immediate) {
        return makeDispatchEntry(target, static_cast<byte>(value), immediate);
    }
	constexpr inline byte makeJumpByte(bool ifthenelse, bool conditional, bool iffalse, bool link) noexcept {
		return stdiris::encodeFlag<byte, 0b00001000, 3>(
				stdiris::encodeFlag<byte, 0b00000100, 2>(
					stdiris::encodeFlag<byte, 0b00000010, 1>(
						stdiris::encodeFlag<byte, 0b00000001, 0>(0u,
							ifthenelse),
						conditional),
					iffalse),
				link);
	}
	constexpr inline byte makeMoleculeJumpByte(bool ifthenelse, bool conditional, bool iffalse, bool link, bool wide48) noexcept {
		return stdiris::encodeFlag<byte, 0b00010000, 4>(makeJumpByte(ifthenelse, conditional, iffalse, link), wide48);
	}
	constexpr inline DispatchTableEntry makeJumpConstant(bool ifthenelse, bool conditional, bool iffalse, bool immediate, bool link) noexcept {
        return makeDispatchEntry(ExecutionUnitTarget::BranchUnit, makeJumpByte(ifthenelse, conditional, iffalse, link), immediate);
	}
	constexpr inline DispatchTableEntry makeMoleculeJumpConstant(bool ifthenelse, bool conditional, bool iffalse, bool immediate, bool link, bool wide48) noexcept {
		return makeDispatchEntry(ExecutionUnitTarget::BranchUnit, makeMoleculeJumpByte(ifthenelse, conditional, iffalse, link, wide48), immediate);
	}
	constexpr inline std::tuple<bool, bool, bool, bool> decomposeJumpByte(byte input) noexcept {
		return std::make_tuple(stdiris::decodeFlag<byte, 0b00000001>(input), stdiris::decodeFlag<byte, 0b00000010>(input), stdiris::decodeFlag<byte, 0b00000100>(input), stdiris::decodeFlag<byte, 0b00001000>(input));
	}
	constexpr inline std::tuple<bool, bool, bool, bool, bool> decomposeMoleculeJumpByte(byte input) noexcept {
		return std::make_tuple(stdiris::decodeFlag<byte, 0b00000001>(input), stdiris::decodeFlag<byte, 0b00000010>(input), stdiris::decodeFlag<byte, 0b00000100>(input), stdiris::decodeFlag<byte, 0b00001000>(input), stdiris::decodeFlag<byte, 0b00010000>(input));
	}
    void Core::executeMolecule() {
        // decode the operation first!
        static std::map<Operation, DispatchTableEntry> table = {
            { Operation::Set32, makeDispatchEntry(ExecutionUnitTarget::MoveUnit, Operation::Set32, true) },
            { Operation::Set48, makeDispatchEntry(ExecutionUnitTarget::MoveUnit, Operation::Set48, true) },
			{ Operation:: BranchUnconditionalImmediate32 ,        makeMoleculeJumpConstant( false, false, false, true, false, false) } ,
			{ Operation:: BranchUnconditionalImmediate32Link ,    makeMoleculeJumpConstant( false, false, false, true, true, false) } ,
			{ Operation:: BranchConditionalTrueImmediate32 ,      makeMoleculeJumpConstant( false, true, false, true, false, false) } ,
			{ Operation:: BranchConditionalTrueImmediate32Link ,  makeMoleculeJumpConstant( false, true, false, true, true, false) } ,
			{ Operation:: BranchConditionalFalseImmediate32 ,     makeMoleculeJumpConstant( false, true, true, true, false, false) } ,
			{ Operation:: BranchConditionalFalseImmediate32Link , makeMoleculeJumpConstant( false, true, true, true, true, false) } ,
			{ Operation:: BranchUnconditionalImmediate48 ,        makeMoleculeJumpConstant( false, false, false, true, false, true) } ,
			{ Operation:: BranchUnconditionalImmediate48Link ,    makeMoleculeJumpConstant( false, false, false, true, true, true) } ,
			{ Operation:: BranchConditionalTrueImmediate48 ,      makeMoleculeJumpConstant( false, true, false, true, false, true) } ,
			{ Operation:: BranchConditionalTrueImmediate48Link ,  makeMoleculeJumpConstant( false, true, false, true, true, true) } ,
			{ Operation:: BranchConditionalFalseImmediate48 ,     makeMoleculeJumpConstant( false, true, true, true, false, true) } ,
			{ Operation:: BranchConditionalFalseImmediate48Link , makeMoleculeJumpConstant( false, true, true, true, true, true) } ,
        };
		auto result = table.find(decodeMoleculeOperation(current));
		if (result == table.end()) {
			throw stdiris::Problem("Illegal molecule instruction!");
		}
        ExecutionUnitTarget unit;
        byte dispatch;
        bool immediate;
        std::tie(unit, dispatch, immediate) = result->second;
        auto moveOperation = [this, op = static_cast<Operation>(dispatch), immediate]() {
            auto isSet = [](Operation op) { return op == Operation::Set32 || op == Operation::Set48; };
            auto getImmediateWord = [this](Operation op) {
                switch(op) {
                    case Operation::Set32:
                        return decodeImmediate32(current);
                    case Operation::Set48:
                        return decodeImmediate48(current);
                    default:
                        throw stdiris::Problem("Illegal operation to get a word from!");
                }
            };
            if (immediate) {
                if (isSet(op)) {
                    operandSet(decodeMoleculeDestination(current), getImmediateWord(op));
                } else {
                    throw stdiris::Problem("unimplemented move operation specified!");
                }
            } else {
                throw stdiris::Problem("no immediate operations currently defined!");
            }
        };
		auto jumpOperation = [this, dispatch, immediate]() {
			if (!immediate) {
				throw stdiris::Problem("register based jump instructions don't exist in wide mode");
			}
			auto ifthenelse = false, conditional = false, iffalse = false, link = false, wide48 = false;
			std::tie(ifthenelse, conditional, iffalse, link, wide48) = decomposeMoleculeJumpByte(dispatch);
			auto newAddr = static_cast<word>(0);
			auto cond = true;
			advanceIp = false;
			auto ip = getInstructionPointer();
			auto dest = operandGet(decodeMoleculeDestination(current));
			auto immediateSelector = wide48 ? decodeImmediate48 : decodeImmediate32;
			if (conditional) {
				cond = (iffalse ? (dest == 0) : (dest != 0));
				if (ifthenelse) {
					//newAddr = operandGet(cond ? src0Ind : src1Ind);
					throw stdiris::Problem("ifthenelse not supported in wide mode!");
				} else {
					newAddr = cond ? (ip + immediateSelector(current)) : ip + 1;
				}
			} else {
				newAddr = ip + immediateSelector(current);
			}
			getInstructionPointer() = newAddr;
			if (link && cond) {
				getLinkRegister() = ip + 1;
			}
		};
        switch(unit) {
            case ExecutionUnitTarget::MoveUnit:
                moveOperation();
                break;
			case ExecutionUnitTarget::BranchUnit:
				jumpOperation();
				break;
            default:
                throw stdiris::Problem("Provided unit does not have molecule sized instructions!");
        }
    }
	DispatchTableEntry aluEntry(ALU::Operation op, bool immediate) noexcept {
		return makeDispatchEntry(ExecutionUnitTarget::ALU, op, immediate);
	}
	DispatchTableEntry compareEntry(CompareUnit::Operation op, bool immediate) noexcept {
		return makeDispatchEntry(ExecutionUnitTarget::CompareUnit, op, immediate);
	}
	DispatchTableEntry moveEntry(Operation op, bool immediate) noexcept {
		return makeDispatchEntry(ExecutionUnitTarget::MoveUnit, op, immediate);
	}

	void Core::executeAtom(InstructionAtom atom) {
		static std::map<Operation, DispatchTableEntry> table = {
				{ Operation::Add, aluEntry(ALU::Operation::Add, false) },
				{ Operation::Sub, aluEntry(ALU::Operation::Subtract, false ) },
				{ Operation::Mul, aluEntry(ALU::Operation::Multiply, false ) } ,
				{ Operation::Div, aluEntry(ALU::Operation::Divide, false ) },
				{ Operation::Rem, aluEntry(ALU::Operation::Remainder, false ) },
				{ Operation::ShiftLeft, aluEntry(ALU::Operation::ShiftLeft, false ) },
				{ Operation::ShiftRight, aluEntry(ALU::Operation::ShiftRight, false ) },
				{ Operation::BinaryNot, aluEntry(ALU::Operation::UnaryNot, false) },
				{ Operation::BinaryAnd, aluEntry(ALU::Operation::BinaryAnd, false ) },
				{ Operation::BinaryOr, aluEntry(ALU::Operation::BinaryOr, false ) },
				{ Operation::BinaryXor, aluEntry(ALU::Operation::BinaryXor, false ) },
				{ Operation::BinaryNand, aluEntry(ALU::Operation::BinaryNand, false ) },
				{ Operation::BinaryAndImmediate, aluEntry(ALU::Operation::BinaryAnd, true ) },
				{ Operation::BinaryOrImmediate, aluEntry(ALU::Operation::BinaryOr, true ) },
				{ Operation::BinaryXorImmediate, aluEntry(ALU::Operation::BinaryXor, true ) },
				{ Operation::BinaryNandImmediate, aluEntry(ALU::Operation::BinaryNand, true ) },
				{ Operation::AddImmediate, aluEntry(ALU::Operation::Add, true  ) },
				{ Operation::SubImmediate, aluEntry(ALU::Operation::Subtract, true  ) },
				{ Operation::MulImmediate, aluEntry(ALU::Operation::Multiply, true  ) } ,
				{ Operation::DivImmediate, aluEntry(ALU::Operation::Divide, true  ) },
				{ Operation::RemImmediate, aluEntry(ALU::Operation::Remainder, true  ) },
				{ Operation::ShiftLeftImmediate, aluEntry(ALU::Operation::ShiftLeft, true ) },
				{ Operation::ShiftRightImmediate, aluEntry(ALU::Operation::ShiftRight, true ) },
				{ Operation::LessThan, compareEntry(CompareUnit::Operation::LessThan, false) },
				{ Operation::LessThanImmediate, compareEntry(CompareUnit::Operation::LessThan, true) },
				{ Operation::LessThanOrEqualTo, compareEntry(CompareUnit::Operation::LessThanOrEqualTo, false) },
				{ Operation::LessThanOrEqualToImmediate, compareEntry(CompareUnit::Operation::LessThanOrEqualTo, true) },
				{ Operation::GreaterThan, compareEntry(CompareUnit::Operation::GreaterThan, false) },
				{ Operation::GreaterThanImmediate, compareEntry(CompareUnit::Operation::GreaterThan, true) },
				{ Operation::GreaterThanOrEqualTo, compareEntry(CompareUnit::Operation::GreaterThanOrEqualTo, false) },
				{ Operation::GreaterThanOrEqualToImmediate, compareEntry(CompareUnit::Operation::GreaterThanOrEqualTo, true) },
				{ Operation::Eq, compareEntry(CompareUnit::Operation::Eq, false) },
				{ Operation::EqImmediate, compareEntry(CompareUnit::Operation::Eq, true) },
				{ Operation::Neq, compareEntry(CompareUnit::Operation::Neq, false) },
				{ Operation::NeqImmediate, compareEntry(CompareUnit::Operation::Neq, true) },
				{ Operation:: BranchUnconditionalImmediate ,        makeJumpConstant( false, false, false, true, false) } ,
				{ Operation:: BranchUnconditionalImmediateLink ,    makeJumpConstant( false, false, false, true, true) } ,
				{ Operation:: BranchUnconditionalRegister ,         makeJumpConstant( false, false, false, false, false) } ,
				{ Operation:: BranchUnconditionalRegisterLink ,     makeJumpConstant( false, false, false, false, true) } ,
				{ Operation:: BranchConditionalTrueImmediate ,      makeJumpConstant( false, true, false, true, false) } ,
				{ Operation:: BranchConditionalTrueImmediateLink ,  makeJumpConstant( false, true, false, true, true) } ,
				{ Operation:: BranchConditionalTrueRegister ,       makeJumpConstant( false, true, false, false, false) } ,
				{ Operation:: BranchConditionalTrueRegisterLink ,   makeJumpConstant( false, true, false, false, true) } ,
				{ Operation:: BranchConditionalFalseImmediate ,     makeJumpConstant( false, true, true, true, false) } ,
				{ Operation:: BranchConditionalFalseImmediateLink , makeJumpConstant( false, true, true, true, true) } ,
				{ Operation:: BranchConditionalFalseRegister ,      makeJumpConstant( false, true, true, false, false) } ,
				{ Operation:: BranchConditionalFalseRegisterLink ,  makeJumpConstant( false, true, true, false, true) } ,
				{ Operation:: BranchIfThenElseNormalPredTrue ,      makeJumpConstant( true, true, false, false, false) } ,
				{ Operation:: BranchIfThenElseNormalPredFalse ,     makeJumpConstant( true, true, true, false, false) } ,
				{ Operation:: BranchIfThenElseLinkPredTrue ,        makeJumpConstant( true, true, false, false, true) } ,
				{ Operation:: BranchIfThenElseLinkPredFalse ,       makeJumpConstant( true, true, true, false, true) } ,
				{ Operation::Move, moveEntry(Operation::Move, false) },
				{ Operation::Swap, moveEntry(Operation::Swap, false) },
				{ Operation::Set16, moveEntry(Operation::Set16, true) },
		};
		auto result = table.find(getOperation(atom));
		if (result == table.end()) {
			throw stdiris::Problem("Illegal single atom instruction!");
		}
		auto tuple = result->second;
		ExecutionUnitTarget target;
		byte subAction;
		bool immediate;
		std::tie(target, subAction, immediate) = tuple;
		auto moveOperation = [this, op = static_cast<Operation>(subAction), immediate, atom]() {
			auto dest = getDestinationRawValue(atom);
			auto src = getSource0RawValue(atom);
			if (op == Operation::Move) {
				operandSet(dest, operandGet(src));
			} else if (op == Operation::Set16) {
				operandSet(dest, getImmediate(atom));
			} else if (op == Operation::Swap) {
				auto tmp = operandGet(dest);
				operandSet(dest, operandGet(src));
				operandSet(src, tmp);
			} else {
				throw stdiris::Problem("Registered but unimplemented move unit operation!");
			}
		};
		auto jumpOperation = [this, subAction, immediate, atom]() {
			auto ifthenelse = false, conditional = false, iffalse = false, link = false;
			auto result = decomposeJumpByte(subAction);
			std::tie(ifthenelse, conditional, iffalse, link) = result;
			auto newAddr = static_cast<word>(0);
			auto cond = true;
			advanceIp = false;
			auto ip = getInstructionPointer();
			auto dest = operandGet(getDestinationRawValue(atom));
			auto src0Ind = getSource0RawValue(atom);
			auto src1Ind = getSource1RawValue(atom);
			if (conditional) {
				cond = (iffalse ? (dest == 0) : (dest != 0));
				if (ifthenelse) {
					newAddr = operandGet(cond ? src0Ind : src1Ind);
                    cond = true; // make sure that if we have a link, that it is always done!
				} else {
					newAddr = cond ? (immediate ? (ip + getImmediate(atom)) : operandGet(src0Ind)) : ip + 1;
				}
			} else {
				newAddr = immediate ? (ip + getImmediate(atom)) : dest;
			}
			getInstructionPointer() = newAddr;
			if (link && cond) {
				getLinkRegister() = ip + 1;
			}
		};
		switch (target) {
			case ExecutionUnitTarget::ALU:
				performOperation(_alu, static_cast<ALU::Operation>(subAction), immediate, atom);
				break;
			case ExecutionUnitTarget::CompareUnit:
				performOperation(_compare, static_cast<CompareUnit::Operation>(subAction), immediate, atom);
				break;
			case ExecutionUnitTarget::BranchUnit:
				jumpOperation();
				break;
			case ExecutionUnitTarget::MoveUnit:
				moveOperation();
				break;
			default:
				throw stdiris::Problem("Registered execution unit target is not yet implemented!");
		}
	}

	void Core::link(std::istream& input) {
		constexpr static auto bufSize = sizeof(word) * 2;
		char buf[bufSize] = { 0 };
		for(auto lineNumber = static_cast<int>(0); input.good(); ++lineNumber) {
			input.read(buf, bufSize);
            auto gcount = input.gcount();
			if (gcount < static_cast<decltype(gcount)>(bufSize) && gcount > 0) {
				throw stdiris::Problem("unaligned object file found!");
			} else if (gcount == 0) {
				if (input.eof()) {
					break;
				} else {
					throw stdiris::Problem("Something bad happened while reading input file!");
				}
			}
			// first 8 bytes are an address, second 8 are a value
			auto address = iris20::encodeWord(buf[0], buf[1], buf[2], buf[3], buf[4], buf[5], buf[6], buf[7]);
			auto value = iris20::encodeWord(buf[8], buf[9], buf[10], buf[11], buf[12], buf[13], buf[14], buf[15]);
			if (debugEnabled()) {
				std::cerr << "addr: 0x " << std::hex << address << ": value: 0x" << std::hex << value << std::endl;
			}
			_controller.write(address, value);
		}
	}

	word readFromStandardIn(word address) {
		auto value = static_cast<byte>(0);
		std::cin >> std::noskipws >> value;
		return static_cast<word>(value);
	}
	void writeToStandardOut(word address, word value) {
		// read register 0 and register 1
		std::cout.put(static_cast<char>(value));
	}
	//Core::Core() noexcept : _devices(ArchitectureConstants::IOAddressBase, ArchitectureConstants::IOAddressSize) { }
	void Core::installDevice(MemoryController::SharedIONodePtr device) {
		_controller.install(device);
	}
	void Core::initialize() {
		_controller.initialize();
		auto mem = std::make_shared<MemorySpace>();
		mem->initialize();
		mem->zero();
		_controller.install(mem);
		// install memory handlers
		auto rNothing = stdiris::readNothing<typename GenericIODevice::DataType>;
		auto wNothing = stdiris::writeNothing<typename GenericIODevice::DataType>;
		installIODevice(ArchitectureConstants::IOTerminate, 1, rNothing,
					[this](word addr, word value) {
					execute = false;
					advanceIp = false;
					});
		installDevice(std::make_shared<stdiris::StandardInputOutputDevice<word>>(ArchitectureConstants::IOAddressBase + 1));
		installIODevice(ArchitectureConstants::IOGetMemorySize, 1, [](word addr) { return static_cast<word>(ArchitectureConstants::AddressMax) + 1; }, wNothing);
	}

    void Core::operandSet(byte target, word value) {
        SectionType type;
        byte index;
        std::tie(type, index) = getOperand(target);
        auto& data = gpr[index];
        switch(type) {
            case SectionType::Stack:
				--data;
            case SectionType::Memory:
				_controller.write(data, value);
                break;
            case SectionType::Register:
                data = value;
                break;
            default:
                throw stdiris::Problem("Undefined section type specified!");
        }
    }
	

    word Core::operandGet(byte target) {
        SectionType type;
        byte index;
        std::tie(type, index) = getOperand(target);
        auto& data = gpr[index];
        auto popData = [this](word& caddr) {
			auto outcome = _controller.read(caddr);
            ++caddr;
            return outcome;
        };
        switch(type) {
            case SectionType::Register:
                return data;
            case SectionType::Stack:
                return popData(data);
            case SectionType::Memory:
				return _controller.read(data);
            default:
                throw stdiris::Problem("Undefined section type specified!");
        }
    }

	void Core::installIODevice(word start, word length, typename GenericIODevice::ReadFunction read, typename GenericIODevice::WriteFunction write, typename GenericIODevice::InitializeFunction init, typename GenericIODevice::ShutdownFunction shutdown) {
		installDevice(std::make_shared<GenericIODevice>(ArchitectureConstants::IOAddressBase + start, length, read, write, init, shutdown));
	}

	Core* newCore() noexcept {
		return new iris20::Core();
	}

	word& Core::getRegister(byte index) noexcept {
		return gpr[std::get<byte>(getOperand(index))];
	}
}
