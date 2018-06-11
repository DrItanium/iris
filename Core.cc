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
    constexpr Address getImmediate12(RawInstruction i) noexcept {
        return decodeBits<decltype(i), Address, 0xFFF0'0000, 20>(i);
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
    void Core::decodeArguments(RawInstruction i, Core::OneRegisterWithImmediate& a) noexcept {
        a.imm = getImmediate16(i);
        a.dest = getDestinationIndex(i);
    }
    void Core::decodeArguments(RawInstruction i, Core::TwoRegisterWithImmediate& a) noexcept {
        a.addr = getImmediate12(i);
        a.dest = getDestinationIndex(i);
        a.src = getSourceIndex(i);
    }
    Core::DecodedInstruction Core::decodeInstruction(RawInstruction i) {
        Core::DecodedInstruction tmp;
        auto op = getOpcode(i);
        switch (op) {
#define X(title, style, z) \
            case Opcode :: title : \
                                   tmp = Core::title () ; \
            break;
#define FirstX(title, style, z) X(title, style, z)
#include "Opcodes.def"
#undef X
#undef FirstX
            default:
                std::cout << "@ " << std::hex << _pc << std::endl;
                std::cout << "bad instruction: " << std::hex << i << std::endl;
                std::cout << "bad opcode " << int(op) << std::endl;
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
	DefExec(Illegal) {
		throw Problem("Illegal Instruction Invoked!");
	}
    DefExec(Add) { setDestination(op, getSource(op).integer + getSource2(op).integer); }
    DefExec(Sub) { 
        auto a = getSource(op).integer;
        auto b = getSource2(op).integer;
		setDestination(op, a - b);
    }
    DefExec(Mul) { setDestination(op, getSource(op).integer * getSource2(op).integer); }
    DefExec(Div) { 
		auto denom = getSource2(op).integer;
		if (denom == 0) {
			setDestination(op, Number(0u));
		} else {
			setDestination(op, getSource(op).integer / denom);
		}
	}
    DefExec(Rem) { 
		auto denom = getSource2(op).integer;
		if (denom == 0) {
			setDestination(op, Number(0u));
		} else {
			setDestination(op, getSource(op).integer % denom);
		}
	}
    DefExec(ShiftLeft) { setDestination(op, getSource(op).integer << getSource2(op).integer); }
    DefExec(ShiftRight) { setDestination(op, getSource(op).integer >> getSource2(op).integer); }
    DefExec(And) { setDestination(op, getSource(op).integer & getSource2(op).integer); }
    DefExec(Or) { setDestination(op, getSource(op).integer | getSource2(op).integer); }
    DefExec(Not) { setDestination(op, ~getSource(op).integer); }
    DefExec(Xor) { setDestination(op, getSource(op).integer ^ getSource2(op).integer); }
    DefExec(Nand) { setDestination(op, binaryNand(getSource(op).integer, getSource2(op).integer)); }
    DefExec(Nor) { setDestination(op, binaryNor(getSource(op).integer, getSource2(op).integer)); }
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
    DefExec(Eq) { 
		auto first = getSource(op).integer;
		auto second = getSource2(op).integer;
		setDestination(op, first == second);
	}
    DefExec(Neq) { 
		auto first = getSource(op).integer;
		auto second = getSource2(op).integer;
		setDestination(op, first != second);
	}
    DefExec(LessThan) { 
		auto first = getSource(op).integer;
		auto second = getSource2(op).integer;
		setDestination(op, first < second);
	}
    DefExec(LessThanOrEqualTo) { 
		auto first = getSource(op).integer;
		auto second = getSource2(op).integer;
		setDestination(op, first <= second);
	}
    DefExec(GreaterThan) { 
		auto first = getSource(op).integer;
		auto second = getSource2(op).integer;
		setDestination(op, first > second);
	}
    DefExec(GreaterThanOrEqualTo) { setDestination(op, getSource(op).integer >= getSource2(op).integer); }
    DefExec(Set) { setDestination(op, op._args.imm); }
    DefExec(Load) { 
		setDestination(op, load(getSource(op).address));
    }
    DefExec(Store) { 
		store(getDestination(op).address, getSource(op));
	}
	void Core::push(RegisterIndex index, Number value) noexcept {
		Address stackAddress = getRegister(index).get<Address>() - 1;
		store(stackAddress, value);
		setRegister(index, stackAddress);
	}
	Number Core::pop(RegisterIndex index) noexcept {
		auto addr = getRegister(index).get<Address>();
		auto result = load(addr);
		setRegister(index, addr + 1);
		return result;
	}
    DefExec(Push) {
		push(op._args.dest, getSource(op));
    }
    DefExec(Pop) {
		setDestination(op, pop(op._args.src));
    }
    DefExec(BranchRegister) {
		_pc = getDestination(op).address;
    }
    DefExec(BranchRegisterAndLink) {
		push(op._args.src, _pc);
        _pc = getDestination(op).address;
    }
    DefExec(BranchConditionalRegister) {
        if (getSource(op).getTruth()) {
			_pc = getDestination(op).address;
        }
    }
    DefExec(BranchConditionalRegisterLink) {
        if (getSource(op).getTruth()) {
			push(op._args.src2, _pc);
			_pc = getDestination(op).address;
        }
    }

    void Core::onIODeviceFound(Address addr, IODeviceOp fn) {
        for (auto& a : _io) {
            if (a.respondsTo(addr)) {
                fn(a);
            }
        }
    }
	void Core::store(Address addr, Number value, bool unmapIO) noexcept {
		if (!unmapIO && addr >= ioSpaceStart) {
			onIODeviceFound(addr, [addr, value](auto& a) { a.write(addr, value.address); });
		} else {
			_memory[addr] = value.address;
		}
	}
	Address Core::load(Address addr, bool unmapIO) noexcept {
		if (!unmapIO && addr >= ioSpaceStart) {
			Address outcome = 0;
        	onIODeviceFound(addr, [addr, &outcome](auto& a) { outcome = a.read(addr); });
			return outcome;
		} else {
			return _memory[addr].address;
		}
	}
    DefExec(UnsignedEq) { setDestination(op, getSource(op).address == getSource2(op).address); }
    DefExec(UnsignedNeq) { setDestination(op, getSource(op).address != getSource2(op).address); }
    DefExec(UnsignedLessThan) { setDestination(op, getSource(op).address < getSource2(op).address); }
    DefExec(UnsignedLessThanOrEqualTo) { setDestination(op, getSource(op).address <= getSource2(op).address); }
    DefExec(UnsignedGreaterThan) { setDestination(op, getSource(op).address > getSource2(op).address); }
    DefExec(UnsignedGreaterThanOrEqualTo) { setDestination(op, getSource(op).address >= getSource2(op).address); }
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

    DefExec(UnsignedAdd) { setDestination(op, getSource(op).address + getSource2(op).address); }
    DefExec(UnsignedSub) { 
        auto a = getSource(op).address;
        auto b = getSource2(op).address;
        auto c = a - b;
        setDestination(op, c);
    }
    DefExec(UnsignedMul) { setDestination(op, getSource(op).address * getSource2(op).address); }
	DefExec(UnsignedDiv) { 
		auto denom = getSource2(op).address;
		if (denom == 0) {
			setDestination(op, 0);
		} else {
			setDestination(op, getSource(op).address / denom);
		}
	}
    DefExec(UnsignedRem) { 
		auto denom = getSource2(op).address;
		if (denom == 0) {
			setDestination(op, 0);
		} else {
			setDestination(op, getSource(op).address % denom);
		}
	}
    DefExec(UnsignedShiftLeft) { setDestination(op, getSource(op).address << getSource2(op).address); }
    DefExec(UnsignedShiftRight) { setDestination(op, getSource(op).address >> getSource2(op).address); }
	DefExec(Increment) { setDestination(op, getSource(op).integer + 1); }
	DefExec(Decrement) { setDestination(op, getSource(op).integer - 1); }
	DefExec(UnsignedIncrement) { setDestination(op, getSource(op).address + 1); }
	DefExec(UnsignedDecrement) { setDestination(op, getSource(op).address - 1); }
	DefExec(Call) {
		push(op._args.dest, _pc);
        _pc = op._args.imm;
	}
	DefExec(ConditionalBranch) {
		if (getDestination(op).getTruth()) {
			_pc = op._args.imm;
		}
	}
    DefExec(AddImmediate) { setDestination(op, getSource(op).integer + op._args.imm); }
    DefExec(SubImmediate) { setDestination(op, getSource(op).integer - op._args.imm); }
    DefExec(RightShiftImmediate) { setDestination(op, getSource(op).integer >> op._args.imm); }
    DefExec(LeftShiftImmediate) { setDestination(op, getSource(op).integer << op._args.imm); }
    DefExec(LoadThenIncrement) {
        Core::Load ld;
        ld._args.dest = op._args.dest;
        ld._args.src = op._args.src;
        perform(ld);
        Core::Increment incr;
        incr._args.dest = op._args.src;
        incr._args.src = op._args.src;
        perform(incr);
    }
    DefExec(LessThanImmediate) { 
        setDestination(op, getSource(op).integer < op._args.imm); 
    }
    DefExec(Move) {
        setDestination(op, getSource(op).address);
    }
    DefExec(StoreThenIncrement) {
        Core::Store st;
        st._args.dest = op._args.dest;
        st._args.src = op._args.src;
        perform(st);
        Core::Increment incr;
        // increment the destination this time
        incr._args.dest = op._args.dest;
        incr._args.src = op._args.dest;
        perform(incr);
    }
	DefExec(ReadLine) {
		std::string _input;
		std::getline(std::cin, _input);
		auto length = getSource(op).address;
		Address numToCopy = ((_input.size() > length) ? length : _input.size()) - 1;
		auto address = getDestination(op).address;
		store(address, Number(numToCopy));
		++address;
		for (Address i = 0u; i < numToCopy; ++i, ++address) {
			store(address, _input[i]);
		}
	}
	DefExec(PrintOK) {
		std::cout << "  ok" << std::endl;
	}
	DefExec(Digit) {
		// dest - stack pointer to manipulate
		// src3 - character to convert
		// src2 - number base
		// src - number
		// dest - success flag
		constexpr unsigned char normalizationValue = '0';
		auto numBase = popDestination(op).get<unsigned char>();
		auto character = popDestination(op).get<unsigned char>();
		character -= normalizationValue;
		if (character < normalizationValue) {
			pushDestination(op, false);
			return;
		} else if (character == normalizationValue) {
			pushDestination(op, 0);
			pushDestination(op, true);
			return;
		} 
		auto value = character - normalizationValue;
		if (value > 0x9) {
			value -= 0x7;
			if (value < 0xa) {
				pushDestination(op, false);
				return;
			}
		} 
		if (value >= numBase) {
			pushDestination(op, false);
			return;
		}
		pushDestination(op, Address(value));
		pushDestination(op, true);
	}
	DefExec(Enclose) {
		// taken from the 8086 figforth impl
		auto terminator = popDestination(op).address;
		auto addr = popDestination(op).address;
		pushDestination(op, addr);
		//terminator &= 0x00FF; // clear the upper half
		Integer offsetCounter = -1;
		--addr;
		// scan to first non-terminator char
		do {
			++addr;
			++offsetCounter;
			// wait for non terminator
		} while (load(addr) == terminator);
		if (load(addr) == 0) {
			// found null before first terminator
			pushDestination(op, offsetCounter + 1);
			pushDestination(op, offsetCounter);
			return;
		}
		// found first text character, count the characters
		bool foundTerminatorAtEnd = false;
		do {
			++addr;
			++offsetCounter;
			if (load(addr) == terminator) {
				foundTerminatorAtEnd = true;
				break;
			}
		}  while(load(addr) != 0);
		if (foundTerminatorAtEnd) {
			pushDestination(op, offsetCounter);
			pushDestination(op, offsetCounter+1);
		} else {
			// found null at end of text
			// counters are equal
			pushDestination(op, offsetCounter);
			pushDestination(op, offsetCounter);
		}
	}
    DefExec(PrimitiveFind) {
        // we are loading from the front of a dictionary entry of the format
        // [0-5]: name field address
        // 0: control bits
        // 1: length
        // 2: char 0 
        // 3: char 1
        // 4: char 2
        // 5: char 3
        // 6: link field
        // 7: code field
        // 8: parameter field
        auto nfa = popDestination(op);
        auto stringAddr = popDestination(op);
        // take the string and generate a compressed format from it
        auto stringLength = load(stringAddr.address);
        auto nfaLength = load(nfa.address + 1);
        if (stringLength == nfaLength) {
            auto nfaStringStart = nfa.address + 2;
            auto stringStart = stringAddr.address + 1;
            // now check the lengths (maximum of four characters)
            auto numCharacters = stringLength > 4 ? 4 : stringLength ;
            auto mismatchFound = false;
            for (Address i = 0; i < numCharacters; ++i) {
                auto nw = load(nfaStringStart + i);
                auto sw = load(stringStart + i);
                if (nw != sw) {
                    mismatchFound = true;
                    break;
                }
            }
            if (mismatchFound) {
                // recursively call this routine for the next entry if we have one
                auto linkField = load(nfa.address + 6);
                if (linkField == 0) {
                    pushDestination(op, false);
                } else {
                    // recursively call this because I'm super lazy
                    pushDestination(op, stringAddr);
                    pushDestination(op, linkField);
                    perform(op);
                }
            } else {
                // we found a match
                pushDestination(op, nfa.address + 8);
                pushDestination(op, true);
            }
        } else {
            // recursively call this routine for the next entry if we have one
            auto linkField = load(nfa.address + 6);
            if (linkField == 0) {
                pushDestination(op, false);
            } else {
                // recursively call this because I'm super lazy
                pushDestination(op, stringAddr);
                pushDestination(op, linkField);
                perform(op);
            }
        }
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
                        outputFile << std::hex << _memory[i].address << std::endl;
                    }
                }
                outputFile.close();
            } else if (index == 1) {
                // load core from disk
                std::ifstream inputFile(path);
                if (inputFile.is_open()) {
                    for (int i = coreCacheStart; i < coreCacheEnd; ++i) {
                        inputFile >> std::hex >> _memory[i].address;
                    }
                } else {
                    for (int i = coreCacheStart; i < coreCacheEnd; ++i) {
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
