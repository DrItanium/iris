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


#ifndef _TARGET_IRIS16_IRIS_H
#define _TARGET_IRIS16_IRIS_H
#include <memory>
#include <variant>
#include <optional>
#include <functional>
#include <type_traits>
#include <list>
#include <iostream>
#include "Types.h"
#include "Problem.h"

namespace iris {
	union Number {
		Number(Address a = 0) : address(a) { }
		Number(bool value) : integer(value ? -1 : 0 ) { }
		Number(byte b) : Number(Address(b) & 0xFF) { }
		Number(int i) : integer(Integer(i)) { }
		Number(unsigned int i ) : address(Address(i)) { }
		Number(Integer i) : integer(i) { }
		Number(const Number& other) : address(other.address) { }
		bool getTruth() const noexcept { return address != 0; }
		template<typename T>
		T get() const noexcept {
			using K = std::decay_t<T>;
			if constexpr (std::is_same_v<K, Integer>) {
				return integer;
			} else if constexpr (std::is_same_v<K, Address>) {
				return address;
			} else if constexpr (std::is_same_v<K, bool>) {
				return getTruth();
            } else if constexpr (std::is_same_v<K, signed char>) {
                return static_cast<signed char>(0xFF & integer);
            } else if constexpr (std::is_same_v<K, unsigned char>) {
                return static_cast<unsigned char>(0xFF & address);
            } else if constexpr (std::is_same_v<K, char>) {
                return static_cast<char>(0xFF & address);
			} else {
				static_assert(AlwaysFalse<T>::value, "Number type does not store this kind of value!");
			}
		}
		Integer integer;
		Address address;
		byte bytes[sizeof(Address)];
	};
	class Register {
		public:
			Register();
			Register(Number initialValue);
			~Register();
			void setValue(Number value) noexcept;
			Number getValue() const noexcept { return _value; }
			bool getTruth() const noexcept { return _value.getTruth(); }
			template<typename T>
			T get() const noexcept {
				return _value.get<T>();
			}
			void disableWrites() noexcept { _noWrites = true; }
			bool writesDisabled() const noexcept { return _noWrites; }
		private:
			Number _value;
			bool _noWrites = false;
	};
	using RegisterIndex = byte;
	using DestinationRegister = RegisterIndex;
	using SourceRegister = RegisterIndex;
	enum class Opcode : byte {
#define X(title, style) title, 
#define FirstX(title, style) X(title, style)
#include "Opcodes.def"
#undef FirstX
#undef X
		Count,
	};
	static_assert(Address(Opcode::Count) <= 256, "Too many instructions defined!");
	/**
	 * The iris core is a 16-bit harvard architecture that has multiple memory
	 * spaces. Each of them being 64k words (or double word) each. 
	 * There are three spaces: Memory, Core, and IO. 
	 *
	 * this is a slightly improved version of the iris core as it has support
	 * for multiple stack pointers and such.
	 */
	class Core {
		public: 
			static constexpr Address registerCount = 16;
			static constexpr Address maxAddress = 0xFFFF;
			static constexpr Address ioSpaceStart = 0xFF00;
			static constexpr Address ioSpaceEnd = maxAddress; 
			static constexpr Address coreCacheStart = 0xE000;
			static constexpr Address coreCacheEnd = 0xF000;
			static constexpr Address32 addressSize = 0x10000;
            static constexpr RegisterIndex registerZero = 0;
            // the error codes that the processor will raise as part of an
            // error happening
            enum class ErrorCodes : Address {
                None, // always first and means that all is good :D
                IllegalOpcode,
                // TODO: add more opcodes and put in system handlers for it
                Count,
            };
			using MemoryBlock16 = std::unique_ptr<byte[]>;
			using RegisterFile = std::unique_ptr<Register[]>;
#define DefInstallationTarget(title) struct title final { }
			DefInstallationTarget(SetRegister);
			DefInstallationTarget(WriteByteToMemory);
			DefInstallationTarget(WriteWordToMemory);
#undef DefInstallationTarget
			using SectionInstallationTarget = std::variant<SetRegister, WriteByteToMemory, WriteWordToMemory>;
		public:
			Core();
			void init();
			void shutdown();
			void execute();
			void dump(std::ostream& out);
			void install(std::istream& in);
			template<typename T>
			void install(Address address, T value, SectionInstallationTarget target) {
				std::visit([this, address, value](auto&& kind) {
							using K = std::decay_t<decltype(kind)>;
							if constexpr (std::is_same_v<K, SetRegister>) {
								if (address >= registerCount) {
									throw Problem("Illegal register index!");
								} else {
									_registers[address].setValue(value);
								}
							} else if constexpr (std::is_same_v<K, WriteByteToMemory>) {
								store(address, value, true);
							} else if constexpr (std::is_same_v<K, WriteWordToMemory>) {
								storeNumber(address, value, true);
							} else {
								static_assert(AlwaysFalse<T>::value, "Unimplemented section!");
							}
						}, target);
			}
            using IOWriter = std::function<void(Address, byte)>;
            using IOReader = std::function<byte(Address)>;
            class IODevice {
                public:
                    IODevice(Address responseBegin, Address length = 1, IOReader read = nullptr, IOWriter write = nullptr) : _begin(responseBegin), _end(responseBegin + length), _read(read), _write(write) { }
                    IODevice(const IODevice& other) : _begin(other._begin), _end(other._end), _read(other._read), _write(other._write) { }
                    ~IODevice() = default;
					bool respondsTo(Address addr) const noexcept;
                    byte read(Address addr) ;
                    void write(Address addr, byte value) ;
                    Address getEnd() const noexcept { return _end; }
                private:
					Address _begin;
					Address _end;
                    IOReader _read;
                    IOWriter _write;
            };
            void installIODevice(IODevice dev);
        private:
			void cycle();
            static Register nullReg;
		private:
            struct NoArguments { };
            struct OneRegister { };
            struct TwoRegister { };
            struct ThreeRegister { };
            struct FourRegister { };
            struct OneRegisterWithImmediate { };
            struct TwoRegisterWithImmediate { };
			struct WideThreeRegister { };
			struct WideTwoRegister { };
            struct OneRegisterWithByte { };
			struct ImmediateOnly { };
#define FirstX(title, style) struct title final : style { };
#define X(title, style) FirstX(title, style)
#include "Opcodes.def"
#undef X
#undef FirstX
            
			// functions to contain the logic for each opcode
#define X(title, style) void perform ( const title & value );
#define FirstX(title, style) X(title, style)
#include "Opcodes.def"
#undef X
#undef FirstX
		private:
			void decodeArguments(const NoArguments&) noexcept;
			void decodeArguments(const OneRegister&) noexcept;
			void decodeArguments(const TwoRegister&) noexcept;
			void decodeArguments(const ThreeRegister&) noexcept;
			void decodeArguments(const FourRegister&) noexcept;
			void decodeArguments(const OneRegisterWithImmediate&) noexcept;
			void decodeArguments(const TwoRegisterWithImmediate&) noexcept;
			void decodeArguments(const WideTwoRegister&) noexcept;
			void decodeArguments(const WideThreeRegister&) noexcept;
			void decodeArguments(const OneRegisterWithByte&) noexcept; 
			void decodeArguments(const ImmediateOnly&) noexcept;
		private:
			Register& getRegister(RegisterIndex reg) noexcept;
			byte pop(Register& reg) noexcept;
			Number popNumber(Register& reg) noexcept;
			void push(Register& reg, byte value) noexcept;
			void pushNumber(Register& reg, Number value) noexcept;
            using IODeviceOp = std::function<void(IODevice&)>;
            void onIODeviceFound(Address addr, IODeviceOp fn);
			void store(Address addr, byte value, bool unmapIOSpace = false) noexcept;
            // all number loads and stores are now aligned loads and stores :)
            void storeNumber(Address addr, Number value, bool unmapIOSpace = false) noexcept;
            byte load(Address addr, bool unmapIOSpace = false) noexcept;
            Number loadNumber(Address addr, bool unmapIOSpace = false) noexcept;
		private:
			Address _pc;
            MemoryBlock16 _memory;
			// IO space is special and is really a mapping to native goings
			// on!
			RegisterFile _registers;
			bool _keepExecuting = true;
            std::list<IODevice> _io;
            // arguments as part of decoding
            Register& _dest = nullReg;
			Register& _destNext = nullReg;
            Register& _src = nullReg;
			Register& _srcNext = nullReg;
            Register& _src2 = nullReg;
			Register& _src2Next = nullReg;
            Register& _src3 = nullReg;
			Register& _src3Next = nullReg;
            union {
                Address _addr;
                Integer _imm;
				byte _half;
            };
	};
}
#endif
