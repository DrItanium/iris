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


#ifndef _TARGET_IRIS16_IRIS_H
#define _TARGET_IRIS16_IRIS_H
#include <memory>
#include <variant>
#include <optional>
#include "Types.h"
#include "Problem.h"

namespace iris {
	union Number {
		Number(Address a = 0) : address(a) { }
		Number(bool value) : integer(value ? -1 : 0 ) { }
		Number(byte b) : address(b) { }
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
			virtual void setValue(Number value) noexcept;
			Number getValue() const noexcept { return _value; }
			bool getTruth() const noexcept { return _value.getTruth(); }
			template<typename T>
			T get() const noexcept {
				return _value.get<T>();
			}
		private:
			Number _value;
	};
	class HardwiredRegister : public Register {
		public:
			using Register::Register;
			virtual void setValue(Number) noexcept override;
	};
	using RegisterIndex = byte;
	using DestinationRegister = RegisterIndex;
	using SourceRegister = RegisterIndex;
	enum class Opcode : Address {
#define X(title, style, z) title, 
#define FirstX(title, style, z) X(title, style, z)
#include "Opcodes.def"
#undef FirstX
#undef X
		Count,
	};
	static_assert(Address(Opcode::Count) <= 256, "Too many instructions defined!");
	/**
	 * The iris core is a 16-bit harvard architecture that has multiple memory
	 * spaces. Each of them being 64k words (or double word) each. 
	 * There are four spaces: Code (double word), Data, Stack, and IO. 
	 *
	 * this is a slightly improved version of the iris core as it has support
	 * for multiple stack pointers and such.
	 */
	class Core {
		public: 
			static constexpr Address registerCount = 256;
			static constexpr Address maxAddress = 0xFFFF;
			static constexpr Address32 addressSize = 0x10000;
			using MemoryBlock16 = std::unique_ptr<Number[]>;
			using MemoryBlock32 = std::unique_ptr<RawInstruction[]>;
			using RegisterFile = std::unique_ptr<Register[]>;
		public:
			Core();
			void init();
			void shutdown();
			void execute();
			void dump(std::ostream& out);
			void install(std::istream& in);
		public:

			// the different containers for instruction forms are defined here
			struct NoArguments final { };
			struct OneRegister final { 
				OneRegister() = default;
				~OneRegister() = default;
				DestinationRegister dest;
			};
			struct TwoRegister final {
				TwoRegister() = default;
				~TwoRegister() = default;
				DestinationRegister dest;
				SourceRegister src;
			};
			struct ThreeRegister final {
				ThreeRegister() = default;
				~ThreeRegister() = default;
				DestinationRegister dest;
				SourceRegister src;
				SourceRegister src2;
			};
			struct Immediate16 final {
				Immediate16() = default;
				~Immediate16() = default;
				Address imm;
			};
			struct OneRegisterWithImmediate final {
				OneRegisterWithImmediate() = default;
				~OneRegisterWithImmediate() = default;
				DestinationRegister dest;
				Address imm;
			};
			struct TwoRegisterWithImmediate final {
				TwoRegisterWithImmediate() = default;
				~TwoRegisterWithImmediate() = default;
				DestinationRegister dest;
				SourceRegister src;
				byte src2;
			};
#define X(title, style, z) \
			struct title final { \
				title ( ) { } \
				title (const style & v) : _args(v) { } \
				constexpr Opcode opcode() const noexcept { return Opcode :: title ; } \
				style _args ; } ; 
#define FirstX(title, style, z) X(title, style, z)
#include "Opcodes.def"
#undef FirstX
#undef X
			using DecodedInstruction = std::variant<
#define FirstX(title, style, z) title
#define X(title, style, z) ,title
#include "Opcodes.def"
#undef X
#undef FirstX
				>;
		private:
			// functions to contain the logic for each opcode
			void dispatchInstruction(const DecodedInstruction& inst);
			RawInstruction extractInstruction() noexcept;
#define X(title, style, z) void perform ( const title & value );
#define FirstX(title, style, z) X(title, style, z)
#include "Opcodes.def"
#undef X
#undef FirstX
		private:
			void decodeArguments(RawInstruction, NoArguments&) noexcept;
			void decodeArguments(RawInstruction, OneRegister&) noexcept;
			void decodeArguments(RawInstruction, TwoRegister&) noexcept;
			void decodeArguments(RawInstruction, ThreeRegister&) noexcept;
			void decodeArguments(RawInstruction, Immediate16&) noexcept;
			void decodeArguments(RawInstruction, OneRegisterWithImmediate&) noexcept;
			void decodeArguments(RawInstruction, TwoRegisterWithImmediate&) noexcept;
			DecodedInstruction decodeInstruction(RawInstruction val);
			RawInstruction encodeArguments(const NoArguments&) noexcept;
			RawInstruction encodeArguments(const OneRegister&) noexcept;
			RawInstruction encodeArguments(const TwoRegister&) noexcept;
			RawInstruction encodeArguments(const ThreeRegister&) noexcept;
			RawInstruction encodeArguments(const Immediate16&) noexcept;
			RawInstruction encodeArguments(const OneRegisterWithImmediate&) noexcept;
			RawInstruction encodeArguments(const TwoRegisterWithImmediate&) noexcept;
			RawInstruction encodeInstruction(const DecodedInstruction& inst) noexcept;
		private:
			const Register& getRegister(RegisterIndex reg) const noexcept;
			inline Number getRegisterValue(RegisterIndex reg) const noexcept { return getRegister(reg).getValue(); }
			void setRegister(RegisterIndex reg, Number value) noexcept;
			template<typename T>
			void setDestination(const T& value, Number n) noexcept {
				setRegister(value._args.dest, n);
			}
			template<typename T>
			Number getSource(const T& value) const noexcept {
				return getRegister(value._args.src).getValue();
			}
			template<typename T>
			Number getSource2(const T& value) const noexcept {
				if constexpr (std::is_same_v<T, TwoRegisterWithImmediate>) {
					return Number(value._args.src2);
				} else {
					return getRegister(value._args.src2).getValue();
				}
			}
		private:
			void cycle();
		private:
			Address _pc;
			MemoryBlock16 _data, _stack;
			MemoryBlock32 _code;
			// IO space is special and is really a mapping to native goings
			// on!
			RegisterFile _registers;
			bool _keepExecuting = true;

	};
	//class Core : public syn::ClipsCore<word> {
    //    public:
    //        template<dword capacity>
    //        using WordMemorySpace = syn::FixedSizeLoadStoreUnit<word, dword, capacity>;
    //        using WordMemorySpace64k = WordMemorySpace<ArchitectureConstants::AddressCount>;
    //        using RegisterFile = WordMemorySpace<ArchitectureConstants::RegisterCount>;
    //        using PredicateRegisterFile = syn::FixedSizeLoadStoreUnit<bool, byte, ArchitectureConstants::ConditionRegisterCount>;
    //        using ErrorStorage = WordMemorySpace<ArchitectureConstants::RegistersToSaveOnError>;
    //        using InstructionPointer = syn::Register<QuadWord, ArchitectureConstants::AddressMax>;
    //        using LinkRegister = syn::Register<QuadWord, ArchitectureConstants::AddressMax>;
    //        using PredicateRegisterBlock = syn::Register<word, ArchitectureConstants::AddressMax>;
	//		using Parent = syn::ClipsCore<word>;
	//	public:
	//		Core(syn::CLIPSIOController& io) noexcept;
	//		virtual ~Core();
	//		virtual void initialize() override;
	//		virtual void shutdown() override;
	//		virtual bool cycle() override;
	//		virtual bool handleOperation(void* env, CLIPSValue* ret) override;
	//		virtual word readFromBus(word addr) override;
	//		virtual void writeToBus(word addr, word value) override;
	//	private:
    //        inline QuadWord getInstructionPointer() const noexcept { return _ip.get(); }
    //        inline QuadWord getLinkRegister() const noexcept { return _lr.get(); }
    //        void setInstructionPointer(QuadWord value) noexcept;
    //        void setLinkRegister(QuadWord value) noexcept;
	//		bool getPredicateRegister(byte index) const;
    //        void setPredicateRegister(byte index, bool value);
    //        void incrementInstructionPointer() noexcept;

	//	private:
	//		void dispatch() noexcept;
    //        template<int index>
    //        inline word& getRegister() noexcept {
    //            return gpr[InstructionDecoder::getRegisterIndex<index>(current)];
    //        }
    //        template<int index>
    //        inline byte getPredicateIndex() const noexcept {
    //            return InstructionDecoder::getPredicateIndex<index>(current);
    //        }

    //        template<int index>
    //        inline bool getPredicate() const noexcept {
    //            return getPredicateRegister(getPredicateIndex<index>());
    //        }
    //        template<int index>
    //        inline void setPredicate(bool value) noexcept {
    //            setPredicateRegister(getPredicateIndex<index>(), value);
    //        }
    //        word& destinationRegister() noexcept;
    //        word& source0Register() noexcept;
    //        word& source1Register() noexcept;

    //        bool getPredicateResult() const noexcept;
	//		bool getPredicateInverseResult() const noexcept;
	//		bool getPredicateSource0() const noexcept;
	//		bool getPredicateSource1() const noexcept;
    //        byte getPredicateResultIndex() const noexcept;
	//		byte getPredicateInverseResultIndex() const noexcept;
	//		byte getPredicateSource0Index() const noexcept;
	//		byte getPredicateSource1Index() const noexcept;
    //        void setPredicateResult(bool value) noexcept;
	//		void setPredicateInverseResult(bool value) noexcept;
	//		void setPredicateSource0(bool value) noexcept;
	//		void setPredicateSource1(bool value) noexcept;
    //        word getHalfImmediate() const noexcept;
    //        word getImmediate() const noexcept;
    //        byte getDestinationIndex() const noexcept;
	//	private:
	//		void saveSystemState() noexcept;
	//		void restoreSystemState() noexcept;
	//		void dispatchInterruptHandler();
	//		void restorePredicateRegisters(word input, word mask) noexcept;
	//		word savePredicateRegisters(word mask) noexcept;


	//	private:
	//		bool execute;
	//		bool advanceIp;
	//		raw_instruction current;
    //        InstructionPointer _ip;
    //        LinkRegister _lr;
	//		word _error;
	//		RegisterFile gpr;
	//		WordMemorySpace64k data;
	//		syn::FixedSizeLoadStoreUnit<dword, dword, ArchitectureConstants::AddressMax + 1> instruction;
	//		WordMemorySpace64k stack;
	//		PredicateRegisterBlock _cr;
	//		ErrorStorage _onError;
	//		bool _saveAdvanceIp = false;
	//		bool _saveExecute = false;
    //        bool _inInterruptHandler = false;
	//};
}
#endif
