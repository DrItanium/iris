/**
 * @file
 * Iris architecture assembler rewritten to use pegtl
 * @copyright
 * syn
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


#ifndef IRIS_CORE_ASSEMBLER_H__
#define IRIS_CORE_ASSEMBLER_H__
#include "Problem.h"
#include "AssemblerKeywords.h"
#include "AssemblerStructures.h"

#include <tao/pegtl.hpp>
#include <tao/pegtl/analyze.hpp>
#include <tao/pegtl/contrib/raw_string.hpp>
#include <tao/pegtl/contrib/abnf.hpp>
#include <tao/pegtl/parse.hpp>
#include <tao/pegtl.hpp>
#include <tao/pegtl/contrib/raw_string.hpp>
#include <tao/pegtl/contrib/abnf.hpp>
#include <vector>
#include <sstream>
#include <typeinfo>
#include <iostream>
#include <map>
#include <bitset>

using namespace tao::TAOCPP_PEGTL_NAMESPACE;
namespace iris {
    /**
     * Defines the parsing logic for the iris architecture assembler
     */
	template<typename R>
	struct Action : nothing<R> { };
	struct EndOfLineComment : until<eolf> { };
	template<char tag>
	struct SingleLineComment : disable<one<tag>, EndOfLineComment> { };

	struct AsmSeparator : plus<ascii::space> { };
	struct OptionalSpace : star<ascii::space> { };
	struct SymbolComma : one<','> { };
	struct SymbolEqualsSign : one<'='> { };
	struct SymbolLeftParen : one<'('> { };
	struct SymbolRightParen : one<')'> { };
	template<typename End, typename Entry>
	struct MainParser : until<End, must<Entry>> { };
	template<typename Entry>
	struct MainFileParser :  MainParser<eof, Entry> { };
	template<char prefix>
	struct GenericRegister : if_must<one<prefix>, plus<digit>> { };

	using GPR = GenericRegister<'r'>;
	
	template<auto count>
	RegisterIndex getRegisterIndex(const std::string& name) {
		using T = RegisterIndex;
		using V = decltype(count);
		static_assert(std::is_integral_v<V>, "Provided count must be a number!");
		static_assert(std::is_unsigned_v<V>, "Provided count must be unsigned!");
        static_assert(count > 0, "Can't have zero registers!");
		V value = 0;
		std::string wrapper(name);
		wrapper[0] = '0';
		std::istringstream ss(wrapper);
		ss >> std::dec >> value;
		if (value >= count) {
			throw Problem("target register is too large!");
		} else {
			return static_cast<T>(value);
		}
	}
	template<typename T>
	T getBinaryImmediate(const std::string& text) noexcept {
		std::string temp(text);
		// always replace the second element as it is always the b, x, etc
		temp[1] = '0';
		constexpr auto width = iris::bitwidth<T>;
		static_assert(width <= iris::bitwidth<uint64_t>, "Please provide custom implementation of getBinaryImmediate since type is too large to fit in a unsigned long long!");
		std::bitset<width> bits(temp);
		return static_cast<T>(bits.to_ullong());
	}
	template<typename T>
	T getHexImmediate(const std::string& text) noexcept {
		T value = 0;
		std::string wrapper(text);
		wrapper[1] = '0';
		std::istringstream ss(wrapper);
		ss >> std::hex >> value;
		return static_cast<T>(value);
	}

	template<typename T>
	T getOctalImmediate(const std::string& text) noexcept {
		T value = 0;
		std::string wrapper(text);
		wrapper[1] = '0';
		std::istringstream ss(wrapper);
		ss >> std::oct >> value;
		return static_cast<T>(value);
	}

	template<typename T>
	T getDecimalImmediate(const std::string& text) noexcept {
		T value = 0;
		std::string wrapper(text);
		std::istringstream ss(wrapper);
		ss >> std::dec >> value;
		return static_cast<T>(value);
	}
    template<typename Rule>
    struct SingleEntrySequence : seq<Rule> { };

    template<typename State, typename C>
    struct StatefulSingleEntrySequence : state<State, SingleEntrySequence<C>> { };

    template<typename C0, typename C1, typename Separator = AsmSeparator>
    struct TwoPartComponent : seq<C0, Separator, C1> { };

    template<typename State, typename C0, typename C1, typename Separator = AsmSeparator>
    struct StatefulTwoPartComponent : state<State, TwoPartComponent<C0, C1, Separator>> { };


	template<typename First, typename Second, typename Third, typename Sep0 = AsmSeparator, typename Sep1 = AsmSeparator>
	struct ThreePartComponent : seq<First, Sep0, Second, Sep1, Third> { };

	template<typename State, typename First, typename Second, typename Third, typename Sep0 = AsmSeparator, typename Sep1 = AsmSeparator>
	struct StatefulThreePartComponent : state<State, ThreePartComponent<First, Second, Third, Sep0, Sep1>> { };

    template<typename Register>
	using OneRegister = SingleEntrySequence<Register>;
    template<typename R0, typename R1, typename Separator = AsmSeparator>
	using TwoRegister = TwoPartComponent<R0, R1, Separator>;
    template<typename R0, typename R1, typename R2, typename Separator0 = AsmSeparator, typename Separator1 = AsmSeparator>
	using ThreeRegister = ThreePartComponent<R0, R1, R2, Separator0, Separator1>;

	template<typename T>
	struct OptionalSpaceWrapped : seq<OptionalSpace, T, OptionalSpace> { };
	using EqualsSignSeparator = OptionalSpaceWrapped<SymbolEqualsSign>;
	using CommaSeparator = OptionalSpaceWrapped<SymbolComma>;

    template<char delimiter, typename SymbolClass>
    struct GenericNumeral : if_must<istring<'0', delimiter>, plus<SymbolClass>> { };

	template<char delim = 'x'>
	struct Base16Number : GenericNumeral<delim, xdigit> { };
	using HexadecimalNumber = Base16Number<'x'>;

	template<char delim = 'b'>
	struct Base2Number : GenericNumeral<delim, abnf::BIT> { };
	using BinaryNumber = Base2Number<'b'>;

	struct Base10Number : plus<digit> { };

    template<typename Src0, typename Src1, typename Separator = AsmSeparator>
    struct SourceRegisters : TwoRegister<Src0, Src1, Separator> { };

    struct Lexeme : identifier { };



    template<typename Other>
    struct LexemeOr : sor<Lexeme, Other> { };

    template<typename Operation, typename Operands, typename Separator = AsmSeparator>
    struct Instruction : seq<Operation, Separator, Operands> { };
	template<typename S, typename ... NumberTypes>
	struct StatefulNumber : state<S, sor<NumberTypes...>> { };

	template<typename S>
	struct StatefulNumberAll : StatefulNumber<S, HexadecimalNumber, Base10Number, BinaryNumber> { 
	};
	struct Decimal final { };
	struct Binary final { };
	struct Hexadecimal final { };
	template<typename T, typename K>
	T parseNumber(const std::string& str) {
		if constexpr (std::is_same_v<K, Decimal>) {
			return getDecimalImmediate<T>(str);
		} else if constexpr (std::is_same_v<K, Binary>) {
			return getBinaryImmediate<T>(str);
		} else if constexpr (std::is_same_v<K, Hexadecimal>) {
			return getHexImmediate<T>(str);
		} else if constexpr (std::is_same_v<K, Register>) {
		} else {
			static_assert(AlwaysFalse<K>::value, "Provided number kind is not supported!");
		}
	}

//#define DefAction(rule) template<> struct Action< rule >
//#define DefApplyGeneric(type) template<typename Input> static void apply(const Input& in, type & state)
//#define DefApplyGenericEmpty(type) DefApplyGeneric(type) { }
//        DefAction(Lexeme) {
//            DefApplyGeneric(AssemblerData) {
//    			state.hasLexeme = true;
//    			state.currentLexeme = in.string();
//            }
//    		DefApplyGeneric(syn::StringContainer) {
//    			state.setValue(in.string());
//    		}
//    		DefApplyGeneric(syn::NumberOrStringContainer<word>) {
//				state.setValue(in.string());
//    		}
//
	using Separator = AsmSeparator;

	struct DirectiveCodeHandler {
		template<typename Input>
		DirectiveCodeHandler(const Input& in, AssemblerState& parent) {
			parent.addData(code());
		}
		template<typename Input>
		void success(const Input& in, AssemblerState& parent) {
			parent.addData(code());
		}
	};
	struct DirectiveDataHandler {
		template<typename Input>
		DirectiveDataHandler(const Input& in, AssemblerState& parent) {
		}
		template<typename Input>
		void success(const Input& in, AssemblerState& parent) {
			parent.addData(data());
		}
	};
	struct DirectiveStackHandler {
		template<typename Input>
		DirectiveStackHandler(const Input& in, AssemblerState& parent) { }
		template<typename Input>
		void success(const Input& in, AssemblerState& parent) {
			parent.addData(stack());
		}
	};
	struct OrgDirectiveHandler {
		template<typename Input>
		OrgDirectiveHandler(const Input& in, AssemblerState&) { }
    	template<typename Input>
    	void success(const Input& in, AssemblerState& parent) {
			parent.addData(org(_value));
    	}
		Address _value;
	};
	struct LabelDirectiveHandler {
		template<typename Input>
		LabelDirectiveHandler(const Input& in, AssemblerState& parent) {

		}
		template<typename Input>
		void success(const Input& in, AssemblerState& parent) {
			parent.addLabel(_name);
		}
		std::string _name;
	};

	template<>
	struct Action<Lexeme> {
		template<typename Input>
		static void apply(const Input& in, LabelDirectiveHandler& parent) {
			parent._name = in.string();
		}
	};
	struct CodeDirective : StatefulSingleEntrySequence<DirectiveCodeHandler, sor<TAOCPP_PEGTL_KEYWORD(".text"), TAOCPP_PEGTL_KEYWORD(".code")>> { };
	struct DataDirective : StatefulSingleEntrySequence<DirectiveDataHandler, TAOCPP_PEGTL_KEYWORD(".data")> { };
	struct StackDirective : StatefulSingleEntrySequence<DirectiveStackHandler, TAOCPP_PEGTL_KEYWORD(".stack")> { };
	struct OrgDirective : TwoPartComponent<TAOCPP_PEGTL_KEYWORD(".org"), StatefulNumberAll<OrgDirectiveHandler>> { };
	struct LabelDirective : state<LabelDirectiveHandler, TwoPartComponent<TAOCPP_PEGTL_KEYWORD(".label"), Lexeme>> { };
	// @todo add support for .integer, .address, and label embedding directives
	// @todo add support for string embedding
	struct Directive : sor<CodeDirective, DataDirective, StackDirective, OrgDirective, LabelDirective> { };

	template<typename T, typename K>
	void populateContainer(const std::string& str, T& parent) {
		using A = std::decay_t<T>;
		if constexpr (std::is_same_v<A, OrgDirectiveHandler>) {
			parent._value = parseNumber<decltype(parent._value), K>(str);
		} else if constexpr (std::is_same_v<A, Core::Immediate16>) {
			parent.imm = parseNumber<decltype(parent.imm), K>(str);
		} else if constexpr (std::is_same_v<A, Core::OneRegisterWithImmediate>) {
			parent.imm = parseNumber<decltype(parent.imm), K>(str);
		} else if constexpr (std::is_same_v<A, Core::TwoRegisterWithImmediate>) {
			parent.src2 = parseNumber<decltype(parent.src2), K>(str);
		} else {
			static_assert(AlwaysFalse<A>::value, "Unsupported type!");
		}
	}
    template<typename K>
    struct PopulateNumberType {
		template<typename I>
		static void apply(const I& in, OrgDirectiveHandler& state) {
			populateContainer<OrgDirectiveHandler, K>(in.string(), state);
		}
		template<typename I>
		static void apply(const I& in, Core::Immediate16& state) {
			populateContainer<decltype(state), K>(in.string(), state);
		}
		template<typename I>
		static void apply(const I& in, Core::OneRegisterWithImmediate& state) {
			populateContainer<decltype(state), K>(in.string(), state);
		}
		template<typename I>
		static void apply(const I& in, Core::TwoRegisterWithImmediate& state) {
			populateContainer<decltype(state), K>(in.string(), state);
		}
    };
	template<> struct Action < HexadecimalNumber > : PopulateNumberType<Hexadecimal> { };
	template<> struct Action < BinaryNumber > : PopulateNumberType<Binary> { };
	template<> struct Action < Base10Number > : PopulateNumberType<Decimal> { };
#define FirstX(c, t, str) class Keyword ## c : TAOCPP_PEGTL_KEYWORD(#str) { };
#define X(c, t, s) FirstX(c, t, s)
#include "Opcodes.def"
#undef X
#undef FirstX
    struct Anything : sor<
		Separator,
		//Instruction,
		Directive,
		SingleLineComment<';'>> { };
    struct Main : MainFileParser<Anything> { };

// namespace assembler {
//    	struct HalfImmediateContainer;
//        struct ImmediateContainer : syn::NumberContainer<word> {
//            using syn::NumberContainer<word>::NumberContainer;
//    		template<typename Input>
//    			void success(const Input& in, HalfImmediateContainer& parent);
//    		template<typename Input>
//    			void success(const Input& in, AssemblerInstruction& parent);
//    		template<typename Input>
//    			void success(const Input& in, AssemblerDirective& parent);
//        };
//        struct RegisterIndexContainer : syn::NumberContainer<byte> {
//            using syn::NumberContainer<byte>::NumberContainer;
//            template<typename Input>
//                void success(const Input& in, AssemblerInstruction& parent) {
//    				parent.setField(_index, getValue());
//                }
//            RegisterPositionType _index;
//        };
//    	template<InstructionGroup type>
//    	struct SetInstructionGroup {
//    		static void apply0(AssemblerInstruction& instruction) {
//    			instruction.group = static_cast<byte>(type);
//    		}
//    	};
//        using Separator = syn::AsmSeparator;
//        template<typename First, typename Second, typename Sep = Separator>
//        using SeparatedBinaryThing = syn::TwoPartComponent<First, Second, Sep>;
//        template<typename First, typename Second, typename Third, typename Sep = Separator>
//        using SeparatedTrinaryThing = syn::ThreePartComponent<First, Second, Third, Sep, Sep>;
//        using SingleLineComment = syn::SingleLineComment<';'>;
//        using GeneralPurposeRegister = syn::GPR;
//        using PredicateRegister = syn::PredicateRegister;
//    	template<word count>
//    	struct SetRegisterGeneric {
//    		DefApplyGeneric(AssemblerInstruction) { }
//    		DefApplyGeneric(RegisterIndexContainer) {
//    			state.setValue(syn::getRegister<word, count>(in.string(), syn::reportError));
//    		}
//    	};
//        DefAction(GeneralPurposeRegister) : SetRegisterGeneric<ArchitectureConstants::RegisterCount> { };
//        DefAction(PredicateRegister) : SetRegisterGeneric<ArchitectureConstants::ConditionRegisterCount> { };
//    	template<RegisterPositionType pos>
//    	struct GenericRegisterIndexContainerAction {
//            static void apply0(RegisterIndexContainer& state) {
//                state._index = pos;
//            }
//            static void apply0(AssemblerInstruction& state) { }
//    	};
//        // GPRs
//        using IndirectGPR = syn::SingleEntrySequence<GeneralPurposeRegister>;
//        struct DestinationGPR : IndirectGPR {  };
//        struct Source0GPR : IndirectGPR { };
//        struct Source1GPR : IndirectGPR { };
//    	DefAction(DestinationGPR) : GenericRegisterIndexContainerAction<RegisterPositionType::DestinationGPR> { };
//    	DefAction(Source0GPR) : GenericRegisterIndexContainerAction<RegisterPositionType::Source0GPR> { };
//    	DefAction(Source1GPR) : GenericRegisterIndexContainerAction<RegisterPositionType::Source1GPR> { };
//        template<typename T>
//        using StatefulRegister = state<RegisterIndexContainer, T>;
//        using StatefulDestinationGPR = StatefulRegister<DestinationGPR>;
//        using SourceRegisters = syn::SourceRegisters<StatefulRegister<Source0GPR>, StatefulRegister<Source1GPR>>;
//        using OneGPR = syn::OneRegister<StatefulDestinationGPR>;
//        using TwoGPR = syn::TwoRegister<StatefulDestinationGPR, StatefulRegister<Source0GPR>>;
//        using ThreeGPR = syn::TwoRegister<StatefulDestinationGPR, SourceRegisters>;
//
//        // predicate registers
//        using IndirectPredicateRegister = syn::SingleEntrySequence<PredicateRegister>;
//        struct DestinationPredicateRegister : IndirectPredicateRegister { };
//        struct DestinationPredicateInverseRegister : IndirectPredicateRegister { };
//        struct Source0Predicate : IndirectPredicateRegister { };
//        struct Source1Predicate : IndirectPredicateRegister { };
//        DefAction(DestinationPredicateRegister) : GenericRegisterIndexContainerAction<RegisterPositionType::PredicateDestination> { };
//        DefAction(DestinationPredicateInverseRegister) : GenericRegisterIndexContainerAction<RegisterPositionType::PredicateInverseDestination> { };
//        DefAction(Source0Predicate) : GenericRegisterIndexContainerAction<RegisterPositionType::PredicateSource0> { };
//        DefAction(Source1Predicate) : GenericRegisterIndexContainerAction<RegisterPositionType::PredicateSource1> { };
//
//        using DestinationPredicates = syn::TwoRegister<StatefulRegister<DestinationPredicateRegister>, StatefulRegister<DestinationPredicateInverseRegister>>;
//
//    	struct FullImmediateContainer;
//    	template<typename State = ImmediateContainer>
//        struct Number : syn::StatefulNumberAll<State> { };
//        using Lexeme = syn::Lexeme;
//
//        DefAction(Lexeme) {
//            DefApplyGeneric(AssemblerData) {
//    			state.hasLexeme = true;
//    			state.currentLexeme = in.string();
//            }
//    		DefApplyGeneric(syn::StringContainer) {
//    			state.setValue(in.string());
//    		}
//    		DefApplyGeneric(syn::NumberOrStringContainer<word>) {
//				state.setValue(in.string());
//    		}
//        };
//    	template<typename State = ImmediateContainer>
//        struct LexemeOrNumber : syn::LexemeOr<Number<State>> { };
//        template<SectionType section>
//            struct ModifySection {
//    			template<typename Input>
//    				ModifySection(const Input& in, AssemblerDirective& parent) {
//    					parent.action = AssemblerDirectiveAction::ChangeSection;
//    					parent.section = section;
//    				}
//
//    			template<typename Input>
//    			void success(const Input& in, AssemblerDirective& parent) { }
//            };
//        // directives
//        template<SectionType modSection, typename Directive>
//            struct StatefulSpaceDirective : syn::StatefulSingleEntrySequence<ModifySection<modSection>, Directive> { };
//        struct CodeDirective : StatefulSpaceDirective<SectionType::Code, SymbolCodeDirective> { };
//        struct DataDirective : StatefulSpaceDirective<SectionType::Data, SymbolDataDirective> { };
//
//    	struct OrgDirectiveHandler : public ImmediateContainer {
//    		using Parent = ImmediateContainer;
//    		template<typename Input>
//    		OrgDirectiveHandler(const Input& in, AssemblerDirective& parent) : Parent(in, parent){
//    			parent.action = AssemblerDirectiveAction::ChangeCurrentAddress;
//    		}
//
//    		template<typename Input>
//    		void success(const Input& in, AssemblerDirective& parent) {
//    			parent.address = getValue();
//    		}
//    	};
//        struct OrgDirective : syn::OneArgumentDirective<syn::SymbolOrgDirective, Number<OrgDirectiveHandler>> { };
//    	struct LabelDirectiveHandler : public syn::StringContainer {
//    		using Parent = syn::StringContainer;
//    		template<typename Input>
//    		LabelDirectiveHandler(const Input& in, AssemblerDirective& parent) : Parent(in, parent){
//    			parent.action = AssemblerDirectiveAction::DefineLabel;
//    		}
//    		template<typename Input>
//    		void success(const Input& in, AssemblerDirective& parent) {
//    			parent.currentLexeme = getValue();
//    		}
//    	};
//        struct LabelDirective : state<LabelDirectiveHandler, syn::OneArgumentDirective<syn::SymbolLabelDirective, Lexeme>> { };
//    	struct FullImmediateContainer : public syn::NumberOrStringContainer<word> {
//    		public:
//    			using Parent = syn::NumberOrStringContainer<word>;
//    		public:
//                using Parent::Parent;
//
//    			template<typename Input>
//    			void success(const Input& in, AssemblerInstruction& parent) {
//    				parent.fullImmediate = true;
//    				parent.hasLexeme = !isNumber();
//    				if (isNumber()) {
//    					parent.setImmediate(getNumberValue());
//    				} else {
//    					parent.currentLexeme = getStringValue();
//    				}
//    			}
//    			template<typename Input>
//    			void success(const Input& in, AssemblerDirective& parent) {
//    				parent.action = AssemblerDirectiveAction::StoreWord;
//    				parent.fullImmediate = true;
//    				parent.hasLexeme = !isNumber();
//    				if (isNumber()) {
//    					parent.dataValue = getNumberValue();
//    				} else {
//    					parent.currentLexeme = getStringValue();
//    				}
//    			}
//    	};
//        template<typename T, typename State = ImmediateContainer>
//        using LexemeOrNumberDirective = syn::OneArgumentDirective<T, LexemeOrNumber<State>>;
//        struct DeclareDirective : LexemeOrNumberDirective<syn::SymbolWordDirective, FullImmediateContainer> { };
//        struct Directive : state<
//                           AssemblerDirective,
//                           sor<
//                             OrgDirective,
//                             LabelDirective,
//                             CodeDirective,
//                             DataDirective,
//                             DeclareDirective>> { };
//        using Immediate = LexemeOrNumber<FullImmediateContainer>;
//    	struct HalfImmediateContainer : ImmediateContainer {
//    		using ImmediateContainer::ImmediateContainer;
//    		template<typename I>
//    		void success(const I& in, AssemblerInstruction& inst) {
//    			inst.hasLexeme = false;
//    			inst.fullImmediate = false;
//    			inst.source1 = getValue();
//    		}
//    	};
//
//    	template<typename Input>
//    	void ImmediateContainer::success(const Input& in, HalfImmediateContainer& parent) {
//    		parent.setValue(getValue());
//    	}
//
//    	template<typename Input>
//    	void ImmediateContainer::success(const Input& in, AssemblerInstruction& parent) {
//    		parent.setImmediate(getValue());
//    	}
//
//    	template<typename Input>
//    	void ImmediateContainer::success(const Input& in, AssemblerDirective& parent) {
//    		parent.setImmediate(getValue());
//    	}
//        using HalfImmediate = Number<HalfImmediateContainer>;
//
//        template<InstructionGroup op>
//        struct SubTypeSelector {
//            static constexpr bool legalInstructionGroup(InstructionGroup group) noexcept {
//                switch(group) {
//                    case InstructionGroup::Arithmetic:
//                    case InstructionGroup::ConditionalRegister:
//                    case InstructionGroup::Jump:
//                    case InstructionGroup::Move:
//                    case InstructionGroup::Compare:
//                        return true;
//                    default:
//                        return false;
//                }
//            }
//            static_assert(legalInstructionGroup(op), "Instruction group has no subtypes or is unimplemented!");
//            DefApplyGeneric(AssemblerInstruction) {
//                switch(op) {
//                    case InstructionGroup::Arithmetic:
//                        state.operation = (byte)stringToArithmeticOp(in.string());
//                        break;
//                    case InstructionGroup::ConditionalRegister:
//                        state.operation = (byte)stringToConditionRegisterOp(in.string());
//                        break;
//                    case InstructionGroup::Jump:
//                        state.operation = (byte)stringToJumpOp(in.string());
//                        break;
//                    case InstructionGroup::Move:
//                        state.operation = (byte)stringToMoveOp(in.string());
//                        break;
//                    case InstructionGroup::Compare:
//                        state.operation = (byte)stringToCompareOp(in.string());
//                        break;
//                }
//            }
//        };
//
//        template<typename Operation, typename Operands>
//        using GenericInstruction = syn::Instruction<Operation, Operands>;
//        template<typename Operation>
//        using OneGPRInstruction = GenericInstruction<Operation, OneGPR>;
//        template<typename Operation>
//        using TwoGPRInstruction = GenericInstruction<Operation, TwoGPR>;
//        template<typename Operation>
//        using ThreeGPRInstruction = GenericInstruction<Operation, ThreeGPR>;
//
//        // Arithmetic group
//        using ArithmeticSubTypeSelector = SubTypeSelector<InstructionGroup::Arithmetic>;
//        struct OperationArithmeticThreeGPR : sor<
//                                             SymbolAdd,
//                                             SymbolSub,
//                                             SymbolMul,
//                                             SymbolDiv,
//                                             SymbolRem,
//                                             SymbolShiftLeft,
//                                             SymbolShiftRight,
//                                             SymbolBinaryAnd,
//                                             SymbolBinaryOr,
//                                             SymbolBinaryXor,
//                                             SymbolMin,
//                                             SymbolMax> { };
//        // Just extend off of this single type for now
//        struct OperationArithmeticTwoGPR : SymbolBinaryNot { };
//        struct ArithmeticImmediateOperation : sor<
//                                              SymbolAddImmediate,
//                                              SymbolSubImmediate,
//                                              SymbolMulImmediate,
//                                              SymbolDivImmediate,
//                                              SymbolRemImmediate,
//                                              SymbolShiftLeftImmediate,
//                                              SymbolShiftRightImmediate> { };
//    	DefAction(OperationArithmeticThreeGPR) : ArithmeticSubTypeSelector { };
//    	DefAction(OperationArithmeticTwoGPR) : ArithmeticSubTypeSelector { };
//    	DefAction(ArithmeticImmediateOperation) : ArithmeticSubTypeSelector { };
//
//        struct ArithmeticThreeGPRInstruction : ThreeGPRInstruction<OperationArithmeticThreeGPR> { };
//        struct ArithmeticTwoGPRInstruction : TwoGPRInstruction<OperationArithmeticTwoGPR> { };
//
//        struct ArithmeticTwoGPRHalfImmediateInstruction : SeparatedTrinaryThing<
//                                                          ArithmeticImmediateOperation,
//                                                          TwoGPR,
//                                                          HalfImmediate> { };
//        struct ArithmeticInstruction : sor<
//                                       ArithmeticTwoGPRHalfImmediateInstruction,
//                                       ArithmeticTwoGPRInstruction,
//                                       ArithmeticThreeGPRInstruction> { };
//    	DefAction(ArithmeticInstruction) : SetInstructionGroup<InstructionGroup::Arithmetic> { };
//
//        // Move operations
//    	using MoveOpSubTypeSelector = SubTypeSelector<InstructionGroup::Move>;
//        struct OperationMoveOneGPR : sor<
//                                     SymbolMoveToIP,
//                                     SymbolMoveFromIP,
//                                     SymbolMoveToLR,
//                                     SymbolMoveFromLR,
//                                     SymbolRestoreAllRegisters,
//                                     SymbolSaveAllRegisters> { };
//        struct OperationMoveTwoGPR : sor<
//                                     SymbolMove,
//                                     SymbolSwap,
//                                     SymbolLoadIO,
//                                     SymbolStoreIO,
//                                     SymbolLoad,
//                                     SymbolStore,
//                                     SymbolPush,
//                                     SymbolPop> { };
//        struct OperationMoveTwoGPRHalfImmediate : sor<
//                                                  SymbolLoadWithOffset,
//                                                  SymbolStoreWithOffset,
//                                                  SymbolLoadIOWithOffset,
//                                                  SymbolStoreIOWithOffset> { };
//        struct OperationMoveThreeGPR : sor<
//                                       SymbolLoadCode,
//                                       SymbolStoreCode> { };
//        struct OperationMoveGPRImmediate : sor<
//                                           SymbolStoreImmediate,
//                                           SymbolLoadImmediate,
//                                           SymbolSet,
//                                           SymbolPushImmediate> { };
//
//    	DefAction(OperationMoveOneGPR) : MoveOpSubTypeSelector { };
//    	DefAction(OperationMoveTwoGPR) : MoveOpSubTypeSelector { };
//    	DefAction(OperationMoveTwoGPRHalfImmediate) : MoveOpSubTypeSelector { };
//    	DefAction(OperationMoveThreeGPR) : MoveOpSubTypeSelector { };
//    	DefAction(OperationMoveGPRImmediate) : MoveOpSubTypeSelector { };
//
//        struct MoveOneGPRInstruction : OneGPRInstruction<OperationMoveOneGPR> { };
//        struct MoveTwoGPRInstruction : TwoGPRInstruction<OperationMoveTwoGPR> { };
//        struct MoveTwoGPRHalfImmediateInstruction : SeparatedTrinaryThing<
//                                                    OperationMoveTwoGPRHalfImmediate,
//                                                    TwoGPR,
//                                                    HalfImmediate> { };
//        struct MoveThreeGPRInstruction : ThreeGPRInstruction<OperationMoveThreeGPR> { };
//        struct MoveGPRImmediateInstruction : SeparatedTrinaryThing<
//                                             OperationMoveGPRImmediate,
//                                             StatefulDestinationGPR,
//                                             Immediate> { };
//        struct MoveInstruction : sor<
//                                 MoveGPRImmediateInstruction,
//                                 MoveThreeGPRInstruction,
//                                 MoveTwoGPRHalfImmediateInstruction,
//                                 MoveTwoGPRInstruction,
//                                 MoveOneGPRInstruction> { };
//    	DefAction(MoveInstruction) : SetInstructionGroup<InstructionGroup::Move> { };
//
//        // branch
//    	using BranchOpSubTypeSelector = SubTypeSelector<InstructionGroup::Jump>;
//        template<typename Op, typename S>
//        using BranchUnconditional = SeparatedBinaryThing<Op, S>;
//        template<typename Op, typename S>
//        using BranchConditional = SeparatedTrinaryThing<Op, DestinationPredicateRegister, S>;
//
//        struct OperationBranchOneGPR : sor<
//                                       SymbolBranchUnconditionalLink,
//                                       SymbolBranchUnconditional> { };
//        struct OperationBranchImmediate : sor<
//                                          SymbolBranchUnconditionalImmediateLink,
//                                          SymbolBranchUnconditionalImmediate> { };
//        struct OperationBranchConditionalGPR : sor<
//                                               SymbolBranchConditionalLink,
//                                               SymbolBranchConditional
//                                               > { };
//        struct OperationBranchConditionalImmediate : sor<
//                                                     SymbolBranchConditionalImmediateLink,
//                                                     SymbolBranchConditionalImmediate
//                                                     > { };
//        struct OperationBranchConditionalNoArgs : sor<
//                                                  SymbolBranchConditionalLRAndLink,
//                                                  SymbolBranchConditionalLR
//                                                  > { };
//        struct BranchNoArgsInstruction : sor<
//                                         SymbolBranchUnconditionalLRAndLink,
//                                         SymbolBranchUnconditionalLR,
//                                         SymbolReturnFromError> { };
//
//    	DefAction(OperationBranchOneGPR) : BranchOpSubTypeSelector { };
//    	DefAction(OperationBranchImmediate) : BranchOpSubTypeSelector { };
//    	DefAction(OperationBranchConditionalGPR) : BranchOpSubTypeSelector { };
//    	DefAction(OperationBranchConditionalImmediate) : BranchOpSubTypeSelector { };
//    	DefAction(OperationBranchConditionalNoArgs) : BranchOpSubTypeSelector { };
//    	DefAction(BranchNoArgsInstruction) : BranchOpSubTypeSelector { };
//
//        struct BranchOneGPRInstruction : BranchUnconditional<
//                                         OperationBranchOneGPR,
//                                         StatefulDestinationGPR> { };
//        struct BranchImmediateInstruction : BranchUnconditional<
//                                            OperationBranchImmediate,
//                                            Immediate> { };
//        struct BranchConditionalGPRInstruction : BranchConditional<
//                                                 OperationBranchConditionalGPR,
//                                                 Source0GPR> { };
//        struct BranchConditionalImmediateInstruction : BranchConditional<
//                                                       OperationBranchConditionalImmediate,
//                                                       Immediate> { };
//        struct BranchConditionalNoArgsInstruction : SeparatedBinaryThing<
//                                                    OperationBranchConditionalNoArgs,
//                                                    DestinationPredicateRegister> { };
//
//        struct BranchInstruction : sor<
//                                   BranchOneGPRInstruction,
//                                   BranchImmediateInstruction,
//                                   BranchConditionalGPRInstruction,
//                                   BranchConditionalImmediateInstruction,
//                                   BranchConditionalNoArgsInstruction,
//                                   BranchNoArgsInstruction> { };
//    	DefAction(BranchInstruction) : SetInstructionGroup<InstructionGroup::Jump> { };
//
//        // compare operations
//    	using CompareOpSubTypeSelector = SubTypeSelector<InstructionGroup::Compare>;
//        struct CompareRegisterOperation : sor<
//                                          SymbolEq,
//                                          SymbolNeq,
//                                          SymbolLessThan,
//                                          SymbolGreaterThan,
//                                          SymbolLessThanOrEqualTo,
//                                          SymbolGreaterThanOrEqualTo> { };
//        struct CompareImmediateOperation : sor<
//                                           SymbolEqImmediate,
//                                           SymbolNeqImmediate,
//                                           SymbolLessThanImmediate,
//                                           SymbolGreaterThanImmediate,
//                                           SymbolLessThanOrEqualToImmediate,
//                                           SymbolGreaterThanOrEqualToImmediate> { };
//
//    	DefAction(CompareRegisterOperation) : CompareOpSubTypeSelector { };
//    	DefAction(CompareImmediateOperation) : CompareOpSubTypeSelector { };
//
//    	template<typename Operation, typename ... Sources>
//    	using PredicateDestinationInstruction = seq<Operation, Separator, DestinationPredicates, Separator, Sources...>;
//    	struct CompareRegisterInstruction : PredicateDestinationInstruction<
//    										CompareRegisterOperation,
//    										SourceRegisters> { };
//    	struct CompareImmediateInstruction : PredicateDestinationInstruction<
//    										 CompareImmediateOperation,
//    										 Source0GPR,
//    										 Separator,
//    										 HalfImmediate> { };
//        struct CompareInstruction : sor<
//                                    CompareImmediateInstruction,
//                                    CompareRegisterInstruction
//                                    > { };
//    	DefAction(CompareInstruction) : SetInstructionGroup<InstructionGroup::Compare> { };
//
//        // conditional register actions
//    	using ConditionalRegisterSubTypeSelector = SubTypeSelector<InstructionGroup::ConditionalRegister>;
//    	using StatefulSource0Predicate = StatefulRegister<Source0Predicate>;
//        struct OperationPredicateTwoArgs : sor<
//                                           SymbolCRSwap,
//                                           SymbolCRMove> { };
//        struct OperationPredicateOneGPR : sor<
//                                          SymbolSaveCRs,
//                                          SymbolRestoreCRs> { };
//        struct OperationPredicateFourArgs : sor<
//                                            SymbolCRXor,
//                                            SymbolCRAnd,
//                                            SymbolCROr,
//                                            SymbolCRNand,
//                                            SymbolCRNor> { };
//		struct OperationPredicateThreeArgs : SymbolCRNot { };
//
//    	DefAction(OperationPredicateTwoArgs) : ConditionalRegisterSubTypeSelector { };
//    	DefAction(OperationPredicateOneGPR) : ConditionalRegisterSubTypeSelector { };
//    	DefAction(OperationPredicateFourArgs) : ConditionalRegisterSubTypeSelector { };
//		DefAction(OperationPredicateThreeArgs) : ConditionalRegisterSubTypeSelector { };
//
//        struct PredicateInstructionOneGPR : seq<
//                                            OperationPredicateOneGPR,
//    										Separator,
//    										StatefulDestinationGPR,
//                                            Separator,
//                                            Immediate> { };
//        struct PredicateInstructionTwoArgs : seq<
//                                             OperationPredicateTwoArgs,
//    										 Separator,
//    										 DestinationPredicates> { };
//        struct PredicateInstructionThreeArgs : PredicateDestinationInstruction<
//											   OperationPredicateThreeArgs,
//    										   StatefulSource0Predicate> { };
//        struct PredicateInstructionFourArgs : PredicateDestinationInstruction<
//                                              OperationPredicateFourArgs,
//    										  StatefulSource0Predicate,
//    										  Separator,
//    										  StatefulRegister<Source1Predicate>> { };
//        struct PredicateInstruction : sor<
//                                      PredicateInstructionOneGPR,
//                                      PredicateInstructionTwoArgs,
//                                      PredicateInstructionThreeArgs,
//                                      PredicateInstructionFourArgs> { };
//    	DefAction(PredicateInstruction) : SetInstructionGroup<InstructionGroup::ConditionalRegister> { };
//
//        struct Instruction : state<
//                             AssemblerInstruction,
//                             sor<
//                                ArithmeticInstruction,
//                                MoveInstruction,
//                                BranchInstruction,
//                                CompareInstruction,
//                                PredicateInstruction>> { };
//        struct Anything : sor<
//                          Separator,
//                          Instruction,
//                          Directive,
//                          SingleLineComment> { };
//        struct Main : syn::MainFileParser<Anything> { };
//    } // end namespace assembler
//
//
//
//	using ErrorReportingFunction = std::function<void(const std::string&)>;
//	template<typename T, T count>
//	T getRegister(const char* text, ErrorReportingFunction onError) noexcept {
//        static_assert(count > 0, "Can't have zero registers!");
//		T value = 0;
//		std::string wrapper(text);
//		wrapper[0] = '0';
//		std::istringstream ss(wrapper);
//		ss >> std::dec >> value;
//		if (value >= count) {
//			onError("target register is too large!");
//			return static_cast<T>(0);
//		} else if (value < 0) {
//			onError("target register is less than zero!");
//			return static_cast<T>(0);
//		} else {
//			return static_cast<T>(value);
//		}
//	}
//    template<typename T, T count>
//    T getRegister(const std::string& text, ErrorReportingFunction onError) noexcept {
//        return getRegister<T,count>(text.c_str(), onError);
//    }
//	template<typename T>
//	T getBinaryImmediate(const char* text, ErrorReportingFunction onError) noexcept {
//		std::string temp(text);
//		// always replace the second element as it is always the b, x, etc
//		temp[1] = '0';
//		constexpr auto width = iris::bitwidth<T>;
//		static_assert(width <= iris::bitwidth<unsigned long long>, "Please provide custom implementation of getBinaryImmediate since type is too large to fit in a unsigned long long!");
//		std::bitset<width> bits(temp);
//		return static_cast<T>(bits.to_ullong());
//	}
//    template<typename T>
//    T getBinaryImmediate(const std::string& text, ErrorReportingFunction onError) noexcept {
//        return getBinaryImmediate<T>(text.c_str(), onError);
//    }
//
//
//	template<typename T>
//	T getHexImmediate(const char* text, ErrorReportingFunction onError) noexcept {
//		T value = 0;
//		std::string wrapper(text);
//		wrapper[1] = '0';
//		std::istringstream ss(wrapper);
//		ss >> std::hex >> value;
//		return static_cast<T>(value);
//	}
//    template<typename T>
//    T getHexImmediate(const std::string& text, ErrorReportingFunction onError) noexcept {
//        return getHexImmediate<T>(text.c_str(), onError);
//    }
//
//	template<typename T>
//	T getOctalImmediate(const char* text, ErrorReportingFunction onError) noexcept {
//		T value = 0;
//		std::string wrapper(text);
//		wrapper[1] = '0';
//		std::istringstream ss(wrapper);
//		ss >> std::oct >> value;
//		return static_cast<T>(value);
//	}
//    template<typename T>
//    T getOctalImmediate(const std::string& text, ErrorReportingFunction onError) noexcept {
//        return getOctalImmediate<T>(text.c_str(), onError);
//    }
//
//	template<typename T>
//	T getDecimalImmediate(const char* text, ErrorReportingFunction onError) noexcept {
//		T value = 0;
//		std::string wrapper(text);
//		std::istringstream ss(wrapper);
//		ss >> std::dec >> value;
//		return static_cast<T>(value);
//	}
//    template<typename T>
//    T getDecimalImmediate(const std::string& text, ErrorReportingFunction onError) noexcept {
//        return getDecimalImmediate<T>(text.c_str(), onError);
//    }
//	template<char prefix>
//	struct GenericRegister : tao::TAOCPP_PEGTL_NAMESPACE::if_must<tao::TAOCPP_PEGTL_NAMESPACE::one<prefix>, tao::TAOCPP_PEGTL_NAMESPACE::plus<tao::TAOCPP_PEGTL_NAMESPACE::digit>> { };
//
//    using GPR = GenericRegister<'r'>;
//    using FloatRegister = GenericRegister<'f'>;
//    using PredicateRegister = GenericRegister<'p'>;
//
//    struct EndOfLineComment : tao::TAOCPP_PEGTL_NAMESPACE::until<tao::TAOCPP_PEGTL_NAMESPACE::eolf> { };
//    template<char tag>
//    struct SingleLineComment : tao::TAOCPP_PEGTL_NAMESPACE::disable<tao::TAOCPP_PEGTL_NAMESPACE::one<tag>, EndOfLineComment> { };
//
//	struct AsmSeparator : tao::TAOCPP_PEGTL_NAMESPACE::plus<tao::TAOCPP_PEGTL_NAMESPACE::ascii::space> { };
//	struct OptionalSpace : tao::TAOCPP_PEGTL_NAMESPACE::star<tao::TAOCPP_PEGTL_NAMESPACE::ascii::space> { };
//	struct SymbolComma : tao::TAOCPP_PEGTL_NAMESPACE::one<','> { };
//	struct SymbolEqualsSign : tao::TAOCPP_PEGTL_NAMESPACE::one<'='> { };
//	struct SymbolLeftParen : tao::TAOCPP_PEGTL_NAMESPACE::one<'('> { };
//	struct SymbolRightParen : tao::TAOCPP_PEGTL_NAMESPACE::one<')'> { };
//
//    template<typename Rule>
//    struct SingleEntrySequence : tao::TAOCPP_PEGTL_NAMESPACE::seq<Rule> { };
//
//    template<typename C0, typename C1, typename Separator = AsmSeparator>
//    struct TwoPartComponent : tao::TAOCPP_PEGTL_NAMESPACE::seq<C0, Separator, C1> { };
//
//    template<typename State, typename C0, typename C1, typename Separator = AsmSeparator>
//    struct StatefulTwoPartComponent : tao::TAOCPP_PEGTL_NAMESPACE::state<State, TwoPartComponent<C0, C1, Separator>> { };
//
//
//	template<typename First, typename Second, typename Third, typename Sep0 = AsmSeparator, typename Sep1 = AsmSeparator>
//	struct ThreePartComponent : tao::TAOCPP_PEGTL_NAMESPACE::seq<First, Sep0, Second, Sep1, Third> { };
//
//	template<typename State, typename First, typename Second, typename Third, typename Sep0 = AsmSeparator, typename Sep1 = AsmSeparator>
//	struct StatefulThreePartComponent : tao::TAOCPP_PEGTL_NAMESPACE::state<State, ThreePartComponent<First, Second, Third, Sep0, Sep1>> { };
//
//	template<typename First, typename Second, typename Third, typename Fourth, typename S0 = AsmSeparator, typename S1 = AsmSeparator, typename S2 = AsmSeparator>
//	struct FourPartComponent : tao::TAOCPP_PEGTL_NAMESPACE::seq<First, S0, Second, S1, Third, S2, Fourth> { };
//
//	template<typename State, typename First, typename Second, typename Third, typename Fourth, typename S0 = AsmSeparator, typename S1 = AsmSeparator, typename S2 = AsmSeparator>
//	struct StatefulFourPartComponent : tao::TAOCPP_PEGTL_NAMESPACE::state<State, FourPartComponent<First, Second, Third, Fourth, S0, S1, S2>> { };
//
//    template<typename Register>
//	using OneRegister = SingleEntrySequence<Register>;
//    template<typename R0, typename R1, typename Separator = AsmSeparator>
//	using TwoRegister = TwoPartComponent<R0, R1, Separator>;
//    template<typename R0, typename R1, typename R2, typename Separator0 = AsmSeparator, typename Separator1 = AsmSeparator>
//	using ThreeRegister = ThreePartComponent<R0, R1, R2, Separator0, Separator1>;
//
//	template<typename T>
//	struct OptionalSpaceWrapped : tao::TAOCPP_PEGTL_NAMESPACE::seq<OptionalSpace, T, OptionalSpace> { };
//	using EqualsSignSeparator = OptionalSpaceWrapped<SymbolEqualsSign>;
//	using CommaSeparator = OptionalSpaceWrapped<SymbolComma>;
//
//    template<char delimiter, typename SymbolClass>
//    struct GenericNumeral : tao::TAOCPP_PEGTL_NAMESPACE::if_must<tao::TAOCPP_PEGTL_NAMESPACE::istring<'0', delimiter>, tao::TAOCPP_PEGTL_NAMESPACE::plus<SymbolClass>> { };
//
//	template<char delim = 'x'>
//	struct Base16Number : GenericNumeral<delim, tao::TAOCPP_PEGTL_NAMESPACE::xdigit> { };
//	using HexadecimalNumber = Base16Number<'x'>;
//
//	template<char delim = 'b'>
//	struct Base2Number : GenericNumeral<delim, tao::TAOCPP_PEGTL_NAMESPACE::abnf::BIT> { };
//	using BinaryNumber = Base2Number<'b'>;
//
//	struct Base10Number : tao::TAOCPP_PEGTL_NAMESPACE::plus<tao::TAOCPP_PEGTL_NAMESPACE::digit> { };
//
//    template<typename Src0, typename Src1, typename Separator = AsmSeparator>
//    struct SourceRegisters : TwoRegister<Src0, Src1, Separator> { };
//
//    struct Lexeme : tao::TAOCPP_PEGTL_NAMESPACE::identifier { };
//
//
//    template<typename Other>
//    struct LexemeOr : tao::TAOCPP_PEGTL_NAMESPACE::sor<Lexeme, Other> { };
//
//    template<typename Operation, typename Operands, typename Separator = AsmSeparator>
//    struct Instruction : tao::TAOCPP_PEGTL_NAMESPACE::seq<Operation, Separator, Operands> { };
//
//
//    template<typename End, typename Entry>
//    struct MainParser : tao::TAOCPP_PEGTL_NAMESPACE::until<End, tao::TAOCPP_PEGTL_NAMESPACE::must<Entry>> { };
//    template<typename Entry>
//    struct MainFileParser :  MainParser<tao::TAOCPP_PEGTL_NAMESPACE::eof, Entry> { };
//
//	template<typename S, typename ... NumberTypes>
//	struct StatefulNumber : tao::TAOCPP_PEGTL_NAMESPACE::state<S, tao::TAOCPP_PEGTL_NAMESPACE::sor<NumberTypes...>> { };
//
//	template<typename S>
//	struct StatefulNumberAll : StatefulNumber<S, HexadecimalNumber, Base10Number, BinaryNumber> { };
//	/**
//	 * Used to store the final numeric representation of a system word,
//	 * instructions are a multiple of the system word so this class makes it
//	 * simpler to install instructions into a core or whatever else.
//	 */
//	template<typename Word, typename Address = Word>
//	class AssemblerWord {
//		public:
//			AssemblerWord(Address currAddress, Word value, int width = 1) : _width(width), _currAddress(currAddress), _value(value), _isLabel(false) { }
//			AssemblerWord(Address currAddress, const std::string& label, int width = 1) : _width(width), _currAddress(currAddress), _value(0), _isLabel(true), _label(label) { }
//			virtual ~AssemblerWord() { }
//			inline Address getAddress() const noexcept { return _currAddress; }
//			inline Word getValue() const noexcept { return _value; }
//			inline void setValue(Word value) noexcept { _value = value; }
//			inline bool isLabel() const noexcept { return _isLabel; }
//			inline const std::string& getLabel() const noexcept { return _label; }
//			inline int getWidth() const noexcept { return _width; }
//		protected:
//			int _width;
//			Address _currAddress;
//			Word _value;
//			bool _isLabel;
//			bool _resolveLabel;
//			std::string _label;
//	};
//
//    template<typename Word>
//    class NumberContainer {
//        public:
//            template<typename Input, typename ... States>
//            NumberContainer(const Input& in, States&& ...) { }
//            virtual ~NumberContainer() { }
//            Word getValue() const noexcept { return _value; }
//            virtual void setValue(Word value) noexcept { _value = value; }
//        private:
//            Word _value;
//    };
//	class StringContainer {
//		public:
//			template<typename Input, typename ... States>
//			StringContainer(const Input& in, States&& ...) { }
//			virtual ~StringContainer() { }
//			const std::string& getValue() const noexcept { return _value; }
//			virtual void setValue(const std::string& value) noexcept { _value = value; }
//		private:
//			std::string _value;
//	};
//
//
//	template<typename Word>
//	class NumberOrStringContainer : public NumberContainer<Word>, public StringContainer {
//		public:
//			using NumberParent = NumberContainer<Word>;
//			using StringParent = StringContainer;
//		public:
//			template<typename Input, typename ... States>
//			NumberOrStringContainer(const Input& in, States&& ... s) : NumberParent(in, s...), StringParent(in, s...) { }
//			virtual ~NumberOrStringContainer() { }
//			bool isNumber() const noexcept { return _isNumber; }
//			Word getNumberValue() const noexcept { return NumberParent::getValue(); }
//			const std::string& getStringValue() const noexcept { return StringParent::getValue(); }
//			virtual void setValue(Word value) noexcept override {
//				NumberParent::setValue(value);
//				_isNumber = true;
//			}
//			virtual void setValue(const std::string& value) noexcept override {
//				StringParent::setValue(value);
//				_isNumber = false;
//			}
//		private:
//			bool _isNumber = false;
//	};
//
//	template<typename Address>
//	class NameToAddressMapping : public NumberContainer<Address> {
//		public:
//			using NumberContainer<Address>::NumberContainer;
//			const std::string& getTitle() const noexcept { return _title; }
//			void setTitle(const std::string& value) noexcept { _title = value; }
//		private:
//			std::string _title;
//	};
//
//#define DefSymbol(title, str) struct Symbol ## title : public TAOCPP_PEGTL_STRING( str ) { }
//    DefSymbol(OrgDirective, ".org");
//    DefSymbol(LabelDirective, ".label");
//    DefSymbol(WordDirective,  ".word");
//    DefSymbol(DwordDirective, ".dword");
//
//#define DefKeyword(title, str) struct Keyword ## title : public TAOCPP_PEGTL_KEYWORD( str ) { }
//    DefKeyword(Immediate, "immediate");
//
//
//    template<typename Symbol, typename Value, typename Separator = AsmSeparator>
//    struct OneArgumentDirective : TwoPartComponent<Symbol, Value, Separator> { };
//
//    template<typename State, typename Symbol, typename Value, typename Separator = AsmSeparator>
//    struct StatefulOneArgumentDirective : StatefulTwoPartComponent<State, Symbol, Value, Separator> { };
//
//	template<typename State, typename Data>
//	struct WordDirective : iris::StatefulOneArgumentDirective<State, SymbolWordDirective, Data> { };
//
//	template<typename State, typename Data>
//	struct DwordDirective : iris::StatefulOneArgumentDirective<State, SymbolDwordDirective, Data> { };
//
//    template<typename State, typename Number, typename Separator = AsmSeparator>
//    struct StatefulOrgDirective : StatefulOneArgumentDirective<State, SymbolOrgDirective, Number, Separator> { };
//
//    template<typename State, typename Lexeme, typename Separator = AsmSeparator>
//    struct StatefulLabelDirective : StatefulOneArgumentDirective<State, SymbolLabelDirective, Lexeme, Separator> { };
//
//    template<typename State, typename C>
//    struct StatefulSingleEntrySequence : tao::TAOCPP_PEGTL_NAMESPACE::state<State, SingleEntrySequence<C>> { };
//
//
//	template<typename R>
//	struct Action : tao::TAOCPP_PEGTL_NAMESPACE::nothing<R> { };
//
//	void reportError(const std::string& msg);
//
//
//#define DefAction(rule) template<> struct Action< rule >
//#define DefApplyGeneric(type) template<typename Input> static void apply(const Input& in, type & state)
//#define DefApplyGenericEmpty(type) DefApplyGeneric(type) { }
//
//	template<typename Address>
//	class LabelTracker {
//		public:
//			using LabelMap = std::map<std::string, Address>;
//			using LabelMapIterator = typename LabelMap::iterator;
//			using ConstLabelMapIterator = typename LabelMap::const_iterator;
//		public:
//			LabelTracker() { }
//			virtual ~LabelTracker() { }
//			LabelMapIterator labelsEnd() noexcept { return _labels.end(); }
//			LabelMapIterator labelsBegin() noexcept { return _labels.begin(); }
//			ConstLabelMapIterator labelsCBegin() const noexcept { return _labels.cbegin(); }
//			ConstLabelMapIterator labelsCEnd() const noexcept { return _labels.cend(); }
//
//			LabelMapIterator findLabel(const std::string& name) noexcept {
//				return _labels.find(name);
//			}
//
//			ConstLabelMapIterator findLabel(const std::string& name) const noexcept {
//				return _labels.find(name);
//			}
//
//			void registerLabel(const std::string& name, Address value) {
//				_labels.emplace(name, value);
//			}
//			void reset() noexcept {
//				_labels.clear();
//			}
//		protected:
//			LabelMap _labels;
//	};
//
//	template<typename Address>
//	class AddressTracker {
//		public:
//			AddressTracker(Address initialVal) noexcept : _currentAddress(initialVal) { }
//			AddressTracker() noexcept : _currentAddress(0) { }
//			virtual ~AddressTracker() { _currentAddress = 0; }
//			void setCurrentAddress(Address address) noexcept { _currentAddress = address; }
//			void incrementCurrentAddress() noexcept { ++_currentAddress; }
//			void incrementCurrentAddress(Address val) noexcept { _currentAddress += val; }
//			Address getCurrentAddress() const noexcept { return _currentAddress; }
//			void reset() noexcept { _currentAddress = 0; }
//		protected:
//			Address _currentAddress = 0;
//	};
//
//	template<typename T>
//	class FinishedDataTracker {
//		public:
//			using FinishedDataWithIndexFunc = std::function<void(T&, size_t)>;
//			using FinishedDataFunc = std::function<void(T&)>;
//			using FinishedDataStorage = std::vector<T>;
//			FinishedDataTracker() { }
//			virtual ~FinishedDataTracker() { }
//			void applyToFinishedData(FinishedDataWithIndexFunc fn) {
//				size_t index = 0;
//				for (auto& value : _finishedData) {
//					fn(value, index);
//					++index;
//				}
//			}
//			void applyToFinishedData(FinishedDataFunc fn) {
//				for (auto& value : _finishedData) {
//					fn(value);
//				}
//			}
//			void addToFinishedData(T& value) {
//				_finishedData.emplace_back(value);
//			}
//			void copyToFinishedData(T value) {
//				_finishedData.push_back(value);
//			}
//
//			size_t numberOfFinishedItems() const {
//				return _finishedData.size();
//			}
//			void reset() noexcept {
//				_finishedData.clear();
//			}
//		private:
//			FinishedDataStorage _finishedData;
//	};
//
//	struct StashIntoStringContainer {
//		DefApplyGeneric(StringContainer) {
//			state.setValue(in.string());
//		}
//	};
//
//    template<typename T, typename Separator>
//    struct ThenField : tao::TAOCPP_PEGTL_NAMESPACE::seq<Separator, T> { };
//
} // end namespace iris
#endif // end IRIS_CORE_ASSEMBLER_H__
