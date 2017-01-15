// iris_assembler rewritten to use pegtl
#include <sstream>
#include <typeinfo>
#include <iostream>
#include <map>
#include "syn_base.h"
#include "syn_asm_base.h"
#include "Problem.h"
#include "iris.h"
#include <pegtl.hh>
#include <pegtl/analyze.hh>
#include <pegtl/file_parser.hh>
#include <pegtl/contrib/raw_string.hh>
#include <pegtl/contrib/abnf.hh>
#include <vector>

namespace iris {
	void reportError(const std::string& msg) {
		throw syn::Problem(msg);
	}
	struct AssemblerData {
		AssemblerData() : instruction(false), address(0) { }
		bool instruction;
		word address;
		word dataValue;

		byte group;
		byte operation;
		byte destination;
		byte source0;
		byte source1;
		bool hasLexeme;
		std::string currentLexeme;
		bool getLow, getHigh;
		void reset() noexcept;
	};
	void AssemblerData::reset() noexcept {
		instruction = false;
		address = 0;
		dataValue = 0;
		group = 0;
		operation = 0;
		destination = 0;
		source0 = 0;
		source1 = 0;
		hasLexeme = false;
		currentLexeme.clear();
		getLow = false;
		getHigh = false;
	}
	struct AssemblerState {
		AssemblerState() : currentDataIndex(0), currentCodeIndex(0), inData(false) { }
		word currentDataIndex;
		word currentCodeIndex;
		bool inData;
		word temporaryWord;
		byte temporaryByte;
		AssemblerData current;
		Core core;
		std::map<std::string, word> labelMap;
		std::vector<AssemblerData> finishedData;
		void resetCurrentData() noexcept;
		void setImmediate(word value) noexcept;
		void setHalfImmediate(byte value) noexcept;
		void setHiHalfImmediate(word value) noexcept;
		void setLoHalfImmediate(word value) noexcept;
		void setGroup(InstructionGroup value) noexcept;
		template<typename T>
		void setOperation(T value) noexcept {
			current.operation = static_cast<byte>(value);
		}
		bool inCodeSection() const noexcept;
		bool inDataSection() const noexcept;
		void nowInCodeSection() noexcept;
		void nowInDataSection() noexcept;
		void setCurrentAddress(word value) noexcept;
		void registerLabel(const std::string& label) noexcept;
		word getCurrentAddress() noexcept;
		void incrementCurrentAddress() noexcept;
		void saveToFinished() noexcept;
	};
    template<typename Rule > struct Action : public pegtl::nothing<Rule> { };
#define DefAction(rule) template<> struct Action < rule > 
#define DefApply template<typename Input> static void apply(const Input& in, AssemblerState& state) 
    struct Comment : public pegtl::until<pegtl::eolf> { };
    struct SingleLineComment : public pegtl::disable<pegtl::one<';'>, Comment> { };
	struct Separator : public pegtl::plus<pegtl::ascii::space> { };
	template<char prefix>
	struct GenericRegister : public pegtl::if_must<pegtl::one<prefix>, pegtl::plus<pegtl::digit>> { };
    struct GeneralPurposeRegister : public GenericRegister<'r'> { };
	DefAction(GeneralPurposeRegister) {
		DefApply {
			state.temporaryByte = syn::getRegister<word, ArchitectureConstants::RegisterCount>(in.string(), reportError);
		}
	};
    struct PredicateRegister : public GenericRegister<'p'> { };
	DefAction(PredicateRegister) {
		DefApply {
			state.temporaryByte = syn::getRegister<word, ArchitectureConstants::ConditionRegisterCount>(in.string(), reportError);
		}
	};

    struct DestinationGPR : public pegtl::seq<GeneralPurposeRegister> { };
	DefAction(DestinationGPR) {
		DefApply {
			state.current.destination = state.temporaryByte;
		}
	};
    struct Source0GPR : public pegtl::seq<GeneralPurposeRegister> { };
	DefAction(Source0GPR) {
		DefApply {
			state.current.source0 = state.temporaryByte;
		}
	};
    struct Source1GPR : public pegtl::seq<GeneralPurposeRegister> { };
	DefAction(Source1GPR) {
		DefApply {
			state.current.source1 = state.temporaryByte;
		}
	};
    struct SourceRegisters : public pegtl::seq<Source0GPR, Separator, Source1GPR> { };
    struct OneGPR : public pegtl::seq<DestinationGPR> { };
    struct TwoGPR : public pegtl::seq<DestinationGPR, Separator, Source0GPR> { };
    struct ThreeGPR : public pegtl::seq<DestinationGPR, Separator, SourceRegisters> { };
    struct DestinationPredicateRegister : public pegtl::seq<PredicateRegister> { };
	DefAction(DestinationPredicateRegister) {
		DefApply {
			state.current.destination = iris::encodeLower4Bits(state.current.destination, state.temporaryByte);
		}
	};
    struct DestinationPredicateInverseRegister : public pegtl::seq<PredicateRegister> { };
	DefAction(DestinationPredicateInverseRegister) {
		DefApply {
			state.current.destination = iris::encodeUpper4Bits(state.current.destination, state.temporaryByte);
		}
	};
    struct DestinationPredicate : public pegtl::seq<DestinationPredicateRegister, Separator, DestinationPredicateInverseRegister> { };
    struct Source0Predicate : public pegtl::seq<PredicateRegister> { };
	DefAction(Source0Predicate) {
		DefApply {
			state.current.source0 = iris::encodeLower4Bits(state.current.source0, state.temporaryByte);
		}
	};
    struct Source1Predicate : public pegtl::seq<PredicateRegister> { };
	DefAction(Source1Predicate) {
		DefApply {
			state.current.source0 = iris::encodeUpper4Bits(state.current.source0, state.temporaryByte);
		}
	};

    struct HexadecimalNumber : public pegtl::if_must<pegtl::istring< '0', 'x'>, pegtl::plus<pegtl::xdigit>> { };
	DefAction(HexadecimalNumber) {
		DefApply {
			state.temporaryWord = syn::getHexImmediate<word>(in.string(), reportError);
		}
	};
    struct BinaryNumber : public pegtl::if_must<pegtl::istring<  '0', 'b' >, pegtl::plus<pegtl::abnf::BIT>> { };
	DefAction(BinaryNumber) {
		DefApply {
			state.temporaryWord = syn::getBinaryImmediate<word>(in.string(), reportError);
		}
	};
    struct DecimalNumber : public pegtl::plus<pegtl::digit> { };
	DefAction(DecimalNumber) {
		DefApply {
			state.temporaryWord = syn::getDecimalImmediate<word>(in.string().c_str(), reportError);
		}
	};
    struct Number : public pegtl::sor<HexadecimalNumber, DecimalNumber, BinaryNumber> { };
	template<>
	struct Action<Number> {
		template<typename Input>
		static void apply(const Input& in, AssemblerState& state) {
			state.current.hasLexeme = false;
		}
	};
    struct Lexeme : public pegtl::identifier { };
	template<>
	struct Action<Lexeme> {
		template<typename Input>
		static void apply(const Input& in, AssemblerState& state) {
			state.current.hasLexeme = true;
			state.current.currentLexeme = in.string();
		};
	};
    struct LexemeOrNumber : public pegtl::sor<Lexeme, Number> { };
#define DefSymbol(title, str) \
    struct Symbol ## title : public pegtl_string_t( #str ) { }
    // directives
    DefSymbol(LabelDirective, .label);
    DefSymbol(DataDirective, .data);
    DefSymbol(CodeDirective, .code);
    DefSymbol(OrgDirective, .org);
    DefSymbol(DeclareDirective, .declare);
    DefSymbol(HiDirective, @hi);
    DefSymbol(LoDirective, @lo);

    template<typename T>
    struct ZeroArgumentDirective : public pegtl::seq<T> { };

    template<typename T, typename F>
    struct OneArgumentDirective : public pegtl::seq<T, Separator, F> { };



    struct CodeDirective : public ZeroArgumentDirective<SymbolCodeDirective> { };
	template<>
	struct Action<CodeDirective> {
		template<typename Input>
		static void apply(const Input& in, AssemblerState & state) {
			state.nowInCodeSection();
		}
	};
    struct DataDirective : public ZeroArgumentDirective<SymbolDataDirective> { };

	template<>
	struct Action<DataDirective> {
		template<typename Input>
		static void apply(const Input& in, AssemblerState & state) {
			state.nowInDataSection();
		}
	};

    struct OrgDirective : public OneArgumentDirective<SymbolOrgDirective, Number> {
    };
	template<>
	struct Action<OrgDirective> {
		template<typename I>
		static void apply(const I& in, AssemblerState& state) {
			//state.setCurrentAddress
			state.setCurrentAddress(state.current.dataValue);
		}
	};
    struct LabelDirective : public OneArgumentDirective<SymbolLabelDirective, Lexeme> {
    };
	template<>
	struct Action<LabelDirective> {
		template<typename I>
		static void apply(const I& in, AssemblerState& state) {
			state.registerLabel(state.current.currentLexeme);
			state.resetCurrentData();
		}
	};

    template<typename T>
    struct LexemeOrNumberDirective : public OneArgumentDirective<T, LexemeOrNumber> { };
    struct DeclareDirective : public LexemeOrNumberDirective<SymbolDeclareDirective> {
    };
    struct Directive : public pegtl::sor<OrgDirective, LabelDirective, CodeDirective, DataDirective, DeclareDirective> {

    };
    struct HiDirective : public LexemeOrNumberDirective<SymbolHiDirective> {
    };
    struct LoDirective : public LexemeOrNumberDirective<SymbolLoDirective> {
    };
    struct Immediate : public pegtl::sor<LexemeOrNumber, HiDirective, LoDirective> { };
    struct HalfImmediate : public pegtl::sor<Number, HiDirective, LoDirective> { };
    DefSymbol(Add, add);
    DefSymbol(Sub, sub);
    DefSymbol(Mul, mul);
    DefSymbol(Div, div);
    DefSymbol(Rem, rem);
    DefSymbol(ShiftLeft, shl);
    DefSymbol(ShiftRight, shr);
    DefSymbol(And, and);
    DefSymbol(Or, or);
    DefSymbol(Xor, xor);
    DefSymbol(Min, min);
    DefSymbol(Max, max);
    struct OperationArithmeticThreeGPR : public pegtl::sor<SymbolAdd, SymbolSub, SymbolMul, SymbolDiv, SymbolRem, SymbolShiftLeft, SymbolShiftRight, SymbolAnd, SymbolOr, SymbolXor, SymbolMin, SymbolMax> { };
    struct ArithmeticThreeGPRInstruction : public pegtl::seq<OperationArithmeticThreeGPR, Separator, ThreeGPR> { };
    DefSymbol(Not, not);
    struct OperationArithmeticTwoGPR : public pegtl::sor<SymbolNot> { };
    struct ArithmeticTwoGPRInstruction : public pegtl::seq<OperationArithmeticTwoGPR, Separator, TwoGPR> { };

    DefSymbol(AddImmediate, addi);
    DefSymbol(SubImmediate, subi);
    DefSymbol(MulImmediate, muli);
    DefSymbol(DivImmediate, divi);
    DefSymbol(RemImmediate, remi);
    DefSymbol(ShiftLeftImmediate, shli);
    DefSymbol(ShiftRightImmediate, shri);
    struct OperationArithmeticTwoGPRHalfImmediate : public pegtl::sor< SymbolAddImmediate, SymbolSubImmediate, SymbolMulImmediate, SymbolDivImmediate, SymbolRemImmediate, SymbolShiftLeftImmediate, SymbolShiftRightImmediate> { };
    struct ArithmeticTwoGPRHalfImmediateInstruction : public pegtl::seq<OperationArithmeticTwoGPRHalfImmediate, Separator, TwoGPR, Separator, HalfImmediate> { };

    struct ArithmeticInstruction : public pegtl::sor<ArithmeticTwoGPRHalfImmediateInstruction, ArithmeticTwoGPRInstruction, ArithmeticThreeGPRInstruction> { };

    DefSymbol(MoveToIP, mtip);
    DefSymbol(MoveFromIP, mfip);
    DefSymbol(MoveToLR, mtlr);
    DefSymbol(MoveFromLR, mflr);
    struct OperationMoveOneGPR : public pegtl::sor<SymbolMoveToIP, SymbolMoveFromIP, SymbolMoveToLR, SymbolMoveFromLR> { };
    struct MoveOneGPRInstruction : public pegtl::sor<OperationMoveOneGPR, Separator, OneGPR> { };
    DefSymbol(Move, move);
    DefSymbol(Swap, swap);
    DefSymbol(Load, ld);
    DefSymbol(Store, st);
    DefSymbol(LoadIO, ldio);
    DefSymbol(StoreIO, stio);
    DefSymbol(Push, push);
    DefSymbol(Pop, pop);
    struct OperationMoveTwoGPR : public pegtl::sor<SymbolMove, SymbolSwap, SymbolLoadIO, SymbolStoreIO, SymbolLoad, SymbolStore, SymbolPush, SymbolPop> { };
    struct MoveTwoGPRInstruction : public pegtl::seq<OperationMoveTwoGPR, Separator, TwoGPR> { };
    DefSymbol(LoadWithOffset, ldwo);
    DefSymbol(StoreWithOffset, stwo);
    DefSymbol(LoadIOWithOffset, ldiowo);
    DefSymbol(StoreIOWithOffset, stiowo);
    struct OperationMoveTwoGPRHalfImmediate : public pegtl::sor<SymbolLoadWithOffset, SymbolStoreWithOffset, SymbolLoadIOWithOffset, SymbolStoreIOWithOffset> { };
    struct MoveTwoGPRHalfImmediateInstruction : public pegtl::seq<OperationMoveTwoGPRHalfImmediate, Separator, TwoGPR, Separator, HalfImmediate> { };

    DefSymbol(LoadCode, ldc);
    DefSymbol(StoreCode, stc);
    struct OperationMoveThreeGPR : public pegtl::sor<SymbolLoadCode, SymbolStoreCode> { };
    struct MoveThreeGPRInstruction : public pegtl::seq<OperationMoveThreeGPR, Separator, ThreeGPR> { };

    DefSymbol(PushImmediate, pushi);
    DefSymbol(Set, set);
    DefSymbol(LoadImmediatePrimary, ldi);
    DefSymbol(LoadImmediateSecondary, ldm);
    struct SymbolLoadImmediate : public pegtl::sor<SymbolLoadImmediatePrimary, SymbolLoadImmediateSecondary> { };
    DefSymbol(StoreImmediatePrimary, sti);
    DefSymbol(StoreImmediateAlternateTitle, memset);
    struct SymbolStoreImmediate : public pegtl::sor<SymbolStoreImmediatePrimary, SymbolStoreImmediateAlternateTitle> { };
    struct OperationMoveGPRImmediate : public pegtl::sor<SymbolStoreImmediate, SymbolLoadImmediate, SymbolSet, SymbolPushImmediate> { };

    struct MoveGPRImmediateInstruction : public pegtl::seq<OperationMoveGPRImmediate, Separator, DestinationGPR, Separator, Immediate> { };

    struct MoveInstruction : public pegtl::sor<MoveGPRImmediateInstruction, MoveThreeGPRInstruction, MoveTwoGPRHalfImmediateInstruction, MoveTwoGPRInstruction, MoveOneGPRInstruction> { };

    // branch
	template<typename Op, typename S>
	struct BranchUnconditional : public pegtl::seq<Op, Separator, S> { };
    DefSymbol(BranchUnconditional, b);
    DefSymbol(BranchUnconditionalLink, bl);
    struct OperationBranchOneGPR : public pegtl::sor<SymbolBranchUnconditional, SymbolBranchUnconditionalLink> { };
    struct BranchOneGPRInstruciton : public BranchUnconditional<OperationBranchOneGPR, DestinationGPR> { };
    DefSymbol(BranchUnconditionalImmediate, bi);
    DefSymbol(BranchUnconditionalImmediateLink, bil);
    struct OperationBranchImmediate : public pegtl::sor<SymbolBranchUnconditionalImmediateLink, SymbolBranchUnconditionalImmediate> { };
    struct BranchImmediateInstruction : public BranchUnconditional<OperationBranchImmediate, Immediate> { };

	struct GroupBranchUnconditional : public pegtl::sor<BranchOneGPRInstruciton, BranchImmediateInstruction> { };
    template<typename Op, typename S>
    struct BranchConditional : public pegtl::seq<Op, Separator, DestinationPredicateRegister, Separator, S> { };
    DefSymbol(BranchConditionalTrue, bt);
    DefSymbol(BranchConditionalTrueLink, btl);
    DefSymbol(BranchConditionalFalse, bf);
    DefSymbol(BranchConditionalFalseLink, bfl);
    struct OperationBranchConditionalGPR : public pegtl::sor<SymbolBranchConditionalFalseLink, SymbolBranchConditionalTrueLink, SymbolBranchConditionalFalse, SymbolBranchConditionalTrue> { };
    struct BranchConditionalGPRInstruction : public BranchConditional<OperationBranchConditionalGPR, Source0GPR> { };
    DefSymbol(BranchConditionalTrueImmediate, bit);
    DefSymbol(BranchConditionalTrueImmediateLink, bitl);
    DefSymbol(BranchConditionalFalseImmediate, bif);
    DefSymbol(BranchConditionalFalseImmediateLink, bifl);
    struct OperationBranchConditionalImmediate : public pegtl::sor<SymbolBranchConditionalTrueImmediateLink, SymbolBranchConditionalFalseImmediateLink, SymbolBranchConditionalTrueImmediate, SymbolBranchConditionalFalseImmediate> { };
    struct BranchConditionalImmediateInstruction : public BranchConditional<OperationBranchConditionalImmediate, Immediate> { };
    DefSymbol(IfThenElseTrue, ift);
    DefSymbol(IfThenElseFalse, iff);
    DefSymbol(IfThenElseTrueLink, iftl);
    DefSymbol(IfThenElseFalseLink, iffl);
    struct OperationBranchIfStatement : public pegtl::sor<SymbolIfThenElseTrue, SymbolIfThenElseTrueLink, SymbolIfThenElseFalse, SymbolIfThenElseFalseLink> { };
    struct BranchIfInstruction : public BranchConditional<OperationBranchIfStatement, SourceRegisters> { };
    DefSymbol(BranchLRTrue, blrt);
    DefSymbol(BranchLRTrueLink, blrtl);
    DefSymbol(BranchLRFalse, blrf);
    DefSymbol(BranchLRFalseLink, blrfl);
    struct OperationBranchCondtionalNoArgs : public pegtl::sor<SymbolBranchLRTrue, SymbolBranchLRTrueLink, SymbolBranchLRFalse, SymbolBranchLRFalseLink> { };
    struct BranchConditionalNoArgsInstruction : public pegtl::seq<OperationBranchCondtionalNoArgs, Separator, DestinationPredicateRegister> { };
    DefSymbol(BranchLR, blr);
    DefSymbol(BranchLRLink, blrl);
    struct OperationBranchNoArgs : public pegtl::sor<SymbolBranchLR,  SymbolBranchLRLink> { };
    struct BranchNoArgsInstruction : public pegtl::seq<OperationBranchNoArgs> { };

    struct BranchInstruction : public pegtl::sor<GroupBranchUnconditional, BranchConditionalGPRInstruction, BranchConditionalImmediateInstruction, BranchIfInstruction, BranchConditionalNoArgsInstruction, BranchNoArgsInstruction> { };

    // compare operations
    DefSymbol(Eq, eq);
    DefSymbol(Neq, ne);
    DefSymbol(Lt, lt);
    DefSymbol(Gt, gt);
    DefSymbol(Le, le);
    DefSymbol(Ge, ge);
    DefSymbol(EqImmediate, eqi);
    DefSymbol(NeqImmediate, nei);
    DefSymbol(LtImmediate, lti);
    DefSymbol(GtImmediate, gti);
    DefSymbol(LeImmediate, lei);
    DefSymbol(GeImmediate, gei);
    struct CompareRegisterOperation : public pegtl::sor< SymbolEq, SymbolNeq, SymbolLt, SymbolGt, SymbolLe, SymbolGe> { };
    struct CompareImmediateOperation : public pegtl::sor<SymbolEqImmediate, SymbolNeqImmediate, SymbolLtImmediate, SymbolGtImmediate, SymbolLeImmediate, SymbolGeImmediate> { };
    struct CompareRegisterInstruction : public pegtl::seq<CompareRegisterOperation, Separator, DestinationPredicate, Separator, SourceRegisters> { };
    struct CompareImmediateInstruction : public pegtl::seq<CompareImmediateOperation, Separator, DestinationPredicate, Separator, Source0GPR, Separator, HalfImmediate> { };
    struct CompareInstruction : public pegtl::sor<CompareRegisterInstruction, CompareImmediateInstruction> { };

    // conditional register actions
    DefSymbol(SaveCRs, svcr);
    DefSymbol(RestoreCRs, recr);
    DefSymbol(CRXor, crxor);
    DefSymbol(CRNot, crnot);
    DefSymbol(CRAnd, crand);
    DefSymbol(CROr, cror);
    DefSymbol(CRNand, crnand);
    DefSymbol(CRNor, crnor);
    DefSymbol(CRSwap, crswap);
    DefSymbol(CRMove, crmove);
    struct OperationPredicateTwoArgs : public pegtl::sor<SymbolCRSwap, SymbolCRMove> { };
    struct OperationPredicateThreeArgs : public pegtl::sor<SymbolCRNot> { };
    struct OperationPredicateOneGPR : public pegtl::sor<SymbolSaveCRs, SymbolRestoreCRs> { };
    struct OperationPredicateFourArgs : public pegtl::sor<SymbolCRXor, SymbolCRAnd, SymbolCROr, SymbolCRNand, SymbolCRNor> { };
    struct PredicateInstructionOneGPR : public pegtl::seq<OperationPredicateOneGPR, DestinationGPR> { };
    struct PredicateInstructionTwoArgs : public pegtl::seq<OperationPredicateTwoArgs, DestinationPredicate> { };
    struct PredicateInstructionThreeArgs : public pegtl::seq<OperationPredicateThreeArgs, DestinationPredicate, Source0Predicate> { };
    struct PredicateInstructionFourArgs : public pegtl::seq<OperationPredicateFourArgs, DestinationPredicate, Source0Predicate, Source1Predicate> { };
    struct PredicateInstruction : public pegtl::sor<PredicateInstructionTwoArgs, PredicateInstructionThreeArgs, PredicateInstructionFourArgs> { };



    // registers
    // basic define

#undef DefSymbol
    struct Instruction : public pegtl::sor<ArithmeticInstruction, MoveInstruction, BranchInstruction, CompareInstruction, PredicateInstruction> { };
	template<>
	struct Action<Instruction> {
		template<typename I>
		static void apply(const I& in, AssemblerState& state) {
			std::cout << "Instruction fired!" << std::endl;
			state.saveToFinished();
			state.incrementCurrentAddress();
		}
	};
    struct Statement : public pegtl::sor<Instruction, Directive> { };

	struct Anything : public pegtl::sor<Separator, SingleLineComment,Statement> { };

	struct Main : public pegtl::until<pegtl::eof, pegtl::must<Anything>> { };

	void AssemblerState::setCurrentAddress(word value) noexcept {
		if (inData) {
			currentDataIndex = value;
		} else {
			currentCodeIndex = value;
		}
	}
	void AssemblerState::nowInCodeSection() noexcept {
		inData = false;
	}
	void AssemblerState::nowInDataSection() noexcept {
		inData = true;
	}
	bool AssemblerState::inCodeSection() const noexcept {
		return !inData;
	}
	bool AssemblerState::inDataSection() const noexcept {
		return inData;
	}
	void AssemblerState::setImmediate(word value) noexcept {
		current.source0 = syn::getLowerHalf<word>(value);
		current.source1 = syn::getUpperHalf<word>(value);
	}
	void AssemblerState::setGroup(InstructionGroup value) noexcept {
		current.group = static_cast<byte>(value);
	}
	void AssemblerState::setHalfImmediate(byte value) noexcept {
		current.source1 = value;
	}
	void AssemblerState::setHiHalfImmediate(word value) noexcept {
		setHalfImmediate(syn::getUpperHalf(value));
	}
	void AssemblerState::setLoHalfImmediate(word value) noexcept {
		setHalfImmediate(syn::getLowerHalf(value));
	}


	void AssemblerState::resetCurrentData() noexcept {
		current.reset();
	}

	void AssemblerState::registerLabel(const std::string& value) noexcept {
		labelMap.emplace(value, getCurrentAddress());
	}
	word AssemblerState::getCurrentAddress() noexcept {
		return inData ? currentDataIndex : currentCodeIndex;
	}
	void AssemblerState::incrementCurrentAddress() noexcept {
		if (inData) {
			++currentDataIndex;
		} else {
			++currentCodeIndex;
		}
	}
	void AssemblerState::saveToFinished() noexcept {
		finishedData.emplace_back(current);
		resetCurrentData();
	}

}

int main(int argc, char** argv) {
    pegtl::analyze<iris::Main>();
	iris::AssemblerState state;
    for(int i = 1; i < argc; ++i) {
		pegtl::file_parser(argv[i]).parse<iris::Main, iris::Action>(state);
    }
	
    return 0;
}
