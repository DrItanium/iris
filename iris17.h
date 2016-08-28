#ifndef _TARGET_IRIS17_IRIS_H
#define _TARGET_IRIS17_IRIS_H
#include "iris_base.h"
#include "Core.h"
#include "Problem.h"
#include <cstdint>
#include <sstream>
#include <memory>
#include <vector>
#include <tuple>

namespace iris17 {
    using HWord = uint8_t;
    using Word = uint16_t;
    using DWord = uint32_t;
    using RawInstruction = Word; // this is more of a packet!
    using immediate = HWord;
    using RegisterValue = DWord;
    inline constexpr Word encodeWord (byte a, byte b) noexcept;
    inline constexpr RegisterValue encodeRegisterValue(byte a, byte b, byte c, byte d) noexcept;
    inline void decodeWord(Word value, byte* storage) noexcept;
    inline void decodeRegisterValue(RegisterValue value, byte* storage) noexcept;
    enum ArchitectureConstants  {
        RegisterCount = 16,
        SegmentCount = 256,
        AddressMax = 65535 * SegmentCount,
        MaxInstructionCount = 16,
        MaxSystemCalls = 64,
        // unlike iris16 and iris32, there is a limited set of registers with
        // a majority of them marked for explicit usage, instructions
        // themselves are still 16 bits wide but 32bits are extracted per
        // packet.
        InstructionPointer = RegisterCount - 1,
        LinkRegister = RegisterCount - 2,
        StackPointer = RegisterCount - 3,
        ConditionRegister = RegisterCount - 4,
        AddressRegister = RegisterCount - 5,
        ValueRegister = RegisterCount - 6,
    };

#define DefEnum(type, width) \
	enum class type : width { 
#define EndDefEnum(type, width, maxCount) \
		Count, \
	};  \
	static_assert(static_cast<width>(type :: Count) <= static_cast<width>( maxCount ), "Too many " #type " entries defined!");
#define EnumEntry(type) type,

#include "def/iris17/ops.def"
#include "def/iris17/arithmetic_ops.def"
#include "def/iris17/syscalls.def"
#include "def/iris17/compare.enum"
#include "def/iris17/logical.enum"
#include "def/iris17/memory.enum"
#undef DefEnum
#undef EnumEntry
#undef EndDefEnum

    class DecodedInstruction {
        public:
            DecodedInstruction(RawInstruction input) noexcept : _rawValue(input) { }
			DecodedInstruction(const DecodedInstruction&) = delete;
            RawInstruction getRawValue() const noexcept { return _rawValue; }
#define X(title, mask, shift, type, post) type get ## title () const noexcept { return iris::decodeBits<RawInstruction, type, mask, shift>(_rawValue); }
#include "def/iris17/instruction.def"
#undef X
        private:
            RawInstruction _rawValue;
    };

    template<byte bitmask>
        struct SetBitmaskToWordMask {
            static constexpr bool decomposedBits[] = {
				iris::decodeBits<byte, bool, 0b0001, 0>(bitmask),
				iris::decodeBits<byte, bool, 0b0010, 1>(bitmask),
				iris::decodeBits<byte, bool, 0b0100, 2>(bitmask),
				iris::decodeBits<byte, bool, 0b1000, 3>(bitmask)
            };
            static constexpr byte determineMaskValue(bool value) noexcept { return value ? 0xFF : 0x00; }
			static constexpr Word encodeWord(bool upper, bool lower) {
				return iris::encodeBits<Word, byte, 0xFF00, 8>( 
					iris::encodeBits<Word, byte, 0x00FF, 0>(0, 
						determineMaskValue(lower)), 
					determineMaskValue(upper));
			}
			static constexpr Word lowerMask = encodeWord(decomposedBits[1], decomposedBits[0]);
			static constexpr Word upperMask = encodeWord(decomposedBits[3], decomposedBits[2]);
			static constexpr RegisterValue mask = iris::encodeBits<RegisterValue, Word, 0xFFFF0000, 16>( 
					iris::encodeBits<RegisterValue, Word, 0x0000FFFF, 0>(0, lowerMask), upperMask);

            static constexpr bool readLower = decomposedBits[1] || decomposedBits[0];
            static constexpr bool readUpper = decomposedBits[2] || decomposedBits[3];
        };
    template<byte bitmask>
        constexpr RegisterValue mask() noexcept { return SetBitmaskToWordMask<bitmask>::mask; }
    template<byte bitmask>
        constexpr Word lowerMask() noexcept { return SetBitmaskToWordMask<bitmask>::lowerMask; }
    template<byte bitmask>
        constexpr Word upperMask() noexcept { return SetBitmaskToWordMask<bitmask>::upperMask; }
    template<byte bitmask>
        constexpr bool readLower() noexcept { return SetBitmaskToWordMask<bitmask>::readLower; }
    template<byte bitmask>
        constexpr bool readUpper() noexcept { return SetBitmaskToWordMask<bitmask>::readUpper; }

    constexpr RegisterValue bitmask32 =   SetBitmaskToWordMask<0b1111>::mask;
    constexpr RegisterValue bitmask24 =   SetBitmaskToWordMask<0b0111>::mask;
    constexpr RegisterValue upper16Mask = SetBitmaskToWordMask<0b1100>::mask;
    constexpr RegisterValue lower16Mask = SetBitmaskToWordMask<0b0011>::mask;

    RegisterValue getMask(byte bitmask);

    template<bool isConditional, bool ifForm, bool callForm, bool immediateForm>
        struct BranchFlagsEncoder {
			static constexpr byte flags = 
				iris::encodeBits<byte, bool, 0b1000, 3>(
						iris::encodeBits<byte, bool, 0b0100, 2>(
							iris::encodeBits<byte, bool, 0b0010, 1>(
								iris::encodeBits<byte, bool, 0b0001, 0>(0, 
									immediateForm),
								callForm),
							ifForm),
						isConditional);
        };
    template<byte flags>
        struct BranchFlagsDecoder {
            static constexpr bool isImmediate = iris::decodeBits<byte, bool, 0b0001, 0>(flags);
            static constexpr bool isCall = iris::decodeBits<byte, bool, 0b0010, 1>(flags);
            static constexpr bool isIf = iris::decodeBits<byte, bool, 0b0100, 2>(flags);
            static constexpr bool isConditional = iris::decodeBits<byte, bool, 0b1000, 3>(flags);
        };
    using IfJump = BranchFlagsEncoder<false, true, false, false>;
    using IfCall = BranchFlagsEncoder<false, true, true, false>;

    using CallIndirect = BranchFlagsEncoder<false, false, true, false>;
    using CallDirect = BranchFlagsEncoder<false, false, true, true>;

    using JumpDirect = BranchFlagsEncoder<false, false, false, true>;
    using JumpIndirect = BranchFlagsEncoder<false, false, false, false>;

    using ConditionalJumpDirect = BranchFlagsEncoder<true, false, false, true>;
    using ConditionalJumpIndirect = BranchFlagsEncoder<true, false, false, false>;

    int instructionSizeFromImmediateMask(byte bitmask);

    template<byte bitmask>
        static constexpr int instructionSizeFromImmediateMask() {
            return 1 + (readLower<bitmask>() ? 1 : 0) + (readUpper<bitmask>() ? 1 : 0);
        }

    class Core : public iris::Core {
        public:
            Core();
            virtual ~Core();
            virtual void initialize() override;
            virtual void installprogram(std::istream& stream) override;
            virtual void shutdown() override;
            virtual void dump(std::ostream& stream) override;
            virtual void run() override;
            virtual void link(std::istream& stream) override;
        private:
            void dispatch(DecodedInstruction&& inst);
#define X(title, func) void func ();
#include "def/iris17/misc.def"
#undef X
            template<byte index>
                RegisterValue& registerValue() {
                    switch(index) {
#define X(index) case index : return gpr[index];
#include "def/iris17/registers.def"
#undef X
                        default:
                            std::stringstream msg;
                            msg << "Out of range register index: " << index;
                            throw iris::Problem(msg.str());
                    }
                }
            template<Operation op>
                void operation(DecodedInstruction&& inst) {
                    throw iris::Problem("Unimplemented function!");
                }
            template<byte bitmask>
                RegisterValue retrieveImmediate() {
                    RegisterValue lower = 0;
                    RegisterValue upper = 0;
                    if (readLower<bitmask>()) {
						incrementInstructionPointer();
                        lower = getCurrentCodeWord();
                    }
                    if (readUpper<bitmask>()) {
						incrementInstructionPointer();
                        upper = static_cast<RegisterValue>(getCurrentCodeWord()) << 16;
                    }
                    return mask<bitmask>() & ( lower | upper );
                }




			template<ArithmeticOps op>
			struct RequiresDenominatorCheck {
				static constexpr bool value = false;
			};
#define DefFlags(name) \
			template<byte signature> \
			struct name { 
#define EndDefFlags(name) };
#define Component(fieldName, mask, shift, type) static constexpr type fieldName = (iris::decodeBits<byte, type, mask, shift>(signature)); 
#define Field(fieldName, type, value) static constexpr type fieldName = value ;
#include "def/iris17/logical_generic.sig"
#include "def/iris17/arithmetic.sig"
#include "def/iris17/move.sig"
#include "def/iris17/memory.sig"
#include "def/iris17/set.sig"
#undef Field
#undef Component
#undef DefFlags
#undef EndDefFlags
            template<byte signature>
            void setOperation(DecodedInstruction&& inst) {
				using sFlags = SetFlags<signature>;
				registerValue<sFlags::destination>() = registerValue<sFlags::bitmask>();
            }
            template<byte signature>
                void logicalOperation(DecodedInstruction&& inst) {
                    using lflags = LogicalFlags<signature>;
                    // first make sure that the garbage bits haven't been set (some of these are impossible!)
                    if (lflags::immediate && lflags::immediateError) {
                        throw iris::Problem("Illegal bit set for immediate mode logicalOperation!");
                    } else if (!lflags::immediate && lflags::indirectError) {
                        throw iris::Problem("Illegal bits set for indirect mode logicalOperation!");
                    }
                    if (lflags::immediate) {
                        auto &dest = registerValue(inst.getLogicalImmediateDestination());
                        auto immediate = retrieveImmediate<lflags::bitmask>();
                        switch (lflags::immediateType) {
                            case ImmediateLogicalOps::And:
                                dest = dest & immediate;
                                break;
                            case ImmediateLogicalOps::Or:
                                dest = dest | immediate;
                                break;
                            case ImmediateLogicalOps::Nand:
                                dest = ~(dest & immediate);
                                break;
                            case ImmediateLogicalOps::Xor:
                                dest = dest ^ immediate;
                                break;
							default: 
								throw iris::Problem("Illegal immediate logical flag type");
                        }
                    } else {
                        auto &dest = registerValue(inst.getLogicalRegister0());
                        auto src = registerValue(inst.getLogicalRegister1());
                        switch(lflags::indirectType) {
                            case LogicalOps::And:
                                dest = dest & src;
                                break;
                            case LogicalOps::Or:
                                dest = dest | src;
                                break;
                            case LogicalOps::Not:
                                dest = ~dest;
                                break;
                            case LogicalOps::Xor:
                                dest = dest ^ src;
                                break;
                            case LogicalOps::Nand:
                                dest = ~(dest & src);
                                break;
                            default:
                                throw iris::Problem("Illegal indirect logical operation!");
                        }
                    }
                }
            template<byte signature>
                void arithmeticOperation(DecodedInstruction&& inst) {
                    using aflags = ArithmeticFlags<signature>;
                    auto src = aflags::immediate ? inst.getArithmeticImmediate() : registerValue(inst.getArithmeticSource());
					if (aflags::checkDenominator && src == 0) {
                        throw iris::Problem("Denominator is zero!");
                    }
                    auto &dest = registerValue(inst.getArithmeticDestination());
                    switch(aflags::op) {
                        case ArithmeticOps::Add:
                            dest += src;
                            break;
                        case ArithmeticOps::Sub:
                            dest -= src;
                            break;
                        case ArithmeticOps::Mul:
                            dest *= src;
                            break;
                        case ArithmeticOps::Div:
                            dest /= src;
                            break;
                        case ArithmeticOps::Rem:
                            dest %= src;
                            break;
                        default:
                            throw iris::Problem("Illegal arithmetic operation!");
                    }
                }
            template<byte signature>
                void moveOperation(DecodedInstruction&& inst) {
                    using mflags = MoveFlags<signature>;
                    if (mflags::isError) {
                        throw iris::Problem("Illegal move signature!");
                    } else {
                        registerValue(inst.getMoveRegister0()) = registerValue(inst.getMoveRegister1()) & mask<mflags::bitmask>();
                    }
                }
            template<byte bitmask, bool merge>
                void loadOperation(RegisterValue address) {
                    // use the destination field of the instruction to denote offset, thus we need
                    // to use the Address and Value registers
                    RegisterValue lower = 0;
                    RegisterValue upper = 0;
                    if (readLower<bitmask>()) {
                        lower = loadWord(address);
                    }
                    if (readUpper<bitmask>()) {
                        upper = static_cast<RegisterValue>(loadWord(address + 1)) << 16;
                    }
                    auto static constexpr cMask = mask<bitmask>();
                    if (merge) {
                        getValueRegister() = (cMask & (lower | upper)) | (getValueRegister() & ~cMask);
                    } else {
                        getValueRegister() = cMask & (lower | upper);
                    }
                }
            template<byte bitmask>
                void storeOperation(RegisterValue address) {
                    if (readLower<bitmask>()) {
                        auto static constexpr lmask = lowerMask<bitmask>();
                        Word lower = lmask & iris::decodeBits<RegisterValue, Word, lower16Mask, 0>(getValueRegister());
                        auto loader = loadWord(address) & ~lmask;
                        storeWord(address, lower | loader);
                    }
                    if (readUpper<bitmask>()) {
                        auto static constexpr umask = upperMask<bitmask>();
                        Word upper = umask & iris::decodeBits<RegisterValue, Word, upper16Mask, 16>(getValueRegister());
                        auto loader = loadWord(address + 1) & ~umask;
                        storeWord(address + 1, upper | loader);
                    }
                }


            template<byte bitmask>
                void pushOperation(RegisterValue& pushToStack) {
                    // read backwards because the stack grows upward towards zero
                    if (readUpper<bitmask>()) {
						decrementStackPointer();
                        Word upper = upperMask<bitmask>() & iris::decodeBits<RegisterValue, Word, upper16Mask, 16>(pushToStack);
                        storeWord(getStackPointer(), upper);
                    }
                    if (readLower<bitmask>()) {
						decrementStackPointer();
                        Word lower = lowerMask<bitmask>() & iris::decodeBits<RegisterValue, Word, lower16Mask, 0>(pushToStack);
                        storeWord(getStackPointer(), lower);
                    }
                }

            template<byte bitmask>
                void popOperation(RegisterValue& storage) {
                    RegisterValue lower = 0;
                    RegisterValue upper = 0;
                    if (readLower<bitmask>()) {
                        lower = lowerMask<bitmask>() & loadWord(getStackPointer());
						incrementStackPointer();
                    }
                    if (readUpper<bitmask>()) {
                        upper = upperMask<bitmask>() & loadWord(getStackPointer());
						incrementStackPointer();
                    }
                    storage = iris::encodeBits<RegisterValue, Word, upper16Mask, 16>(iris::encodeBits<RegisterValue, Word, lower16Mask, 0>(static_cast<RegisterValue>(0), lower), upper);
                }

            template<MemoryOperation type, byte bitmask>
                void memoryOperation(DecodedInstruction&& inst) {

                    switch (type) {
                        case MemoryOperation::Load:
                            loadOperation<bitmask, false>(getAddressRegister() + inst.getMemoryOffset());
                            break;
                        case MemoryOperation::LoadMerge:
                            loadOperation<bitmask, true>(getAddressRegister() + inst.getMemoryOffset());
                            break;
                        case MemoryOperation::Store:
                            storeOperation<bitmask>(getAddressRegister() + inst.getMemoryOffset());
                            break;
                        case MemoryOperation::Push:
                            pushOperation<bitmask>(registerValue(inst.getMemoryRegister()));
                            break;
                        case MemoryOperation::Pop:
                            popOperation<bitmask>(registerValue(inst.getMemoryRegister()));
                            break;
                        default:
                            throw iris::Problem("Illegal memory operation type!");
                    }
                }
            template<byte signature>
                inline void memoryOperation(DecodedInstruction&& inst) {
                    using mflags = MemoryFlags<signature>;
                    if (mflags::errorState) {
                        throw iris::Problem("Illegally encoded Memory operation!");
                    } else {
                        memoryOperation<mflags::type, mflags::bitmask>(std::move(inst));
                    }
                }

            RegisterValue& registerValue(byte index);
            inline RegisterValue& getInstructionPointer() noexcept     { return registerValue<ArchitectureConstants::InstructionPointer>(); }
            inline RegisterValue& getStackPointer() noexcept           { return registerValue<ArchitectureConstants::StackPointer>(); }
            inline RegisterValue& getConditionRegister() noexcept      { return registerValue<ArchitectureConstants::ConditionRegister>(); }
            inline RegisterValue& getLinkRegister() noexcept           { return registerValue<ArchitectureConstants::LinkRegister>(); }
            inline RegisterValue& getAddressRegister() noexcept        { return registerValue<ArchitectureConstants::AddressRegister>(); }
            inline RegisterValue& getValueRegister() noexcept          { return registerValue<ArchitectureConstants::ValueRegister>(); }
			void incrementInstructionPointer() noexcept;
			void incrementStackPointer() noexcept;
			void decrementStackPointer() noexcept;
            Word getCurrentCodeWord() noexcept;
            void storeWord(RegisterValue address, Word value);
            Word loadWord(RegisterValue address);
            RegisterValue loadRegisterValue(RegisterValue address);
            void storeRegisterValue(RegisterValue address, RegisterValue value);

        private:
            bool execute = true,
                 advanceIp = true;
            RegisterValue gpr[ArchitectureConstants::RegisterCount] = { 0 };
			std::shared_ptr<Word> memory;
    };

    Core* newCore() noexcept;

#define MustCheckDenominator(op) \
    template<> \
    struct Core::RequiresDenominatorCheck<ArithmeticOps:: op> { \
        static constexpr bool value = true; \
    }
    MustCheckDenominator(Div);
    MustCheckDenominator(Rem);
#undef MustCheckDenominator
#define X(title, mask, shift, type, post) \
	constexpr inline Word encode ## title (Word input, type value) noexcept { \
		return iris::encodeBits<Word, type, mask, shift>(input, value); \
	}
#include "def/iris17/instruction.def"
#undef X
    struct InstructionEncoder {
        int currentLine;
        RegisterValue address;
        Operation type;
        bool immediate;
        bool shiftLeft;
        bool isIf;
        bool isCall;
        bool isConditional;
        byte bitmask;
        byte arg0;
        byte arg1;
        bool isLabel;
        std::string labelValue;
        byte subType;
        CompareCombine combineType;
        RegisterValue fullImmediate;
        using Encoding = std::tuple<int, Word, Word, Word>;
        int numWords();
        Encoding encode();
        void clear();
        private:
#define DefEnum(a, b)
#define EndDefEnum(a, b, c)
#define EnumEntry(type) Encoding encode ## type ();
#include "def/iris17/ops.def"
#undef DefEnum
#undef EndDefEnum
#undef EnumEntry
    };
}
#endif
