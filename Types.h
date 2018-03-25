#include <cstdint>
#include <climits>

#define INDIRECTOR(a, ...) PRIMITIVE_INDIRECTOR(a, __VA_ARGS__)
#define PRIMITIVE_INDIRECTOR(a, ...) a ## __VA_ARGS__
namespace iris {
	using byte = uint8_t;
	using Integer = int16_t;
	using Address = uint16_t;
	using Word = Address;
	using RawInstruction = uint32_t;
	using Address32 = uint32_t;

	template<typename T, typename R, T mask, T shift = 0>
	constexpr T encodeBits(T value, R newValue) noexcept {
		return (value & ~mask) | ((static_cast<T>(newValue) << shift) & mask);
	}
	template<typename T, typename R>
	constexpr T encodeBits(T value, R newValue, T mask, T shift = 0) noexcept {
		return (value & ~mask) | ((static_cast<T>(newValue) << shift) & mask);
	}


	template<typename T, typename R, T mask, T shift = 0>
	constexpr R decodeBits(T value) noexcept {
		return static_cast<R>((value & mask) >> shift);
	}

    template<typename T, typename R>
    constexpr R decodeBits(T value, T mask, T shift = 0) noexcept {
        return static_cast<R>((value & mask) >> shift);
    }

	template<typename T>
	constexpr std::size_t bitwidth = CHAR_BIT * sizeof(T);

	template<typename T, typename R = T>
	constexpr R binaryAnd(T a, T b) noexcept {
		return static_cast<R>(a & b);
	}
	
	template<>
	constexpr bool binaryAnd<bool, bool>(bool a, bool b) noexcept {
		return a && b;
	}
	
	template<typename T, typename R = T>
	constexpr R binaryOr(T a, T b) noexcept {
		return static_cast<R>(a | b);
	}
	
	template<>
	constexpr bool binaryOr<bool, bool>(bool a, bool b) noexcept {
		return a || b;
	}
	
	
	template<typename T, typename R = T>
	constexpr R binaryNot(T a) noexcept {
		return static_cast<R>(~a);
	}
	
	template<>
	constexpr bool binaryNot<bool, bool>(bool a) noexcept {
		return !a;
	}
	
	template<typename T, typename R = T>
	constexpr R binaryXor(T a, T b) noexcept {
		return static_cast<R>(a ^ b);
	}
	
	template<typename T, typename R = T>
	constexpr R binaryNand(T a, T b) noexcept {
		return static_cast<R>(~(a & b));
	}
	
	template<>
	constexpr bool binaryNand(bool a, bool b) noexcept {
		return !(a && b);
	}
	
	template<typename T, typename R = T>
	constexpr R binaryNor(T a, T b) noexcept {
		return ~(a | b);
	}
	
	template<>
	constexpr bool binaryNor(bool a, bool b) noexcept {
		return !(a || b);
	}
	
	template<typename T, typename R = T>
	constexpr R circularShiftLeft(T value, T shift) noexcept {
	    constexpr T width = bitwidth<T> - 1;
	    // taken from the wikipedia entry on circular shifts
	    return static_cast<R>((value << shift) | (value >> ((-shift) & width)));
	}
	
	template<typename T, typename R = T>
	constexpr R circularShiftRight(T value, T shift) noexcept {
	    constexpr T width = bitwidth<T> - 1;
	    // taken from the wikipedia entry on circular shifts
		return static_cast<R>( (value >> shift) | (value << ((-shift) & width)));
	}
	/**
	 * Checks to see if the given value is even.
	 * @param value the value to check evenness against
	 * @return boolean signifying that the given input is even.
	 * @tparam T the type of the thing to check to see if it is even.
	 */
	template<typename T>
	constexpr bool isEven(T value) noexcept {
	    return (value & 1) == 0;
	}
	
	/**
	 * Checks to see if the given value is odd.
	 * @param value the value to check oddness against
	 * @return boolean signifying that the given input is odd.
	 * @tparam T the type of the thing to check to see if it is odd.
	 */
	template<typename T>
	constexpr bool isOdd(T value) noexcept {
		return !isEven<T>(value);
	}
} // end namespace iris
