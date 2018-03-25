#include <cstdint>

namespace iris {
	using byte = uint8_t;
	using Integer = int16_t;
	using Address = uint16_t;
	using Word = Address;
	using RawInstruction = uint32_t;

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

} // end namespace iris
