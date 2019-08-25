#include <iostream>
#include <cstdint>

typedef std::int8_t s8;
typedef std::int16_t s16;
typedef std::int32_t s32;
typedef std::int64_t s64;

typedef std::uint8_t u8;
typedef std::uint16_t u16;
typedef std::uint32_t u32;
typedef std::uint64_t u64;

typedef s64 fint;
typedef u64 fuint;

template<typename ElementType>
struct Array {
	typedef ElementType ElementTypeMember;
	
	ElementType* data;
	u64 length;
	
	Array() = default;
	Array(const Array<ElementType>& other) = default;
	template<fuint c_array_length> Array(ElementType (&c_array)[c_array_length]) {
		data = c_array;
		length = c_array_length;
	}
};

typedef Array<const char> ConstString;

void print(ConstString message) {
	std::cout << message.data;
}

int main() {
	print("hello world!\n");
}