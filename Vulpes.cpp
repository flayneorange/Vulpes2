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

#define fox_assert(condition) if (!(condition)) { (*(fuint*)0) = 0; }

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
	
	ElementType& operator[](fuint index) {
		fox_assert(index < length);
		return data[index];
	}
	ElementType& get(fuint index) {
		fox_assert(index < length);
		return data[index];
	}
};

struct ConstString : Array<const char> {
	ConstString() = default;
	ConstString(const ConstString& other) = default;
	template<fuint c_string_length> ConstString(const char (&c_string)[c_string_length]) {
		data = c_string;
		//Good god C++ why???
		length = c_string_length - 1;
	}
};

void print(ConstString message) {
	//@bug null terminate data or it will crash!!
	std::cout << message.data;
}

void library_test() {
	ConstString c_string_conversion_test = "hi";
	fox_assert(c_string_conversion_test[0] == 'h');
	fox_assert(c_string_conversion_test[1] == 'i');
	fox_assert(c_string_conversion_test.length == 2);
}

int main() {
	library_test();
	print("hello world!\n");
	return 0;
}