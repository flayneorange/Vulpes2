#include <iostream>
#include <cstdint>
#include <limits>

//---Library

//---Types
typedef std::int8_t s8;
typedef std::int16_t s16;
typedef std::int32_t s32;
typedef std::int64_t s64;

typedef std::uint8_t u8;
typedef std::uint16_t u16;
typedef std::uint32_t u32;
typedef std::uint64_t u64;

typedef u8 b8;
typedef u16 b16;
typedef u32 b32;
typedef u64 b64;

typedef s64 fint;
typedef u64 fuint;

//---Utilities
//@todo safe memcpy that checks for overlaps? nicer aliases of those functions?
#define fox_for(iterator_name, count) for (fuint iterator_name = 0; iterator_name < count; ++iterator_name)

#define fox_unreachable (*(fuint*)0) = 0
#define fox_assert(condition) if (!(condition)) { fox_unreachable; }
#define fox_array_length(array) (sizeof(array) / sizeof((array)[0]))

#define fox_interpret_cast(type, expression) (*(type*)&(expression))

#define internal static

template<typename Type>
internal void zero(Type* value) {
	memset(value, 0, sizeof(Type));
}

internal u64 kibibytes(u64 amount) {
	fox_assert(amount < (1ull << 55));
	return amount << 10;
}
internal u64 mebibytes(u64 amount) {
	fox_assert(amount < (1ull << 45));
	return amount << 20;
}
internal u64 gibibytes(u64 amount) {
	fox_assert(amount < (1ull << 35));
	return amount << 30;
}
internal u64 tebibytes(u64 amount) {
	fox_assert(amount < (1ull << 25));
	return amount << 40;
}
internal u64 pebibytes(u64 amount) {
	fox_assert(amount < (1ull << 15));
	return amount << 50;
}
internal u64 exbibytes(u64 amount) {
	fox_assert(amount < (1ull << 5));
	return amount << 60;
}

internal u64 absolute_value(s64 integer) {
	return (u64)(integer < 0 ? -integer : integer);
}

void test_utilities() {
	fox_assert(kibibytes(1) == 1024);
	fox_assert(mebibytes(1) == kibibytes(1) * 1024);
	fox_assert(gibibytes(1) == mebibytes(1) * 1024);
	fox_assert(tebibytes(1) == gibibytes(1) * 1024);
	fox_assert(pebibytes(1) == tebibytes(1) * 1024);
	fox_assert(exbibytes(1) == pebibytes(1) * 1024);
}

//---Optional
//This is used to represent an optional without a value
constexpr struct NilType {} nil;

template<typename ValueType>
struct Optional {
	ValueType value;
	b8 has_value;
	
	Optional(ValueType non_nil_value) {
		value = non_nil_value;
		has_value = true;
	}
	Optional(NilType nil_value) {
		(void)nil_value;
		has_value = false;
	}
	
	operator bool() {
		return has_value;
	}
};

void test_optionals() {
	Optional<u64> has_420 = 420;
	Optional<bool> has_false = false;
	Optional<u64> doesnt_have_69 = nil;
	doesnt_have_69.value = 69;
	fox_assert(has_420 && has_420.value == 420);
	fox_assert(has_false && !has_false.value);
	fox_assert(!doesnt_have_69);
}

//---Array
template<typename ElementType, fuint capacity> struct InternalArray;

template<typename ElementType>
struct Array {
	typedef ElementType ElementTypeMember;
	
	u64 length;
	ElementType* data;
	
	Array() = default;
	Array(const Array<ElementType>& other) = default;
	template<fuint c_array_length> Array(ElementType (&c_array)[c_array_length]) {
		data = c_array;
		length = c_array_length;
	}
	template<fuint capacity> Array(InternalArray<ElementType, capacity>& other) {
		data = other.data;
		length = other.length;
	}
	
	ElementType& operator[](fuint index) const {
		fox_assert(index < length);
		return data[index];
	}
	ElementType& get(fuint index) const {
		fox_assert(index < length);
		return data[index];
	}
	
	operator bool() const {
		return length != 0;
	}
};

template<typename ElementType, fuint capacity>
struct InternalArray {
	typedef ElementType ElementTypeMember;
	
	u64 length;
	ElementType data[capacity];
	
	InternalArray() = default;
	InternalArray(const InternalArray<ElementType, capacity>& other) = default;
	InternalArray(const Array<ElementType>& other) {
		array_copy(this, other);
	}
	InternalArray(const Array<const ElementType>& other) {
		array_copy(this, other);
	}
	
	ElementType& operator[](fuint index) {
		fox_assert(index < length);
		return data[index];
	}
	ElementType& get(fuint index) {
		fox_assert(index < length);
		return data[index];
	}
	
	operator bool() const {
		return length != 0;
	}
};

struct String : Array<char> {
	String() = default;
	String(const String& other) = default;
	String(const Array<char>& other) {
		data = other.data;
		length = other.length;
	}
	template<fuint c_string_length> String(char (&c_string)[c_string_length]) {
		data = c_string;
		//Good god C++ why???
		//@todo we have this everywhere and i don't think i want it everywhere
		length = c_string_length - 1;
	}
};

struct ConstString : Array<const char> {
	ConstString() = default;
	ConstString(const ConstString& other) = default;
	ConstString(const Array<const char>& other) {
		data = other.data;
		length = other.length;
	}
	ConstString(const Array<char>& other) {
		data = other.data;
		length = other.length;
	}
	ConstString(const String& other) {
		data = other.data;
		length = other.length;
	}
	template<fuint c_string_length> ConstString(const char (&c_string)[c_string_length]) {
		data = c_string;
		length = c_string_length - 1;
	}
	ConstString(const char* c_string_without_length) {
		data = c_string_without_length;
		length = strlen(c_string_without_length);
	}
};

template<fuint capacity>
struct InternalString : InternalArray<char, capacity> {
	InternalString() = default;
	InternalString(const InternalString& other) = default;
	InternalString(const String& other) {
		array_copy(this, other);
	}
	InternalString(const ConstString& other) {
		array_copy(this, other);
	}
	template<fuint c_string_length> InternalString(const char (&c_string)[c_string_length]) {
		array_copy(this, c_string, c_string_length - 1);
	}
};

template<typename ArrayType>
internal Array<typename ArrayType::ElementTypeMember> sub_array(const ArrayType& array, fuint start, fuint end) {
	fox_assert(start < array.length);
	fox_assert(end <= array.length);
	fox_assert(start <= end);
	
	Array<typename ArrayType::ElementTypeMember> result;
	result.data = array.data + start;
	result.length = end - start;
	return result;
}

template<typename ArrayType>
internal Array<typename ArrayType::ElementTypeMember> sub_array(const ArrayType& array, fuint start) {
	return sub_array(array, start, array.length);
}

//These never allocate, thus if an array is used it must already have a data buffer ready to recieve the copy
template<typename DestinationArrayType>
internal void array_copy(DestinationArrayType* destination, const typename DestinationArrayType::ElementTypeMember* source_data, fuint source_length) {
	memcpy(destination->data, source_data, source_length * sizeof(DestinationArrayType::ElementTypeMember));
	destination->length = source_length;
}

template<typename ElementType, fuint capacity, typename SourceArrayType>
internal void array_copy(InternalArray<ElementType, capacity>* destination, const SourceArrayType& source) {
	fox_assert(capacity >= source.length);
	array_copy(destination, source.data, source.length);
}

template<typename ElementType, fuint capacity, typename SourceArrayType>
internal void array_copy(Array<ElementType>* destination, const SourceArrayType& source) {
	fox_assert(destination.length >= source.length);
	array_copy(destination, source.data, source.length);
}

template<typename ElementType>
internal bool array_equals(const ElementType* data0, fuint length0, const ElementType* data1, fuint length1) {
	return length0 == length1 && memcmp(data0, data1, length0 * sizeof(ElementType)) == 0;
}

template<typename ArrayType>
internal Optional<fuint> find(const ArrayType& array, const typename ArrayType::ElementTypeMember& element) {
	fox_for (index, array.length) {
		if (array[index] == element) {
			return index;
		}
	}
	return nil;
}

//@todo replace with <=> operator when available
template<typename ElementType0, typename ElementType1>
internal bool operator==(const Array<ElementType0>& array0, const Array<ElementType1> array1) {
	return array_equals(array0.data, array0.length, array1.data, array1.length);
}

template<typename ElementType0, fuint capacity0, typename ElementType1, fuint capacity1>
internal bool operator==(const InternalArray<ElementType0, capacity0> array0, const InternalArray<ElementType1, capacity1> array1) {
	return array_equals(array0.data, array0.length, array1.data, array1.length);
}

template<typename ElementType0, typename ElementType1, fuint capacity1>
internal bool operator==(const Array<ElementType0> array0, const InternalArray<ElementType1, capacity1> array1) {
	return array_equals(array0.data, array0.length, array1.data, array1.length);
}

template<typename ElementType0, fuint capacity0, typename ElementType1>
internal bool operator==(const InternalArray<ElementType0, capacity0> array0, const Array<ElementType1> array1) {
	return array_equals(array0.data, array0.length, array1.data, array1.length);
}

template<typename ElementType, typename fuint c_string_length>
internal bool operator==(const Array<ElementType> array, const char (&c_string)[c_string_length]) {
	return array_equals(array.data, array.length, c_string, c_string_length - 1);
}

template<typename ElementType, typename fuint c_string_length>
internal bool operator==(const char (&c_string)[c_string_length], const Array<ElementType> array) {
	return array_equals(array.data, array.length, c_string, c_string_length - 1);
}

template<typename ElementType, fuint capacity, typename fuint c_string_length>
internal bool operator==(const InternalArray<ElementType, capacity> array, const char (&c_string)[c_string_length]) {
	return array_equals(array.data, array.length, c_string, c_string_length - 1);
}

template<typename ElementType, fuint capacity, typename fuint c_string_length>
internal bool operator==(const char (&c_string)[c_string_length], const InternalArray<ElementType, capacity> array) {
	return array_equals(array.data, array.length, c_string, c_string_length - 1);
}

//@todo replace with <=> operator when available
template<typename ElementType0, typename ElementType1>
internal bool operator!=(const Array<ElementType0>& array0, const Array<ElementType1> array1) {
	return !array_equals(array0.data, array0.length, array1.data, array1.length);
}

template<typename ElementType0, fuint capacity0, typename ElementType1, fuint capacity1>
internal bool operator!=(const InternalArray<ElementType0, capacity0> array0, const InternalArray<ElementType1, capacity1> array1) {
	return !array_equals(array0.data, array0.length, array1.data, array1.length);
}

template<typename ElementType0, typename ElementType1, fuint capacity1>
internal bool operator!=(const Array<ElementType0> array0, const InternalArray<ElementType1, capacity1> array1) {
	return !array_equals(array0.data, array0.length, array1.data, array1.length);
}

template<typename ElementType0, fuint capacity0, typename ElementType1>
internal bool operator!=(const InternalArray<ElementType0, capacity0> array0, const Array<ElementType1> array1) {
	return !array_equals(array0.data, array0.length, array1.data, array1.length);
}

template<typename ElementType, typename fuint c_string_length>
internal bool operator!=(const Array<ElementType> array, const char (&c_string)[c_string_length]) {
	return !array_equals(array.data, array.length, c_string, c_string_length - 1);
}

template<typename ElementType, typename fuint c_string_length>
internal bool operator!=(const char (&c_string)[c_string_length], const Array<ElementType> array) {
	return !array_equals(array.data, array.length, c_string, c_string_length - 1);
}

template<typename ElementType, fuint capacity, typename fuint c_string_length>
internal bool operator!=(const InternalArray<ElementType, capacity> array, const char (&c_string)[c_string_length]) {
	return !array_equals(array.data, array.length, c_string, c_string_length - 1);
}

template<typename ElementType, fuint capacity, typename fuint c_string_length>
internal bool operator!=(const char (&c_string)[c_string_length], const InternalArray<ElementType, capacity> array) {
	return !array_equals(array.data, array.length, c_string, c_string_length - 1);
}

internal void print(ConstString message) {
	std::cout.write(message.data, message.length);
}

template<u64 capacity>
internal void print(InternalString<capacity> message) {
	std::cout.write(message.data, message.length);
}

internal void test_arrays() {
	//Test string conversion
	ConstString test_hello = "hello";
	char hello_variable[] = "hello";
	String test_hello_string = hello_variable;
	InternalString<5> test_hello_internal_string = "hello";
	fox_assert(test_hello.length == 5);
	fox_assert(test_hello_string.length == 5);
	fox_assert(test_hello_internal_string.length == 5);
	
	//Test array char/string promotion
	Array<const char> test_hello_char_array;
	test_hello_char_array.data = hello_variable;
	test_hello_char_array.length = 5;
	fox_assert(test_hello_char_array == ConstString(test_hello_char_array));
	
	//Test equality
	fox_assert(test_hello == test_hello && !(test_hello != test_hello));
	fox_assert(test_hello_internal_string == test_hello_internal_string && !(test_hello_internal_string != test_hello_internal_string));
	fox_assert(test_hello == "hello");
	fox_assert(test_hello_string == "hello");
	fox_assert(test_hello_internal_string == "hello");
	fox_assert(test_hello == test_hello_string);
	fox_assert(test_hello_string == test_hello_internal_string);
	fox_assert(test_hello_internal_string == test_hello);
	
	//Test find
	auto h_index = find(test_hello, 'h');
	fox_assert(h_index);
	fox_assert(h_index.value == 0);
	auto e_index = find(test_hello, 'e');
	fox_assert(e_index);
	fox_assert(e_index.value == 1);
	auto o_index = find(test_hello, 'o');
	fox_assert(o_index);
	fox_assert(o_index.value == 4);
	auto z_index = find(test_hello, 'z');
	fox_assert(!z_index);
	
	//Test (internal)array conversions and operations
	InternalArray<u64, 2> two_u64s;
	static_assert(sizeof(two_u64s.data) == 2 * sizeof(u64));
	zero(&two_u64s);
	two_u64s.length = 2;
	two_u64s[0] = 1;
	two_u64s[1] = 2;
	fox_assert(two_u64s == two_u64s && !(two_u64s != two_u64s));
	
	Array<u64> two_u64s_not_internal = two_u64s;
	fox_assert(two_u64s_not_internal == two_u64s_not_internal && !(two_u64s_not_internal != two_u64s_not_internal));
	fox_assert(two_u64s_not_internal.length == 2);
	fox_assert(two_u64s == two_u64s_not_internal);
	
	//Array = internal should set pointers, not copy
	two_u64s[0] = 10;
	fox_assert(two_u64s_not_internal[0] == 10);
	fox_assert(two_u64s_not_internal[1] == 2);
	
	//internal = array should copy because it cant set pointers because there aren't pointers
	InternalArray<u64, 2> two_more_u64s = two_u64s_not_internal;
	two_more_u64s[0] = 1;
	fox_assert(two_u64s[0] == 10);
	fox_assert(two_more_u64s[0] == 1);
	fox_assert(two_more_u64s[1] == 2);
}

//---String conversion
internal u64 unsigned_integer_from_string(ConstString integer_string, u64 base) {
	u64 result = 0;
	u64 exponent = 0;
	u64 new_exponent = 1;
	for (s64 integer_string_index = (s64)(integer_string.length - 1); integer_string_index >= 0; --integer_string_index) {
		//We do this at the loop start since we dont want to make the check if the loop will end
		fox_assert(new_exponent > exponent); //Detect overflow
		exponent = new_exponent;
		
		u64 digit = 0;
		auto digit_character = integer_string[integer_string_index];
		if (digit_character <= '9') {
			fox_assert(digit_character >= '0');
			digit = digit_character - '0';
		} else if (digit_character <= 'Z') {
			fox_assert(digit_character >= 'A');
			digit = digit_character - 'A' + 10;
		} else {
			fox_assert('a' <= digit_character && digit_character <= 'z');
			digit = digit_character - 'a' + 10;
		}
		
		result += digit * exponent;
		new_exponent = exponent * base;
	}
	return result;
}

internal s64 signed_integer_from_string(ConstString integer_string, u64 base) {
	//First parse the unsigned part
	auto is_negative = integer_string[0] == '-';
	ConstString unsigned_integer_string = is_negative ? sub_array(integer_string, 1) : integer_string;
	u64 unsigned_integer = unsigned_integer_from_string(unsigned_integer_string, base);
	
	//Check that the absolute_value is ok for a signed value
	fox_assert(unsigned_integer <= (is_negative ? (1ull << 63) : ((1ull << 63) - 1)));
	
	//Return with appropriate sign
	return is_negative ? -(s64)unsigned_integer : (s64)unsigned_integer;
}

//internal helper for string_from_unsigned_integer
//out_buffer is written to back to front and 
internal char* write_string_from_unsigned_integer(char* buffer_end, u64 integer, u64 base) {
	fox_assert(base <= 255);
	auto cursor = buffer_end;
	do {
		cursor--;
		auto digit = (u8)(integer % base);
		auto digit_character = (digit <= 9) ? ((char)(digit + '0')) : ((char)((digit - 10) + 'a'));
		*cursor = digit_character;
		integer /= base;
	} while (integer);
	return cursor;
}

template<u64 max_unsigned_integer_string_length>
internal InternalString<max_unsigned_integer_string_length> string_from_unsigned_integer(u64 integer, u64 base) {
	InternalString<max_unsigned_integer_string_length> integer_string;
	zero(&integer_string);
	
	auto buffer_end = integer_string.data + max_unsigned_integer_string_length;
	auto buffer_start = write_string_from_unsigned_integer(buffer_end, integer, base);
	
	auto length = buffer_end - buffer_start;
	memmove(integer_string.data, buffer_start, length);
	integer_string.length = length;
	return integer_string;
}

template<u64 max_signed_integer_string_length>
internal InternalString<max_signed_integer_string_length> string_from_signed_integer(s64 integer, u64 base) {
	InternalString<max_signed_integer_string_length> integer_string;
	zero(&integer_string);
	
	auto buffer_end = integer_string.data + max_signed_integer_string_length;
	auto integer_absolute_value = absolute_value(integer);
	auto buffer_start = write_string_from_unsigned_integer(buffer_end, integer_absolute_value, base);
	
	if (integer < 0) {
		buffer_start--;
		*buffer_start = '-';
	}
	
	auto length = buffer_end - buffer_start;
	memmove(integer_string.data, buffer_start, length);
	integer_string.length = length;
	return integer_string;
}

#define generate_string_from_integer_for_base(signage, ArgumentType, base, max_digits) internal InternalString<max_digits> string_from_##signage##_integer_##base(ArgumentType integer) { return string_from_##signage##_integer<max_digits>(integer, base); }

//binary
generate_string_from_integer_for_base(unsigned, u64, 2, 64);
generate_string_from_integer_for_base(signed, s64, 2, 65);

//seximal
generate_string_from_integer_for_base(unsigned, u64, 6, 25);
generate_string_from_integer_for_base(signed, s64, 6, 26);

//octal
generate_string_from_integer_for_base(unsigned, u64, 8, 22);
generate_string_from_integer_for_base(signed, s64, 8, 23);

//decimal
generate_string_from_integer_for_base(unsigned, u64, 10, 20);
generate_string_from_integer_for_base(signed, s64, 10, 20);

//dozenal
generate_string_from_integer_for_base(unsigned, u64, 12, 18);
generate_string_from_integer_for_base(signed, s64, 12, 19);

//hex
generate_string_from_integer_for_base(unsigned, u64, 16, 16);
generate_string_from_integer_for_base(signed, s64, 16, 17);

internal void test_string_conversion() {
	//First test exhaustively with 1 base each which do/do not use letters
	//base 10
	fox_assert(unsigned_integer_from_string("0", 10) == 0ull);
	fox_assert(unsigned_integer_from_string("69", 10) == 69ull);
	fox_assert(unsigned_integer_from_string("420", 10) == 420ull);
	fox_assert(unsigned_integer_from_string("18446744073709551615", 10) == 18446744073709551615ull);
	
	fox_assert(string_from_unsigned_integer_10(0ull) == "0");
	fox_assert(string_from_unsigned_integer_10(69ull) == "69");
	fox_assert(string_from_unsigned_integer_10(420ull) == "420");
	fox_assert(string_from_unsigned_integer_10(18446744073709551615ull) == "18446744073709551615");
	
	fox_assert(signed_integer_from_string("-9223372036854775808", 10) == -9223372036854775808ll);
	fox_assert(signed_integer_from_string("-69", 10) == -69ll);
	fox_assert(signed_integer_from_string("0", 10) == 0ll);
	fox_assert(signed_integer_from_string("420", 10) == 420ll);
	fox_assert(signed_integer_from_string("9223372036854775807", 10) == 9223372036854775807ll);
	
	fox_assert(string_from_signed_integer_10(-9223372036854775808ll) == "-9223372036854775808");
	fox_assert(string_from_signed_integer_10(-69ll) == "-69");
	fox_assert(string_from_signed_integer_10(0ll) == "0");
	fox_assert(string_from_signed_integer_10(420ll) == "420");
	fox_assert(string_from_signed_integer_10(9223372036854775807ll) == "9223372036854775807");
	
	//base 16
	fox_assert(unsigned_integer_from_string("0", 16) == 0ull);
	fox_assert(unsigned_integer_from_string("45", 16) == 69ull);
	fox_assert(unsigned_integer_from_string("1a4", 16) == 420ull);
	fox_assert(unsigned_integer_from_string("FFFFFFFFFFFFFFFF", 16) == 18446744073709551615ull);
	
	fox_assert(string_from_unsigned_integer_16(0ull) == "0");
	fox_assert(string_from_unsigned_integer_16(69ull) == "45");
	fox_assert(string_from_unsigned_integer_16(420ull) == "1a4");
	fox_assert(string_from_unsigned_integer_16(18446744073709551615ull) == "ffffffffffffffff");
	
	fox_assert(signed_integer_from_string("-8000000000000000", 16) == -9223372036854775808ll);
	fox_assert(signed_integer_from_string("-45", 16) == -69ll);
	fox_assert(signed_integer_from_string("0", 16) == 0ll);
	fox_assert(signed_integer_from_string("1A4", 16) == 420ll);
	fox_assert(signed_integer_from_string("7fffffffffffffff", 16) == 9223372036854775807ll);
	
	fox_assert(string_from_signed_integer_16(-9223372036854775808ll) == "-8000000000000000");
	fox_assert(string_from_signed_integer_16(-69ll) == "-45");
	fox_assert(string_from_signed_integer_16(0ll) == "0");
	fox_assert(string_from_signed_integer_16(420ll) == "1a4");
	fox_assert(string_from_signed_integer_16(9223372036854775807ll) == "7fffffffffffffff");
	
	//Next test maximums and signed minimum to ensure our max digit counts are correct
	//base 2
	fox_assert(unsigned_integer_from_string("1111111111111111111111111111111111111111111111111111111111111111", 2) == 18446744073709551615ull);
	
	fox_assert(string_from_unsigned_integer_2(18446744073709551615ull) == "1111111111111111111111111111111111111111111111111111111111111111");
	
	fox_assert(signed_integer_from_string("-1000000000000000000000000000000000000000000000000000000000000000", 2) == -9223372036854775808ll);
	fox_assert(signed_integer_from_string("111111111111111111111111111111111111111111111111111111111111111", 2) == 9223372036854775807ll);
	
	fox_assert(string_from_signed_integer_2(-9223372036854775808ll) == "-1000000000000000000000000000000000000000000000000000000000000000");
	fox_assert(string_from_signed_integer_2(9223372036854775807ll) == "111111111111111111111111111111111111111111111111111111111111111");
	
	//base 6
	fox_assert(unsigned_integer_from_string("3520522010102100444244423", 6) == 18446744073709551615ull);
	
	fox_assert(string_from_unsigned_integer_6(18446744073709551615ull) == "3520522010102100444244423");
	
	fox_assert(signed_integer_from_string("-1540241003031030222122212", 6) == -9223372036854775808ll);
	fox_assert(signed_integer_from_string("1540241003031030222122211", 6) == 9223372036854775807ll);
	
	fox_assert(string_from_signed_integer_6(-9223372036854775808ll) == "-1540241003031030222122212");
	fox_assert(string_from_signed_integer_6(9223372036854775807ll) == "1540241003031030222122211");
	
	//base 8
	fox_assert(unsigned_integer_from_string("1777777777777777777777", 8) == 18446744073709551615ull);
	
	fox_assert(string_from_unsigned_integer_8(18446744073709551615ull) == "1777777777777777777777");
	
	fox_assert(signed_integer_from_string("-1000000000000000000000", 8) == -9223372036854775808ll);
	fox_assert(signed_integer_from_string("777777777777777777777", 8) == 9223372036854775807ll);
	
	fox_assert(string_from_signed_integer_8(-9223372036854775808ll) == "-1000000000000000000000");
	fox_assert(string_from_signed_integer_8(9223372036854775807ll) == "777777777777777777777");
	
	//base 12
	fox_assert(unsigned_integer_from_string("839365134A2A240713", 12) == 18446744073709551615ull);
	
	fox_assert(string_from_unsigned_integer_12(18446744073709551615ull) == "839365134a2a240713");
	
	fox_assert(signed_integer_from_string("-41A792678515120368", 12) == -9223372036854775808ll);
	fox_assert(signed_integer_from_string("41A792678515120367", 12) == 9223372036854775807ll);
	
	fox_assert(string_from_signed_integer_12(-9223372036854775808ll) == "-41a792678515120368");
	fox_assert(string_from_signed_integer_12(9223372036854775807ll) == "41a792678515120367");
}

//---Allocators
internal fuint align_offset_up(fuint offset, fuint align) {
	fox_assert(align);
	return offset + ((~offset + 1) & (align - 1));
}

struct LinearAllocator {
	u8* start;
	u8* end;
	u8* cursor;
};

struct LinearAllocatorRestorePoint {
	u8* old_cursor;
};

internal void initialize(LinearAllocator* allocator, void* zero_initialized_memory, fuint size) {
	allocator->start = allocator->cursor = (u8*)zero_initialized_memory;
	allocator->end = (u8*)zero_initialized_memory + size;
}

internal void initialize(LinearAllocator* allocator, Array<u8> zero_initialized_memory) {
	initialize(allocator, zero_initialized_memory.data, zero_initialized_memory.length);
}

internal void fox_assert_memory_is_zero(u8* memory, fuint size) {
	fox_for (memory_index, size) {
		fox_assert(!memory[memory_index]);
	}
}

internal void* allocate_block(LinearAllocator* allocator, fuint size, fuint align) {
	fox_assert(align);
	
	allocator->cursor = (u8*)align_offset_up((fuint)allocator->cursor, align);
	auto result = allocator->cursor;
	allocator->cursor += size;
	fox_assert(allocator->cursor < allocator->end);
	fox_assert_memory_is_zero(result, size);
	return (void*)result;
}

internal void* reallocate_block(LinearAllocator* allocator, void* old_block, fuint old_size, fuint new_size, fuint align) {
	fox_assert(align);
	fox_assert(new_size >= old_size);
	
	if ((u8*)old_block + old_size == allocator->cursor) {
		//We should already be aligned since the last allocation should have had the same alignment
		fox_assert(allocator->cursor == (u8*)align_offset_up((fuint)allocator->cursor, align));
		
		auto new_memory = allocator->cursor;
		auto size_difference = new_size - old_size;
		fox_assert_memory_is_zero(new_memory, size_difference);
		allocator->cursor += size_difference;
		return old_block;
	}
	
	auto new_block = allocate_block(allocator, new_size, align);
	memcpy(new_block, old_block, old_size);
	return new_block;
}

internal LinearAllocatorRestorePoint create_restore_point(LinearAllocator* allocator) {
	LinearAllocatorRestorePoint restore_point;
	zero(&restore_point);
	restore_point.old_cursor = allocator->cursor;
	return restore_point;
}

internal void restore(LinearAllocator* allocator, LinearAllocatorRestorePoint restore_point) {
	auto old_cursor = restore_point.old_cursor;
	auto current_cursor = allocator->cursor;
	fox_assert(current_cursor >= old_cursor);
	memset(old_cursor, 0, current_cursor - old_cursor);
	allocator->cursor = old_cursor;
}

template<typename Type, typename AllocatorType>
internal Type* allocate(AllocatorType* allocator) {
	return (Type*)allocate_block(allocator, sizeof(Type), alignof(Type));
}

template<typename Type, typename AllocatorType>
internal Array<Type> allocate_array(fuint length, AllocatorType* allocator) {
	Array<Type> new_array;
	zero(&new_array);
	new_array.data = (Type*)allocate_block(allocator, sizeof(Type) * length, alignof(Type));
	new_array.length = length;
	return new_array;
}

internal void test_allocators() {
	//Test initialization
	InternalArray<u8, 1024> memory;
	zero(&memory);
	memory.length = 1024;
	LinearAllocator test_allocator;
	initialize(&test_allocator, memory);
	fox_assert(test_allocator.start == memory.data);
	fox_assert(test_allocator.start == test_allocator.cursor);
	fox_assert(test_allocator.end == memory.data + memory.length);
	
	//Test allocation
	auto test_restore = create_restore_point(&test_allocator);
	allocate<u32>(&test_allocator);
	fox_assert(test_allocator.cursor == memory.data + 4);
	allocate<u8>(&test_allocator);
	fox_assert(test_allocator.cursor == memory.data + 5);
	allocate<u16>(&test_allocator);
	fox_assert(test_allocator.cursor == memory.data + 8);
	allocate_array<u64>(3, &test_allocator);
	fox_assert(test_allocator.cursor == memory.data + 32);
	
	//Test restore
	restore(&test_allocator, test_restore);
	
	//Test reallocation
	fox_assert(test_allocator.cursor == memory.data);
	auto allocated_memory = allocate<u16>(&test_allocator);
	*allocated_memory = 0xdead;
	auto reallocated_memory = reallocate_block(&test_allocator, allocated_memory, 2, 4, 2);
	fox_assert(allocated_memory == reallocated_memory);
	fox_assert(*allocated_memory == 0xdead);
	
	restore(&test_allocator, test_restore);
	allocated_memory = allocate<u16>(&test_allocator);
	allocate<u16>(&test_allocator);
	reallocated_memory = reallocate_block(&test_allocator, allocated_memory, 2, 4, 2);
	fox_assert(allocated_memory != reallocated_memory);
	fox_assert(reallocated_memory == memory.data + 4);
	fox_assert(test_allocator.cursor == memory.data + 8);
}

//---Dynamic Arrays
template<typename ElementType, typename AllocatorType>
internal void expand(Array<ElementType>* array, fuint count, AllocatorType* allocator) {
	auto old_length = array->length;
	auto new_length = old_length + count;
	array->data = (ElementType*)reallocate_block(allocator, array->data, old_length * sizeof(ElementType), new_length * sizeof(ElementType), alignof(ElementType));
	array->length = new_length;
}

template<typename ArrayType, typename AllocatorType = void>
internal void push(ArrayType* array, typename ArrayType::ElementTypeMember new_element, AllocatorType* allocator = nullptr) {
	auto new_element_index = array->length;
	expand(array, 1, allocator);
	array->get(new_element_index) = new_element;
}

template<typename ArrayType, typename AllocatorType = void>
internal typename ArrayType::ElementTypeMember* push_zero(ArrayType* array, AllocatorType* allocator = nullptr) {
	auto new_element_index = array->length;
	expand(array, 1, allocator);
	zero(&array->get(new_element_index));
	return &array->get(new_element_index);
}

template<typename ArrayType, typename AllocatorType = void>
internal void push_array(ArrayType* array, const typename ArrayType::ElementTypeMember* new_elements_data, fuint new_elements_length, AllocatorType* allocator = nullptr) {
	auto new_element_start_index = array->length;
	expand(array, new_elements_length, allocator);
	memcpy(array->data + new_element_start_index, new_elements_data, new_elements_length * sizeof(ArrayType::ElementTypeMember));
}

template<typename DestinationArrayType, typename SourceArrayType, typename AllocatorType = void>
internal void push_array(DestinationArrayType* array, SourceArrayType new_elements, AllocatorType* allocator = nullptr) {
	push_array(array, new_elements.data, new_elements.length, allocator);
}

template<typename DestinationArrayType, typename SourceArrayType, typename AllocatorType = void>
internal void write(DestinationArrayType* array, SourceArrayType string, AllocatorType* allocator = nullptr) {
	push_array(array, string.data, string.length, allocator);
}

template<typename ArrayType, fuint c_string_length, typename AllocatorType = void>
internal void write(ArrayType* array, const char (&c_string)[c_string_length], AllocatorType* allocator = nullptr) {
	push_array(array, c_string, c_string_length - 1, allocator);
}

template<typename ArrayType, typename AllocatorType = void>
internal void write_char(ArrayType* array, char character, AllocatorType* allocator = nullptr) {
	push(array, character, allocator);
}

template<typename ArrayType, typename AllocatorType = void>
internal void write_uint(ArrayType* array, u64 integer, AllocatorType* allocator = nullptr) {
	write(array, string_from_unsigned_integer_10(integer), allocator);
}

internal void test_dynamic_arrays() {
	u8 test_buffer[1024];
	LinearAllocator test_allocator;
	initialize(&test_allocator, test_buffer, 1024);
	
	//Test basic dynamicness
	Array<u8> test_array0;
	zero(&test_array0);
	push(&test_array0, 0, &test_allocator);
	fox_assert(test_array0.data == test_buffer);
	fox_assert(test_array0.length == 1);
	fox_assert(test_array0[0] == 0);
	push(&test_array0, 1, &test_allocator);
	fox_assert(test_array0.data == test_buffer);
	fox_assert(test_array0.length == 2);
	fox_assert(test_array0[1] == 1);
	
	//Make sure push_array works
	Array<u8> test_array1;
	zero(&test_array1);
	push_array(&test_array1, test_array0, &test_allocator);
	fox_for (index, 2) {
		fox_assert(test_array1[index] == index);
	}
	
	//Make sure reallocation moves things as necessary
	push(&test_array0, 2, &test_allocator);
	fox_assert(test_array0.data != test_buffer);
	fox_assert(test_array0.length == 3);
	fox_for (index, 3) {
		fox_assert(test_array0[index] == index);
	}
	
	//Make sure the same functions work on strings
	String hello_seven;
	zero(&hello_seven);
	write(&hello_seven, "hello", &test_allocator);
	write(&hello_seven, "7", &test_allocator);
	fox_assert(hello_seven == ConstString("hello7"));
}

//---Files
enum class FileStatus {
	invalid,
	read,
	empty,
	error,
};

//TODO: more detailed error messaging
template<typename AllocatorType>
internal FileStatus read_entire_file(const char* path, AllocatorType* allocator, Array<u8>* out_buffer) {
	auto file_status = FileStatus::error;
	
	auto* file = fopen(path, "rb+");
	if (file) {
		fseek(file, 0, SEEK_END);
		fuint size = ftell(file);
		if (size) {
			*out_buffer = allocate_array<u8>(size, allocator);
			rewind(file);
			auto bytes_read = fread(out_buffer->data, 1, size, file);
			if (bytes_read == size) {
				file_status = FileStatus::read;
			} 
		} else {
			file_status = FileStatus::empty;
		}
		
		fclose(file);
	}
	
	return file_status;
}

internal void test_file_io() {
	u8 buffer[128];
	LinearAllocator test_allocator;
	initialize(&test_allocator, Array<u8>(buffer));
	Array<u8> file_contents;
	zero(&file_contents);
	auto status = read_entire_file("FoxLibTestFile", &test_allocator, &file_contents);
	fox_assert(status == FileStatus::read);
	ConstString file_contents_string;
	file_contents_string.data = (const char*)file_contents.data;
	file_contents_string.length = file_contents.length;
	fox_assert(file_contents_string == ConstString("hewwo"));
	zero(&file_contents);
	status = read_entire_file("FoxLibTestEmptyFile", &test_allocator, &file_contents);
	fox_assert(status == FileStatus::empty);
	fox_assert(!file_contents);
	status = read_entire_file("File name that definitely does not exist", &test_allocator, &file_contents);
	fox_assert(status == FileStatus::error);
}

internal void test_foxlib() {
	test_utilities();
	test_optionals();
	test_arrays();
	test_string_conversion();
	test_allocators();
	test_dynamic_arrays();
	test_file_io();
}
