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
constexpr fuint max_unsigned_integer_string_length = 20;
constexpr fuint max_signed_integer_string_length = 20;

internal u64 unsigned_integer_from_string(ConstString integer_string) {
	fox_assert(1 <= integer_string.length && integer_string.length <= max_unsigned_integer_string_length);
	u64 result = 0;
	u64 exponent = 1;
	for (s64 integer_string_index = (s64)(integer_string.length - 1); integer_string_index >= 0; --integer_string_index) {
		auto digit_character = integer_string[integer_string_index];
		fox_assert('0' <= digit_character && digit_character <= '9');
		u64 digit = digit_character - '0';
		result += digit * exponent;
		exponent *= 10;
	}
	return result;
}

internal s64 signed_integer_from_string(ConstString integer_string) {
	fox_assert(1 <= integer_string.length && integer_string.length <= max_signed_integer_string_length);
	
	//First parse the unsigned part
	auto is_negative = integer_string[0] == '-';
	ConstString unsigned_integer_string = is_negative ? sub_array(integer_string, 1) : integer_string;
	u64 unsigned_integer = unsigned_integer_from_string(unsigned_integer_string);
	
	//Check that the absolute_value is ok for a signed value
	fox_assert(unsigned_integer <= (is_negative ? (1ull << 63) : ((1ull << 63) - 1)));
	
	//Return with appropriate sign
	return is_negative ? -(s64)unsigned_integer : (s64)unsigned_integer;
}

//internal helper for string_from_unsigned_integer
//out_buffer is written to back to front and 
internal char* write_string_from_unsigned_integer(char* buffer_end, u64 integer) {
	auto cursor = buffer_end;
	do {
		cursor--;
		auto digit = (u8)(integer % 10);
		auto digit_character = (char)(digit + '0');
		*cursor = digit_character;
		integer /= 10;
	} while (integer);
	return cursor;
}

internal InternalString<max_unsigned_integer_string_length> string_from_unsigned_integer(u64 integer) {
	InternalString<max_unsigned_integer_string_length> integer_string;
	zero(&integer_string);
	
	auto buffer_end = integer_string.data + max_unsigned_integer_string_length;
	auto buffer_start = write_string_from_unsigned_integer(buffer_end, integer);
	
	auto length = buffer_end - buffer_start;
	memmove(integer_string.data, buffer_start, length);
	integer_string.length = length;
	return integer_string;
}

internal InternalString<max_signed_integer_string_length> string_from_signed_integer(s64 integer) {
	InternalString<max_signed_integer_string_length> integer_string;
	zero(&integer_string);
	
	auto buffer_end = integer_string.data + max_signed_integer_string_length;
	auto integer_absolute_value = absolute_value(integer);
	auto buffer_start = write_string_from_unsigned_integer(buffer_end, integer_absolute_value);
	
	if (integer < 0) {
		buffer_start--;
		*buffer_start = '-';
	}
	
	auto length = buffer_end - buffer_start;
	memmove(integer_string.data, buffer_start, length);
	integer_string.length = length;
	return integer_string;
}

internal void test_string_conversion() {
	fox_assert(unsigned_integer_from_string("0") == 0ull);
	fox_assert(unsigned_integer_from_string("69") == 69ull);
	fox_assert(unsigned_integer_from_string("420") == 420ull);
	fox_assert(unsigned_integer_from_string("18446744073709551615") == 18446744073709551615ull);
	
	fox_assert(string_from_unsigned_integer(0ull) == "0");
	fox_assert(string_from_unsigned_integer(69ull) == "69");
	fox_assert(string_from_unsigned_integer(420ull) == "420");
	fox_assert(string_from_unsigned_integer(18446744073709551615ull) == "18446744073709551615");
	
	fox_assert(signed_integer_from_string("-9223372036854775808") == -9223372036854775808ll);
	fox_assert(signed_integer_from_string("-69") == -69ll);
	fox_assert(signed_integer_from_string("0") == 0ll);
	fox_assert(signed_integer_from_string("420") == 420ll);
	fox_assert(signed_integer_from_string("9223372036854775807") == 9223372036854775807ll);
	
	fox_assert(string_from_signed_integer(-9223372036854775808ll) == "-9223372036854775808");
	fox_assert(string_from_signed_integer(-69ll) == "-69");
	fox_assert(string_from_signed_integer(0ll) == "0");
	fox_assert(string_from_signed_integer(420ll) == "420");
	fox_assert(string_from_signed_integer(9223372036854775807ll) == "9223372036854775807");
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
	write(array, string_from_unsigned_integer(integer), allocator);
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
	
	auto* file = fopen(path, "r+");
	if (file) {
		fseek(file, 0, SEEK_END);
		fuint size = ftell(file);
		if (size) {
			*out_buffer = allocate_array<u8>(size, allocator);
			rewind(file);
			auto bytes_read = fread(out_buffer->data, 1, size, file);
			if (bytes_read == size) {
				file_status = FileStatus::read;
			} else {
				file_status = FileStatus::error;
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

//---Vulpes
LinearAllocator heap_stack;

//---SourceSite
struct SourceSite {
	ConstString path;
	String source;
	u64 line;
	char* line_start;
	u64 column;
};

template<typename AllocatorType>
void write(String* buffer, SourceSite source_site, AllocatorType* allocator) {
	write(buffer, "File: ", allocator);
	write(buffer, source_site.path, allocator);
	write(buffer, " line ", allocator);
	write_uint(buffer, source_site.line, allocator);
	write(buffer, " column ", allocator);
	write_uint(buffer, source_site.column, allocator);
	write(buffer, "\n", allocator);
	
	//print the line
	//@todo print context
	//@todo print error as range rather than just start
	auto line_end = source_site.line_start;
	auto source_end = source_site.source.data + source_site.source.length;
	while (line_end < source_end && *line_end != '\n') {
		line_end++;
	}
	
	String line;
	zero(&line);
	line.data = source_site.line_start;
	line.length = line_end - source_site.line_start;
	write(buffer, line, allocator);
	write(buffer, "\n", allocator);
	
	//print a cursor to the start of the site
	//-------------------------------^
	fox_for (source_index, source_site.column - 1) {
		write(buffer, "-", allocator);
	}
	write(buffer, "^", allocator);
	write(buffer, "\n", allocator);
}

//---Lexer
bool is_space_character(char* character) {
	return *character <= ' ';
}

bool is_operator_character(char* character) {
	return find(ConstString("=+"), *character);
}

bool is_number_character(char* character) {
	return '0' <= *character && *character <= '9';
}

bool is_identifier_character(char* character) {
	return !is_space_character(character)
		&& !is_operator_character(character);
}

//Language keywords/operators 
//Aligns with keyword_strings;
enum class Keyword {
	assign,
	add,
};
ConstString keyword_strings[] = {
	"=",
	"+",
};

enum class TokenKind : u8 {
	invalid,
	keyword,
	integer,
	string,
	identifier,
};

struct Token {
	SourceSite source_site;
	union {
		Keyword keyword_value;
		ConstString identifier_value;
		ConstString string_value;
		u64 integer_value;
	};
	TokenKind kind;
};

template<typename AllocatorType>
void write(String* buffer, Token token, AllocatorType* allocator) {
	switch (token.kind) {
		case TokenKind::keyword: {
			write(buffer, "Keyword (", allocator);
			write(buffer, keyword_strings[(fuint)token.keyword_value], allocator);
			write(buffer, ")", allocator);
		} break;
		
		case TokenKind::integer: {
			write(buffer, "Integer (", allocator);
			write_uint(buffer, token.integer_value, allocator);
			write(buffer, ")", allocator);
		} break;
		
		case TokenKind::string: {
			write(buffer, "String (", allocator);
			write(buffer, token.string_value, allocator);
			write(buffer, ")", allocator);
		} break;
		
		case TokenKind::identifier: {
			write(buffer, "Identifier (", allocator);
			write(buffer, token.identifier_value, allocator);
			write(buffer, ")", allocator);
		} break;
		
		default: {
			fox_unreachable;
		} break;
	}
}

Optional<Array<Token>> lex(String source, ConstString path) {
	fox_assert(source);
	
	Array<Token> tokens;
	zero(&tokens);
	
	//Replace '\r\n' with ' \n'
	fox_for (source_index, source.length) {
		if (source[source_index] == '\r') {
			if (source_index + 1 < source.length && source[source_index + 1] == '\n') {
				source[source_index] = ' ';
				source_index++;
			} else {
				source[source_index] = '\n';
			}
		}
	}
	
	auto cursor = source.data;
	auto source_end = source.data + source.length;
	SourceSite site_cursor;
	zero(&site_cursor);
	site_cursor.path = path;
	site_cursor.source = source;
	site_cursor.line = 1;
	site_cursor.line_start = source.data;
	site_cursor.column = 1;
	
	auto increment_cursor = [&]() {
		cursor++;
		site_cursor.column++;
	};
	
	while (cursor < source_end) {
		//Skip whitespace
		while (is_space_character(cursor)) {
			if (*cursor == '\n') {
				cursor++;
				site_cursor.line++;
				site_cursor.line_start = cursor;
				site_cursor.column = 1;
			} else {
				increment_cursor();
			}
			
			if (cursor == source_end) {
				return tokens;
			}
		}
		
		auto push_new_token = [&](TokenKind kind, SourceSite source_site) {
			auto new_token = push_zero(&tokens, &heap_stack);
			new_token->kind = kind;
			new_token->source_site = source_site;
			return new_token;
		};
		
		//Check for operators
		if (is_operator_character(cursor)) {
			auto operator_start_site = site_cursor;
			auto operator_start = cursor;
			
			while (cursor < source_end && is_operator_character(cursor)) {
				increment_cursor();
			}
			ConstString operator_string;
			zero(&operator_string);
			operator_string.data = operator_start;
			operator_string.length = cursor - operator_start;
			
			auto keyword_index_optional = find(Array<ConstString>(keyword_strings), operator_string);
			if (keyword_index_optional) {
				auto new_token = push_new_token(TokenKind::keyword, operator_start_site);
				new_token->keyword_value = (Keyword)keyword_index_optional.value;
			} else {
				String error_message;
				zero(&error_message);
				write(&error_message, operator_start_site, &heap_stack);
				write(&error_message, "Syntax error: ", &heap_stack);
				write(&error_message, operator_string, &heap_stack);
				write(&error_message, " is not a valid operator.\n", &heap_stack);
				print(error_message);
				return nil;
			}
		}
		
		//Check for numbers
		else if (is_number_character(cursor)) {
			auto number_start_site = site_cursor;
			auto number_start = cursor;
			while (cursor < source_end && is_number_character(cursor)) {
				increment_cursor();
			}
			
			if (cursor == source_end || is_space_character(cursor) || is_operator_character(cursor)) {
				ConstString number_string;
				zero(&number_string);
				number_string.data = number_start;
				number_string.length = cursor - number_start;
				
				u64 integer_value = unsigned_integer_from_string(number_string);
				
				auto new_token = push_new_token(TokenKind::integer, number_start_site);
				new_token->integer_value = integer_value;
			} else {
				String error_message;
				zero(&error_message);
				write(&error_message, site_cursor, &heap_stack);
				write(&error_message, "Syntax error: Expected end-of-file, whitespace, or operator after integer literal.\n", &heap_stack);
				print(error_message);
				return nil;
			}
		}
		
		//Check for string literals
		else if (*cursor == '\'') {
			auto string_start_site = site_cursor;
			
			increment_cursor();
			
			String string_value;
			zero(&string_value);
			
			while (true) {
				if (*cursor == '\\') {
					if (cursor + 1 == source_end) {
						String error_message;
						zero(&error_message);
						write(&error_message, site_cursor, &heap_stack);
						write(&error_message, "Syntax error: end of file after escape sequence start.\n", &heap_stack);
						print(error_message);
						return nil;
					}
					
					if (is_space_character(cursor + 1)) {
						String error_message;
						zero(&error_message);
						write(&error_message, site_cursor, &heap_stack);
						write(&error_message, "Syntax error: whitespace after escape sequence start.\n", &heap_stack);
						print(error_message);
						return nil;
					}
					
					switch (cursor[1]) {
						case '\\': {
							write(&string_value, "\\", &heap_stack);
						} break;
						
						case 'n': {
							write(&string_value, "\n", &heap_stack);
						} break;
						
						case '\'': {
							write(&string_value, "'", &heap_stack);
						} break;
						
						default: {
							String error_message;
							zero(&error_message);
							write(&error_message, site_cursor, &heap_stack);
							write(&error_message, "Syntax error: unrecognized escape sequence \\", &heap_stack);
							write_char(&error_message, cursor[1], &heap_stack);
							write(&error_message, "\n", &heap_stack);
							print(error_message);
							return nil;
						} break;
					}
					
					cursor += 2;
				} else if (*cursor == '\'') {
					increment_cursor();
					break;
				} else {
					write_char(&string_value, *cursor, &heap_stack);
					increment_cursor();
				}
				
				if (cursor >= source_end) {
					String error_message;
					zero(&error_message);
					write(&error_message, string_start_site, &heap_stack);
					write(&error_message, "Syntax error: unexpected end of file inside string.\n", &heap_stack);
					print(error_message);
					return nil;
				}
			}
			
			auto new_token = push_new_token(TokenKind::string, string_start_site);
			new_token->string_value = string_value;
		}
		
		//anything else is considered an identifier character
		else {
			fox_assert(is_identifier_character(cursor));
			
			auto identifier_start_site = site_cursor;
			auto identifier_start = cursor;
			while (cursor < source_end && is_identifier_character(cursor)) {
				increment_cursor();
			}
			
			ConstString identifier;
			zero(&identifier);
			identifier.data = identifier_start;
			identifier.length = cursor - identifier_start;
			
			auto new_token = push_new_token(TokenKind::identifier, identifier_start_site);
			new_token->identifier_value = identifier;
		}
	}
	
	return tokens;
}

int main(int argument_count, char** arguments) {
	test_foxlib();
	
	u64 memory_size = mebibytes(1);
	initialize(&heap_stack, calloc(memory_size, 1), memory_size);
	
	if (argument_count > 1) {
		ConstString file_path = arguments[1];
		Array<u8> file_data;
		zero(&file_data);
		auto file_read_status = read_entire_file(file_path.data, &heap_stack, &file_data);
		switch (file_read_status) {
			case FileStatus::read: {
				auto file_string = fox_interpret_cast(String, file_data);
				auto tokens = lex(file_string, file_path);
				fox_assert(tokens);
				
				String tokenized_file;
				zero(&tokenized_file);
				fox_for (itoken, tokens.value.length) {
					write(&tokenized_file, tokens.value[itoken], &heap_stack);
					write(&tokenized_file, "\n", &heap_stack);
					write(&tokenized_file, tokens.value[itoken].source_site, &heap_stack);
					write(&tokenized_file, "\n", &heap_stack);
				}
				print(tokenized_file);
			} break;
			
			case FileStatus::empty: {
				String error_message;
				zero(&error_message);
				write(&error_message, "File ", &heap_stack);
				write(&error_message, file_path, &heap_stack);
				write(&error_message, " is empty!\n", &heap_stack);
				print(error_message);
			} break;
			
			case FileStatus::error: {
				String error_message;
				zero(&error_message);
				write(&error_message, "File ", &heap_stack);
				write(&error_message, file_path, &heap_stack);
				write(&error_message, " is inaccessible!\n", &heap_stack);
				print(error_message);
			} break;
			
			default: {
				fox_unreachable;
			} break;
		}
	} else {
		print("Usage: vulpes.exe <file to compile>\n");
	}
	
	return 0;
}