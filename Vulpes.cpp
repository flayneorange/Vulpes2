#include <iostream>
#include <cstdint>

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

typedef s64 fint;
typedef u64 fuint;

//---Utilities
#define fox_for(iterator_name, count) for (fuint iterator_name = 0; iterator_name < count; ++iterator_name)

#define fox_unreachable (*(fuint*)0) = 0
#define fox_assert(condition) if (!(condition)) { fox_unreachable; }

#define fox_interpret_cast(type, expression) (*(type*)&(expression))

#define internal static

template<typename Type>
internal void zero(Type* value) {
	memset(value, 0, sizeof(Type));
}

#define kibibytes(amount) ((amount) * 1024)
#define mebibytes(amount) (kibibytes(amount) * 1024)
#define gibibytes(amount) (mebibytes(amount) * 1024)
#define tebibytes(amount) (gibibytes(amount) * 1024)
#define pebibytes(amount) (tebibytes(amount) * 1024)
#define exbibytes(amount) (pebibytes(amount) * 1024)
#define zebibytes(amount) (exbibytes(amount) * 1024)
#define yobibytes(amount) (zebibytes(amount) * 1024)

//---Array
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

struct String : Array<char> {
	String() = default;
	String(const String& other) = default;
	template<fuint c_string_length> String(char (&c_string)[c_string_length]) {
		data = c_string;
		//Good god C++ why???
		length = c_string_length - 1;
	}
};

struct ConstString : Array<const char> {
	ConstString() = default;
	ConstString(const ConstString& other) = default;
	ConstString(const String& other) {
		data = other.data;
		length = other.length;
	}
	template<fuint c_string_length> ConstString(const char (&c_string)[c_string_length]) {
		data = c_string;
		//Good god C++ why???
		length = c_string_length - 1;
	}
	ConstString(const char* c_string_without_length) {
		data = c_string_without_length;
		length = strlen(c_string_without_length);
	}
};

template<typename ElementType>
internal bool array_equals(const ElementType* data0, fuint length0, const ElementType* data1, fuint length1) {
	if (length0 == length1) {
		return memcmp(data0, data1, length0 * sizeof(ElementType)) == 0;
	}
	return false;
}

template<typename ElementType0, typename ElementType1>
internal bool operator==(Array<ElementType0> array0, Array<ElementType1> array1) {
	return array_equals(array0.data, array0.length, array1.data, array1.length);
}

template<typename ElementType0, typename ElementType1>
internal bool operator!=(Array<ElementType0> array0, Array<ElementType1> array1) {
	return !array_equals(array0.data, array0.length, array1.data, array1.length);
}

internal void print(ConstString message) {
	std::cout.write(message.data, message.length);
}

internal void test_arrays() {
	ConstString c_string_conversion_test = "hi";
	fox_assert(c_string_conversion_test[0] == 'h');
	fox_assert(c_string_conversion_test[1] == 'i');
	fox_assert(c_string_conversion_test.length == 2);
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

internal void initialize(LinearAllocator* allocator, void* memory, fuint size) {
	allocator->start = allocator->cursor = (u8*)memory;
	allocator->end = (u8*)memory + size;
}

internal void initialize(LinearAllocator* allocator, Array<u8> memory) {
	initialize(allocator, memory.data, memory.length);
}

internal void* allocate_block(LinearAllocator* allocator, fuint size, fuint align) {
	fox_assert(align);
	
	allocator->cursor = (u8*)align_offset_up((fuint)allocator->cursor, align);
	auto result = (void*)allocator->cursor;
	allocator->cursor += size;
	fox_assert(allocator->cursor < allocator->end);
	return result;
}

internal void* reallocate_block(LinearAllocator* allocator, void* old_block, fuint old_size, fuint new_size, fuint align) {
	fox_assert(align);
	fox_assert(new_size >= old_size);
	
	if ((u8*)old_block + old_size == allocator->cursor) {
		//We should already be aligned since the last allocation should have had the same alignment
		fox_assert(allocator->cursor == (u8*)align_offset_up((fuint)allocator->cursor, align));
		
		allocator->cursor += new_size - old_size;
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
	allocator->cursor = restore_point.old_cursor;
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
	//Test initialization, we aren't using an actual buffer here because it doesn't matter
	constexpr fuint memory_size = 1024;
	u8 memory_buffer[memory_size];
	
	LinearAllocator test_allocator;
	initialize(&test_allocator, memory_buffer, memory_size);
	fox_assert(test_allocator.start == memory_buffer);
	fox_assert(test_allocator.start == test_allocator.cursor);
	fox_assert(test_allocator.end == memory_buffer + memory_size);
	
	//Test allocation
	auto test_restore = create_restore_point(&test_allocator);
	allocate<u32>(&test_allocator);
	fox_assert(test_allocator.cursor == memory_buffer + 4);
	allocate<u8>(&test_allocator);
	fox_assert(test_allocator.cursor == memory_buffer + 5);
	allocate<u16>(&test_allocator);
	fox_assert(test_allocator.cursor == memory_buffer + 8);
	allocate_array<u64>(3, &test_allocator);
	fox_assert(test_allocator.cursor == memory_buffer + 32);
	
	//Test restore
	restore(&test_allocator, test_restore);
	
	//Test reallocation
	fox_assert(test_allocator.cursor == memory_buffer);
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
	fox_assert(*allocated_memory == 0xdead);
	fox_assert(reallocated_memory == memory_buffer + 4);
	fox_assert(test_allocator.cursor == memory_buffer + 8);
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
internal typename ArrayType::ElementTypeMember& push(ArrayType* array, typename ArrayType::ElementTypeMember new_element, AllocatorType* allocator = nullptr) {
	auto new_element_index = array->length;
	expand(array, 1, allocator);
	return array->get(new_element_index) = new_element;
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

template<typename ArrayType, fuint c_string_length, typename AllocatorType = void>
internal void push_array(ArrayType* array, const char (&c_string)[c_string_length], AllocatorType* allocator = nullptr) {
	push_array(array, c_string, c_string_length - 1, allocator);
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
	fox_for (i, 2) {
		fox_assert(test_array1[i] == i);
	}
	
	//Make sure reallocation moves things as necessary
	push(&test_array0, 2, &test_allocator);
	fox_assert(test_array0.data != test_buffer);
	fox_assert(test_array0.length == 3);
	fox_for (i, 3) {
		fox_assert(test_array0[i] == i);
	}
	
	//Make sure the same functions work on strings
	String hello_seven;
	push_array(&hello_seven, "hello", &test_allocator);
	push_array(&hello_seven, "7", &test_allocator);
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
	auto* file = fopen(path, "r+");
	if (file) {
		fseek(file, 0, SEEK_END);
		fuint size = ftell(file);
		if (size) {
			*out_buffer = allocate_array<u8>(size, allocator);
			rewind(file);
			auto bytes_read = fread(out_buffer->data, 1, size, file);
			if (bytes_read == size) {
				return FileStatus::read;
			}
			return FileStatus::error;
		}
		return FileStatus::empty;
	}
	return FileStatus::error;
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
	//library tests
	test_arrays();
	test_allocators();
	test_dynamic_arrays();
	test_file_io();
}

//---Vulpes
LinearAllocator heap_stack;

int main(int argument_count, char** arguments) {
	test_foxlib();
	
	u64 memory_size = mebibytes(1);
	initialize(&heap_stack, malloc(memory_size), memory_size);
	
	if (argument_count > 1) {
		ConstString file_path = arguments[1];
		Array<u8> file_data;
		zero(&file_data);
		auto file_read_status = read_entire_file(file_path.data, &heap_stack, &file_data);
		switch (file_read_status) {
			case FileStatus::read: {
				print(fox_interpret_cast(ConstString, file_data));
			} break;
			
			case FileStatus::empty: {
				String error_message;
				zero(&error_message);
				push_array(&error_message, "File ", &heap_stack);
				push_array(&error_message, file_path, &heap_stack);
				push_array(&error_message, " is empty!\n", &heap_stack);
				print(error_message);
			} break;
			
			case FileStatus::error: {
				String error_message;
				zero(&error_message);
				push_array(&error_message, "File ", &heap_stack);
				push_array(&error_message, file_path, &heap_stack);
				push_array(&error_message, " is inaccessible!\n", &heap_stack);
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