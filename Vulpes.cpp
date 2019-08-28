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
#define fox_assert(condition) if (!(condition)) { (*(fuint*)0) = 0; }

template<typename Type>
void zero(Type* value) {
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

void test_arrays() {
	ConstString c_string_conversion_test = "hi";
	fox_assert(c_string_conversion_test[0] == 'h');
	fox_assert(c_string_conversion_test[1] == 'i');
	fox_assert(c_string_conversion_test.length == 2);
}

//---Allocators
fuint align_offset_up(fuint offset, fuint align) {
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

void initialize(LinearAllocator* allocator, void* memory, fuint size) {
	allocator->start = allocator->cursor = (u8*)memory;
	allocator->end = (u8*)memory + size;
}

void initialize(LinearAllocator* allocator, Array<u8> memory) {
	initialize(allocator, memory.data, memory.length);
}

void* allocate_block(LinearAllocator* allocator, fuint size, fuint align) {
	fox_assert(align);
	
	allocator->cursor = (u8*)align_offset_up((fuint)allocator->cursor, align);
	auto result = (void*)allocator->cursor;
	allocator->cursor += size;
	fox_assert(allocator->cursor < allocator->end);
	return result;
}

void* reallocate_block(LinearAllocator* allocator, void* old_block, fuint old_size, fuint new_size, fuint align) {
	fox_assert(align);
	fox_assert(new_size >= old_size);
	
	if ((u8*)old_block + old_size == allocator->cursor) {
		//We should already be aligned since the last allocation should have had the same alignment
		fox_assert(allocator->cursor == (u8*)align_offset_up((fuint)allocator->cursor, align));
		
		allocator->cursor += new_size - old_size;
		return old_block;
	}
	
	return allocate_block(allocator, new_size, align);
}

LinearAllocatorRestorePoint create_restore_point(LinearAllocator* allocator) {
	LinearAllocatorRestorePoint restore_point;
	zero(&restore_point);
	restore_point.old_cursor = allocator->cursor;
	return restore_point;
}

void restore(LinearAllocator* allocator, LinearAllocatorRestorePoint restore_point) {
	allocator->cursor = restore_point.old_cursor;
}

template<typename Type, typename AllocatorType>
Type* allocate(AllocatorType* allocator) {
	return (Type*)allocate_block(allocator, sizeof(Type), alignof(Type));
}

template<typename Type, typename AllocatorType>
Array<Type> allocate_array(AllocatorType* allocator, fuint length) {
	Array<Type> new_array;
	zero(&new_array);
	new_array.data = (Type*)allocate_block(allocator, sizeof(Type) * length, alignof(Type));
	return new_array;
}

void test_allocators() {
	//Test initialization, we aren't using an actual buffer here because it doesn't matter
	constexpr fuint memory_size = 1024;
	LinearAllocator test_allocator;
	initialize(&test_allocator, nullptr, memory_size);
	fox_assert(test_allocator.start == nullptr);
	fox_assert(test_allocator.start == test_allocator.cursor);
	fox_assert(test_allocator.end == (u8*)memory_size);
	
	//Test allocation
	auto test_restore = create_restore_point(&test_allocator);
	allocate<u32>(&test_allocator);
	fox_assert(test_allocator.cursor == (void*)4);
	allocate<u8>(&test_allocator);
	fox_assert(test_allocator.cursor == (void*)5);
	allocate<u16>(&test_allocator);
	fox_assert(test_allocator.cursor == (void*)8);
	allocate_array<u64>(&test_allocator, 3);
	fox_assert(test_allocator.cursor == (void*)32);
	
	//Test restore
	restore(&test_allocator, test_restore);
	
	//Test reallocation
	fox_assert(test_allocator.cursor == nullptr);
	auto allocated_memory = allocate<u16>(&test_allocator);
	auto reallocated_memory = reallocate_block(&test_allocator, allocated_memory, 2, 4, 2);
	fox_assert(allocated_memory == reallocated_memory);
	
	restore(&test_allocator, test_restore);
	allocated_memory = allocate<u16>(&test_allocator);
	allocate<u16>(&test_allocator);
	reallocated_memory = reallocate_block(&test_allocator, allocated_memory, 2, 4, 2);
	fox_assert(allocated_memory != reallocated_memory);
	fox_assert(reallocated_memory == (void*)4);
	fox_assert(test_allocator.cursor == (void*)8);
}

void print(ConstString message) {
	std::cout.write(message.data, message.length);
}

//---Vulpes
LinearAllocator heap_stack;

int main() {
	//library tests
	test_arrays();
	test_allocators();
	
	u64 memory_size = mebibytes(1);
	initialize(&heap_stack, malloc(memory_size), memory_size);
	
	print("hello world!\n");
	return 0;
}