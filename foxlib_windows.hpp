#define WIN32_LEAN_AND_MEAN
#define VC_EXTRALEAN
#include <windows.h>

//Allocate a large block of memory from the system at a fixed address
//if 0 is provided as the base address a reliable base address for the target system will be used
//@todo we should use MEM_LARGE_PAGES if size is sufficiently large
internal void* allocate_large_block_fixed_address_system(void* base_address, u64 size) {
	if (base_address == 0) {
		base_address = (void*)tebibytes(2);
	}
	return VirtualAlloc(base_address, size, MEM_COMMIT|MEM_RESERVE, PAGE_READWRITE);
}