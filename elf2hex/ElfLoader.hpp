#ifndef ELF_LOADER_HPP
#define ELF_LOADER_HPP
bool load_elf(const char* elf_filename, char* mem_buf, size_t mem_buf_sz);
#endif
