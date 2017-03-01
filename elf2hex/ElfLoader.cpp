
#include <fstream>
#include <iostream>

#include <string.h>
#include <elf.h>

#include "ElfLoader.hpp"

template <typename Elf_Ehdr, typename Elf_Phdr>
bool load_elf_specific(char* elf_buf, size_t elf_buf_sz, char* mem_buf, size_t mem_buf_sz) {
    // 64-bit ELF
    Elf_Ehdr *ehdr = (Elf_Ehdr*) elf_buf;
    Elf_Phdr *phdr = (Elf_Phdr*) (elf_buf + ehdr->e_phoff);
    if (elf_buf_sz < ehdr->e_phoff + ehdr->e_phnum * sizeof(Elf_Phdr)) {
        std::cerr << "ERROR: load_elf: file too small for expected number of program header tables" << std::endl;
        return false;
    }
    // loop through program header tables
    for (int i = 0 ; i < ehdr->e_phnum ; i++) {
        if ((phdr[i].p_type == PT_LOAD) && (phdr[i].p_memsz > 0)) {
            if (phdr[i].p_memsz < phdr[i].p_filesz) {
                std::cerr << "ERROR: load_elf: file size is larger than memory size" << std::endl;
                return false;
            }
            if (phdr[i].p_filesz > 0) {
                if (phdr[i].p_offset + phdr[i].p_filesz > elf_buf_sz) {
                    std::cerr << "ERROR: load_elf: file section overflow" << std::endl;
                    return false;
                }
                // start of file section: elf_buf + phdr[i].p_offset
                // end of file section: elf_buf + phdr[i].p_offset + phdr[i].p_filesz
                // start of memory: phdr[i].p_paddr
                if (phdr[i].p_paddr + phdr[i].p_filesz > mem_buf_sz) {
                    std::cerr << "ERROR: load_elf: file section will overflow output buffer" << std::endl;
                    return false;
                }
                memcpy( (void *) (mem_buf + phdr[i].p_paddr), (void *) (elf_buf + phdr[i].p_offset), phdr[i].p_filesz );
            }
            if (phdr[i].p_memsz > phdr[i].p_filesz) {
                // copy 0's to fill up remaining memory
                if (phdr[i].p_paddr + phdr[i].p_memsz > mem_buf_sz) {
                    std::cerr << "ERROR: load_elf: zeros at end of file section will overflow output buffer" << std::endl;
                    return false;
                }
                size_t zeros_sz = phdr[i].p_memsz - phdr[i].p_filesz;
                memset( (void *) (mem_buf + phdr[i].p_paddr + phdr[i].p_filesz), 0, zeros_sz);
            }
        }
    }
    return true;
}

bool load_elf(const char* elf_filename, char* mem_buf, size_t mem_buf_sz) {
    std::ifstream elffile;
    elffile.open(elf_filename, std::ios::in | std::ios::binary);

    if (!elffile.is_open()) {
        std::cerr << "ERROR: load_elf: failed opening file \"" << elf_filename << "\"" << std::endl;
        return false;
    }

    elffile.seekg(0, elffile.end);
    size_t elf_buf_sz = elffile.tellg();
    elffile.seekg(0, elffile.beg);

    // Read the entire file. If it doesn't fit in host memory, it won't fit in the risc-v processor
    char* elf_buf = new char[elf_buf_sz];
    elffile.read(elf_buf, elf_buf_sz);

    if (!elffile) {
        std::cerr << "ERROR: load_elf: failed reading elf header" << std::endl;
        return false;
    }

    if (elf_buf_sz < sizeof(Elf32_Ehdr)) {
        std::cerr << "ERROR: load_elf: file too small to be a valid elf file" << std::endl;
        return false;
    }

    // make sure the header matches elf32 or elf64
    Elf32_Ehdr *ehdr = (Elf32_Ehdr *) elf_buf;
    unsigned char* e_ident = ehdr->e_ident;
    if (e_ident[EI_MAG0] != ELFMAG0
            || e_ident[EI_MAG1] != ELFMAG1
            || e_ident[EI_MAG2] != ELFMAG2
            || e_ident[EI_MAG3] != ELFMAG3) {
        std::cerr << "ERROR: load_elf: file is not an elf file" << std::endl;
        return false;
    }

    if (e_ident[EI_CLASS] == ELFCLASS32) {
        // 32-bit ELF
        return load_elf_specific<Elf32_Ehdr, Elf32_Phdr>(elf_buf, elf_buf_sz, mem_buf, mem_buf_sz);
    } else if (e_ident[EI_CLASS] == ELFCLASS64) {
        // 64-bit ELF
        return load_elf_specific<Elf64_Ehdr, Elf64_Phdr>(elf_buf, elf_buf_sz, mem_buf, mem_buf_sz);
    } else {
        std::cerr << "ERROR: load_elf: file is neither 32-bit nor 64-bit" << std::endl;
        return false;
    }
}

