
// Copyright (c) 2017 Massachusetts Institute of Technology
//
// Permission is hereby granted, free of charge, to any person
// obtaining a copy of this software and associated documentation
// files (the "Software"), to deal in the Software without
// restriction, including without limitation the rights to use, copy,
// modify, merge, publish, distribute, sublicense, and/or sell copies
// of the Software, and to permit persons to whom the Software is
// furnished to do so, subject to the following conditions:
//
// The above copyright notice and this permission notice shall be
// included in all copies or substantial portions of the Software.
//
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
// MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
// BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
// ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
// CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
// SOFTWARE.

#include <iostream>
#include <fstream>
#include <iomanip>

#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#include "ElfFile.hpp"

void printUsage(const char* program_name) {
    std::cerr << "Usage: " << program_name << " <elf-file> <base-address> <length> <output-hex>" << std::endl;
    std::cerr << "This program converts a specified address range from an ELF file into a hex file" << std::endl;
    std::cerr << "  elf-file        input ELF file to convert to a hex file" << std::endl;
    std::cerr << "  base-address    base address of output hex file" << std::endl;
    std::cerr << "                    This value is interpreted as decimal by default, but it can also be" << std::endl;
    std::cerr << "                    interpreted as octal with a '0' prefix or hex with a '0x' or '0X' prefix" << std::endl;
    std::cerr << "  length          intended length of output hex file" << std::endl;
    std::cerr << "                    This value can use a K, M, or G suffix" << std::endl;
    std::cerr << "  output-hex      filename for output hex file" << std::endl;
}

int main(int argc, char* argv[]) {
    if (argc != 5) {
        std::cerr << "ERROR: Incorrect command line arguments" << std::endl;
        printUsage(argv[0]);
        exit(1);
    }

    char *elf_filename = argv[1];
    char *base_address_string = argv[2];
    char *length_string = argv[3];
    char *hex_filename = argv[4];

    // parse base address
    char *endptr = 0;
    unsigned long long base_address = strtoull(base_address_string, &endptr, 0);
    if (strcmp(endptr, "") != 0) {
        // conversion failure
        std::cerr << "ERROR: base-address expected to be a number" << std::endl;
        printUsage(argv[0]);
        exit(1);
    }

    // parse length
    unsigned long long length = strtoull(length_string, &endptr, 0);
    if (strcmp(endptr, "K") == 0) {
        length *= 1024;
    } else if (strcmp(endptr, "M") == 0) {
        length *= 1024 * 1024;
    } else if (strcmp(endptr, "G") == 0) {
        length *= 1024 * 1024 * 1024;
    } else if (strcmp(endptr, "") != 0) {
        // conversion failure
        std::cerr << "ERROR: length expected to be a number with an optional prefix K, M, or G" << std::endl;
        printUsage(argv[0]);
        exit(1);
    }

    // Command line arguments are parsed and ready to go

    ElfFile elf_file;
    if (!elf_file.open(elf_filename)) {
        std::cerr << "ERROR: failed opening ELF file" << std::endl;
        exit(1);
    }

    std::vector<ElfFile::Section> sections = elf_file.getSections();

    // write output_buff to hex_filename in hex file format
    std::ofstream hex_file;
    hex_file.open(hex_filename, std::ios_base::out);
    if (!hex_file) {
        std::cerr << "ERROR: unable to open \"" << hex_filename << "\" for writing" << std::endl;
        exit(1);
    }

    uint64_t prev_hex_addr = 0;
    uint64_t curr_hex_addr = 0;
    uint64_t section_offset = 0;
    for (int i = 0 ; i < sections.size() ; i++) {
        if (base_address + length < sections[i].base) {
            // This section starts after the last address in the hex file.
            continue;
        }
        if (sections[i].base < base_address) {
            // This section starts at a lower address than the hex file base
            // address. Compute section_offset to correspond to base_address.
            section_offset = base_address - sections[i].base;
            curr_hex_addr = 0;
        } else {
            curr_hex_addr = sections[i].base - base_address;
            section_offset = 0;
        }

        // assumes 32-bit width for output hex file
        hex_file << "@" << std::hex << std::setw(0) << (curr_hex_addr >> 2) << std::endl;
        while ((sections[i].base + section_offset < base_address + length) && (section_offset < sections[i].data_size)) {
            // write stuff
            uint32_t *hex_data = (uint32_t*) &sections[i].data[section_offset];
            hex_file << std::hex << std::setw(0) << *hex_data << std::endl;
            section_offset += 4;
        }
        while ((sections[i].base + section_offset < base_address + length) && (section_offset < sections[i].section_size)) {
            // write zeros
            hex_file << std::hex << std::setw(0) << 0 << std::endl;
            section_offset += 4;
        }
    }

    hex_file << "@" << std::hex << std::setw(0) << (length >> 2) << std::endl;

    hex_file.close();

    return 0;
}
