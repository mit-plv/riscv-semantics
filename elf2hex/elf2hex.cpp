#include <iostream>
#include <fstream>
#include <iomanip>

#include <stdlib.h>
#include <stdint.h>
#include <string.h>

#include "ElfLoader.hpp"

void printUsage(const char* program_name) {
    std::cerr << "Usage: " << program_name << " <elf-file> [<output-hex>]" << std::endl;
    std::cerr << "if <output-hex> is not specified, the program will write output to \"<elf-file>.hex\"" << std::endl;
}

int main(int argc, char* argv[]) {
    if (argc < 2) {
        std::cerr << "ERROR: This program expects at least one argument" << std::endl;
        printUsage(argv[0]);
        exit(1);
    }
    char *elf_filename = argv[1];
    char *hex_filename;
    if (argc >= 3) {
        hex_filename = new char[strlen(argv[2]) + 1];
        strcpy(hex_filename, argv[2]);
    } else {
        hex_filename = new char[strlen(argv[1]) + 4 + 1];
        strcpy(hex_filename, argv[1]);
        strcat(hex_filename, ".hex");
    }

    // We expect the output to be no more than 256 MB for instruction memory and 64 KB for data memory.
    // We rounding up to the next 4 MB block
    const size_t output_sz = (256 + 4) << 20;
    char* output_buff = new char[output_sz];
    // zero output_buff
    memset((void *) output_buff, 0, output_sz);
    // load elf file to output_buff
    bool load_elf_success = load_elf(elf_filename, output_buff, output_sz);
    if (!load_elf_success) {
        std::cerr << "ERROR: load_elf(elf_filename = \"" << elf_filename << "\", output_buff, output_sz = " << output_sz << ") failed" << std::endl;
        exit(1);
    }

    // write output_buff to hex_filename in hex file format
    std::ofstream hex_file;
    hex_file.open(hex_filename, std::ios_base::out);
    if (!hex_file) {
        std::cerr << "ERROR: unable to open \"" << hex_filename << "\" for writing" << std::endl;
        exit(1);
    }
    uint32_t* hex_data = (uint32_t *) output_buff;
    size_t hex_data_sz = output_sz / sizeof(uint32_t);
    bool jump = false;
    for (int word_addr = 0 ; word_addr < hex_data_sz ; word_addr++) {
        if (hex_data[word_addr] == 0) {
            jump = true;
        } else {
            if (jump) {
                hex_file << "@" << std::hex << std::setw(0) << word_addr << std::endl;
            }
            hex_file << std::hex << std::setw(8) << std::setfill('0') << hex_data[word_addr] << std::endl;
            jump = false;
        }
    }
    hex_file.close();

    delete[] output_buff;
    delete[] hex_filename;
    return 0;
}
