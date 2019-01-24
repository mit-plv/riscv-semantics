#include <iostream>
#include <iomanip>
#include <sstream>
#include <map>
#include <string> 
#include <vector>

#include <fstream>
#include "simif.h"
#include "processor.h"
#include "encoding.h"
#include "devices.h"
#include "disasm.h"
#include "mmu.h"
#include "CircularBuffer.hpp"

#include "ElfFile.h"

bool debug = false;

typedef struct VerificationPacket {
  uint64_t pc;
  uint32_t instruction;
  bool exception;
  bool interrupt;
  uint64_t cause; // only 4 bits
  uint64_t addr;
  bool valid_addr;
  uint64_t data;
  bool valid_dst;
  uint64_t dst; // Bit for fpr/valid
} VerificationPacket;

class tandemspike_t : public simif_t {
public:
  tandemspike_t(const char* elf_path) : packets(0),  device_tree("device_tree.bin", std::ios::binary), data(), proc("rv64ima", this, 0),outBuffer(40), disassembler(new disassembler_t(64)) {
    consecutive_traps = 0;
    errors = 0;
    data_sz = 0x8000000;
//    data = (char*) calloc(data_sz, 1);
    ElfFile prog;
    prog.open(elf_path);
    for (auto& it : prog.getSections()) {
      if (it.base >= 0x80000000ull && ((((char*) it.base) + it.data_size) <= (char*)(0x80000000ull + data_sz))) {
        for( int i=0; i< it.data_size; i++) data[i+it.base] = it.data[i];
//	memcpy((void*) (data + (((uint64_t) it.base) - 0x80000000ull)), it.data, it.data_size);
      } else {
	std::cerr << "Skipping section at " << it.base << std::endl;
      }
    }
    int addrdt=0;
    char bytedt=0;
    while(device_tree.get(bytedt)){ data[addrdt]=bytedt; addrdt++;}
    proc.get_state()->pc = 0x80000000;
  }

  char* addr_to_mem(reg_t addr) {return NULL;}
     // Does not have any real memory because we need to hijack every load and store.

  bool mmio_load(reg_t addr, size_t len, uint8_t* bytes){
      actual_packet.data = 0;
      if (addr == 0x200bff8){
          actual_packet.data = timer;
          if(len != 4) {std::cerr<< "Failure trying to read timer not with a word?!\n"<<std::endl;exit(1);}
          memcpy(bytes, &timer, len);
      }
      else {
      if (addr != 0xfff0){
      for (int i=0;i < len; i++) bytes[i] = data[addr+i];
//      memcpy(bytes, data+(addr- 0x80000000ull), len);
      memcpy(&(actual_packet.data), bytes, len);}
      else {memcpy(bytes, &(actual_packet.data),len);}}
      actual_packet.addr = addr;
  }

  bool mmio_store(reg_t addr, size_t len, const uint8_t* bytes){
      actual_packet.data = 0;
      if (addr != 0xfff0){
      for(int i=0; i< len; i++) {
          data[addr+i] = bytes[i];
          ((char*)(&actual_packet.data))[i]=bytes[i];}}
//      memcpy(data+ addr - 0x80000000ull,bytes, len);
//      memcpy((void*) &(actual_packet.data), (void*)bytes ,  len);
      actual_packet.addr = addr;
  }

  void proc_reset(unsigned id){}
  void dump_state() {
    std::cerr << "pc = 0x" << std::hex << proc.get_state()->pc << std::endl;
    for (int i = 0 ; i < 32 ; i = i+1) {
        std::cerr << "  x" << std::dec << i << " = 0x" << std::hex << proc.get_state()->XPR[i] << std::endl;
    }
   std::cerr << "   csr mcounteren" << std::hex <<proc.get_csr(0x306) << std::endl; 

  }

  void step(VerificationPacket synchronization_packet){
    actual_packet = synchronization_packet;

    if (synchronization_packet.interrupt) {
        // try to force the specified interrupt
        proc.get_state()->mip = 1 << synchronization_packet.cause;
    }

    actual_packet.pc = proc.get_state()->pc;
    // Read new instruction
    try {
        actual_packet.instruction = (uint32_t) proc.get_mmu()->access_icache(actual_packet.pc)->data.insn.bits();
    } catch(...) {
        // if the current instruction didn't cause an exception or an interrupt, we should have been able to get the current instruction
        if (!(synchronization_packet.interrupt || synchronization_packet.exception)) {
            std::cerr << "[ERROR] access_icache(0x" << std::hex << actual_packet.pc <<  ") failed even though there was no interrupt or exception for the current verification packet." << std::endl;
        }
    }

    if(debug) std::cerr << "Instruction match\n" ; 
    proc.step(1);

    if (synchronization_packet.interrupt && ((synchronization_packet.cause == 3) || (synchronization_packet.cause == 9) || (synchronization_packet.cause == 11))) {
        // if forcing MSIP, SEIP, or MEIP, zero out the MIP CSR
        proc.get_state()->mip = 0;
    }

    // check if instruction caused an exception, and if so, figure out the cause
    if (!synchronization_packet.interrupt) {
        if (proc.get_state()->pc == (proc.get_state()->mtvec & ~3)) {
            if (proc.get_state()->mcause >> 31) {
                actual_packet.interrupt = 1;
                actual_packet.exception = 0;
            } else {
                actual_packet.exception = 1;
                actual_packet.interrupt = 0;
            }
            actual_packet.cause = proc.get_state()->mcause;
        } else if (proc.get_state()->pc == (proc.get_state()->stvec & ~3)) {
            if (proc.get_state()->scause >> 31) {
                actual_packet.interrupt = 1;
                actual_packet.exception = 0;
            } else {
                actual_packet.exception = 1;
                actual_packet.interrupt = 0;
            }
            actual_packet.cause = proc.get_state()->scause;
        } else {
            actual_packet.exception = 0;
        }
    }

//    if (actual_packet.instruction != synchronization_packet.instruction) {
//        // instructions don't match, so we need to figure out the destination for the actual packet
//        // assume its always writing to the XPR specified in rd
//        actual_packet.dst = ((actual_packet.instruction >> 7) & 0x1F);
//        actual_packet.valid_dst = synchronization_packet.
//    }
    if (synchronization_packet.valid_dst) { // Packet valid
       actual_packet.data = proc.get_state()->XPR[actual_packet.dst];

       if(debug) std::cerr << "\nBring data in from register "<< actual_packet.dst << "\n" << actual_packet.data;
    }


    // Move forward the processor and adjust if required.
  }


  bool comparePackets(VerificationPacket procP, VerificationPacket spikeP) {
    if (spikeP.pc == 0xffffffe0003c5724) dump_state();
    bool match = true;
    match = match && (procP.pc == spikeP.pc);
if(debug)    std::cerr <<  "pc "  << match << "\n"  ;
    match = match && (procP.instruction == spikeP.instruction);
if(debug)    std::cerr <<  "instruction "  << match << "\n"  ;
    match = match && (procP.exception == spikeP.exception);
if(debug)    std::cerr <<  "exception "  << match << "\n"  ;
    match = match && (procP.interrupt == spikeP.interrupt);
if(debug)    std::cerr <<  "interrupt "  << match << "\n"  ;
    if (procP.exception || procP.interrupt) {
        match = match && (procP.cause == spikeP.cause);
if(debug)     std::cerr <<  "cause "  << match << "\n"  ;
    } else {
        match = match && (!(procP.valid_dst) || (procP.dst == spikeP.dst));
 if(debug)     std::cerr <<  "dst_valid "  << procP.valid_dst  << "\n"  ;
 if(debug)    std::cerr <<  "dst "  << match  << "\n"  ;
        if (procP.valid_addr && procP.valid_dst) {return match;} // Special case for amo instructions
        if (spikeP.addr == 0xfff0) {return match;} // special case for mmio
        match = match && (!(procP.valid_dst || procP.valid_addr ) || (procP.data == spikeP.data));
 if(debug)    std::cerr <<  "data "  << std::hex << match << " proc" << std::hex << procP.data << " spike " << std::hex << spikeP.data << "\n"  ;
// Ajouter une hypothese sur la validité
        match = match && (!(procP.valid_addr) || (procP.addr == spikeP.addr));
 if(debug)    std::cerr <<  "addr "  << std::hex << match << "\n"  ;
    }
    return match;
  }


  std::string verificationPacketToString(VerificationPacket p) {
    std::ostringstream buffer;
    buffer << "[inst 0x" << std::setfill('0') << std::setw(16) << std::hex << packets << "] ";
    // pc
    buffer << "0x" << std::setfill('0') << std::setw(16) << std::hex << p.pc << ": ";
    // instruction data
    buffer << "(0x" << std::setfill('0') << std::setw(8) << std::hex << p.instruction << ") ";
    // instruction disassembled
    std::string assembly = (disassembler->disassemble(p.instruction));
    buffer << std::left << std::setfill(' ') << std::setw(32) << assembly;
    if (p.exception) {
      switch (p.cause) {
      case 0:
	buffer << " [Exception: Instruction address misaligned]";
	break;
      case 1:
	buffer << " [Exception: Instruction access fault]";
	break;
      case 2:
	buffer << " [Exception: Illegal instruction]";
	break;
      case 3:
	buffer << " [Exception: Breakpoint]";
	break;
      case 4:
	buffer << " [Exception: Load address misaligned]";
	break;
      case 5:
	buffer << " [Exception: Load access fault]";
	break;
      case 6:
	buffer << " [Exception: Store/AMO address misaligned]";
	break;
      case 7:
	buffer << " [Exception: Store/AMO access fault]";
	break;
      case 8:
	buffer << " [Exception: Environment call from U-mode]";
	break;
      case 9:
	buffer << " [Exception: Environment call from S-mode]";
	break;
      case 10:
	buffer << " [Exception: Environment call from H-mode]";
	break;
      case 11:
	buffer << " [Exception: Environment call from M-mode]";
	break;
      case 12:
	buffer << " [Exception: Instruction page fault]";
	break;
      case 13:
	buffer << " [Exception: Load page fault]";
	break;
      case 15:
	buffer << " [Exception: Store/AMO page fault]";
	break;
      default:
	buffer << " [Unknown Exception]";
      }
    } else if (p.interrupt) {
      switch (p.cause) {
      case 0x00:
	buffer << " [Interrupt: User software interrupt]";
	break;
      case 0x01:
	buffer << " [Interrupt: Supervisor software interrupt]";
	break;
      case 0x02:
	buffer << " [Interrupt: Hypervisor software interrupt]";
	break;
      case 0x03:
	buffer << " [Interrupt: Machine software interrupt]";
	break;
      case 0x04:
	buffer << " [Interrupt: User timer interrupt]";
	break;
      case 0x05:
	buffer << " [Interrupt: Supervisor timer interrupt]";
	break;
      case 0x06:
	buffer << " [Interrupt: Hypervisor timer interrupt]";
	break;
      case 0x07:
	buffer << " [Interrupt: Machine timer interrupt]";
	break;
      case 0x08:
	buffer << " [Interrupt: User external interrupt]";
	break;
      case 0x09:
	buffer << " [Interrupt: Supervisor external interrupt]";
	break;
      case 0x0A:
	buffer << " [Interrupt: Hypervisor external interrupt]";
	break;
      case 0x0B:
	buffer << " [Interrupt: Machine external interrupt]";
	break;
      default:
	buffer << " [Unknown Interrupt]";
      }
    } else if (p.dst & 0x40) {
      // destination register
      const char* regName = NULL;
      if (p.dst & 0x20) {
	regName = fpr_name[p.dst & 0x1f];
      } else {
	regName = xpr_name[p.dst & 0x1f];
      }
      buffer << " [" << regName << " = 0x" << std::hex << p.data << "]";
    }
    switch (p.instruction & 0x7F) {
    case 0x03: // Load
    case 0x23: // Store
    case 0x2F: // AMO
    case 0x07: // FP-Load
    case 0x27: // FP-Store
      buffer << " (addr = 0x" << std::hex << p.addr << ")";
    }
    return buffer.str();
  }


  bool check_packet(VerificationPacket packet){
    // Step
    if (packet.interrupt || packet.exception) {
        
        consecutive_traps += 1;
    } else {
        consecutive_traps = 0;
    }

    packets++;
    bool match = comparePackets(packet, actual_packet);

    if (!match) {
        errors++;
        std::ostringstream buffer;
        buffer << "[ERROR] Verification error in packet " << packets << " (instruction ANDY screwed up here)" << std::endl;
        buffer << "  [PROC]  " << verificationPacketToString(packet) << std::endl;
        buffer << "  [SPIKE] " << verificationPacketToString(actual_packet);
        outBuffer.addLine(buffer.str());
        outBuffer.printToOStream(&std::cerr, 20);
    } else {
        outBuffer.addLine(verificationPacketToString(packet));
    }

    // XXX: this was to temporarily print everything
    // outBuffer.printToOStream(&std::cerr, 20000);

    if ((errors > 0) || (consecutive_traps > 2)) {
        if (consecutive_traps > 2) {
            std::ostringstream buffer;
            buffer << "[ERROR] More than 2 consecutive traps" << std::endl;
            outBuffer.addLine(buffer.str());
        }
        outBuffer.printToOStream(&std::cerr, 20);
        // and let's abort!
        std::cerr << " Too many errors" ;
        exit(1);
    }

    return match;
  }
  int timer;

private:
  disassembler_t *disassembler;
  std::ifstream device_tree;
  processor_t proc;
  unsigned int packets;
  unsigned int consecutive_traps;
  unsigned int errors;
  std::map<uint64_t, uint8_t> data; // Memory of the machine
  VerificationPacket actual_packet;
  size_t data_sz;
  CircularBuffer outBuffer;
};

int main(int argc, char* argv[]) {
  if (argc < 2) {std::cerr<<"Please path path toward elf"; exit(1);}
  const char* elf_path = argv[1];
  // TODO Add some ways to read the path from the command line
  tandemspike_t sim(elf_path);
  VerificationPacket haskell_packet;
  unsigned long long count = 0;
  int read;
  bool valid_timer; 
  int timer; 
  std::string command;
  while (1) {
    // step the haskell model
//    std::cout << "n" << std::endl;
    do {
      std::cin >> command;
    } while (command != "s");
    if (debug) std::cerr << "Newpacket"; 
    // Parse packet
    std::cin >> haskell_packet.pc;
    if (debug) std::cerr << "pcok" << haskell_packet.pc; 
    std::cin >> haskell_packet.instruction;
    if (debug) std::cerr << "instruction " << haskell_packet.instruction; 
    std::cin >> haskell_packet.exception;
    if (debug) std::cerr << "exception " << haskell_packet.exception; 
    std::cin >> haskell_packet.interrupt;
    if (debug) std::cerr << "interrupt " << haskell_packet.interrupt; 
    std::cin >> haskell_packet.cause;
    if (debug) std::cerr << "cause " << haskell_packet.cause; 
    std::cin >> haskell_packet.addr;
    if (debug) std::cerr << "addr" << haskell_packet.interrupt; 
    std::cin >> haskell_packet.valid_addr;
    if (debug) std::cerr << "valid_addr " << haskell_packet.valid_addr; 
    std::cin >> haskell_packet.data;
    if (debug) std::cerr << "data " << haskell_packet.data; 
    std::cin >> haskell_packet.valid_dst;
    if (debug) std::cerr << "valid_dst" << haskell_packet.valid_dst; 
    std::cin >> haskell_packet.dst;
    if (debug) std::cerr << "dst" << haskell_packet.dst; 
    std::cin >> valid_timer;
    if (debug) std::cerr << "valid_timer" << valid_timer; 
    std::cin >> timer;
    if (debug) std::cerr << "timer" << timer; 
    if (valid_timer) sim.timer=timer;
      std::cin >> command;
    if(command != "e") {std::cerr << "Not e at the end of a packet"; exit(1);}  
    sim.step(haskell_packet);
//    sim.dump_state();
    sim.check_packet(haskell_packet);
    count++;
    if (count% 100000 ==0) std::cout << count<< std::endl;
    if (count ==  13623540) debug=true;
    // step from this class
    // Check_packet
  }
}
