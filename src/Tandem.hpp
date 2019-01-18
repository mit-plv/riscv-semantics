#include "spike/simif.h"
#include "spike/processor.h"
#include "spike/disasm.h"


class CustomSpike : public simif_t {
    public:
        CustomSpike(const char* isa_string, const char* elf_file, size_t memory_sz);
        ~CustomSpike();
        char* addr_to_mem(reg_t addr);
        bool mmio_load(reg_t addr, size_t len, uint8_t* bytes);
        bool mmio_store(reg_t addr, size_t len, const uint8_t* bytes);
        void proc_reset(unsigned id) { /* do nothing */ }
        VerificationPacket step(VerificationPacket synchronization_packet);
        void dump_state();
    private:
        processor_t proc;
        char* data;
        size_t data_sz;
        uint32_t tandem_mmio_inst; // used for mmio_load
        reg_t tandem_mmio_data; // used for mmio_load
        // output a trace of mmio loads and stores
        bool trace_mmio;
};
