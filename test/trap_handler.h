#ifndef TRAP_HANDLER_H
#define TRAP_HANDLER_H

// Wrapper for a user-defined C function, trap_handler().
// Saves and restores a0, uses mret.
void _trap_handler();
asm("_trap_handler:\n"
    "  csrw mscratch,a0\n"
    "  call trap_handler\n"
    "  csrrw a0,mepc,zero\n"
    "  addi a0,a0,4\n"
    "  csrrw zero,mepc,a0\n"
    "  csrr a0,mscratch\n"
    "  mret");

#endif
