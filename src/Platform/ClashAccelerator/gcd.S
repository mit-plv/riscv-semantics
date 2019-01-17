.section ".text.init"
    .globl _start

. = 0x0
_start:
            li a0, 0x4000
            li a1, 0x4004
            lw a0, 0(a0)
            lw a1, 0(a1)
            jal gcd
            li a1, 0x4008
            sw a0, 0(a1) 
            j _start
gcd:        beqz a0, a0_is_zero
gcd_loop:   beqz a1, gcd_end
            bgeu a1, a0, a1_ge_a0
            mv t0, a1
            mv a1, a0
            mv a0, t0
            j gcd_loop
a1_ge_a0:   sub a1, a1, a0
            j gcd_loop
a0_is_zero: mv a0, a1
gcd_end:    ret
