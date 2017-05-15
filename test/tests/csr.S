        .text
        .globl main
main:   li t1,42
        csrrw zero,mstatus,t1   # Set mstatus to 42
        csrrwi t1,mstatus,13    # Set t1 to mstatus, set mstatus to 13
        csrrw t2,mstatus,zero   # Set t2 to mstatus
        sub a0,t1,t2            # a0 = t1 - t2 = 29
        ret
