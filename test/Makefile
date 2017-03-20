soft:
	riscvcc -m32 -march=RV32IM -DRISCV -nostdlib -nostartfiles -c init.S
	riscvcc -m32 -march=RV32IM -DRISCV -nostdlib -nostartfiles -c add.c -o intermediate.o
	riscvcc -m32 -march=RV32IM -DRISCV -nostdlib -nostartfiles -o add -Tlink.ld intermediate.o init.o
	elf2hex add
