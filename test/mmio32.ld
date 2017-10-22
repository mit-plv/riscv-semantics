OUTPUT_ARCH( "riscv" )
 ENTRY( main )
 SECTIONS
 {
 /* text: test code section */
 . = 0x200;
 .text.init : { init32.o(.text) }
 .text :
 {
 intermediate32.o(.text)
 mmio32.o(.text)
 }
 /* data: Initialized data segment */
 .data :
 {
 intermediate32.o(.data)
 mmio32.o(.data)
 }
 /* End of uninitialized data segment */
 _end = .;
 }
