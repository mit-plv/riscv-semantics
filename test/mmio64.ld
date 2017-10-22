OUTPUT_ARCH( "riscv" )
 ENTRY( main )
 SECTIONS
 {
 /* text: test code section */
 . = 0x200;
 .text.init : { init64.o(.text) }
 .text :
 {
 intermediate64.o(.text)
 mmio64.o(.text)
 }
 /* data: Initialized data segment */
 .data :
 {
 intermediate64.o(.data)
 mmio64.o(.data)
 }
 /* End of uninitialized data segment */
 _end = .;
 }
