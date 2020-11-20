 OUTPUT_ARCH( "riscv" )
 ENTRY( _start )
 SECTIONS
 {
 /* text: test code section */
 . = 0x0;
 .text :
 {
 *.o(.text)
 }
 /* data: Initialized data segment */
 .data :
 {
 *.o(.data)
 }
 /* End of uninitialized data segment */
 _end = .;
 }

