OUTPUT_ARCH( "riscv" )
ENTRY( _start)

SECTIONS
{
  . = 0x80000000;
  .text.init : { *(.text.init) } 
  . = ALIGN(0x1000);
  .text : { *(.text) }
  . = ALIGN(0x1000);
  .data : { *(.data) }
  .bss : { *(.bss) }
 
/* .text.init : { init64.o(.text) }*/
/*.text :
{
 mmio64.o(.text)
intermediate64.o(.text) 
}
.data :
{
intermediate64.o(.data)
mmio64.o(.data)
}
*/
/*. = ALIGN(0x1000);
 End of uninitialized data segment */
 _end = .;
}
