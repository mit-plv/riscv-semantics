CONNECTALDIR ?= connectal
S2H_INTERFACES = \
	PuppetMasterRequest:PuppetMaster.memAndEnq \
	MemoryRequest:PuppetMaster.extmem\
	MemDDRRequest:PuppetMaster.ddrMem
H2S_INTERFACES= \
	PuppetMaster:PuppetMasterIndication
BSVFILES += \
	Hardware/PuppetMaster.bsv
BSVPATH += / \
	Hardware/Puppets \
	Hardware \
	$(CONNECTALDIR)/bsv
CPPFILES = mainS2H.cpp
CONNECTALFLAGS += --mainclockperiod=20
CONNECTALFLAGS += --verilog=ddr3_v2_0/ 
CONNECTALFLAGS += --bscflags " -D BSIM -steps-max-intervals 50"
CONNECTALFLAGS += --bscflags " -show-schedule" 
# CONNECTALFLAGS += --verilog=ddr3_v2_0/ddr3_v2_0/ddr3_v2_0/user_design/rtl 
# CONNECTALFLAGS += --verilog=ddr3_v2_0/ddr3_v2_0/ddr3_v2_0/user_design/rtl/phy 
# CONNECTALFLAGS += --verilog=ddr3_v2_0/ddr3_v2_0/ddr3_v2_0/user_design/rtl/clocking 
# CONNECTALFLAGS += --verilog=ddr3_v2_0/ddr3_v2_0/ddr3_v2_0/user_design/rtl/controller 
# CONNECTALFLAGS += --verilog=ddr3_v2_0/ddr3_v2_0/ddr3_v2_0/user_design/rtl/ecc 
# CONNECTALFLAGS += --verilog=ddr3_v2_0/ddr3_v2_0/ddr3_v2_0/user_design/rtl/ip_top 
# CONNECTALFLAGS += --verilog=ddr3_v2_0/ddr3_v2_0/ddr3_v2_0/user_design/rtl/ui
include $(CONNECTALDIR)/Makefile.connectal

# As the connectal build takes roughly an infinite time, here is a non
# connectal build just to check the bsv.
softx86:
	gcc -m32 -g management.c -o management

soft:
	riscvcc -m32 -march=RV32IM -DRISCV -nostdlib -nostartfiles -c crt.S
	riscvcc -m32 -march=RV32IM -DRISCV -nostdlib -nostartfiles -c management.c
	riscvcc -m32 -march=RV32IM -DRISCV -nostdlib -nostartfiles -o puppet -Tlink.ld management.o crt.o
	elf2hex 8 8192 puppet |  sed 's/.\{8\}/& /g' >  hexpuppet
cleannotconnectal:
	rm -rf Hardware/bofiles
notconnectal:
	mkdir -p Hardware/bofiles
	cp Hardware/ProcConfig.bsv Hardware/bofiles/
	cp Hardware/Puppets/Opcodes.defines Hardware/bofiles
# Various tooling
	bsc -dATS -bdir Hardware/bofiles/ Hardware/Puppets/Ehr.bsv
	bsc -dATS -bdir Hardware/bofiles/ Hardware/StackThomas.bsv
	bsc -dATS -bdir Hardware/bofiles/ Hardware/MaxVector.bsv
	bsc -dATS -bdir Hardware/bofiles/ Hardware/Puppets/ConcatReg.bsv
	bsc -dATS -bdir Hardware/bofiles/ Hardware/Puppets/RegUtil.bsv
# RISCV and puppets
	bsc -bdir Hardware/bofiles/ Hardware/Puppets/VerificationPacket.bsv
	bsc -bdir Hardware/bofiles/ Hardware/Puppets/RVTypes.bsv
	bsc -bdir Hardware/bofiles/ Hardware/Puppets/Abstraction.bsv
	bsc -bdir Hardware/bofiles/ Hardware/Puppets/Opcodes.bsv
	bsc -bdir Hardware/bofiles/ Hardware/Puppets/RVRFile.bsv
	bsc -bdir Hardware/bofiles/ Hardware/Puppets/RVAlu.bsv
	bsc -bdir Hardware/bofiles/ Hardware/Puppets/RVControl.bsv
	bsc -bdir Hardware/bofiles/ Hardware/Puppets/RVCsrFile.bsv
	bsc -bdir Hardware/bofiles/ Hardware/Puppets/RVDecode.bsv
	bsc -bdir Hardware/bofiles/ Hardware/Puppets/RVMemory.bsv
	bsc -bdir Hardware/bofiles/ Hardware/Puppets/RVMulDiv.bsv
	bsc -bdir Hardware/bofiles/ Hardware/Puppets/RVExec.bsv
	bsc -bdir Hardware/bofiles/ Hardware/Puppets/Core.bsv
	bsc -bdir Hardware/bofiles/ Hardware/Puppets/BRAMTightlyCoupledMemory.bsv
	bsc -bdir Hardware/bofiles/ Hardware/Puppets/Puppet.bsv
	bsc -bdir Hardware/bofiles/ Hardware/DDRSim.bsv
# Puppetmaster.
	bsc -bdir Hardware/bofiles/ Hardware/PMTypes.bsv
	bsc -bdir Hardware/bofiles/ Hardware/PM.bsv
	bsc -bdir Hardware/bofiles/ Hardware/PuppetMaster.bsv
