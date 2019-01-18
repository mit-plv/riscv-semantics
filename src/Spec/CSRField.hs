module Spec.CSRField where
import Prelude

data CSRField = MXL | Extensions | -- misa
                SXL | UXL | TSR | TW | TVM | MXR | SUM | MPRV | XS | FS | MPP |
                SPP | MPIE | SPIE | UPIE | MIE | SIE | UIE | SD | -- mstatus (and sstatus)
                MTVecBase | MTVecMode | -- mtvec
                MEDeleg | -- medeleg
                MIDeleg | -- mideleg
                MEIP | SEIP | UEIP | MTIP | STIP | UTIP | MSIP | SSIP | USIP | -- mip
                MEIE | SEIE | UEIE | MTIE | STIE | UTIE | MSIE | SSIE | USIE | -- mie
                MCycle | -- mcycle
                MInstRet | -- minstret
                MHPM | MIR | MTM | MCY | -- mcounteren
                -- TODO: mcountinhibit
                MScratch | -- mscratch
                MEPC | -- mepc
                MCauseInterrupt | MCauseCode | -- mcause
                MTVal | -- mtval
                -- Supervisor-level CSRs:
                STVecBase | STVecMode | -- stvec
                SScratch | -- sscratch
                SEPC | -- sepc
                SCauseInterrupt | SCauseCode | -- scause
                STVal | -- stval
                MODE | ASID | PPN | -- satp
                FFlags | FRM -- fflags, frm, fcsr
  deriving (Ord, Eq, Show)

-- WPRI is implicit in a field's lack of existence here.
data FieldType = RO | RW | WLRL | WARL deriving (Show, Eq)

fieldType MXL = WARL
fieldType Extensions = WARL
fieldType XS = RO
fieldType SD = RO
fieldType MTVecBase = WARL
fieldType MTVecMode = WARL
fieldType MEDeleg = WARL
fieldType MIDeleg = WARL
fieldType MHPM = WARL
fieldType MIR = WARL
fieldType MTM = WARL
fieldType MCY = WARL
fieldType MEPC = WARL
fieldType MCauseCode = WLRL
fieldType MTVal = WARL
fieldType STVecBase = WARL
fieldType STVecMode = WARL
fieldType SEPC = WARL
fieldType SCauseCode = WLRL
fieldType STVal = WARL
fieldType MODE = WARL
fieldType ASID = WARL
fieldType PPN = WARL
fieldType _ = RW
