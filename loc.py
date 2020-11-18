import sys

file2proj = {
  "./Platform/BufferMMIO.hs": "Simulator",
  "./Platform/ClashAccelerator.hs": "Clash",
  "./Platform/Clash.hs": "Clash",
  "./Platform/CleanTest.hs": "???",
  "./Platform/Clint.hs": "Simulator",
  "./Platform/ExecuteClash.hs": "Clash",
  "./Platform/GcdExpr.hs": "Gcd",
  "./Platform/MainCompliance.hs": "riscv-tests",
  "./Platform/MainMM.hs": "WMM",
  "./Platform/MainRun.hs": "???",
  "./Platform/MainTest.hs": "???",
  "./Platform/MemoryModelTracking.hs": "WMM",
  "./Platform/Minimal32.hs": "???",
  "./Platform/Minimal64.hs": "???",
  "./Platform/MMIO.hs": "???",
  "./Platform/Plic.hs": "Simulator",
  "./Platform/Pty.hs": "Simulator",
  "./Platform/Run32.hs": "???",
  "./Platform/RunCompliance.hs": "riscv-tests",
  "./Platform/RunFast.hs": "Simulator",
  "./Platform/Run.hs": "???",
  "./Platform/RunMM.hs": "WMM",
  "./Platform/Test.hs": "???",
  "./Platform/TestTLB.hs": "???",
  "./Platform/TLBExperiment.hs": "???",
  "./Spec/CSRField.hs": "CSR fields",
  "./Spec/CSRFile.hs": "CSR fields",
  "./Spec/CSRFileIO.hs": "CSR fields",
  "./Spec/CSRGetSet.hs": "CSR fields",
  "./Spec/CSR.hs": "CSR fields",
  "./Spec/CSRSpec.hs": "CSR fields",
  "./Spec/Decode.hs": "Decode",
  "./Spec/Execute32.hs": "Execute",
  "./Spec/ExecuteA64.hs": "Execute",
  "./Spec/ExecuteA.hs": "Execute",
  "./Spec/ExecuteCSR.hs": "Execute",
  "./Spec/ExecuteF64.hs": "Execute",
  "./Spec/ExecuteF.hs": "Execute",
  "./Spec/Execute.hs": "Execute",
  "./Spec/ExecuteI64.hs": "Execute",
  "./Spec/ExecuteI.hs": "Execute",
  "./Spec/ExecuteM64.hs": "Execute",
  "./Spec/ExecuteM.hs": "Execute",
  "./Spec/Machine.hs": "Primitives",
  "./Spec/Memory.hs": "???",
  "./Spec/Spec.hs": "???",
  "./Spec/VirtualMemory.hs": "???",
  "./Utility/Elf.hs": "Utilities",
  "./Utility/ListMemory.hs": "Utilities",
  "./Utility/MapMemory.hs": "Utilities",
  "./Utility/Utility.hs": "Utilities"
}

proj2loc = {}

for line in sys.stdin:
    line = line.strip()
    if line == '': continue
    if line == 'language,filename,blank,comment,code,"github.com/AlDanial/cloc v 1.82"': continue
    [language, filename, blankLines, commentLines, codeLines] = line.split(",")
    if language == 'SUM': continue
    if filename in file2proj:
        proj = file2proj[filename]
        oldcount = proj2loc.get(proj, 0)
        proj2loc[proj] = oldcount + int(codeLines)
    else:
        print(f'  "{filename}": "???",')

def table(title, projects):
    print(r'\begin{tabular}{lr}')
    print(r'\toprule')
    print(f'{title:15} &  LOC \\\\')
    print(r'\midrule')
    total = 0
    for proj in projects:
        print(f'{proj:15} & {proj2loc[proj]:4} \\\\')
        total += proj2loc[proj]
        proj2loc.pop(proj)
    print(r'\midrule')
    print(f'{"Total":15} & {total:4} \\\\')
    print(r'\bottomrule')
    print(r'\end{tabular}')

table("Spec part", ["Primitives", "Decode", "CSR fields", "Execute", "Utilities"])
print(r"\hspace{2em}")
table("Use case", ["Simulator", "riscv-tests", "Clash", "Gcd", "WMM"])
if proj2loc:
    print("")
    print(r"\todo{also count " + ", ".join(proj2loc.keys()) + "}")
