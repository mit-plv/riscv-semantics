#!/usr/bin/python3

# Copyright (c) 2018 Rishiyur S. Nikhil

# Modified and adapted by bthom@mit.edu

import sys
import os
import stat
import subprocess

# ================================================================

simulator = "stack exec -- riscv-semantics-rv32im-compliance device_tree.bin"

num_executed = 0
num_passed   = 0
num_sig_matches = 0
num_sig_mismatches = 0

# ================================================================

def main (argv = None):
    elfs_path = os.path.abspath (os.path.normpath (argv [1]))
    logs_path = os.path.abspath (os.path.normpath (argv [2]))
    os.makedirs (logs_path, exist_ok=True)
    max_level = 20
    traverse (max_level, 0, elfs_path, logs_path, 0)
    sys.stdout.write ("Executed:             {0} tests\n".format (num_executed))
    sys.stdout.write ("PASS:                 {0}\n".format (num_passed))
    sys.stdout.write ("Signature matches:    {0}\n".format (num_sig_matches))
    sys.stdout.write ("Signature mismatches: {0}\n".format (num_sig_mismatches))

# ================================================================
# Recursively traverse the dir tree below elf_path and process each file

def traverse (max_level, level, elfs_path, logs_path, verbosity):
    st = os.stat (elfs_path)
    is_dir = stat.S_ISDIR (st.st_mode)
    is_regular = stat.S_ISREG (st.st_mode)
    do_foreachfile_function (level, is_dir, is_regular, elfs_path, logs_path)
    if is_dir and level < max_level:
        for entry in os.listdir (elfs_path):
            elfs_path1 = os.path.join (elfs_path, entry)
            traverse (max_level, level + 1, elfs_path1, logs_path, verbosity)
    return 0

# ================================================================
# This function is applied to every path in the
# recursive traversal

def do_foreachfile_function (level, is_dir, is_regular, path, logs_path):
    prefix = ""
    for j in range (level): prefix = "  " + prefix

    # directories
    if is_dir:
        print ("%s%d dir %s" % (prefix, level, path))

    # regular files
    elif is_regular:
        dirname  = os.path.dirname (path)
        basename = os.path.basename (path)
        # print ("%s%d %s" % (prefix, level, path))
        do_regular_file_function (level, dirname, basename, logs_path)

    # other files
    else:
        print ("%s%d Unknown file type: %s" % (prefix, level, os.path.basename (path)))

# ================================================================
# For each ELF file, execute it in the Forvis simulator

def do_regular_file_function (level, dirname, basename, logs_path):
    global num_executed
    global num_passed
    global num_sig_matches
    global num_sig_mismatches

    (rootname, ext) = os.path.splitext (basename)
    signame = os.path.join (logs_path, rootname + ".signature")

    # Ignore files without .elf extension
    if not (ext == ".elf"):
        return

    elf_file = os.path.join (dirname, basename)

    arch = mkArchString (elf_file)
    if arch == None: return

    # For debugging only
    # prefix = ""
    # for j in range (level): prefix = "  " + prefix
    # sys.stdout.write ("{0}{1} ACTION:    {2}\n".format (prefix, level, elf_file))


    # Compose command to be run in sub-process
    command = simulator +" " + signame + " " +   elf_file
    print(command)
    # Show command to be executed in sub-process, for info.
#    sys.stdout.write ("----------------\n")
#    sys.stdout.write ("Test {0}\n".format (basename))
#    sys.stdout.write ("    Exec: {0}\n".format (command [0]))
#    sys.stdout.write ("        {0}  {1}  \n".format (command [1], command [2]))
#    for x in command [4:]:
#        sys.stdout.write ("        {0}\n".format (x))

    # Run command command as a sub-process
    completed_process = subprocess.call(command, shell=True)
    num_executed = num_executed + 1
    passed = completed_process == 0
    if passed:
        sys.stdout.write ("    PASS")
        num_passed = num_passed + 1
    else:
        sys.stdout.write ("    FAIL")

    # Diff the produced signature with a reference signature
    refs_path = os.path.join (dirname.replace ("work","riscv-test-suite"),  "references")
    refname = os.path.join (refs_path, rootname + ".reference_output")

    command = ["diff", "-s", refname, signame]
    sys.stdout.write ("    Diffing:\n")
    sys.stdout.write ("    {0}\n".format (refname))
    sys.stdout.write ("    {0}\n".format (signame))
    completed_process = subprocess.call(command)

    if completed_process == 0:
        num_sig_matches    = num_sig_matches + 1
        sys.stdout.write ("    => are identical (signature match)\n")
    else:
        num_sig_mismatches = num_sig_mismatches + 1
        sys.stdout.write ("    => are not identical (signature mismatch)\n")

    return

# ================================================================
# Make an architecture string (e.g., RV64AIMSU) from the test name
# WARNING: this is somewhat fragile in that the test name sometimes does not give full info

def mkArchString (filepath):
    filepath = filepath.lower()
    arch = None
    if "rv32imc" in filepath: 
        arch = None
    elif "rv32im" in filepath:
        arch = "RV32IM"
    elif "rv32i" in filepath:
        arch = "RV32I"
    return arch

# ================================================================
# For non-interactive invocations, call main() and use its return value
# as the exit code.
if __name__ == '__main__':
  sys.exit (main (sys.argv))
