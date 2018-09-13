#!/bin/bash


uname="$(uname -s)"
unameM="$(uname -m)"
fileHeader="gnu-mcu-eclipse-riscv-none-gcc-7.2.0-2-20180111-2230-"
urlHeader="https://github.com/gnu-mcu-eclipse/riscv-none-gcc/releases/download/v7.2.0-2-20180110/"
installPath=gnu-mcu-eclipse/riscv-none-gcc/7.2.0-2-20180111-2230
binFile=$installPath/bin/riscv-none-embed-gcc

if [ -e $binFile ]
then
    echo "Riscv-gcc has been installed."
    echo "To uninstall riscv-gcc, use 'make uninstall-gcc'."
    exit 0
fi

echo "Downloading riscv-gcc..."
case $uname in
    Linux*)
        if [ $unameM = "x86_64" ]; then
            file=$fileHeader"centos64.tgz"
        else
            file=$fileHeader"centos32.tgz"
        fi
        curl -# -L -O $urlHeader$file
        ;;
    Darwin*)
        file=$fileHeader"osx.tgz"
        curl -# -L -O $urlHeader$file
        ;;
    CYGWIN*)
        if [ $unameM = "x86_64" ]; then
            file=$fileHeader"centos64.tgz"
        else
            file=$fileHeader"centos32.tgz"
        fi
        curl -# -L -O $urlHeader$file
        ;;
    MINGW32*)
        echo "MINGW32 not supported yet."
        exit 1
        ;;
    MINGW64*)
        echo "MINGW64 not supported yet."
        exit 1
        ;;
    *)
        echo "UNKNOWN:$uname"
        exit 1
esac

tar xf $file
rm -f $file
#chmod -R -w $installPath
cd ..
echo "Riscv-gcc has been installed."
