Install riscv gcc from ubuntu repo.
git clone --branch v5.9 --depth 1 https://github.com/torvalds/linux riscv-linux
cp riscv-semantics-linux-config riscv-linux/.config
./build-busybox.sh
cd riscv-linux
make ARCH=riscv CROSS_COMPILE=riscv64-linux-gnu- -j4
