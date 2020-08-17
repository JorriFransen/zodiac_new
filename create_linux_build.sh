#! /usr/bin/env sh

LLVM_PREFIX=/home/jorri/dev/llvm/install_release

check_cmake_result () {
    cmake_res=$?
    if [[ $cmake_res -ne 0 ]]; then
        echo
        echo Error encountered while generating cmake project...
        echo
        exit $cmake_res
    fi
}

call_cmake () {
    cmake .. -G Ninja ../.. -DCMAKE_BUILD_TYPE="$1" -DCMAKE_PREFIX_PATH="${LLVM_PREFIX}"
    echo 
}

rm -rf build_gcc
rm -rf build_clang
rm -rf build

mkdir -p build_gcc/debug
mkdir -p build_gcc/release

pushd build_gcc
pushd debug

call_cmake Debug
check_cmake_result

popd
pushd release

call_cmake Release
check_cmake_result

popd
popd


mkdir -p build_clang/debug
mkdir -p build_clang/release

pushd build_clang
pushd debug

CC=clang CXX=clang++ call_cmake Debug
check_cmake_result

popd
pushd release

CC=clang CXX=clang++ call_cmake Release
check_cmake_result

popd
popd

ln -s build_clang/debug build
