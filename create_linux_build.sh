#! /usr/bin/env bash

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
    cmake .. -G Ninja ../.. -DCMAKE_BUILD_TYPE="$1" -DCMAKE_PREFIX_PATH="${LLVM_PREFIX}" $2
    check_cmake_result
    echo
}

rm -rf build_gcc
rm -rf build_clang

mkdir -p build_gcc/debug
mkdir -p build_gcc/release

pushd build_gcc
pushd debug

call_cmake Debug

popd
#pushd release

#call_cmake Release

#popd
popd


mkdir -p build_clang/debug
mkdir -p build_clang/release
mkdir -p build_clang/tracy

pushd build_clang
pushd debug

CC=clang CXX=clang++ call_cmake Debug

popd
#pushd release

#CC=clang CXX=clang++ call_cmake Release

#popd

pushd tracy
CC=clang CXX=clang++ call_cmake Release "-DTRACY_ENABLE=1"
popd

popd


ln -s build_clang/debug build
