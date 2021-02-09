#! /usr/bin/env sh

wd=$(dirname $0)

echo "wd: $wd"

for f in $wd/tests/*.zdc
do
    echo "==================================================================="
    echo "===================================================================\n"
    echo COMPILING AND RUNNING BYTECODE
    echo  Running command: ./zodiac $f
    echo
    ./zodiac $f
    echo
    _exe_name=${f%.*}
    exe_name=./${_exe_name#"$wd"/tests/}
    if [ -e $exe_name ]
    then
        echo !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        echo RUNNING EXE: $exe_name
        echo
        eval $exe_name
        rm $exe_name
        rm $exe_name.o
    fi
    echo
    echo "==================================================================="
    echo "===================================================================\n\n"
done
