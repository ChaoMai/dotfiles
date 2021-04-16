#!/usr/bin/env bash

# project type
# Makefile:       1
# CMakeLists.txt: 2
# Invalid:        3
function check_project_type() {
    if [[ -e "Makefile" ]]; then
        echo 1
    elif [[ -e "CMakeLists.txt" ]]; then
        echo 2
    else
        echo 3
    fi
}

function cmake_clean() {
    local project_type=$(check_project_type)
    if [[ -e cmake-build ]]; then
        rm -rf cmake-build/

        if [[ $? -ne 0 ]]; then
            echo "cannot rm cmake-build/"
            exit -1
        fi
    fi
}

function cmake_debug() {
    cmake_cmake "Debug"
}

function cmake_release() {
    cmake_cmake "Release"
}

function cmake_cmake() {
    local build_type=$1
    if [[ -z "$build_type" ]]; then
        echo "cmake build type is empty"
        exit -1
    fi

    local project_type=$(check_project_type)
    if [[ $project_type -eq 3 ]]; then
        echo "invalid project"
        exit -1
    fi

    # prepare .ccls
    if [[ ! -e .ccls || ! -s .ccls ]]; then
        echo "%compile_commands.json" > .ccls
    fi

    # prepare cmake-build folder
    if [[ ! -e cmake-build ]]; then
        mkdir cmake-build/
    fi

    if [[ $project_type -eq 1 ]]; then
        check_scan_build=$(command -v intercept-build >/dev/null 2>&1 || echo $?)

        if [[ $check_scan_build -eq 1 ]]; then
            echo "scan-build isn't available"
            exit -1
        fi

        intercept-build make

        if [[ $? -ne 0 ]]; then
            echo "scan-build cannot generate compile_commands.json"
            exit -1
        fi

        mv compile_commands.json cmake-build/
    elif [[ $project_type -eq 2 ]]; then
        cd cmake-build/
        cmake .. -DCMAKE_EXPORT_COMPILE_COMMANDS=ON -DCMAKE_BUILD_TYPE=$build_type

        if [[ $? -ne 0 ]]; then
            echo "cmake cannot generate compile_commands.json"
            exit -1
        fi
    fi
}

function cmake_make() {
    local project_type=$(check_project_type)
    if [[ $project_type -eq 3 ]]; then
        echo "invalid project"
        exit -1
    fi

    if [[ $project_type -eq 1 ]]; then
        make -j 5

        if [[ $? -ne 0 ]]; then
            echo "make failed"
            exit -1
        fi
    elif [[ $project_type -eq 2 ]]; then
        if [[ ! -e cmake-build ]]; then
            echo "cmake-build directory doesn't exist"
            exit -1
        fi

        cd cmake-build/
        make -j 5

        if [[ $? -ne 0 ]]; then
            echo "make failed"
            exit -1
        fi
    fi
}

function cmake_make_clean() {
    local project_type=$(check_project_type)
    if [[ $project_type -eq 3 ]]; then
        echo "invalid project"
        exit -1
    fi

    if [[ $project_type -eq 1 ]]; then
        make clean

        if [[ $? -ne 0 ]]; then
            echo "make clean failed"
            exit -1
        fi
    elif [[ $project_type -eq 2 ]]; then
        if [[ ! -e cmake-build ]]; then
            echo "cmake-build directory doesn't exist"
            exit -1
        fi

        cd cmake-build/
        make clean

        if [[ $? -ne 0 ]]; then
            echo "make clean failed"
            exit -1
        fi
    fi
}

func=$1

if [[ "$func" == "cmake_clean" ]]; then
    cmake_clean
elif [[ "$func" == "cmake_debug" ]]; then
    cmake_debug
elif [[ "$func" == "cmake_release" ]]; then
    cmake_release
elif [[ "$func" == "cmake_make" ]]; then
    cmake_make
elif [[ "$func" == "cmake_make_clean" ]]; then
    cmake_make_clean
fi

