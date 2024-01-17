#! /bin/bash

cmake -S. -B_build -DCOMPILE_TARGET=summa_openwq -DCMAKE_BUILD_TYPE=debug
cmake --build _build -j 4