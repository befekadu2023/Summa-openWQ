#! /bin/bash
# Set path to the top of Summa-OpenWQ with PROJECT_DIR
# Add the sytnethic tests directory if desired
export PROJECT_DIR=$(realpath $(pwd)/../../../)
# export SYNTHETIC_TESTS=

docker run -d -it --name SUMMA-openWQ \
    -v $PROJECT_DIR:/code/Summa-OpenWQ \
    summa-openwq:latest