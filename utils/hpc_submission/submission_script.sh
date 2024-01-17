#!/bin/bash
#SBATCH --cpus-per-task=1
#SBATCH --time=48:00:00
#SBATCH --mem=4G
#SBATCH --job-name=Summa-OpenWQ
#SBATCH --account=
#SBATCH --output=

module load singularity

# ALL PATHS THAT START WITH CLUSTER ARE RELATIVE TO THE CLUSTER, NOT THE CONTAINER

# PATH TO THE APPTAINER CONTAINER (SIF FILE)
CLUSTER_APPTAINER_SIF=""

# PATH to the Summa-openWQ source code
CLUSTER_SUMMA_OPENWQ_DIR=""

# PATH to the input directory 
# (both the openWQ and Summa input files should be accesible from here)
CLUSTER_INPUT_DIR=""

# PATH to the output directory 
# (both the openWQ and Summa output files will be written here)
# This submission script will create a subdirectory for each job if using 
# the $SLURM_ARRAY_TASK_ID variable and for summa and openWQ output
CLUSTER_OUPUT_DIR=""
mkdir -p $CLUSTER_OUPUT_DIR/out_$SLURM_ARRAY_TASK_ID/
mkdir -p $CLUSTER_OUPUT_DIR/out_$SLURM_ARRAY_TASK_ID/hdf5
mkdir -p $CLUSTER_OUPUT_DIR/out_$SLURM_ARRAY_TASK_ID/netcdf

# ALL PATHS THAT START WITH CONTAINER ARE RELATIVE TO THE CONTAINER
# Path to binary used to run Summa-openWQ
CONTAINER_SUMMA_OPENWQ_BIN=""

# Path to the output directory in the container (does not need to be changed)
# The configuration files for openWQ and Summa will need to correspond to this path
CONTAINER_OUTPUT_DIR="/output"

# Path to the input directory in the container (does not need to be changed)
# The configuration files for openWQ and Summa will need to correspond to this path
CONTAINER_INPUT_DIR="/input"

# Path to the openWQ master json file. THe rest of the path afer the 
# CONTAINER_INPUT_DIR needs to be added
CONTAINER_MASTER_JSON="$CONTAINER_INPUT_DIR/"

#######
# Simulation Set up
#######

gru_max=21412
gru_count=700
# This path is relative to the container
summa_file_manager=/input/Summa_Settings/fileManager.txt
offset=$SLURM_ARRAY_TASK_ID

gru_start=$(( 1 + gru_count*offset ))
check=$(( $gru_start + $gru_count ))

# Adust the number of grus for the last job
if [ $check -gt $gru_max ]  
then
    gru_count=$(( gru_max-gru_start+1 ))
fi

# Start the shell
singularity exec \
  --bind $CLUSTER_SUMMA_OPENWQ_DIR:/code/Summa-openWQ \
  --bind $CLUSTER_OUPUT_DIR/out_$SLURM_ARRAY_TASK_ID:$CONTAINER_OUTPUT_DIR \
  --bind $CLUSTER_INPUT_DIR:$CONTAINER_INPUT_DIR \
  --env master_json=$CONTAINER_MASTER_JSON \
  --env GMON_OUT_PREFIX=$CONTAINER_OUTPUT_DIR/gmon.out \
  $CLUSTER_APPTAINER_SIF \
  $CONTAINER_SUMMA_OPENWQ_BIN -g $gru_start $gru_count -m $summa_file_manager