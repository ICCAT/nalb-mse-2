#!/bin/bash

#SBATCH -J ALB_MSE

#SBATCH --ntasks=1
#SBATCH --cpus-per-task=1

#SBATCH --nodes=1
#SBATCH --ntasks-per-node=1
#SBATCH --mem=10GB  # total memory per node
#SBATCH --time=12:00:00
#SBATCH --constraint=cascadelake
#SBATCH --output=logs_init/%x_%a_job_%j.stdout # Output file
#SBATCH --error=logs_init/%x_%a_job_%j.stderr # Error file


module load GSL
module load R/4.4.1-gfbf-2023a

echo Starting job $SLURM_JOB_NAME - task $SLURM_JOBID - iter $SLURM_ARRAY_TASK_ID

echo TMP_DIR $TMP_DIR

srun Rscript Run_R4b.R

module unload R/4.4.1-gfbf-2023a
module unload GSL