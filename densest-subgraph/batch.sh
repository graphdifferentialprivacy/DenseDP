#!/bin/bash

#SBATCH -N 1
#SBATCH -c 40
#SBATCH --mem=300000
#SBATCH -A biocomplexity
#SBATCH -p bii
#SBATCH --time=7-00:00:00

module load sbt

cd ~/git/denset-test-quickpar/densest-subgraph
sbt "run ${SLURM_ARRAY_TASK_ID}"
