#!/bin/csh
#SBATCH --time=14-00:00:00
#SBATCH --mail-type=ALL
#SBATCH --nodelist=compute-5-4
source ~/loadR.csh 3.3.2
echo $R_LIBS
echo 'start Rscript hpc_wslab.R'
Rscript $HOME/hpc_wslab.R
