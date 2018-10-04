#!/bin/sh
#SBATCH --account=hisafe
#SBATCH --partition=defq
module purge
module load jre/jre.8_x64
cd /nfs/work/hisafe/Capsis4
sh capsis.sh -p script safe.pgms.ScriptGen /lustre/lecomtei/tests/example_exp/agroforestry/agroforestry.sim
