#!/bin/sh
rm /nfs/work/hisafe/Capsis4/var/*.log*
sbatch monocrop.sh
sbatch agroforestry.sh
sbatch forestry.sh
