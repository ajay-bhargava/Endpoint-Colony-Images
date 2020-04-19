#!/bin/bash

# Job Parameters
#SBATCH --job-name=DIST
#SBATCH --mem=16G
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --cpus-per-task=32
#SBATCH --mail-type=END,FAIL # notifications for job done & fail
#SBATCH --mail-user=ajay.bhargava@crick.ac.uk
#SBATCH --partition=cpu
#SBATCH --output=reports/%A_%a_%u_report.out
#SBATCH --error=errors/%A_%a_%u_log.err
#SBATCH --time=0-12:00:00

echo "SLURM_JOBID="$SLURM_JOBID
echo "SLURM_JOB_NODELIST"=$SLURM_JOB_NODELIST
echo "SLURM_NNODES"=$SLURM_NNODES
echo "SLURM_NCPUS"=$SLURM_JOB_CPUS_PER_NODE

echo "R Processing tools..."
module load Singularity
SRCDIR=$HOME/working/Ajay/Thesis/Experiments/Subclone-Size-Distribution/scheduling/

echo "Loading CentOS 7 Environment and script:"
singularity exec -B /camp/lab/sahaie/working/ -i /home/camp/bhargaa/working/Ajay/HPC/environments/singularity/r_imaging-deploy_v3.simg $SRCDIR/Program-Caller.sh
