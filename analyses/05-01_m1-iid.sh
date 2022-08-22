#!/bin/bash
#SBATCH --job-name="i-iid"
#SBATCH --cpus-per-task=64
#SBATCH --mem-per-cpu=1G
#SBATCH --time=02:00:00
#SBATCH --mail-user=radoslaw.panczak@ispm.unibe.ch
#SBATCH --mail-type=end,fail

ml vital-it
ml R/3.6.1

Rscript ~/ISPM_excess-mortality-voting/analyses/05-02_m1-iid.R
