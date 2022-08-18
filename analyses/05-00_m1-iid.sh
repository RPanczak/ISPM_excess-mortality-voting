#!/bin/bash
#SBATCH --job-name="INLA iid"
#SBATCH --cpus-per-task=32
#SBATCH --mem-per-cpu=2G
#SBATCH --time=48:00:00
#SBATCH --mail-user=radoslaw.panczak@ispm.unibe.ch
#SBATCH --mail-type=end,fail

ml vital-it
ml R/3.6.1

Rscript ~/ISPM_excess-mortality-voting/analyses/05-01_m1-iid.R
