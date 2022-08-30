#!/bin/bash
#SBATCH --job-name="i-bym2"
#SBATCH --cpus-per-task=128
#SBATCH --mem-per-cpu=512
#SBATCH --time=12:00:00
#SBATCH --mail-user=radoslaw.panczak@ispm.unibe.ch
#SBATCH --mail-type=begin,end,fail

ml vital-it
ml R/3.6.1

Rscript ~/ISPM_excess-mortality-voting/analyses/05-04_m2-bym2.R
