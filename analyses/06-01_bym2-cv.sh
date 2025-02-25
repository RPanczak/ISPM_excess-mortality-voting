#!/bin/bash
#SBATCH --job-name="bym2-cv"
#SBATCH --cpus-per-task=128
#SBATCH --mem-per-cpu=512
#SBATCH --time=01:00:00
#SBATCH --mail-user=radoslaw.panczak@ispm.unibe.ch
#SBATCH --mail-type=begin,end,fail

ml vital-it
ml R/3.6.1

Rscript ~/ISPM_excess-mortality-voting/analyses/06-02_bym2-cv.R
