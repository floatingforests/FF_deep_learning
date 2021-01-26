#!/bin/bash

# specify which shell to use.  Bash is recommended unless
# there is a compelling reason to use another.

# Sample slurm submission script for the Gibbs compute cluster
# Lines beginning with # are comments, and will be ignored by
# the interpreter.  Lines beginning with #SBATCH are directives
# to the scheduler.  These in turn can be commented out by
# adding a second # (e.g. ##SBATCH lines will not be processed
# by the scheduler).
#
#
# set name of job
#SBATCH --job-name=vgg_ff_image_mod
#

# set the number of nodes
##SBATCH -N1

# set the number of processes per node
#SBATCH -n 8

#set an account to use
#if not used then default will be used
##SBATCH --account=scavenger

# set the number of GPU cards per node
# --gres=gpu[[:type]:count]
#SBATCH --gres=gpu:GTX980:4

#Or can use this
##SBATCH --gres=gpu:2


# set max wallclock time  DD-HH:MM:SS
#SBATCH --time=14-10:00:00


#To get error and output
#SBATCH --error=vgg_ff_image_mod_myRecord.err
#SBATCH --output=vgg_ff_image_mod_myRecord.out
#

#Optional
# set the partition where the job will run
##SBATCH --partition=GTX670

#Optional
# mail alert at start, end and abortion of execution
##SBATCH --mail-type=ALL

# send mail to this address
#SBATCH --mail-user=jarrett.byrnes@umb.edu


# Put your job commands here, including loading any needed
# modules.

# module load
module load python/3.5.1
module load R/4.0.3

#cd to where the script lives
cd /home/jarrett.byrnes/floating-forests/FF_deep_learning/scripts/kellymod

#execute
Rscript 03_vgg_mod.R