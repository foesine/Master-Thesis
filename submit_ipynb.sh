#!/bin/bash
#SBATCH --job-name=simulation        # Name of the job
#SBATCH --ntasks=1                   # Number of tasks (typically 1 for R scripts)
#SBATCH --time=2-00:00:00            # Maximum runtime (format:D-HH:MM:SS), max 2 days
#SBATCH --mem=32G                    # Memory per node
#SBATCH --partition=medium           # Partition use default
#SBATCH -o /user/emma.foessing01/u11969/logs/run_%J.out
#SBATCH -e /user/emma.foessing01/u11969/logs/run_%J.error

# Load necessary modules
module purge  # Clear all loaded modules
#module load rev/11.06
module load r/4.4.0

which R

# Set R_LIBS_USER to the custom library path
export R_LIBS_USER=/user/emma.foessing01/u11969/R_libs

# Install dependencies first
Rscript -e 'list_of_packages <- c("bnlearn", "arulesCBA", "network", "igraph", "xgboost","data.table", "RSNNS");
                                install.packages(list_of_packages, dependencies = TRUE, repos="http://cran.us.r-project.org", lib="/user/emma.foessing01/u11969/R_libs")'


# Ensure Jupyter and nbconvert are available
pip install --user nbconvert

# Execute Jupyter notebook
jupyter nbconvert --to notebook --execute --ExecutePreprocessor.kernel_name=ir /user/emma.foessing01/u11969/Master-Thesis/Simulation.ipynb --output /user/emma.foessing01/u11969/Master-Thesis/executed_sim.ipynb --ExecutePreprocessor.timeout=-1