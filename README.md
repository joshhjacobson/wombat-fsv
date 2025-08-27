# WOMBAT FSV: A Bayesian framework to assess the impact of fine-scale variation on coarse-resolution flux inversions

This repository contains code to reproduce the results from the WOMBAT FSV extension of the [WOMBAT v2.S framework](https://arxiv.org/abs/2503.09065). Unless stated otherwise, all commands are to be run in the root directory of the repository.


# Installation and environment configuration

This workflow requires R version 4+ and Python version 3.12+, along with a variety of dependency packages in both languages. The easiest way to set up an environment in which to run this code is to use [Conda](https://docs.conda.io/). Instructions for setting up an appropriate conda environment are provided below. If you do not wish to use R through conda, you can adapt the instructions below to a local R installation. For what remains, we assume you have conda installed.

Create a conda environment and set it to use [conda-forge](https://conda-forge.org/):

```
conda create --yes --prefix .conda_env
conda activate ./.conda_env
conda config --env --add channels conda-forge
conda config --env --set channel_priority strict
```

Install conda packages:

```
conda install r-base pkg-config udunits2 gdal libgdal netcdf4 cdo nco
```

Set your CRAN mirror. WOMBAT was made in Australia, so we choose the mirror run by CSIRO, but you can pick your favourite:

```
cat > .conda_env/lib/R/etc/Rprofile.site <<- EOF
local({
  r <- getOption('repos')
  r['CRAN'] <- 'https://cran.csiro.au'
  options(repos = r)
})
EOF
```

Install R dependencies (this might take a while):

```
Rscript -e "renv::restore()"
```


# Getting data

All input data sets go into the `data` directory. There are a few files already there, but the rest will need to be retrieved as described below:

- Posterior samples of model parameters from the WOMBAT v2.S inversion. These samples are [available on Zenodo [link forthcoming]](); the file `samples-LNLGISSIF.rds` should be placed in the `data` directory.
- Gridded coefficients of the GPP and respiration climatology, and gridded residual term for GPP, respiration, and ocean fluxes; we will provide the archive `wombat-fsv-data.tar.gz` containing these files upon request.

Once you have the archive mentioned above, extract the files into the root directory of this repository with

```
tar xzf ~/path/to/wombat-fsv-data.tar.gz
```

The contents of the archive will automatically be added to the `data` directory.


# Running the workflow

The workflow of this repository is split into three main stages:

1. `module-base`: Constructs the grid file and uses conservative remapping to aggregate all time-series terms to the 2 x 2.5 degree grid resolution.
2. `module-samples`: Fits exponential covariance functions to gridded processes by maximum likelihood; performs change-of-support calculations to obtain variances at the basis-function resolution; constructs additional constraints for NEE flux; and performs sampling for the FSV parameter.
3. `module-results`: Summarises the results as a series of plots and tables.

The simplest way to reproduce all results is to run

```
WOMBAT_LOG_LEVEL=debug OMP_NUM_THREADS=8 make -j4 results_targets
```

This will automatically build all "base" and "samples" targets, and create a `figures` directory where all the plots and outputs will be generated. The `-j` option specifies the number of targets to build in parallel, and the `OMP_NUM_THREADS` variable allocates the number of threads available for computations. You can modify these to suit your local system.


## Additional support

Note that it may be helpful to run each module in sequence. For example, you can begin with the "base" module using

```
WOMBAT_LOG_LEVEL=debug OMP_NUM_THREADS=8 make -j4 base_targets
```

It is also possible to build targets one at a time. For example, use

```
WOMBAT_LOG_LEVEL=debug OMP_NUM_THREADS=8 make intermediates/delta-samples-05.rds
```

to build the file containing samples of the $\boldsymbol{\delta}$ parameter using a scale factor of $\gamma^{\omega} = 0.5$.

Depending on your machine, generating the samples will take a couple of hours to run, so you may wish to use a terminal multiplexer (e.g., the [Screen](https://linuxize.com/post/how-to-use-linux-screen/) utility) to avoid connection issues. Note that you will need to reactivate your conda environment when running build commands in a screen session.
