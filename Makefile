UTILS_PARTIAL = partials/utils.R
export UTILS_PARTIAL
DISPLAY_PARTIAL = partials/display.R
export DISPLAY_PARTIAL
UTILS_CPP_PARTIAL = partials/utils.cpp
export UTILS_CPP_PARTIAL
HMC_EXACT_CPP_PARTIAL = partials/hmc-exact.cpp
export HMC_EXACT_CPP_PARTIAL
MLE_PARTIAL = partials/mle.R
export MLE_PARTIAL


GEOS_2X25_GRID = data/geos.2x25.grid
REGION_GRID = data/region-grid.rds
PERTURBATIONS = data/perturbations.fst
PERTURBATIONS_AUGMENTED = data/perturbations-augmented.fst
CONTROL_EMISSIONS = data/control-emissions.fst
BASIS_VECTORS = data/basis-vectors.fst
CONSTRAINTS = data/constraints.rds
ALPHA_SAMPLES = data/samples-LNLGISSIF.rds

SIB4_INPUT_YEARS = 2014 2015 2016 2017 2018 2019 2020

SIB4_CLIMATOLOGY_ASSIM_2x25 = data/sib4-climatology-assim-2x25.nc
SIB4_CLIMATOLOGY_RESP_TOT_2x25 = data/sib4-climatology-resp-tot-2x25.nc
SIB4_RESIDUAL_ASSIM_HOURLY = $(foreach SIB4_YEAR,$(SIB4_INPUT_YEARS),data/sib4-hourly-residual-assim-$(SIB4_YEAR).nc)
SIB4_RESIDUAL_RESP_TOT_HOURLY = $(foreach SIB4_YEAR,$(SIB4_INPUT_YEARS),data/sib4-hourly-residual-resp-tot-$(SIB4_YEAR).nc)
LANDSCHUTZER_INVENTORY_RESIDUAL = data/spco2_MPI-SOM_FFN_v2020_residual.nc

include module-base.mk
include module-samples.mk
include module-results.mk

all:
	echo "Please refer to the README for instructions on how to run this project"

base_targets: $(BASE_TARGETS)
samples_targets: $(SAMPLES_TARGETS)
results_targets: $(RESULTS_TARGETS)
