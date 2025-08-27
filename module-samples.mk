
MLE_CLIMATOLOGY_ASSIM = intermediates/mle-climatology-assim.fst
MLE_CLIMATOLOGY_RESP_TOT = intermediates/mle-climatology-resp-tot.fst
MLE_RESIDUAL_ASSIM = $(foreach SIB4_YEAR,$(SIB4_INPUT_YEARS),intermediates/mle-residual-assim-$(SIB4_YEAR).fst)
MLE_RESIDUAL_RESP_TOT = $(foreach SIB4_YEAR,$(SIB4_INPUT_YEARS),intermediates/mle-residual-resp-tot-$(SIB4_YEAR).fst)
MLE_RESIDUAL_OCEAN = intermediates/mle-residual-ocean.fst

VARIANCE_CLIMATOLOGY_ASSIM = intermediates/variance-climatology-assim.fst
VARIANCE_CLIMATOLOGY_RESP_TOT = intermediates/variance-climatology-resp-tot.fst
VARIANCE_RESIDUAL_ASSIM = $(foreach SIB4_YEAR,$(SIB4_INPUT_YEARS),intermediates/variance-residual-assim-$(SIB4_YEAR).fst)
VARIANCE_RESIDUAL_RESP_TOT = $(foreach SIB4_YEAR,$(SIB4_INPUT_YEARS),intermediates/variance-residual-resp-tot-$(SIB4_YEAR).fst)
VARIANCE_RESIDUAL_OCEAN = intermediates/variance-residual-ocean.fst
VARIANCES = $(VARIANCE_CLIMATOLOGY_ASSIM) \
	$(VARIANCE_CLIMATOLOGY_RESP_TOT) \
	$(VARIANCE_RESIDUAL_ASSIM) \
	$(VARIANCE_RESIDUAL_RESP_TOT) \
	$(VARIANCE_RESIDUAL_OCEAN)

DELTA_PRECISION_DIAGONALS = intermediates/delta-precision-diagonals.fst
CONSTRAINTS_NEE = intermediates/constraints-nee.rds

SCALE_FACTORS = 01 05 10 15
DELTA_SAMPLES_BASE = intermediates/delta-samples
DELTA_SAMPLES = $(foreach SCALE_FACTOR,$(SCALE_FACTORS),$(DELTA_SAMPLES_BASE)-$(SCALE_FACTOR).rds)

SAMPLES_TARGETS += $(DELTA_SAMPLES)


$(DELTA_SAMPLES_BASE)-%.rds: \
	src/samples.R \
	$(BASIS_VECTORS) \
	$(CONSTRAINTS) \
	$(CONSTRAINTS_NEE) \
	$(ALPHA_SAMPLES) \
	$(DELTA_PRECISION_DIAGONALS)
	Rscript $< \
		--basis-vectors $(BASIS_VECTORS) \
		--constraints $(CONSTRAINTS) \
		--constraints-nee $(CONSTRAINTS_NEE) \
		--alpha-samples $(ALPHA_SAMPLES) \
		--delta-precision-diagonals $(DELTA_PRECISION_DIAGONALS) \
		--scale-factor $* \
		--output $@

$(DELTA_PRECISION_DIAGONALS): \
	src/delta-precision-diagonals.R \
	$(BASIS_VECTORS) \
	$(VARIANCES)
	Rscript $< \
		--basis-vectors $(BASIS_VECTORS) \
		--var-climatology-assim $(VARIANCE_CLIMATOLOGY_ASSIM) \
		--var-climatology-resp-tot $(VARIANCE_CLIMATOLOGY_RESP_TOT) \
		--var-residual-assim $(VARIANCE_RESIDUAL_ASSIM) \
		--var-residual-resp-tot $(VARIANCE_RESIDUAL_RESP_TOT) \
		--var-residual-ocean $(VARIANCE_RESIDUAL_OCEAN) \
		--output $@

$(CONSTRAINTS_NEE): \
	src/constraints-nee.R \
	$(BASIS_VECTORS) \
	$(CONTROL_EMISSIONS) \
	$(PERTURBATIONS)
	Rscript $< \
		--basis-vectors $(BASIS_VECTORS) \
		--control-emissions $(CONTROL_EMISSIONS) \
		--perturbations $(PERTURBATIONS) \
		--output $@

$(VARIANCE_CLIMATOLOGY_ASSIM): \
	src/variance-climatology.R \
	$(MLE_CLIMATOLOGY_ASSIM) \
	$(SIB4_CLIMATOLOGY_ASSIM_2x25) \
	$(REGION_GRID_DF)
	Rscript $< \
		--input-climatology $(SIB4_CLIMATOLOGY_ASSIM_2x25) \
		--mle-climatology $(MLE_CLIMATOLOGY_ASSIM) \
		--region-grid-df $(REGION_GRID_DF) \
		--output $@

$(VARIANCE_CLIMATOLOGY_RESP_TOT): \
	src/variance-climatology.R \
	$(MLE_CLIMATOLOGY_RESP_TOT) \
	$(SIB4_CLIMATOLOGY_RESP_TOT_2x25) \
	$(REGION_GRID_DF)
	Rscript $< \
		--input-climatology $(SIB4_CLIMATOLOGY_RESP_TOT_2x25) \
		--mle-climatology $(MLE_CLIMATOLOGY_RESP_TOT) \
		--region-grid-df $(REGION_GRID_DF) \
		--output $@

intermediates/variance-residual-assim-%.fst: \
	src/variance-residual.R \
	intermediates/mle-residual-assim-%.fst \
	$(SIB4_RESIDUAL_ASSIM_HOURLY_2X25) \
	$(REGION_GRID_DF)
	Rscript $< \
		--input-residual intermediates/sib4-hourly-residual-assim-2x25-$*.nc \
		--mle-residual intermediates/mle-residual-assim-$*.fst \
		--region-grid-df $(REGION_GRID_DF) \
		--output $@

intermediates/variance-residual-resp-tot-%.fst: \
	src/variance-residual.R \
	intermediates/mle-residual-resp-tot-%.fst \
	$(SIB4_RESIDUAL_RESP_TOT_HOURLY_2X25) \
	$(REGION_GRID_DF)
	Rscript $< \
		--input-residual intermediates/sib4-hourly-residual-resp-tot-2x25-$*.nc \
		--mle-residual intermediates/mle-residual-resp-tot-$*.fst \
		--region-grid-df $(REGION_GRID_DF) \
		--output $@

$(VARIANCE_RESIDUAL_OCEAN): \
	src/variance-ocean.R \
	$(MLE_RESIDUAL_OCEAN) \
	$(LANDSCHUTZER_INVENTORY_RESIDUAL_2X25) \
	$(REGION_GRID_DF)
	Rscript $< \
		--input-residual $(LANDSCHUTZER_INVENTORY_RESIDUAL_2X25) \
		--mle-residual $(MLE_RESIDUAL_OCEAN) \
		--region-grid-df $(REGION_GRID_DF) \
		--output $@


$(MLE_CLIMATOLOGY_ASSIM): \
	src/mle-climatology.R \
	$(MLE_PARTIAL) \
	$(SIB4_CLIMATOLOGY_ASSIM_2x25) \
	$(REGION_GRID_DF)
	Rscript $< \
		--input-climatology $(SIB4_CLIMATOLOGY_ASSIM_2x25) \
		--region-grid-df $(REGION_GRID_DF) \
		--output $@

$(MLE_CLIMATOLOGY_RESP_TOT): \
	src/mle-climatology.R \
	$(MLE_PARTIAL) \
	$(SIB4_CLIMATOLOGY_RESP_TOT_2x25) \
	$(REGION_GRID_DF)
	Rscript $< \
		--input-climatology $(SIB4_CLIMATOLOGY_RESP_TOT_2x25) \
		--region-grid-df $(REGION_GRID_DF) \
		--output $@

intermediates/mle-residual-assim-%.fst: \
	src/mle-residual.R \
	$(MLE_PARTIAL) \
	$(SIB4_RESIDUAL_ASSIM_HOURLY_CENTRED_2X25)
	Rscript $< \
		--input-residual intermediates/sib4-hourly-residual-centred-assim-2x25-$*.fst \
		--output $@

intermediates/mle-residual-resp-tot-%.fst: \
	src/mle-residual.R \
	$(MLE_PARTIAL) \
	$(SIB4_RESIDUAL_RESP_TOT_HOURLY_CENTRED_2X25)
	Rscript $< \
		--input-residual intermediates/sib4-hourly-residual-centred-resp-tot-2x25-$*.fst \
		--output $@

$(MLE_RESIDUAL_OCEAN): \
	src/mle-ocean.R \
	$(MLE_PARTIAL) \
	$(LANDSCHUTZER_INVENTORY_RESIDUAL_2X25) \
	$(REGION_GRID_DF)
	Rscript $< \
		--input-residual $(LANDSCHUTZER_INVENTORY_RESIDUAL_2X25) \
		--region-grid-df $(REGION_GRID_DF) \
		--output $@
