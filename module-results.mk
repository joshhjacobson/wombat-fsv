$(shell mkdir -p figures)

REGIONS = global Region01 Region02 Region03 Region04 Region05 Region06 Region07 Region08 Region09 Region10 Region11 RegionNZ
FLUX_DECOMPOSITIONS = \
	$(foreach SCALE_FACTOR,$(SCALE_FACTORS),\
	$(foreach REGION,$(REGIONS),\
	figures/flux-decomposition-$(REGION)_$(SCALE_FACTOR).pdf))

RESULTS_TARGETS += \
	figures/flux-annual-average-comparison.tex \
	figures/flux-annual-average-05.csv \
	figures/flux-net-decomposition-global-05.pdf \
	figures/flux-net-decomposition-global-10.pdf \
	figures/residual-standard-deviation-Region02.pdf


figures/flux-annual-average-comparison.tex: \
	src/flux-annual-average-comparison.R \
	$(PERTURBATIONS_AUGMENTED) \
	$(ALPHA_SAMPLES) \
	$(DELTA_SAMPLES)
	Rscript $< \
		--perturbations-augmented $(PERTURBATIONS_AUGMENTED) \
		--samples-alpha $(ALPHA_SAMPLES) \
		--samples-delta-list $(DELTA_SAMPLES) \
		--output $@

figures/flux-annual-average-%.csv: \
	src/flux-annual-average.R \
	$(PERTURBATIONS_AUGMENTED) \
	$(ALPHA_SAMPLES) \
	$(DELTA_SAMPLES)
	Rscript $< \
		--perturbations-augmented $(PERTURBATIONS_AUGMENTED) \
		--samples-alpha $(ALPHA_SAMPLES) \
		--samples-delta intermediates/delta-samples-$*.rds \
		--scale-factor $* \
		--output $@

figures/flux-net-decomposition-global-%.pdf: \
	src/flux-net-decomposition.R \
	$(PERTURBATIONS_AUGMENTED) \
	$(ALPHA_SAMPLES) \
	$(DELTA_SAMPLES) \
	$(DISPLAY_PARTIAL)
	Rscript $< \
		--perturbations-augmented $(PERTURBATIONS_AUGMENTED) \
		--samples-alpha $(ALPHA_SAMPLES) \
		--samples-delta intermediates/delta-samples-$*.rds \
		--output $@

figures/residual-standard-deviation-%.pdf: \
	src/residual-standard-deviation.R \
	$(PERTURBATIONS_AUGMENTED) \
	$(DELTA_PRECISION_DIAGONALS)
	Rscript $< \
		--perturbations-augmented $(PERTURBATIONS_AUGMENTED) \
		--delta-precision-diagonals $(DELTA_PRECISION_DIAGONALS) \
		--region $* \
		--output $@

# Supplementary analyses

figures/flux-global-%.pdf: \
	src/flux-net-global.R \
	$(PERTURBATIONS_AUGMENTED) \
	$(ALPHA_SAMPLES) \
	$(DELTA_SAMPLES)
	Rscript $< \
		--perturbations-augmented $(PERTURBATIONS_AUGMENTED) \
		--samples-alpha $(ALPHA_SAMPLES) \
		--samples-delta intermediates/delta-samples-$*.rds \
		--design-case $* \
		--output $@

figures/flux-regional-%.pdf: \
	src/flux-net-regional.R \
	$(PERTURBATIONS_AUGMENTED) \
	$(ALPHA_SAMPLES) \
	$(DELTA_SAMPLES)
	Rscript $< \
		--perturbations-augmented $(PERTURBATIONS_AUGMENTED) \
		--samples-alpha $(ALPHA_SAMPLES) \
		--samples-delta intermediates/delta-samples-$*.rds \
		--design-case $* \
		--output $@

figures/flux-decomposition-%.pdf: \
	src/flux-decomposition.R \
	$(PERTURBATIONS_AUGMENTED) \
	$(ALPHA_SAMPLES) \
	$(DELTA_SAMPLES)
	Rscript $< \
		--perturbations-augmented $(PERTURBATIONS_AUGMENTED) \
		--samples-alpha $(ALPHA_SAMPLES) \
		--samples-delta intermediates/delta-samples-$(lastword $(subst _, ,$*)).rds \
		--region $(firstword $(subst _, ,$*)) \
		--design-case $(lastword $(subst _, ,$*)) \
		--output $@
