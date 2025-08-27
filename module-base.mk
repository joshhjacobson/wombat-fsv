$(shell mkdir -p intermediates)

REGION_GRID_DF = intermediates/region-grid-df.fst

SIB4_RESIDUAL_ASSIM_HOURLY_2X25 = $(foreach SIB4_YEAR,$(SIB4_INPUT_YEARS),intermediates/sib4-hourly-residual-assim-2x25-$(SIB4_YEAR).nc)
SIB4_RESIDUAL_RESP_TOT_HOURLY_2X25 = $(foreach SIB4_YEAR,$(SIB4_INPUT_YEARS),intermediates/sib4-hourly-residual-resp-tot-2x25-$(SIB4_YEAR).nc)
LANDSCHUTZER_INVENTORY_RESIDUAL_2X25 = intermediates/spco2_MPI-SOM_FFN_v2020_residual_2x25.nc

SIB4_RESIDUAL_ASSIM_HOURLY_CENTRED_2X25 = $(foreach SIB4_YEAR,$(SIB4_INPUT_YEARS),intermediates/sib4-hourly-residual-centred-assim-2x25-$(SIB4_YEAR).fst)
SIB4_RESIDUAL_RESP_TOT_HOURLY_CENTRED_2X25 = $(foreach SIB4_YEAR,$(SIB4_INPUT_YEARS),intermediates/sib4-hourly-residual-centred-resp-tot-2x25-$(SIB4_YEAR).fst)

BASE_TARGETS += \
	$(REGION_GRID_DF) \
	$(SIB4_RESIDUAL_ASSIM_HOURLY_CENTRED_2X25) \
	$(SIB4_RESIDUAL_RESP_TOT_HOURLY_CENTRED_2X25) \
	$(LANDSCHUTZER_INVENTORY_RESIDUAL_2X25)


intermediates/sib4-hourly-residual-centred-%.fst: \
	src/remove-diurnal-cycle.R \
	$(SIB4_RESIDUAL_ASSIM_HOURLY_2X25) \
	$(SIB4_RESIDUAL_RESP_TOT_HOURLY_2X25) \
	$(REGION_GRID_DF)
	Rscript $< \
		--input-residual intermediates/sib4-hourly-residual-$*.nc \
		--region-grid-df $(REGION_GRID_DF) \
		--output $@

$(SIB4_RESIDUAL_ASSIM_HOURLY_2X25) &: \
	src/regrid-decomposition.sh \
	$(GEOS_2X25_GRID) \
	$(SIB4_RESIDUAL_ASSIM_HOURLY)
	bash $< \
		$(GEOS_2X25_GRID) \
		"$(SIB4_RESIDUAL_ASSIM_HOURLY)" \
		intermediates

$(SIB4_RESIDUAL_RESP_TOT_HOURLY_2X25) &: \
	src/regrid-decomposition.sh \
	$(GEOS_2X25_GRID) \
	$(SIB4_RESIDUAL_RESP_TOT_HOURLY)
	bash $< \
		$(GEOS_2X25_GRID) \
		"$(SIB4_RESIDUAL_RESP_TOT_HOURLY)" \
		intermediates

$(LANDSCHUTZER_INVENTORY_RESIDUAL_2X25): \
	$(LANDSCHUTZER_INVENTORY_RESIDUAL) \
	$(GEOS_2X25_GRID)
	cdo -w -f nc2 -z zip_6 \
		-remapcon,$(GEOS_2X25_GRID) \
		-selyear,2014/2019 \
		$(LANDSCHUTZER_INVENTORY_RESIDUAL) \
		$@

$(REGION_GRID_DF): \
	src/region-grid-df.R \
	$(REGION_GRID) \
	$(CONTROL_EMISSIONS)
	Rscript $< \
		--region-grid $(REGION_GRID) \
		--control-emissions $(CONTROL_EMISSIONS) \
		--output $@
