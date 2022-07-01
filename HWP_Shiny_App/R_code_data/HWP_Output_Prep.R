###### This script operates a bit like a function.  The purpose is to provide 
#####   model output values that may be used for generating Shiny plots, 
###     downloading tables, or providing to the Monte Carlo.  



eu_ratios <- model.outputs$eu_ratios
eu.reduced_array <- model.outputs$eu.reduced_array
dp.total_array <- model.outputs$dp.total_array
empty.eu_array <- model.outputs$empty.eu_array
first.eu_array <- model.outputs$first.eu_array
harv_cf1 <- model.outputs$harv_cf1
landfill.fx.ratio <- model.outputs$landfill.fx.ratio
landfill.hl <- model.outputs$landfill.hl
dump.hl <- model.outputs$dump.hl
recov.hl <- model.outputs$recov.hl
eur.pulp <- model.outputs$eur.pulp

eu_array <- model.outputs$eu_array
eec_array <- model.outputs$eec_array
fuel_array <- model.outputs$fuel_array
dec.input_array <- model.outputs$dec.input_array
ewoec_array <- model.outputs$ewoec_array
dumps.discard_array <- model.outputs$dumps.discard_array 
landfill.discard_array <- model.outputs$landfill.discard_array
recov.discard_array <- model.outputs$recov.discard_array
compost.input_array <- model.outputs$compost.input_array
bwoec.input_array <- model.outputs$bwoec.input_array
swdsCtotal_array <- model.outputs$swdsCtotal_array
lf.fixed.cumsum_array <- model.outputs$lf.fixed.cumsum_array
landfill_array <- model.outputs$landfill_array
dumps_array <- model.outputs$dumps_array
pu.final_array <- model.outputs$pu.final_array
pu_array <- model.outputs$pu_array
recov_array <- model.outputs$recov_array
