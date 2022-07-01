

harv.red.hwp <- data.frame(Year = (hwp.yr + (0:(d.yrs - 1))), name = 0)
colnames(harv.red.hwp)[2] <- eval(ownr.sel)
harv.red.hwp[1, 2] <- harv.hwp[harv.hwp$Year == hwp.yr, ownr.index + 1]

tpr.red.hwp <- tpr.hwp[,c(1, yr.index + 1)]
ppr.red.hwp <- ppr.hwp[,c(1, yr.index + 1)]
eur.red.hwp <- if ("data.table" %in% class(eur.hwp)) eur.hwp[, c(1, yr.index + 1), with = FALSE] else eur.hwp[, c(1, yr.index + 1)]

discard.fates.red.hwp <- if (length(hwp.yr:years[length(years)]) < d.yrs) {   # Takes all discard fate data starting at hwp.yr, adds repeats if runs out of info
  xtra.yrs <- d.yrs - length(hwp.yr:years[length(years)])
  extend.disc.fates <- data.frame(matrix(unlist(rep(discard.fates.hwp[,length(years) + 2], xtra.yrs)), ncol = xtra.yrs))
  names(extend.disc.fates) <- (hwp.yr + (d.yrs - xtra.yrs)):(hwp.yr + (d.yrs - 1))
  cbind(discard.fates.hwp[, c(1:2, (yr.index + 2):(length(years) + 2))], extend.disc.fates)
} else {
  discard.fates.hwp[, c(1:2, (yr.index + 2):(yr.index + 2 + d.yrs - 1))]
} 


# Run the HWP model that is minimized to the selected state/year/ownership for the Sankey display
#  This function can be found in PlotFunctions1.r 
hwp.sankey.output <- HwpModel.Sankey.fcn(harv = harv.red.hwp, 
                                         bfcf = bfcf.hwp,
                                         tpr = tpr.red.hwp,
                                         ppr = ppr.red.hwp,
                                         ratio_cat = ratio_cat.hwp,
                                         ccf_conversion = ccf_conversion.hwp,
                                         eur = eur.red.hwp,
                                         eu_half.lives = eu_half.lives.hwp,
                                         discard.fates = discard.fates.red.hwp,
                                         discard.hl = discard.hl.hwp, 
                                         hwp.yr = hwp.yr, 
                                         ownership.names = ownership.names, 
                                         N.EUR = N.EUR, PIU.LOSS = PIU.LOSS, years = years, yr.index = yr.index,
                                         ownr.index = ownr.index, d.yrs = d.yrs)



# Harvest
eur_mmtc <- sum(hwp.sankey.output$eu_matrix[, 1])
# harvest to EEC
eec_mmtc <- sum(hwp.sankey.output$eec_matrix[, 1])
# harvest to PIU
eu.reduced_mmtc <- sum(hwp.sankey.output$eu.reduced_matrix[, 1])
# harvest to discard
dp_mmtc <- sum(hwp.sankey.output$dp_matrix[, 1])

# EEC
# eec_mmtc  # See above. Already calculated

# PIU
# PIU to Discard
pu.discard_mmtc <- sum(hwp.sankey.output$pu.discard_matrix[, 2:d.yrs])
# PIU to PIU
pu_mmtc <- eu.reduced_mmtc - pu.discard_mmtc   # Don't think I need this metric


# Discard
# Discard to SWDS
landfill.input_mmtc <- sum(hwp.sankey.output$landfill.input_matrix[, 1:d.yrs])
dumps.input_mmtc <- sum(hwp.sankey.output$dumps.input_matrix[, 1:d.yrs])
swds.input_mmtc <- landfill.input_mmtc + dumps.input_mmtc
# Discard to Compost
compost.input_mmtc  <- sum(hwp.sankey.output$compost.input_matrix[, 1:d.yrs])
# Discard to BWOEC
bwoec.input_mmtc  <- sum(hwp.sankey.output$bwoec.input_matrix[, 1:d.yrs])
# Discard to Recovered
recov.input_mmtc <- sum(hwp.sankey.output$recov.input_matrix[, 1:d.yrs])
# Discard to DEC (Discard Energy Capture, previously BWEC or Burned with Energy Capture)
dec.input_mmtc <- sum(hwp.sankey.output$dec.input_matrix[, 1:d.yrs])

# SWDS
#swds_mmtc  <- sum(swdsCtotal_matrix[, 7, yr.index])
# SWDS to EWOEC
dumps.discard_mmtc  <- sum(hwp.sankey.output$dumps.discard_matrix[, 1:d.yrs])
landfill.discard_mmtc  <- sum(hwp.sankey.output$landfill.discard_matrix[, 1:d.yrs])
swds.discard_mmtc <- dumps.discard_mmtc + landfill.discard_mmtc
# SWDS to SWDS
lf.fixed_mmtc <- sum(hwp.sankey.output$lf.fixed_matrix[, 1:d.yrs])
lf.available_mmtc <- sum(hwp.sankey.output$landfill_matrix[, d.yrs])
#dumps_matrix <- hwp.sankey.output$dumps_matrix
#dumps_matrix[, 2:3] <- hwp.sankey.output$dumps_matrix[, 2:3] - hwp.sankey.output$dumps_matrix[, 1:2]
#dumps_mmtc <- sum(dumps_matrix[, 1:3])
dumps_mmtc <- sum(hwp.sankey.output$dumps_matrix[, d.yrs])
swds_mmtc <- sum(lf.fixed_mmtc, lf.available_mmtc, dumps_mmtc)


# 1952 discards check: 
#dumps_mmtc + dumps.discard_mmtc + landfill.discard_mmtc + lf.fixed_mmtc + lf.available_mmtc + 
#  bwoec.input_mmtc + compost.input_mmtc + recov_mmtc + recov.discard_mmtc


# Compost
# Compost to EWOEC
#compost.input_mmtc  # See above. Already created

# BWOEC
# BWOEC to EWOEC
#bwoec.input_mmtc  # See above. Already created


# Recovered
recov_mmtc <- sum(hwp.sankey.output$recov_matrix[, d.yrs])
# Recovered to EWOEC
recov.discard_mmtc <- sum(hwp.sankey.output$recov.discard_matrix[, 1:d.yrs])

# EWOEC
ewoec_mmtc <- sum(swds.discard_mmtc, bwoec.input_mmtc, compost.input_mmtc, recov.discard_mmtc)
# Correct check = landfill/dump/recov yrs 2 & 3, bwoec/compost yrs 1 & 2


nodes <- data.frame(name = c("Harvest", "Emitted with Energy Capture", "Products in Use", "Discard", "Dumps", "Landfill, Permanent", 
                             "Landfill, Decomposing", "Compost", "Burned", "Recovered", "Emitted without\nEnergy Capture", "Discard Energy Capture"))


mat.mmtc <- matrix(0, nrow = length(nodes$name), ncol = length(nodes$name))
mat.mmtc[1,] <- c(0, eec_mmtc, eu.reduced_mmtc, dp_mmtc, 0, 0, 0, 0, 0, 0, 0, 0)
mat.mmtc[3,] <- c(0, 0, 0, pu.discard_mmtc, 0, 0, 0, 0, 0, 0, 0, 0)
mat.mmtc[4,] <- c(0, 0, 0, 0, dumps.input_mmtc,	lf.fixed_mmtc,	(landfill.input_mmtc - lf.fixed_mmtc), compost.input_mmtc, bwoec.input_mmtc, recov.input_mmtc, 0, dec.input_mmtc)
mat.mmtc[5,] <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, dumps.discard_mmtc, 0)
mat.mmtc[6,] <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
mat.mmtc[7,] <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, landfill.discard_mmtc, 0)
mat.mmtc[8,] <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, compost.input_mmtc, 0)
mat.mmtc[9,] <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, bwoec.input_mmtc, 0)
mat.mmtc[10,] <- c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, recov.discard_mmtc, 0)
mat.mmtc[12,] <- c(0, dec.input_mmtc, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)          # Need to reorganize columns

nodes.zero <- which(apply(mat.mmtc, 1, sum) + apply(mat.mmtc, 2, sum) == 0)

#if (input$metrictype == "2") {    # Changing MMT C / Tg C values to CO2e if Tg CO2e selected
#  mat.mmtc <- mat.mmtc * 44/12   
#}

if (length(nodes.zero) > 0) {
  mat.mmtc <- mat.mmtc[-nodes.zero, -nodes.zero]
  nodes2 <- data.frame(name = nodes$name[-nodes.zero])
} else {
  nodes2 <- nodes
}

colnames(mat.mmtc) = rownames(mat.mmtc) = nodes2$name

links <- mat.mmtc %>% 
  as.data.frame() %>% 
  rownames_to_column(var = "source") %>% 
  pivot_longer(cols = 2:(length(nodes2$name) + 1), names_to = "target", values_to = "value") %>%
  filter(value != 0)


links$IDsource <- match(links$source, nodes2$name) - 1 
links$IDtarget <- match(links$target, nodes2$name) - 1

links <- as.data.frame(links)

















