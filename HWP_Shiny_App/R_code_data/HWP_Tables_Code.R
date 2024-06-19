#####         HWP Model Code for Generating Tables           ###
# 
#      This code is called upon by the HWP_PROGRAM_CODE.R.
#       The code will only run if the option is set to TRUE in HWP_MODEL_OPTIONS.csv.
#      It will only run if the OUTPUT_TABLES option in the HWP_MODEL_OPTIONS.csv file is set to TRUE.
#
####################################################################################################

############ Table functions ###################

# Table function. If no ownership data (i.e., only TOTAL data), this provides alternative output
blanktab.fcn <- function(){
  x <- data.frame(x1 = paste0("Table is empty, no ownership values"))
  return(x) 
}



# Table function, used to obtain ownership values from different data frames (see Table 1, Table 6)
#t.owner.group.fcn <- function(matrix.empty, own.group.names, df1, df2) {
#  for(i in 1:length(own.group.names)) {
#    t1 <- c1 <- matrix(0, (N.YEARS - STARTYR.POSITION + 1), length(tab_categories[[i]]))
#    for(j in 1:length(tab_categories[[i]])) {
##      t1[,j] <- get(tab_categories[[i]][j], df1) 
#      c1[,j] <- get(tab_categories[[i]][j], df2)       
#    }
#    matrix.empty[, i * 2] <- apply(t1, 1, sum)/1e6
##    matrix.empty[, i * 2 + 1 ] <- apply(c1, 1, sum)/1e6
#  }
#  return(matrix.empty)
#}



# Table function. The following code produces a version of Table 3 for all ownerships.  
t3.allowners.fcn <- function(array.name, nyrs, n.owners){
  mat1 <- matrix(0, nyrs, n.owners)
  for (i in 1:nyrs) {    
    mat1[i, ] <- apply(array.name[, 1:n.owners , i], 2, sum)
  }
  return(mat1 / 1e6)
}



###################### Table creation code ###############################



######################
##  Annual_Harvest.csv - Harvest in BBF and MMT C, yearly and cumulatively

# Harvest in BBF, reported as yearly amount and cumulatively
harv_BBF_yr <- harv.hwp 
harv_BBF_yr[is.na(harv_BBF_yr)] <- 0 
harv_BBF_yr[2:(N.OWNERSHIP + 1)] <- harv_BBF_yr[2:(N.OWNERSHIP + 1)] / 1e6
harv_BBF_cumsum <- if (N.OWNERSHIP > 1) {
  apply(harv_BBF_yr[, 2:(N.OWNERSHIP + 1)], 2, cumsum)   
} else {
  cumsum(harv_BBF_yr[, 2:(N.OWNERSHIP + 1)])
}
# Harvest in MMT C, reported as yearly amount and cumulatively
harv_TgC_yr <- harv_BBF_yr
harv_TgC_yr[, 2:(N.OWNERSHIP + 1)] <- 0
for (i in 1:N.OWNERSHIP) {
  harv_TgC_yr[, (i + 1)] <- apply(eu_array[, i, ], 2, sum)  / 1e6
}

harv_TgC_cumsum <- if (N.OWNERSHIP > 1) {
  apply(harv_TgC_yr[, 2:(N.OWNERSHIP + 1)], 2, cumsum) 
} else {
  cumsum(harv_TgC_yr[, 2:(N.OWNERSHIP + 1)])
}

t1 <- cbind(harv_BBF_yr, harv_BBF_cumsum, harv_TgC_yr[, 2:(N.OWNERSHIP + 1)], harv_TgC_cumsum, harv_TgC_yr[, 2:(N.OWNERSHIP + 1)] * 44/12, harv_TgC_cumsum * 44/12 )
harv_types <- c("_BBF_yr", "_BBF_cumsum", "_TgC_yr", "_TgC_cumsum", "_TgCO2e_yr", "_TgCO2e_cumsum")
if (N.OWNERSHIP == 1) {
  names(t1) <- c("Year", "Total_BBF_yr", "Total_BBF_cumsum", "Total_TgC_yr", "Total_TgC_cumsum", "Total_TgCO2e_yr", "Total_TgCO2e_cumsum")
} else {
  for (j in 1:6) {
    names(t1)[1 + (1:N.OWNERSHIP) + (j - 1) * N.OWNERSHIP] <- paste0(names(t1)[  1 + (1:N.OWNERSHIP) + (j - 1) * N.OWNERSHIP], harv_types[j])
  }
}  
  
  

#######################################
##  Data for Utilized Timber Halflives

sml <- eu_half.lives.hwp %>%
  mutate(ShortMedLong = ifelse(EU_HalfLife > 0 & EU_HalfLife <= 6, "Short", 
                               ifelse(EU_HalfLife > 6 & EU_HalfLife <= 30, "Medium", 
                                      ifelse(EU_HalfLife > 30, "Long", "Fuel"))))

sml2 <-   cbind(sml, eu_array[, N.OWNERSHIP, ]) %>% 
  pivot_longer(cols = 4:(N.YEARS + 3), names_to = "Years", values_to = "TgC") %>%
  group_by(ShortMedLong, Years) %>%
  summarize(sumTgC = sum(TgC)/1e6, .groups = "drop_last")

totals <- sml2 %>% group_by(Years) %>%
  summarize(totalTgC = sum(sumTgC))

t2.stacked <- left_join(sml2, totals, by = "Years") %>%
  mutate(pct = sumTgC / totalTgC, 
         sumTgCO2e = sumTgC * 44/12, 
  totalTgCO2e = totalTgC * 44/12)

t2.expansion.fcn <- function(.type) {
  t2.x <- t2.stacked %>% dplyr::filter(ShortMedLong == .type) %>% ungroup() %>% dplyr::select(-ShortMedLong, -totalTgC, -totalTgCO2e) %>%
    relocate("Years", "sumTgC", "sumTgCO2e", "pct")
  names(t2.x)[-1] <- paste0(names(t2.x)[-1], "_", .type)
  return(t2.x)
  }

t2.fuel <- t2.expansion.fcn("Fuel")
t2.short <- t2.expansion.fcn("Short")
t2.med <- t2.expansion.fcn("Medium")
t2.long <- t2.expansion.fcn("Long")
t2.total <- t2.stacked %>% ungroup() %>% dplyr::select(Years, totalTgC, totalTgCO2e) %>% distinct()

t2 <- left_join(t2.fuel, t2.short, by = "Years") %>% left_join(t2.med, by = "Years") %>% left_join(t2.long, by = "Years") %>% left_join(t2.total, by = "Years")

  ######################
  ##  Table_Cumulative.C.Disposition
  
  
  #  NOTE ABOUT OUTPUTS
  #  If the ownership data is complete for all years, then the sum of individual ownerships for the four categories should equal the 
  #   totals (e.g., California).  However, if, like Oregon, there are no ownership data before 1962 but there are totals, then there will be various EWOEC, PIU, SWDS
  #   values in play that are complicated and the sums don't work.  Maybe we don't want to have this material by individual ownership?  Need to think on this.  
  #   I think what is going on is that the Total material is already emitting a great deal of CO2, disproportionate to the newly-tracked inputs.  Thus, there is more 
  #   EWOEC from the system than anticipated, and less in pools.  The amounts balance (that is, PIU + SWDS + EWOEC from the individual ownerships = sum from the totals).
  
  #   
#  eec <- cumsum(apply(eec_array[, N.OWNERSHIP, ], 2, sum))/1e6
#  ewoec <- cumsum(apply(ewoec_array[, N.OWNERSHIP, ], 2, sum))/1e6
  #eecC02 <- (44/12) * eec     # Redefining the emissions arrays in MMT of CO2
  #ewoecCO2 <- (44/12) * ewoec
#  swdsC <- apply(swdsCtotal_array[, N.OWNERSHIP, ], 2, sum)/1e6
#  pu.all <- apply(pu.final_array[, N.OWNERSHIP, ], 2, sum)/1e6    # Products in Use and Recovered
#  t3 <- data.frame(Year = yearsUse, eec, ewoec, piu = pu.all, swds = swdsC, row.names = NULL)
#  t3$Total_Emitted <- eec + ewoec
#  t3$Total_Pools <- t3$piu + t3$swds
  #t3_C <- data.frame(yearsUse, eec, ewoec, pu.all, swdsC, TotalHWP = t3$TotalHWP, row.names = NULL)   # A CO2-free version for saving to CVS
  
  #write_csv(t3, paste0(TABLELOC, "Table_3.csv"))
  #write_csv(t3_C, paste0(TABLELOC, "Table_3_noCO2.csv"))
  
  
  tab3.all.owners.fcn <- function() {
    eec.sum_array <- data.frame(t3.allowners.fcn(eec_array, length(years), (N.OWNERSHIP - 1)))
    ewoec.sum_array <- data.frame(t3.allowners.fcn(ewoec_array, length(years), (N.OWNERSHIP - 1)))
    pu.sum_array <- data.frame(t3.allowners.fcn(pu.final_array, length(years), (N.OWNERSHIP - 1)))
    swdsC.sum_array <- data.frame(t3.allowners.fcn(swdsCtotal_array, length(years), (N.OWNERSHIP - 1)))
    
    colnames(eec.sum_array) <- paste0(ownership.names[1:N.OWNERSHIP - 1], "_eec_TgC")
    colnames(ewoec.sum_array) <- paste0(ownership.names[1:N.OWNERSHIP - 1], "_ewoec_TgC")
    colnames(pu.sum_array) <- paste0(ownership.names[1:N.OWNERSHIP - 1], "_pu_TgC")
    colnames(swdsC.sum_array) <- paste0(ownership.names[1:N.OWNERSHIP - 1], "_swds_TgC")
    
    #  sum.df <- tibble()
    sum.df2 <- tibble(Years = yearsUse, eec.sum_array, ewoec.sum_array, pu.sum_array, swdsC.sum_array)
    
    sum.df3 <- sum.df2
    sum.df3[,2:((ncol(sum.df2) - 1) / 2 + 1)] <- apply(sum.df2[,2:((ncol(sum.df2) - 1) / 2 + 1)], 2, cumsum)
    #sum.df3$total_emissions <- apply(sum.df2[,2:((ncol(sum.df2) -1) /2 + 1)], 1, sum)
    #sum.df3$total_pools <- apply(sum.df2[,((ncol(sum.df2) -1) /2 + 2):ncol(sum.df2)], 1, sum)
    #sum.df3[,-1]
    #  write_csv(sum.df3, paste0(TABLELOC, "Table_3_AllOwners_TgC.csv"))
    return(sum.df3)
  }
  
  if (N.OWNERSHIP > 1) {
    t3 <- tab3.all.owners.fcn()
    t3.5 <- t3
    t3.5[, -1] <- t3[, -1] * 44/12
    names(t3.5)[-1] <- paste0(names(t3.5)[-1], "O2e")
  } else {t3 <- t3.5 <- blanktab.fcn()} # If owners are listed, then will append owner info, otherwise sticking with t3



  ######################
  ##  Table 4 - Cumulative carbon storage and emissions in/from different pools/sources.  Emissions are provided in MMTC and CO2e
  
  pu <- apply(pu_array[, N.OWNERSHIP, ], 2, sum)/1e6         # The pu_array is a cumulative input & decay array.  pu.all (above) = pu_array + recyc_array
  rc <- apply(recov_array[, N.OWNERSHIP, ], 2, sum)/1e6   # recycled.  recyc_array = cumulative input & decay
  
  # lf = landfill, both portions that decompose and not, calculated above
  
  fw <- cumsum(apply(eec_array[, N.OWNERSHIP, ], 2, sum))/1e6     # fuel wood.  eec_array = annual emission, so cumulatively summed.
  dec <-  cumsum(apply(dec.input_array[, N.OWNERSHIP, ], 2, sum))/1e6        # Discard [Burned w/] Energy Capture (Burned disposal products) 
#  lf <- apply((lf.fixed.cumsum_array[, N.OWNERSHIP, ] + landfill_array[, N.OWNERSHIP, ]), 2, sum)/1e6  # Carbon in landfills.  Includes both the accumulated carbon that won't decompose and the portion that is subject to a half life
  lf.fx <- apply(lf.fixed.cumsum_array[, N.OWNERSHIP, ], 2, sum)/1e6  # Landfill, inert.
  lf.avail <- apply(landfill_array[, N.OWNERSHIP, ], 2, sum)/1e6      # Landfill, available for decomposition.
  lf_e <- cumsum(apply(landfill.discard_array[, N.OWNERSHIP, ], 2, sum))/1e6 # landfill emissions. Annual, so cumulatively summed
  dumps <- apply(dumps_array[, N.OWNERSHIP, ], 2, sum)/1e6                           # Carbon in dumps
  dmp_e <- cumsum(apply(dumps.discard_array[, N.OWNERSHIP, ], 2, sum))/1e6   # dump emissions. Annual, so cumulatively summed
  rc_e <- cumsum(apply(recov.discard_array[, N.OWNERSHIP, ], 2, sum))/1e6   # recycling emissions. Annual, so cumulatively summed 
  bn_wo <- cumsum(apply(bwoec.input_array[, N.OWNERSHIP, ], 2, sum))/1e6     # burn emissions w/o energy capture. Annual, so cumulatively summed
  cp <- cumsum(apply(compost.input_array[, N.OWNERSHIP, ], 2, sum))/1e6     # compost emissions. Annual, so cumulatively summed
  
  t4 <- data.frame(yearsUse, PIU_TgC = pu + rc, SWDS_TgC = lf.fx + lf.avail + dumps, EEC_TgC = fw + dec, EWOEC_TgC = lf_e + dmp_e + rc_e + bn_wo + cp, # Summary of TgC categories
                   PIU_TgCO2e = (pu + rc) * 44/12, SWDS_TgCO2e = (lf.fx + lf.avail + dumps) * 44/12, EEC_TgCO2e = (fw + dec) * 44/12,                  # Summary of TgCO2e categories
                   EWOEC_TgCO2e = (lf_e + dmp_e + rc_e + bn_wo + cp) * 44/12)
  
  t4.5 <- data.frame(yearsUse, PIU_TgC = pu, Recovered_TgC = rc, Landfill_fixed_TgC = lf.fx, Landfill_available_TgC = lf.avail, Dumps_TgC = dumps,  
                   Fuelwood_TgC = fw, DumpEmit_TgC = dmp_e,  LandfillEmit_TgC = lf_e, RecoveredEmit_TgC = rc_e, Compost_TgC = cp, BurnNoCapture_TgC = bn_wo, DiscardEnergyCapture_TgC = dec,
                   PIU_TgCO2e = pu * 44/12, Recovered_TgCO2e = rc * 44/12, Landfill_fixed_TgCO2e = lf.fx * 44/12, Landfill_available_TgCO2e = lf.avail * 44/12, Dumps_TgCO2e = dumps * 44/12,  
                   Fuelwood_TgCO2e = fw * 44/12, DumpEmit_TgCO2e = dmp_e * 44/12,  LandfillEmit_TgCO2e = lf_e * 44/12, RecoveredEmit_TgCO2e = rc_e * 44/12, Compost_TgCO2e = cp * 44/12, 
                   BurnNoCapture_TgCO2e = bn_wo * 44/12, DiscardEnergyCapture_TgCO2e = dec * 44/12, row.names = NULL)
  
  
  ########################################
  ##  Table 4.8 - Cumulative carbon from different halflife categories in PIU/SWDS/EEC/EWOEC
  
  st <- which(sml$ShortMedLong == "Short")  # Relies on the data frame "sml" from above
  md <- which(sml$ShortMedLong == "Medium")
  lng <- which(sml$ShortMedLong == "Long")
 
  # Emitted with energy capture
  eec <- cumsum(apply(eec_array[, N.OWNERSHIP, ], 2, sum))/1e6
  
   # Products in Use
  pu.st <- apply(pu.final_array[st, N.OWNERSHIP, ], 2, sum)/1e6     # Products in Use with short halflife
  pu.md <- apply(pu.final_array[md, N.OWNERSHIP, ], 2, sum)/1e6     # Products in Use with medium halflife
  pu.lng <- apply(pu.final_array[lng, N.OWNERSHIP, ], 2, sum)/1e6     # Products in Use with long halflife
  
  # SWDS
  swds.st <- apply(swdsCtotal_array[st, N.OWNERSHIP, ], 2, sum)/1e6
  swds.md <- apply(swdsCtotal_array[md, N.OWNERSHIP, ], 2, sum)/1e6
  swds.lng <- apply(swdsCtotal_array[lng, N.OWNERSHIP, ], 2, sum)/1e6
  # Fuel (EEC), 1 category
  # Use eec for fuel + discard BWEC?
  # EWOEC 
  ewoec.st <- cumsum(apply(ewoec_array[st, N.OWNERSHIP, ], 2, sum))/1e6
  ewoec.md <- cumsum(apply(ewoec_array[md, N.OWNERSHIP, ], 2, sum))/1e6
  ewoec.lng <- cumsum(apply(ewoec_array[lng, N.OWNERSHIP, ], 2, sum))/1e6
  
  hl.data <- tibble(years = yearsUse, swds.st, swds.md, swds.lng, pu.st, pu.md, pu.lng, ewoec.st = ewoec.st, 
                    ewoec.md = ewoec.md, ewoec.lng = ewoec.lng, eec = eec)
  names(hl.data)[-1] <- paste0(names(hl.data)[-1], "_TgC")
  hl.data2 <- hl.data[, -1] * 44/12
  names(hl.data2) <- paste0(names(hl.data2), "O2e")
  t4.8 <- cbind(hl.data, hl.data2)
  
  
  
  ######################
  ##  Table 5 - Changes in HWP C stocks
 
  ewoec <- cumsum(apply(ewoec_array[, N.OWNERSHIP, ], 2, sum))/1e6
  swdsC <- apply(swdsCtotal_array[, N.OWNERSHIP, ], 2, sum)/1e6
  pu.all <- apply(pu.final_array[, N.OWNERSHIP, ], 2, sum)/1e6    # Products in Use and Recovered
  
  change.fcn <- function(x) x[2:length(x)] - x[1:(length(x) - 1)]
  swdsC_change <- change.fcn(swdsC)
  pu_change <- change.fcn(pu.all)
  eec_change <- change.fcn(eec)      # This is the same as: apply(eec_array[, N.OWNERSHIP, ], 2, sum)/1e6  (i.e., above a cumulative sum function was applied and we remove it here) 
  ewoec_change <- change.fcn(ewoec)    # Really just the annual EWOEC emissions
  nsc <- swdsC_change + pu_change  # nsc = Net Stock Change (PIU + SWDS)
  #ntc <- nsc - eec_change - ewoec_change
  harvest <- swdsC_change + pu_change + eec_change + ewoec_change
  swdsC_change_CO2 <- (44/12) * swdsC_change
  pu_change_CO2 <- (44/12) * pu_change
  eec_change_CO2 <- (44/12) * eec_change
  ewoec_change_CO2 <- (44/12) * ewoec_change
  nsc_CO2 <- (44/12) * nsc
 # ntc_CO2 <- (44/12) * ntc  
  harvest_CO2e <- (44/12) * harvest
  t5 <- data.frame(Year = yearsUse[2:length(yearsUse)], SWDSchangeTgC = swdsC_change, PIUchangeTgC = pu_change, EECchangeTgC = eec_change,
                   EWOECchangeTgC = ewoec_change, NetStockChangeTgC = nsc, Harvest = harvest, 
                   SWDS_changeTgCO2e = swdsC_change_CO2, PIU_changeTgCO2e = pu_change_CO2, EECchangeTgCO2e = eec_change_CO2,
                   EWOECchangeTgCO2e = ewoec_change_CO2, NetStockChangeTgCO2e = nsc_CO2, Harvest_CO2e = harvest_CO2e, row.names = NULL)
  
   
  
  
  
  
  
  
  
  
  