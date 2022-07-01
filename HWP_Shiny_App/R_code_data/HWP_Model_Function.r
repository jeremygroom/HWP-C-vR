

########################################################
###            Model Functions 
########################################################



#LT.fcn: See PlotFunctions.1

#WhichMin.fcn:  EmptyFirst.fcn depends on this function.  See PlotFunctions.1

#EmptyFirst.fcn:  See PlotFunctions.1

#HLCreation.fcn:  See PlotFunctions.1

#Decay.fcn:  See PlotFunctions.1



########################################################
## Model Code
########################################################

HwpModel.fcn <- function(harv, bfcf, tpr, ppr, ratio_cat, ccf_conversion, eur, eu_half.lives, discard.fates, discard.hl, 
                         ownership.names, N.EUR, N.OWNERSHIP, 
                         N.YEARS, PIU.LOSS) {
  
  # Constructing data to place in the End Use Products array
  harv[is.na(harv) == T] <- 0        # Replace NAs with zeros
  
  a1 <- unlist(lapply(harv[,1],  LT.fcn, y = bfcf$EndYear ))
  harv$conv1 <- bfcf$Conversion[a1 + 1]
  
  harv_cf <- harv
  harv_cf[,2:ncol(harv)] <- ((harv[,2:ncol(harv)] * 1000) / harv$conv1)/100  #converting board-feet to cubic feet 
  harv_cf <- harv_cf[,1:(length(ownership.names) + 1)]
  
  
  # tidying data, preparing for entry into array
  tpr1 <- tpr %>% pivot_longer(cols = 2:ncol(tpr), names_to = "Year", values_to = "ProdRatio")  #Seeking one value per row
  tpr1$Year <- as.numeric(tpr1$Year)
  
  harv_cf1 <- harv_cf %>% pivot_longer(cols = 2:(length(ownership.names) + 1), names_to = "Source", values_to = "Vol_cf")  #Seeking one value per row
  
  pp_ratios <- ratio_cat %>% 
    select(c(1,2)) %>% 
    distinct() %>% 
    left_join(ppr, by = "PrimaryProductID") %>% 
    pivot_longer(cols = 3:(N.YEARS + 2), names_to = "Year", values_to = "PP_Values")
  
  pp_ratios$Year <- as.numeric(pp_ratios$Year)
  
  
  pp_ratios2 <- left_join(pp_ratios, tpr1, by = c("TimberProductID", "Year"))
  pp_ratios2$TP_PPratio <- pp_ratios2$PP_Values * pp_ratios2$ProdRatio
  
  ppr_preArray <- left_join(pp_ratios2, harv_cf1, by = "Year") %>%    #Combining data, creating column of the product of the Product Ratio and cubic feet of harvest
    mutate(PP_cf = TP_PPratio * Vol_cf) %>%
    arrange(Year, factor(Source, levels = ownership.names))
  
  ### End Use Ratios in MTC
  # This is the product of  Vol_cf * ProdRatio * PP_values * EUratio * MGCconversion
  
  ccf_mt <- ccf_conversion      # getting hundred cubic feet (CCF) to metric ton (MT) carbon conversion factors
  
  eu_ratios <- eur %>%    # the resulting number of rows should be 175616 (yrs *  ownerships * number of End Use Products)
    left_join(ratio_cat[,1:3], by = "EndUseID") %>%     ## The wood vol in cubic feet is already part of ppr_preArray, so don't need to join with harv_cf1
    pivot_longer(cols = 2:(N.YEARS + 1), names_to = "Year", values_to = "EU_Values") %>%
    mutate(Year = as.numeric(Year)) %>%
    left_join(ccf_mt, by = "PrimaryProductID" ) %>%
    left_join(ppr_preArray, by = c("TimberProductID", "PrimaryProductID" , "Year")) %>%
    arrange(Year, factor(Source, levels = ownership.names)) %>%
    mutate(MTC = EU_Values * ProdRatio * PP_Values * CCFtoMTconv * Vol_cf)  # metric tons of carbon 
  
  # eu_array is the total annual C into each of the use/ownership categories
  eu_array <- array(eu_ratios$MTC, c(N.EUR, N.OWNERSHIP, N.YEARS))      # creating the array (worksheet: CheckEUC)
  dimnames(eu_array) <-  list(c(1:N.EUR), ownership.names, c(min(harv_cf$Year):max(harv_cf$Year)))
  
  
  # Finding the End Use Product rows that correspond to fuel wood
  eur.fuel <- grep("fuel", ratio_cat$EndUseProduct)
  
  # Emitted with Energy Capture (eec) array
  fuel_array <- array(0, c(N.EUR, N.OWNERSHIP, N.YEARS))
  fuel_array[eur.fuel, , ] <- eu_array[eur.fuel, , ]  # Grabbing the entries that correspond with fuel wood
  dimnames(fuel_array) <-  list(c(1:N.EUR), ownership.names, c(min(harv_cf$Year):max(harv_cf$Year)))
  
  # Products in Use array (pu_array)
  euhl <- as.matrix(eu_half.lives)   # matrices run much faster in for-loops than data frames or tibbles
  
  
  # Need the discarded products array (dp_array), generated every year from new timber harvest
  eur.pulp <- grep("pulp", ratio_cat$EndUseProduct)      #End use ratio rows for wood pulp (100% used, no discard)
  eur.fuel.pulp <- sort(c(eur.fuel, eur.pulp))      #End use ratio rows for fuel wood and wood pulp (both not discarded)
  
  dp_array <- PIU.LOSS * eu_array             # Multiply the eu_array by the loss constant to create the discarded products array (dp_array).
  dp_array[eur.fuel.pulp, , ] <- 0            # Removing fuel wood from the dp_array because it is assumed burned in the given year (no discard). 
  # Pulp is removed too because there is no PIU loss for it.
  
  
  # Need to develop an array where the products in use take into account the new products and the half-life of earlier products
  nonPIU.loss <- rep(1.0 - PIU.LOSS, N.EUR)
  nonPIU.loss[eur.fuel.pulp] <- 1                 # Again, no PIU change for pulp or fuel wood
  nonPIU.loss_array <- array(rep(nonPIU.loss, N.OWNERSHIP * N.YEARS), c(N.EUR, N.OWNERSHIP, N.YEARS)) 
  
  eu.reduced_array <- eu_array * nonPIU.loss_array      # This is the End Use array reduced by the Placed in Use Loss (1-PIU.loss) except for pulp and fuel        
  eu.reduced_array[eur.fuel, , ] <- 0                   # No end use for fuel (burned to create fuel_array)
  
  
  # This bit of code helps the for-loops operating on arrays run faster
  output_empty.first <- EmptyFirst.fcn(eu.reduced_array, N.EUR, N.OWNERSHIP)
  empty.eu_array <- output_empty.first[["ea"]]   # owners/EUP where no values
  first.eu_array <- output_empty.first[["fa"]]   # owners/EUP year where first value
  
  
  # Products in Use array (determining cumulative totals over time with decay)
  pu.totals_decay <- Decay.fcn(empty.eu_array, first.eu_array, eu.reduced_array, euhl, N.EUR, N.OWNERSHIP, N.YEARS)
  pu_array <- pu.totals_decay[["decay"]]
  pu.discard_array <- pu.totals_decay[["da"]]
  
  
  # Joining the 8% loss when placed into use with the half-life loss
  dp.total_array <- dp_array + pu.discard_array
  
  
  
  # Products In Use =                pu_array  ( + pu_recovered_array, the recovered/recycled bits from the Discarded Products that re-entered use)
  # Emitted with Energy Capture =    fuel_array ( + dec_array, the "discard [burned with] energy capture" fraction of the Discarded Products array)
  # Discarded Products =             dp.total_array        
  
  
  
  ###### Initiating Discarded Products fates  ######
  
  # First, creating arrays for calculating the proportion going to different fates.  
  discard.fates2 <- discard.fates[, 1:(N.YEARS + 2) ]
  
  # Setting up the individual values for discard - burned/recovered/composted/landfills/dumps by year and paper/wood type
  dec.prop <- DiscardProd.fcn("DEC", discard.fates2, N.YEARS)
  burn.prop <- DiscardProd.fcn("BWoEC", discard.fates2, N.YEARS)
  recov.prop <- DiscardProd.fcn("Recovered", discard.fates2, N.YEARS)
  compost.prop <- DiscardProd.fcn("Composted", discard.fates2, N.YEARS)
  landfill.prop <- DiscardProd.fcn("Landfills", discard.fates2, N.YEARS)
  dumps.prop <- DiscardProd.fcn("Dumps", discard.fates2, N.YEARS)
  
  recov.hl <- HLCreation.fcn(5, discard.hl, eur.pulp, N.EUR)
  landfill.hl <- HLCreation.fcn(4, discard.hl, eur.pulp, N.EUR)
  landfill.fx.ratio <- HLCreation.fcn(3, discard.hl, eur.pulp, N.EUR)
  dump.hl <- HLCreation.fcn(2, discard.hl, eur.pulp, N.EUR)
  
  # filling in arrays for different disposal fate proportions by year
  
  eu_ratios$paper <- ifelse(eu_ratios$EndUseID %in% eur.pulp, 1, 0)
  
  eu_ratios2 <- left_join(eu_ratios, dec.prop[,-1], by = c("Year", "paper")) %>%
    left_join(burn.prop[,-1], by = c("Year", "paper")) %>%
    left_join(recov.prop[, -1], by = c("Year", "paper")) %>%
    left_join(compost.prop[, -1], by = c("Year", "paper")) %>%
    left_join(landfill.prop[, -1], by = c("Year", "paper")) %>%
    left_join(dumps.prop[, -1], by = c("Year", "paper"))
  
  #Creating the six immediate fates for Discarded Products (the sixth is Burned with Energy Capture, which will be a fraction of Burned)
  dec.input_array <- array(eu_ratios2$DEC, c(N.EUR, N.OWNERSHIP, N.YEARS)) * dp.total_array
  bwoec.input_array <- array(eu_ratios2$BWoEC, c(N.EUR, N.OWNERSHIP, N.YEARS)) * dp.total_array
  recov.input_array <- array(eu_ratios2$Recovered, c(N.EUR, N.OWNERSHIP, N.YEARS))  * dp.total_array      
  compost.input_array <- array(eu_ratios2$Composted, c(N.EUR, N.OWNERSHIP, N.YEARS))  * dp.total_array      
  landfill.input_array <- array(eu_ratios2$Landfills, c(N.EUR, N.OWNERSHIP, N.YEARS)) * dp.total_array      
  dumps.input_array <- array(eu_ratios2$Dumps, c(N.EUR, N.OWNERSHIP, N.YEARS)) * dp.total_array   
  
  
  # Carbon locked up in landfills (fixed; does not decay): lf.fixed_array.  
  lf.fixed_array <- array(rep(as.numeric(landfill.fx.ratio[,2]), N.OWNERSHIP * N.YEARS), c(N.EUR, N.OWNERSHIP, N.YEARS)) * landfill.input_array
  
  # The remaining carbon in the landfill available for decay:
  lf.available_array <- landfill.input_array  - lf.fixed_array
  
  # Recovered:
  recov_decay <- Decay.fcn(empty.eu_array, first.eu_array, recov.input_array, recov.hl, N.EUR, N.OWNERSHIP, N.YEARS) # The empty.eu_array or first.eu_array remain the same as earlier,
  # as the distribution of the values over time does not change.  
  recov_array <- recov_decay[["decay"]]
  recov.discard_array <- recov_decay[["da"]]
  
  # Landfill (portion that decays): 
  landfill_decay <- Decay.fcn(empty.eu_array, first.eu_array, lf.available_array, landfill.hl, N.EUR, N.OWNERSHIP, N.YEARS) 
  landfill_array <- landfill_decay[["decay"]]
  landfill.discard_array <- landfill_decay[["da"]]
  
  # Dumps
  dumps_decay <- Decay.fcn(empty.eu_array, first.eu_array, dumps.input_array, dump.hl, N.EUR, N.OWNERSHIP, N.YEARS) 
  dumps_array <- dumps_decay[["decay"]]
  dumps.discard_array <- dumps_decay[["da"]]
  
  
  ######## Final fates ###########
  
  # Emitted With Energy Capture (fuelwood plus discarded products Burned with Energy Capture )
  eec_array <- fuel_array + dec.input_array
  
  #Emitted Without Energy Capture: combination of decay output
  ewoec_array <- dumps.discard_array + landfill.discard_array + recov.discard_array + compost.input_array + bwoec.input_array
  
  
  # Combine materials remaining in the landfill with material that will not decay in the landfill
  lf.fixed.cumsum_array <- lf.fixed_array
  for(i in 1:N.EUR) {
    for(j in 1:N.OWNERSHIP)
      lf.fixed.cumsum_array[i, j, ] <- as.numeric(cumsum(lf.fixed_array[i, j, ]))
  }
  
  swdsCtotal_array <- lf.fixed.cumsum_array + landfill_array + dumps_array
  
  
  # Combining the products in use array with recovered products to have a final products in use array
  pu.final_array <- pu_array + recov_array
  
  
  
  return(list(eu_ratios = eu_ratios, 
              empty.eu_array = empty.eu_array,
              first.eu_array = first.eu_array,
              harv_cf1 = harv_cf1,
              recov.hl = recov.hl,
              landfill.hl = landfill.hl,
              landfill.fx.ratio = landfill.fx.ratio,
              dump.hl = dump.hl,
              eur.pulp = eur.pulp,
              eu_array = eu_array, 
              eu.reduced_array = eu.reduced_array,
              eec_array = eec_array,
              dp.total_array = dp.total_array,
              fuel_array = fuel_array, 
              dec.input_array = dec.input_array, 
              ewoec_array = ewoec_array,
              dumps.discard_array = dumps.discard_array, 
              landfill.discard_array = landfill.discard_array, 
              recov.discard_array = recov.discard_array,
              compost.input_array = compost.input_array, 
              bwoec.input_array = bwoec.input_array, 
              swdsCtotal_array = swdsCtotal_array, 
              lf.fixed_array = lf.fixed_array, 
              lf.fixed.cumsum_array = lf.fixed.cumsum_array, 
              landfill_array = landfill_array, 
              dumps_array = dumps_array, 
              pu.final_array = pu.final_array, 
              pu_array = pu_array, 
              recov_array = recov_array))
  
}



