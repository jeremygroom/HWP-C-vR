# Container for functions to be used in the HWP Shiny app

 # Function for download buttons
button.fcn <- function(plot_output, plot_name) {
  downloadHandler(
    filename = plot_name
    , content = function(file) {
      ggsave(file,
             plot = {
               y <- plot_output
               y <- y + theme(text=element_text(size=12)) 
               y
               }, 
             device = "png", height = 5, width = 7, units = "in", dpi = 300)
    })
}



  # Function for determining axis demarcations for plots
#y.axis.fcn <- function(y.values, div.1m) {  # y values, divide by 1m (1e6)? (T/F).  This function is used in adjusting plot y axes
#  j <- if(div.1m) 1e6 else 1
#  ymax <- max(y.values/j)
#  ymin <- min(y.values/j)
#  breaks.y <- {if(ymax == 0) 0.5 else
#    if(ymax < 20) 2 else
#      if(ymax >= 20 & ymax < 50) 5 else 
#        if(ymax >= 50 & ymax < 100) 10 else 
#          if(ymax >= 100 & ymax < 500) 50 else 100} 
#  max.y <- ceiling(ymax/breaks.y) * breaks.y
#  min.y <- floor(ymin/breaks.y) * breaks.y
#  if(max.y == 0) max.y <- 1 
#  if(min.y == 0) min.y <- -1
#  return(list(breaks.y = breaks.y, max.y = max.y, min.y = min.y))                  
#}

y.axis.fcn <- function(y.values, div.1m) {  # y values, divide by 1m (1e6)? (T/F).  This function is used in adjusting plot y axes
  j <- if(div.1m) 1e6 else 1
  ymax <- max(y.values/j)
  ymin <- min(y.values/j)
  yrange <- range(y.values/j)
  abs.yrange <- yrange[2] - yrange[1]
  breaks.y <- {if(abs.yrange <= 3) 0.5 else
    if(abs.yrange >= 3 & abs.yrange < 6) 1 else
      if(abs.yrange >= 6 & abs.yrange < 20) 2 else
        if(abs.yrange >= 20 & abs.yrange < 50) 5 else 
          if(abs.yrange >= 50 & abs.yrange < 100) 10 else 
            if(abs.yrange >= 100 & abs.yrange < 250) 25 else 
              if(abs.yrange >= 250 & abs.yrange < 500) 50 else 
                if(abs.yrange >= 500 & abs.yrange < 1500) 100 else 
                  200} 
  max.y <- ceiling(ymax/breaks.y) * breaks.y
  min.y <- floor(ymin/breaks.y) * breaks.y
  #  if(max.y == 0) max.y <- 1 
  #  if(min.y == 0) min.y <- -1
  return(list(breaks.y = breaks.y, max.y = max.y, min.y = min.y))                  
}

## For generating y-values for figure 2-2, Carbon Storage and Emissions. Needed to sum all positive and negative values separately.
p.e.y.fcn <- function(dat.file, targ.col){
  dat.max <- dat.file %>% filter(get(targ.col) > 0) %>% group_by(years) %>% mutate(y.vals = sum(get(targ.col)))
  dat.min <- dat.file %>% filter(get(targ.col) < 0) %>% group_by(years) %>% mutate(y.vals = sum(get(targ.col)))
  max6.y.1 <- y.axis.fcn(bind_rows(dat.max, dat.min)$y.vals, F)
}




# Function for preparing harvest data for cumulative sum allocations to ownership, with missing and non-missing initial ownership data
cumu.fcn <- function(f1.data, f.total.data, own.startyr, yrs){
  f1.data <- apply(as.matrix(f1.data), 2, cumsum)
  # Need to add values for early non-ownership years if they exist, e.g., Oregon
  if (own.startyr != yrs[1]) {
    #browser()
    sum.harv1 <- sum(f1.data[which(yrs == own.startyr),]) # Total value for first year with ownership data
    prop.harv1 <- f1.data[which(yrs == own.startyr),] / sum.harv1 # Proportion of harvest in each category
    add.harv1 <- prop.harv1 * f.total.data$values[which(yrs == (own.startyr - 1))] * 1e6 # Applying proportion vector to prevoius cumsum total
    x <- f1.data[which(yrs >= own.startyr),]  # Identifying ownership years cumsum values
    x2 <- t(x) + add.harv1                                             
    x3 <- t(x2)
    f1.data[which(yrs >= own.startyr), ] <- x3 # Adding cumsum amounts from year before ownerships data began to all columns and rows of non-zero ownership data
  }
  f1.data <- data.frame(f1.data)
  return(f1.data)
}


# Function for allowing users to enter long titles that automatically wrap.  See Monte Carlo figures.
wrapper <- function(x, n) paste(strwrap(x, n), collapse = "\n")



# Function for placing images using ggplot

ggplot_pic <- function(img){
  ggplot(mapping = aes(1:10, 1:10)) +
    annotation_raster(img, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf) +
    geom_point(color = "transparent") + 
    theme(axis.text = element_blank(),
          axis.title = element_blank(),
          axis.ticks = element_blank())
}


#### Sankey diagram functions, Figure 9 ######
rep.val.fcn <- function(ratio.hwp, hwpyr, dyrs) {
  dup.x <- ratio.hwp
  for(i in 1:(dyrs - 1)) {
    dup.x <- rbind(dup.x, ratio.hwp)
  }
  dup.x$Year <- rep(seq(hwpyr, hwpyr + (dyrs - 1), 1), each = nrow(ratio.hwp))
  dup.x
}



## Update to decay.fcn for Sankey diagram.  Running in advance of the loop
decay.fcn.s <- function(target.matrix, decay.vals, n.eur, dyrs) {                  # Previously Defined
  decay.matrix <- totals.matrix <- discard.matrix1.5 <- matrix(0, nrow = n.eur, ncol = dyrs)    
  decay.matrix[, 1] <- target.matrix[, 1]   # The first year = initial values
  for(k in 1:n.eur) {  
    totals.matrix[k, ] <- cumsum(target.matrix[k, ])
    
    for(i in 2:dyrs) {
      decay.matrix[k, i] <- as.numeric(target.matrix[k, i] + decay.matrix[k, i-1] * exp(-1 * log(2)/decay.vals[[k,2]]))
    }
  }
  #  # Products in Use Discards (End Uses Totals -minus- Pruducts in Use array )
  discard.matrix1 <- totals.matrix - decay.matrix  # The cumulative sums of carbon put into use ever minus the actual amount that remains after decay.
  discard.matrix1.5[, 2:dyrs] <- discard.matrix1[, 1:(dyrs - 1)]  # Filling in with shifted discard.matrix1 values accounting for previous year carbon emitted
  discard.matrix <- discard.matrix1 - discard.matrix1.5     # Annual emissions = for each year, the cumulative sum of in-use carbon  
  #  #   minus the remaining carbon, minus the emitted values from the previous year.
  #  # Output
  totals.decay.output <- list(decay = decay.matrix, da = discard.matrix)
} 


# Sankey version of a function for creating columns of proportion values for wood and paper by discard type
DiscardProd.s.fcn <- function(fate.type, disc.fates, dyrs) {
  disc.fates %>% filter(DiscardDestination == fate.type) %>%
    pivot_longer(cols = 3:(dyrs + 2), names_to = "Year", values_to = fate.type) %>%
    select(-DiscardDestination) %>%
    mutate(Year = as.numeric(Year),
           paper = ifelse(DiscardType == "paper", 1, 0))
}

############# Toned-down HWP model run on existing data to produce Sankey output ########################
##  The HWP model is used to generate decay histories for user-input start years and decay length
HwpModel.Sankey.fcn <- function(harv, mbfccf, tpr, ppr, ratio_cat, ccf_conversion, eur, eu_half.lives, discard.fates, discard.hl, 
                                hwp.yr, ownership.names, N.EUR, PIU.WOOD.LOSS, PIU.PAPER.LOSS, years, yr.index, ownr.index, d.yrs) {
  
  # Constructing data to place in the End Use Products array
  a1 <- LT.fcn(harv[1,1], mbfccf$Years)       # Alteration of main code.  Only need first year's value since that is only harvest
  harv$conv1 <- mbfccf$Conversion[a1 + 1]
  harv$conv1 <- ifelse(is.na(harv$conv1) == T,  # If selected years exceed data range, using most recent conversion factor to fill in gaps
                       harv$conv1[max(which(is.na(harv$conv1) == F))], harv$conv1)
   
  harv_cf <- harv
  harv_cf[,2:ncol(harv)] <- ((harv[,2:ncol(harv)] * 1000) / harv$conv1)/100  #converting board-feet to cubic feet 
  harv_cf <- harv_cf[,1:2]
 

  
  # tidying data, preparing for entry into array
  tpr1 <- tpr %>% pivot_longer(cols = 2:ncol(tpr), names_to = "Year", values_to = "ProdRatio")  #Seeking one value per row
  tpr1$Year <- as.numeric(tpr1$Year)
  tpr1 <- rep.val.fcn(tpr1, hwp.yr, d.yrs)
  
  harv_cf1 <- harv_cf %>% pivot_longer(cols = 2, names_to = "Source", values_to = "Vol_cf")  #Seeking one value per row
  
  pp_ratios <- ratio_cat %>% 
    select(c(1,2)) %>% 
    distinct() %>% 
    left_join(ppr, by = "PrimaryProductID") %>% 
    pivot_longer(cols = 3, names_to = "Year", values_to = "PP_Values")
  
  pp_ratios$Year <- as.numeric(pp_ratios$Year)
  pp_ratios <- rep.val.fcn(pp_ratios, hwp.yr, d.yrs)
  
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
    pivot_longer(cols = 2, names_to = "Year", values_to = "EU_Values") %>%
    mutate(Year = as.numeric(Year)) 
  eu_ratios <- rep.val.fcn(eu_ratios, hwp.yr, d.yrs)  # NEW
  eu_ratios <- eu_ratios %>% left_join(ccf_mt, by = "PrimaryProductID" ) %>%
    left_join(ppr_preArray, by = c("TimberProductID", "PrimaryProductID" , "Year")) %>%
    arrange(Year, factor(Source, levels = ownership.names)) %>%
    mutate(MTC = EU_Values * ProdRatio * PP_Values * CCFtoMTconv * Vol_cf)  # metric tons of carbon 
  
  # eu_array is the total annual C into each of the use/ownership categories
  eu_matrix <- matrix(eu_ratios$MTC, nrow = N.EUR)      # previously eu_array
  dimnames(eu_matrix) <-  list(c(1:N.EUR), c(hwp.yr:(hwp.yr + d.yrs - 1)))  
  
  # Finding the End Use Product rows that correspond to fuel wood
  eur.fuel <- grep("fuel", ratio_cat$EndUseProduct)
  
  # Emitted with Energy Capture (eec) array
  fuel_matrix <- matrix(0, nrow = N.EUR, ncol = d.yrs)                                # Previously fuel_array
  fuel_matrix[eur.fuel, ] <- eu_matrix[eur.fuel, ] 
  
  # Products in Use array (pu_array)
  euhl <- as.matrix(eu_half.lives)   # matrices run much faster in for-loops than data frames or tibbles
  
  
  # Need the discarded products array (dp_array), generated every year from new timber harvest
  eur.pulp <- grep("pulp", ratio_cat$EndUseProduct)      #End use ratio rows for wood pulp (100% used, no discard)
#  eur.fuel.pulp <- sort(c(eur.fuel, eur.pulp))      #End use ratio rows for fuel wood and wood pulp (both not discarded)
  
  PIU.LOSS <- matrix(rep(PIU.WOOD.LOSS, N.EUR), ncol = 1)
  PIU.LOSS[eur.pulp,] <- PIU.PAPER.LOSS
  
  dp_matrix <- sweep(eu_matrix, MARGIN = 1, PIU.LOSS, `*`)    # Multiply the eu_matrix by the loss vector to create the discarded products matrix (dp_array).
  dp_matrix[eur.fuel,  ] <- 0            # Removing fuel wood from the dp_matrix because it is assumed burned in the given year (no discard). 
  

  # Need to develop an array where the products in use take into account the new products and the half-life of earlier products
  nonPIU.loss <- 1.0 - PIU.LOSS
  nonPIU.loss[eur.fuel] <- 1                 # Again, no PIU change for pulp or fuel wood
  eu.reduced_matrix <- sweep(eu_matrix, MARGIN = 1, nonPIU.loss, `*`) # This is the End Use matrix reduced by the Placed in Use Loss (1-PIU.loss) except for fuel
  eu.reduced_matrix[eur.fuel, ] <- 0                   # No end use for fuel (burned to create fuel_array)
  
  
  
  # Creating products in use for first and 2nd year (no input into 2nd year)
  pu.totals_decay <- decay.fcn.s(eu.reduced_matrix, euhl, N.EUR, d.yrs)
  pu_matrix <- pu.totals_decay[["decay"]]
  pu.discard_matrix <- pu.totals_decay[["da"]]
  
  
  # Joining the 8% loss when placed into use with the half-life loss
  dp.total_matrix <- dp_matrix + pu.discard_matrix
  
  
  
  # Products In Use =                pu_matrix  ( + pu_recovered_matrix, the recovered/recycled bits from the Discarded Products that re-entered use)
  # Emitted with Energy Capture =    fuel_matrix ( + dec_matrix, the "Discard [burned with] Energy Capture" fraction of the Discarded Products matrix)
  # Discarded Products =             dp.total_matrix        
  
  
  
  ###### Initiating Discarded Products fates  ######
  

  # Setting up the individual values for discard - burned/recovered/composted/landfills/dumps by year and paper/wood type
  dec.prop <- DiscardProd.s.fcn("DEC", discard.fates, d.yrs)
  burn.prop <- DiscardProd.s.fcn("BWoEC", discard.fates, d.yrs)
  recov.prop <- DiscardProd.s.fcn("Recovered", discard.fates, d.yrs)
  compost.prop <- DiscardProd.s.fcn("Composted", discard.fates, d.yrs)
  landfill.prop <- DiscardProd.s.fcn("Landfills", discard.fates, d.yrs)
  dumps.prop <- DiscardProd.s.fcn("Dumps", discard.fates, d.yrs)
  
  recov.hl <- HLCreation.fcn(5, discard.hl, eur.pulp, N.EUR)
  landfill.hl <- HLCreation.fcn(4, discard.hl, eur.pulp, N.EUR)
  landfill.fx.ratio <- HLCreation.fcn(3, discard.hl, eur.pulp, N.EUR)
  dump.hl <- HLCreation.fcn(2, discard.hl, eur.pulp, N.EUR)
  
  # filling in matrices for different disposal fate proportions by year
  
  eu_ratios$paper <- ifelse(eu_ratios$EndUseID %in% eur.pulp, 1, 0)
  
  eu_ratios2 <- left_join(eu_ratios, dec.prop[,-1], by = c("Year", "paper")) %>%
    left_join(burn.prop[,-1], by = c("Year", "paper")) %>%
    left_join(recov.prop[, -1], by = c("Year", "paper")) %>%
    left_join(compost.prop[, -1], by = c("Year", "paper")) %>%
    left_join(landfill.prop[, -1], by = c("Year", "paper")) %>%
    left_join(dumps.prop[, -1], by = c("Year", "paper"))
  
  #Creating the six immediate fates for Discarded Products (the sixth is Burned with Energy Capture, which will be a fraction of Burned)
  dec.input_matrix <- matrix(eu_ratios2$DEC, nrow = N.EUR) * dp.total_matrix
  bwoec.input_matrix <- matrix(eu_ratios2$BWoEC, nrow = N.EUR) * dp.total_matrix
  recov.input_matrix <- matrix(eu_ratios2$Recovered, nrow = N.EUR)  * dp.total_matrix      
  compost.input_matrix <- matrix(eu_ratios2$Composted, nrow = N.EUR)  * dp.total_matrix      
  landfill.input_matrix <- matrix(eu_ratios2$Landfills, nrow = N.EUR) * dp.total_matrix      
  dumps.input_matrix <- matrix(eu_ratios2$Dumps, nrow = N.EUR) * dp.total_matrix   
  
  
  # Carbon locked up in landfills (fixed; does not decay): lf.fixed_matrix.  
  lf.fixed_matrix <- matrix(rep(as.numeric(landfill.fx.ratio[,2]), d.yrs), nrow = N.EUR) * landfill.input_matrix
  
  # The remaining carbon in the landfill available for decay:
  lf.available_matrix <- landfill.input_matrix  - lf.fixed_matrix
  
  # Recovered:
  recov_decay <- decay.fcn.s(recov.input_matrix, recov.hl, N.EUR, d.yrs) # The empty.eu_matrix or first.eu_matrix remain the same as earlier,
  # as the distribution of the values over time does not change.  
  recov_matrix <- recov_decay[["decay"]]
  recov.discard_matrix <- recov_decay[["da"]]
  
  # Landfill (portion that decays): 
  landfill_decay <- decay.fcn.s(lf.available_matrix, landfill.hl, N.EUR, d.yrs) 
  landfill_matrix <- landfill_decay[["decay"]]
  landfill.discard_matrix <- landfill_decay[["da"]]
  
  # Dumps
  dumps_decay <- decay.fcn.s(dumps.input_matrix, dump.hl, N.EUR, d.yrs) 
  dumps_matrix <- dumps_decay[["decay"]]
  dumps.discard_matrix <- dumps_decay[["da"]]
  
  
  ######## Final fates ###########
  
  # Emitted With Energy Capture (fuelwood plus discarded products Burned with Energy Capture )
  eec_matrix <- fuel_matrix + dec.input_matrix
  
  #Emitted Without Energy Capture: combination of decay output
  ewoec_matrix <- dumps.discard_matrix + landfill.discard_matrix + recov.discard_matrix + compost.input_matrix + bwoec.input_matrix
  
  
  # Combine materials remaining in the landfill with material that will not decay in the landfill
  lf.fixed.cumsum_matrix <- lf.fixed_matrix
  for (i in 1:N.EUR) {
    lf.fixed.cumsum_matrix[i, ] <- as.numeric(cumsum(lf.fixed_matrix[i,  ]))
  }
  
  swdsCtotal_matrix <- lf.fixed.cumsum_matrix + landfill_matrix + dumps_matrix
  
  
  # Combining the products in use matrix with recovered products to have a final products in use matrix
  pu.final_matrix <- pu_matrix + recov_matrix
  
  
  
  return(list(eu_ratios = eu_ratios, 
              harv_cf1 = harv_cf1,
              recov.hl = recov.hl,
              landfill.hl = landfill.hl,
              landfill.fx.ratio = landfill.fx.ratio,
              dump.hl = dump.hl,
              eur.pulp = eur.pulp,
              eu_matrix = eu_matrix, eec_matrix = eec_matrix, fuel_matrix = fuel_matrix, dec.input_matrix = dec.input_matrix, ewoec_matrix = ewoec_matrix,
              dumps.discard_matrix = dumps.discard_matrix, landfill.discard_matrix = landfill.discard_matrix, recov.discard_matrix = recov.discard_matrix,
              compost.input_matrix = compost.input_matrix, bwoec.input_matrix = bwoec.input_matrix, swdsCtotal_matrix = swdsCtotal_matrix, lf.fixed.cumsum_matrix = lf.fixed.cumsum_matrix, 
              landfill_matrix = landfill_matrix, dumps_matrix = dumps_matrix, pu.final_matrix = pu.final_matrix, pu_matrix = pu_matrix, recov_matrix = recov_matrix,
              pu.discard_matrix = pu.discard_matrix, dp.total_matrix = dp.total_matrix, recov.input_matrix = recov.input_matrix, landfill.input_matrix = landfill.input_matrix, 
              dumps.input_matrix = dumps.input_matrix, landfill_matrix = landfill_matrix, lf.fixed_matrix = lf.fixed_matrix, eu.reduced_matrix = eu.reduced_matrix,
              dp_matrix = dp_matrix))
}



####  Functions shared by Sankey diagram and HWP functions ####

LT.fcn <- function(x, y) length(y[y <= x - 1])      #Less-Than function.  Involved in matching cubic foot conversion factors to board-feet values

# Function to create the half lives for recovered material, landfill, and dumps
HLCreation.fcn <- function(discard.col, discard.halflives, pulp, n.eur) {
  hl.vector <- rep(discard.halflives[2, discard.col], n.eur)  # Create vector EUR long, fill with wood half-life values for discard type.
  hl.vector[pulp] <- discard.halflives[1, discard.col]    #  Replace vector positions that correspond to "pulp" with paper half-life values for that discard type.
  hl.matrix <- matrix(c(1:n.eur, hl.vector), n.eur, 2) # Creates matrix with a column corresponding to EUR categories and another with corresponding half-lives.
}





#########################################################################################
#### ----------------------- HWP Model Functions ----------------------------------- ####
#########################################################################################


# The WhichMin.fcn function finds which entry is the first to appear (first year).  If it is year = 1, then changed to year 2 because
#  the matrix where this will be used starts on the second year if the first year has data.
WhichMin.fcn <- function(x) {y <- tryCatch(min(which(x > 0)), warning = function(x) 0)
if (y == 1) y <- 2
y
}

## The EmptyFirst.fcn is run to speed up the for-loops in the subsequent function "Decay.fcn".  It does this by finding which year-rows 
## contain no data (empty.array) and for the other rows which years are the first to contain values (first).  The outputs are two N.EUR x N.OWNERSHIP arrays.
EmptyFirst.fcn <- function(target.array, n.eur, n.ownership){        
  empty.array <- first.array <- matrix(0, n.eur, n.ownership)     # first.array = for vectors of years where there is a value, the first year  
  for (i in 1:n.ownership) {                                   # empty.array = vectors of years where for given owner, EUP, there are no values
    empty.array[, i] <- apply(target.array[, i, ], 1, sum)
    first.array[, i] <-   apply(target.array[, i, ], 1, WhichMin.fcn)
  }
  return(empty.first.output <- list(ea = empty.array, fa = first.array))
}



### This function conducts several summaries and calculations.  It creates a "decay array" from the target array that decays the target array
## over time and accounts for annual inputs from the target array.  It creates a "totals array" that is the cumulative sum over years of  group
##  every EU/owner.  The loop includes a 'next' call and starts the cumulative sums at the first non-zero entry to reduce computer time.  
Decay.fcn <- function(empty.array, first.array, target.array, decay.matrix, N.EUR, N.OWNERSHIP, N.YEARS) {
  decay.array <- totals.array <- discard.array1.5 <- array(0, c(N.EUR, N.OWNERSHIP, N.YEARS))    # Initiating with zeros
  decay.array[, , 1] <- target.array[, , 1]   # The first year = initial values
  for (k in 1:N.EUR) {  
    for (j in 1:N.OWNERSHIP) {
      if (empty.array[k,j] == 0) next 
      totals.array[k, j, ] <- cumsum(target.array[k, j,])
      
      for (i in first.array[k,j]:N.YEARS) {
        decay.array[k, j, i] <- as.numeric(target.array[k, j, i] + decay.array[k, j, i - 1] * exp(-1 * log(2)/decay.matrix[[k,2]]))
      }
    }
  }
  # Products in Use Discards (End Uses Totals -minus- Pruducts in Use array )
  discard.array1 <- totals.array - decay.array  # The cumulative sums of carbon put into use ever minus the actual amount that remains after decay.
  discard.array1.5[, , 2:N.YEARS] <- discard.array1[, , 1:(N.YEARS - 1)]  # Filling in with shifted discard.array1 values accounting for previous year carbon emitted
  discard.array <- discard.array1 - discard.array1.5     # Annual emissions = for each year, the cumulative sum of in-use carbon  
  #   minus the remaining carbon, minus the emitted values from the previous year.
  # Output
  totals.decay.output <- list(decay = decay.array, da = discard.array)
}  



## A function for creating columns of proportion values for wood and paper by discard type
#DiscardProd.fcn <- function(fate.type, disc.fates) {
#  disc.fates %>% filter(DiscardDestination == fate.type) %>%
#    pivot_longer(cols = 3:(N.YEARS + 2), names_to = "Year", values_to = fate.type) %>%
#    select(-DiscardDestination) %>%
#    mutate(Year = as.numeric(Year),
#           paper = ifelse(DiscardType == "paper", 1, 0))
#}



# A function for creating columns of proportion values for wood and paper by discard type
DiscardProd.fcn <- function(fate.type, disc.fates, N.YEARS) {
  disc.fates %>% filter(DiscardDestination == fate.type) %>%
    pivot_longer(cols = 3:(N.YEARS + 2), names_to = "Year", values_to = fate.type) %>%
    select(-DiscardDestination) %>%
    mutate(Year = as.numeric(Year),
           paper = ifelse(DiscardType == "paper", 1, 0))
}


