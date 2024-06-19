###########  MC Code  ###################

###########################################################################################################
### HWP Carbon Fate Modeling in R
###  Monte Carlo Simulation
###
###  This code runs the Monte Carlo simulation by first stating functions, then by drawing the random variable values
###   for each iteration and altering the input data prior to the Monte Carlo.  Finally it runs the Monte Carlo,
###   obtaining the altered values to be used with each iteration.  The code will output all iteration outputs (optional)
###   figures, and a plot to verify that the model reached convergence.  
###
###    Monte Carlo options, such as iteration number, are set in the HWP_MODEL_OPTIONS.csv file.
###
###    data.table functions are used instead of dplyr/tidyr for processing speed.
###
###########################################################################################################



#############################################################
###  ---               FUNCTIONS                  --- #######
#############################################################


## Placing functions outside of the MC loop:

## Update to decay.fcn.  Running in advance of the loop
decay.fcn2 <- function(empty.matrix, first.matrix, target.matrix, decay.vals, N.YEARS, N.EUR) {                  
  decay.matrix <- totals.matrix <- discard.matrix1.5 <- matrix(0, nrow = N.EUR, ncol = N.YEARS)    
  decay.matrix[, 1] <- target.matrix[, 1]   # The first year = initial values
  for (k in 1:N.EUR) {  
    if (empty.matrix[k] == 0) next 
    totals.matrix[k, ] <- cumsum(target.matrix[k, ])
    
    for (i in first.matrix[k]:N.YEARS) {
      decay.matrix[k, i] <- as.numeric(target.matrix[k, i] + decay.matrix[k, i - 1] * exp(-1 * log(2)/decay.vals[[k,2]]))
    }
  }
  #  # Products in Use Discards (End Uses Totals -minus- Pruducts in Use array )
  discard.matrix1 <- totals.matrix - decay.matrix  # The cumulative sums of carbon put into use ever minus the actual amount that remains after decay.
  discard.matrix1.5[, 2:N.YEARS] <- discard.matrix1[, 1:(N.YEARS - 1)]  # Filling in with shifted discard.matrix1 values accounting for previous year carbon emitted
  discard.matrix <- discard.matrix1 - discard.matrix1.5     # Annual emissions = for each year, the cumulative sum of in-use carbon  
  #  #   minus the remaining carbon, minus the emitted values from the previous year.
  #  # Output
  totals.decay.output <- list(decay = decay.matrix, da = discard.matrix)
}  


# build a function to create the half lives for recycling, landfill, and dumps
#hl.creation.fcn <- function(discard.col, N.EUR) {
#  hl.vector <- rep(discard.hl[2, discard.col], N.EUR)
#  hl.vector[eur.pulp] <- discard.hl[1, discard.col]
#  hl.matrix <- matrix(c(1:N.EUR, hl.vector), N.EUR, 2)
#}


# Functions for the triangular distributions

# Normality to uniform to triangle.  See
# https://stats.stackexchange.com/questions/66610/generate-pairs-of-random-numbers-uniformly-distributed-and-correlated/66617#66617

gen.gauss.cop.fcn <- function(r, n) {       # This function generates, from normal distributions, random correlated uniform distributions
  rho <- 2 * sin(r * pi/6)        # Pearson correlation
  P <- toeplitz(c(1, rho, rho))        # Correlation matrix
  d <- nrow(P)                    # Dimension
  ## Generate sample
  U <- pnorm(matrix(rnorm(n*d), ncol = d) %*% chol(P))
  return(U)
}

UnifTri.fcn <- function(u, a, b, c) {   # Function for transforming a [0,1] uniform distribution to a triangular distribution
  Fc = (c - a)/(b - a)
  if (u < Fc) a + sqrt(u * (b - a) * (c - a)) else b - sqrt((1 - u) * (b - a) * (c - a))
}


# MC Function for wood and paper.  This is a sum-to-one function, but run once for paper and once for wood.
wp.fcn <- function(wp.iter, wp.matrix, wp.mc, N.ITER, N.YEARS) {     #(Iteration number, wood or paper matrix, wood or paper random variable vector)
  for (q in 1:N.ITER) {        # 3 seconds for 2000 iter                                     #   Columns must sum to one.
    wp.matrix2 <- wp.matrix                       # q = iteration index.
    wp.matrix2[,1] <- q
    
    for (r in 1:N.YEARS) {
      max.wp <- which(wp.matrix2[1:6,(2 + r)] == max(wp.matrix2[1:6,(2 + r)]))                    # What is the location of the max value in the column?
      orig.rem.wp <- 1 - wp.matrix2[max.wp, (2 + r)]                                        # 1 - the max value (this is the sum of the remaining values). 
      wp.matrix2[max.wp, (2 + r)] <- wp.matrix2[max.wp, (2 + r)] * wp.mc[q]   # Adjusting the max value by the MC proportion
      if (wp.matrix2[max.wp, (2 + r)] >= 1)  {                                               # If the max value > 1, need to truncate at 1
        wp.matrix2[max.wp, (2 + r)] <- 1;                                                   #        and set all others to 0, start internal loop over.
        wp.matrix2[-max.wp, (2 + r)] <- 0; next}
      wp.rem <- 1 - wp.matrix2[max.wp, (2 + r)]                                            # This is the new remaining sum of the vector (not counting adjusted max).
      max.ratio <- wp.rem / orig.rem.wp                                                     # The ratio of the new remainder / old remainder is what each remaining
      wp.matrix2[-max.wp, (2 + r)] <- wp.matrix2[-max.wp, (2 + r)] * max.ratio            #      value is adjusted by.  
    }
    wp.iter[(q * 6 - 5):(q * 6), ] <- wp.matrix2                                                     
  }
  return(wp.iter)
}

# MC Function for use for landfill fixed ratios and half-life values for landfills, dumps, and recovered/recycled
# The function ensures that the paper and wood random variables are correlated.
disposal.decay.fcn <- function(ratio, c1, c2, param.name, N.ITER, N.EUR) {
  dd.adj1 <- vapply(c1, UnifTri.fcn, a = mc.adj.hwp$a[mc.adj.hwp$`Parameter_Name` == param.name & mc.adj.hwp$Paper == 1], 
                    b = mc.adj.hwp$b[mc.adj.hwp$`Parameter_Name` == param.name & mc.adj.hwp$Paper == 1], c = 1, numeric(1))   # Transforming MCV columns into triangular distributions
  dd.adj2 <- vapply(c2, UnifTri.fcn, a = mc.adj.hwp$a[mc.adj.hwp$`Parameter_Name` == param.name & mc.adj.hwp$Paper == 0], 
                    b = mc.adj.hwp$b[mc.adj.hwp$`Parameter_Name` == param.name & mc.adj.hwp$Paper == 0], c = 1, numeric(1))
  
  dd.p <- matrix(c(1:N.EUR, rep(ratio[,2],  N.ITER)), nrow = N.EUR, byrow = F)  # Filling a matrix with original fixed landfill ratios for wood and paper
  dd.p1 <- data.frame(lapply(data.frame(dd.p), unlist), stringsAsFactors = FALSE)          # Matrix is in an ugly form. Transforming to data frame
  dd.p1.paper <- sweep(dd.p1[eur.pulp,], MARGIN = 2, c(1, dd.adj2), "*")                #  Multiplying each wood column by corresponding MC value
  dd.p1.wood <- sweep(dd.p1[-eur.pulp,], MARGIN = 2, c(1, dd.adj1), "*")                #  Multiplying each paper column by corresponding MC value
  dd.p2 <- bind_rows(dd.p1.wood, dd.p1.paper) %>% arrange(X1)                          # Need to combine them, arranged by EUR order
  dd.p2 <- as.matrix(dd.p2)                                                            # Matrix for speed
  return(dd.p2)
}                                                           


# MC prep function.  Flexibly allows for different years and numbers of breaks for sequentially correlated but changing variables (e.g., for Harvest, where the
# CI range narrows in more recent years). 
corr.param.periods.fcn <- function(inp.table, inp.name, start.mcv, START.YEAR, END.YEAR) {
  n.inp.periods <- length(mc.adj.hwp$`Parameter_Name`[mc.adj.hwp$`Parameter_Name` == inp.name])  # Number of time periods with different MC CI ranges for the parameter of interest
  inp.cop <- chol(toeplitz(c(1, rep(R, (n.inp.periods - 1)))))                                    # Copula of size according to n.inp.periods
  mcv[, (start.mcv + 1):(start.mcv + n.inp.periods)] <- pnorm(qnorm(mcv[,(start.mcv + 1):(start.mcv + n.inp.periods)]) %*% inp.cop) # Correlating the random variables within a year
  
  mc.adj.hwp.inp <- mc.adj.hwp[mc.adj.hwp$`Parameter_Name` == inp.name,]                         # Small table of mc.adj.hwp for this single type of variable
  inp.yr.breaks <- which(years %in% c(START.YEAR, mc.adj.hwp.inp$`Last_Year`[1:(nrow(mc.adj.hwp.inp) - 1)], END.YEAR))  # Years bracketing time periods
  
  for (i in 1:n.inp.periods) {  # Bit of code that will define n different variables
    do.call("<-",list(paste0("type.adj", i), vapply(mcv[, start.mcv + i], UnifTri.fcn, a = mc.adj.hwp.inp$a[i], b = mc.adj.hwp.inp$b[i], c = 1, numeric(1))))
  }
  per.1.index <- c(1, rep(0, n.inp.periods - 1))  #need to index the first period for the next for-loop
  type.rep <- NULL
  for (i in 1:n.inp.periods) {
    new.rep <- rep(get(paste0("type.adj", i)), inp.yr.breaks[i + 1] - inp.yr.breaks[i] + per.1.index[i]) # Obtaining a vector of specific year-positions from a random variable.
    type.rep <- c(type.rep, new.rep)                                                                     #  Now attaching them end-to-end to create one vector of random numbers with different CIs.
  }
  return(list(yr.adj = matrix(type.rep, nrow = N.YEARS, ncol = N.ITER, byrow = T), end.mcv = start.mcv + n.inp.periods))
}


# MC function: finds triangular distribution endpoints from given confidence intervals.  Includes targ(et) lower CI, iteration start value
# (defalut = 0.5), and confidence interval (e.g., 0.9 is a 90% CI, 0.95 would be a 95% CI). Since triangular distribution is symmetric 
# around 1.0, the function only finds the lower end point and then calculates the upper end point from it.  Uses an optimization algorithm.
ab.boundaries.fcn <- function(targ, start.val, ci) {
  lower.ci <- (1 - ci)/2
  s <- start.val
  END <- FALSE
  lowbound <- 0     # The lowbound and highbound will move inward toward the solution
  highbound <- 1
  iter <- 1       # If iter proceeds for more than 100 iterations, the statement stops
  
  while (END == FALSE) {
    b = 1 - s + 1
    z <- qtriangle(lower.ci, s, b, 1)
    if (z < targ & abs(z - targ) > 0.001) {
      lowbound <- s 
      s <- (highbound - lowbound)/2 + s}
    if (z > targ & abs(z - targ) > 0.001)  {
      highbound <- s
      s <- s - (highbound - lowbound)/2}
    if (abs(z - targ) <= 0.001) END <- TRUE
    iter <- iter + 1
    if (iter > 100) END <- TRUE
    if (END == TRUE) return(list(a = s, b = b))
  }
}




#Function for saving the array.  If MC.ARRAY.OUT is FALSE, this step will be skipped
mc.save.array.fcn <- function() {
  MCarray.fcn <- function(arrayX, array_name) {
    x1 <- data.frame(arrayX[, , 1])
    x2 <- data.frame(arrayX[, , 2])
    x3 <- data.frame(arrayX[, , 3])
    x4 <- data.frame(arrayX[, , 4])
    
    x <- list(x1, x2, x3, x4)
    names(x) <- c("eec", "ewoec", "swdsC",  "pu")
    
    write_xlsx(x, path = paste0("MC_Output/MC_Tables/", array_name))
  }
  
  # Saving arrays clumped by summary arrays
  MCarray.fcn(MCout, "MC_Iterations.xlsx")
}



########################  MC PREPARATION CODE #############################

cop2 <- chol(toeplitz(c(1, R)))  #Creating a Choleski transformation for three-column variables.


###### Create random value matrices via LHS

# ccf_mt = cubic feet to metric tons C.  N.PPR values (variables) although some are not used.  [N.PPR,1]   (Variable Index =1:N.PPR)

mcv <- randomLHS(N.ITER, (nrow(mc.adj.hwp)))  # All MC draws.  Almost instantaneous if N.ITER = 100.  When N.ITER = 500, 15 seconds. 


# 1 = Conversion factors CCF to MTC
# 2 = End-use product half-life draws
# 3 = End Use Product Ratios
# 4, 5 = Discarded dispositions for paper and wood (Landfill portion that does not decay)
# 6, 7 = Landfill decay limits (portion not subject to decay) for paper and wood
# 8, 9 = Landfill half lives (for portion subject to decay), paper and wood
# 10, 11 = Dump half lives, paper and wood
# 12:13 =  Recovered half lives, paper and wood


# This bit of code transforms pairs of Uniform draws from the Monte Carlo to Normal distributions, using the 
# Choleski transformation to induce correlation, and then transforms them back to Uniform distributions.
mcv[,4:5] <- pnorm(qnorm(mcv[,4:5]) %*% cop2) # Discarded dispositions (Landfill portion that doesn't decay)
mcv[,6:7] <- pnorm(qnorm(mcv[,6:7]) %*% cop2) # Discarded dispositions (Landfill decay half-life)
mcv[,8:9] <- pnorm(qnorm(mcv[,8:9]) %*% cop2) # Discarded dispositions (Dump half-life)
mcv[,10:11] <- pnorm(qnorm(mcv[,10:11]) %*% cop2) # Discarded dispositions (Recovered half-life)
mcv[,12:13] <- pnorm(qnorm(mcv[,12:13]) %*% cop2) # Discarded disposition ratios for wood/paper 



########  Simplifying MC input by removing ownerships aside from "Total" ############

eu.mc_ratios <- setDT(eu_ratios)[Source == "Total",]

empty.eu.mc <- empty.eu_array[, N.OWNERSHIP]   # Simplifying earlier values already calculated
first.eu.mc <- first.eu_array[, N.OWNERSHIP]   # Simplifying earlier values already calculated

###### MC DRAWS - MC ALTERATIONS TO EACH VARIABLE  ######


# Finding the triangular distribution endpoints.  Editing user-defined triangular distributions for Monte Carlo from the template tab "MonteCarloValues"
mc.adj.hwp$b <- mc.adj.hwp$a  <- 0
mc.adj.hwp[,10:11] <- matrix(unlist(mapply(ab.boundaries.fcn, targ = mc.adj.hwp$MinCI, start.val = 0.5, ci = mc.adj.hwp$CI)), nrow(mc.adj.hwp), 2, byrow = T)


##### The following code changes the MC draws into triangular distributions, alters the target parameters, and creates matrices or arrays of the
#####   altered versions to use for each iteration

### ccf_mt operations (change to mcv[,1:N.PPR] to give each of the PP variables a different ccf to mt adjustment):
ccf_mt.adj <- t(matrix(vapply(mcv[, 1], UnifTri.fcn, a = mc.adj.hwp$a[mc.adj.hwp$`Parameter_Name` == "CCFtoMTC"], 
                              b = mc.adj.hwp$b[mc.adj.hwp$`Parameter_Name` == "CCFtoMTC"], 
                              c = 1, numeric(1)), nrow = N.ITER))   # Uniform LHS draws to triangular dist

ccf_mt.adj.df <- data.table(PrimaryProductID = 1:N.PPR, as.matrix(ccf_conversion.hwp[,2]) %*% ccf_mt.adj) # Multiplying columns of conversion values by each iteration's offset

#Data table left join
eu.ratios.CCF.MT <- ccf_mt.adj.df[eu.mc_ratios[, .(EndUseID, PrimaryProductID, TimberProductID, Year)], on = .(PrimaryProductID), allow.cartesian = TRUE]
setcolorder(eu.ratios.CCF.MT, c(1, (ncol(eu.ratios.CCF.MT) - 2) : ncol(eu.ratios.CCF.MT), 2:(N.ITER + 1)))


### End use half life
hl1 <- vapply(mcv[, 2], UnifTri.fcn, a = mc.adj.hwp$a[mc.adj.hwp$`Parameter_Name` == "EndUse_HalfLives"], 
              b = mc.adj.hwp$b[mc.adj.hwp$`Parameter_Name` == "EndUse_HalfLives"], c = 1, numeric(1))
hl2 <- matrix(rep(hl1, each = N.EUR), nrow = N.EUR, byrow = F)
hl3 <- sweep(as.matrix(hl2), 1, as.matrix(eu_half.lives.hwp[,2]), "*")  # multiplies half-live value across all random variables



### EUR Iterations
eu.mc <- vapply(mcv[, 3], UnifTri.fcn, a = mc.adj.hwp$a[mc.adj.hwp$`Parameter_Name` == "EndUseRatios"], 
                b = mc.adj.hwp$b[mc.adj.hwp$`Parameter_Name` == "EndUseRatios"], c = 1, numeric(1))      # One random triangular variable vector for EUR, not three like TPR, PPR

eur_ppr <- setDT(ratio_cat.hwp)[, .(EndUseID, PrimaryProductID)]
eur.hwp.mc <- setDT(eur.hwp)
eur2 <- eur.hwp[eur_ppr, on = .(EndUseID), allow.cartesian = TRUE]
setcolorder(eur2, c(1, ncol(eur2)))                         # Need columns EndUseID and PrimaryProductID to be first.
eur.iter <- matrix(0, N.EUR * N.ITER, N.YEARS + 3)            # Initializing matrices.  We need, for each iteration and year, a vector of the N.PPR EUR values,
eur.matrix <- as.matrix(data.frame(iter = 0, eur2))               #   adjusted by the appropriate MC value (at least the max of the N.PPR values is adjusted

ppr.id <- matrix(1:N.PPR, N.PPR, 1)
ppr.which.fcn <- function(pprId, eurId) {which(eurId == pprId) }
eur_sto <- apply(ppr.id, 1, ppr.which.fcn, eurId = eur_ppr[,2])     # Which EUR sets must sum to one?
eur_sto_length <- lapply(eur_sto, length)                       # Length of EUR sets that sum to one.

# Need a group x year matrix to show which value is the largest
eur_sto_gt1 <- which(eur_sto_length > 1)
eur_sto_gt1_length <- length(eur_sto_gt1)
eur_sto_matrix <- matrix(0, eur_sto_gt1_length, N.YEARS)

for (m in 1:eur_sto_gt1_length) {
  for (w in 1:N.YEARS) {
    if (sum(eur2[eur_sto[[eur_sto_gt1[m]]],2 + w]) == 0) eur_sto_matrix[m, w] <- 999 else{       # If the whole EUR set = 0, give the storage matrix cell a value of 999.
      eur_sto_matrix[m, w] <-  which(eur2[eur_sto[[eur_sto_gt1[m]]],2 + w] == max(eur2[eur_sto[[eur_sto_gt1[m]]],2 + w]))  } # Otherwise, enter in the cell the location of the max value. 
  }
}

#EUR loop to alter the sum-to-one sets of EUR values by year and iteration. 
for (q in 1:N.ITER) {                              #   Columns must sum to one.
  eur.matrix2 <- eur.matrix                       # q = iteration index.
  eur.matrix2[,1] <- q
  
  for (r in 1:N.YEARS) {
    for (s in 1:eur_sto_gt1_length) {
      if (eur_sto_matrix[s, r] == 999) next
      if (eur.matrix2[eur_sto[[eur_sto_gt1[s]]][eur_sto_matrix[s,r]], (3 + r)] == 1) next
      eur.matrix2.1 <- eur.matrix2[eur_sto[[eur_sto_gt1[s]]], (3 + r)]
      max.eur <- eur_sto_matrix[s,r]                # What is the location of the max value in the column?
      if (eur.matrix2.1[max.eur] == 1) next                         # If, among a set of values, one is 1.0, it remains unchanged. 
      orig.rem.eur <- 1 - eur.matrix2[eur_sto[[eur_sto_gt1[s]]][eur_sto_matrix[s,r]], (3 + r)]                                        # 1 - the max value (this is the sum of the remaining values). 
      eur.matrix2.1[max.eur] <- eur.matrix2.1[max.eur] * eu.mc[q]   # Adjusting the max value by the MC proportion
      if (eur.matrix2.1[max.eur] >= 1)  {                                               # If the max value > 1, need to truncate at 1
        eur.matrix2.1[max.eur] <- 1;                                                   #        and set all others to 0, start internal loop over.
        eur.matrix2.1[-max.eur] <- 0; next}
      eur.rem <- 1 - eur.matrix2.1[max.eur]                                            # This is the new remaining sum of the vector (not counting adjusted max).
      max.ratio <- eur.rem / orig.rem.eur                                                     # The ratio of the new remainder / old remainder is what each remaining
      eur.matrix2.1[-max.eur] <- eur.matrix2.1[-max.eur] * max.ratio            #      value is adjusted by.  
      eur.matrix2[eur_sto[[eur_sto_gt1[s]]], (3 + r)] <- eur.matrix2.1
    }
  }
  eur.iter[(q * N.EUR - (N.EUR - 1)):(q * N.EUR), ] <- eur.matrix2                         # Entering adjustments for each iteration/EUR set            
}

eur.iter <- data.table(eur.iter)
setnames(eur.iter, c("iter", "EndUseID", "PrimaryProductID", paste0("X", years)))

eur.iter1 <- melt(eur.iter, id.vars = 1:3, measure = patterns("X"), variable.name = "Year", value.name = "eur_adj")
eur.iter2 <- dcast(eur.iter1, Year + EndUseID ~ iter, value.var = "eur_adj")
eur.iter2$Year <- as.numeric(substr(eur.iter2$Year, 2, 5))

eu_ratios.eur <- eur.iter2[eu.mc_ratios[, .(EndUseID, PrimaryProductID, TimberProductID, Year)], on = .(Year, EndUseID)]
setcolorder(eu_ratios.eur, c(1:2, (ncol(eu_ratios.eur) - 1):ncol(eu_ratios.eur), 3:(N.ITER + 2)))


### Wood and Paper discarded disposition iterations
paper.mc <- vapply(mcv[, 4], UnifTri.fcn, a = mc.adj.hwp$a[mc.adj.hwp$`Parameter_Name` == "DiscardedDispositionRatios" & mc.adj.hwp$Paper == 1], 
                   b = mc.adj.hwp$b[mc.adj.hwp$`Parameter_Name` == "DiscardedDispositionRatios" & mc.adj.hwp$Paper == 1], c = 1, numeric(1))
wood.mc <- vapply(mcv[, 5], UnifTri.fcn, a = mc.adj.hwp$a[mc.adj.hwp$`Parameter_Name` == "DiscardedDispositionRatios" & mc.adj.hwp$Paper == 0], 
                  b = mc.adj.hwp$b[mc.adj.hwp$`Parameter_Name` == "DiscardedDispositionRatios" & mc.adj.hwp$Paper == 0], c = 1, numeric(1))


df3 <- data.table(order = 1:12, discard.fates.hwp[, 1:(N.YEARS + 2)])   # Numbering the rows for Discard Fates 1 through 12
df3 <- setorder(df3, DiscardType, order)
df3 <- df3[, !c("DiscardType","DiscardDestination")]

p.iter <- as.data.table(matrix(0, 6 * N.ITER, N.YEARS + 2))    # Paper: Initial columns for iteration, order
p.matrix <- data.table(iter = 0, df3[1:6,])           

w.iter <- as.data.table(matrix(0, 6 * N.ITER, N.YEARS + 2))    # Wood: Initial columns for iteration, order
w.matrix <- data.table(iter = 0, df3[7:12,])           

paper.iter <- wp.fcn(as.matrix(p.iter), as.matrix(p.matrix), paper.mc, N.ITER, N.YEARS)   # Running the above function for paper, then wood
wood.iter <- wp.fcn(as.matrix(w.iter), as.matrix(w.matrix), wood.mc, N.ITER, N.YEARS)

paper.wood.iter <- data.frame(cbind(paper.iter, wood.iter[, -c(1:2)])) # Combining the results for paper and wood
paper.wood.iter <- as.matrix(paper.wood.iter)

# Breaking the values out of paper.wood.iter by discard fate 
dec.iter <- t(paper.wood.iter[paper.wood.iter[,2] == 1, -c(1:2)])
burn.iter <- t(paper.wood.iter[paper.wood.iter[,2] == 3, -c(1:2)])
recov.iter <- t(paper.wood.iter[paper.wood.iter[,2] == 5, -c(1:2)])
compost.iter <- t(paper.wood.iter[paper.wood.iter[,2] == 7, -c(1:2)])
landfill.iter <- t(paper.wood.iter[paper.wood.iter[,2] == 9, -c(1:2)])
dumps.iter <- t(paper.wood.iter[paper.wood.iter[,2] == 11, -c(1:2)])


# Discarded disposition ratios for paper and wood

lf.fx <- disposal.decay.fcn(landfill.fx.ratio, mcv[,6], mcv[,7], "LandfillDecayLimits" , N.ITER, N.EUR)  # Landfill disposition ratio, fixed
lf.dd <- disposal.decay.fcn(landfill.hl, mcv[,8], mcv[,9], "Landfill_HalfLives", N.ITER, N.EUR)         # Landfill proportion subject to decay, disposition ratio
dmp.dd <- disposal.decay.fcn(dump.hl, mcv[,10], mcv[,11], "Dump_HalfLives", N.ITER, N.EUR)         # dump disposition ratio
rec.dd <- disposal.decay.fcn(recov.hl, mcv[,12], mcv[,13], "Recovered_HalfLives", N.ITER, N.EUR)        # Recovered disposition ratio

### Timber harvest
harv.mc_cf <- setDT(harv_cf1)[Source == "Total", ]
harv.yr.adj <- corr.param.periods.fcn(harv_cf1, "Harvest", 13, START.YEAR, END.YEAR)  # Correlating the necessary parameters.
harv.yr.adj2 <- data.table(Year = years, drop(matrix(harv.mc_cf$Vol_cf, ncol = 1)) * harv.yr.adj$yr.adj)  # scalar vector multiplied by the matrix

eu_ratios.harv <- harv.yr.adj2[eu.mc_ratios[, .(EndUseID, PrimaryProductID, TimberProductID, Year)], on = "Year"] # Pivot table, columns for each iteration
setcolorder(eu_ratios.harv, c(1, (ncol(eu_ratios.harv) - 2):ncol(eu_ratios.harv), 2:(N.ITER + 1)))



### Timber Products Ratio
tpr.yr.adj <- corr.param.periods.fcn(setDT(tpr.hwp), "TimberProdRatios", harv.yr.adj$end.mcv, START.YEAR, END.YEAR)

tpr.yr.adj2 <- cbind(Year = years, tpr.yr.adj$yr.adj)                                              # Adding a year-column to the beginning.

# TPR iterations
tpr.iter <- matrix(0, N.TPR * N.ITER, N.YEARS + 2)                                       #   We need, for each iteration and year, a vector of the 40 TPR ratios,
tpr.matrix <- as.matrix(data.frame(iter = 0, tpr.hwp))  

for (q in 1:N.ITER) {                              #   Columns must sum to one.
  tpr.matrix2 <- tpr.matrix                       # q = iteration index.
  tpr.matrix2[,1] <- q
  
  for (r in 1:N.YEARS) {
    max.tpr <- which(tpr.matrix2[,(2 + r)] == max(tpr.matrix2[,(2 + r)]))                    # What is the location of the max value in the column?
    if (tpr.matrix2[max.tpr] == 1) next                         # If, among a set of values, one is 1.0, it remains unchanged. 
    orig.rem.tpr <- 1 - tpr.matrix2[max.tpr, (2 + r)]                                        # 1 - the max value (this is the sum of the remaining values). 
    tpr.matrix2[max.tpr, (2 + r)] <- tpr.matrix2[max.tpr, (2 + r)] * tpr.yr.adj2[r, q + 1]   # Adjusting the max value by the MC proportion
    if (tpr.matrix2[max.tpr, (2 + r)] >= 1)  {                                               # If the max value > 1, need to truncate at 1
      tpr.matrix2[max.tpr, (2 + r)] <- 1;                                                   #        and set all others to 0, start internal loop over.
      tpr.matrix2[-max.tpr, (2 + r)] <- 0; next}
    tpr.rem <- 1 - tpr.matrix2[max.tpr, (2 + r)]                                            # This is the new remaining sum of the vector (not counting adjusted max).
    max.ratio <- tpr.rem / orig.rem.tpr                                                     # The ratio of the new remainder / old remainder is what each remaining
    tpr.matrix2[-max.tpr, (2 + r)] <- tpr.matrix2[-max.tpr, (2 + r)] * max.ratio            #      value is adjusted by.  
  }
  tpr.iter[(q * N.TPR - (N.TPR - 1)):(q * N.TPR), ] <- tpr.matrix2                                                      # Stacking all 40 x years by iteration.
}

tpr.iter <- as.data.table(tpr.iter)
names(tpr.iter) <- c("iter", "TimberProductID", paste0("X", years))

tpr.iter1 <- melt(tpr.iter, id.vars = 1:2, measure = patterns("X"), variable.name = "Year", value.name = "tpr_adj")
tpr.iter2 <- dcast(tpr.iter1, Year + TimberProductID ~ iter, value.var = "tpr_adj")
tpr.iter2$Year <- as.numeric(substr(tpr.iter2$Year, 2, 5))

eu_ratios.tpr <- tpr.iter2[eu.mc_ratios[, .(EndUseID, PrimaryProductID, TimberProductID, Year)], on = c("Year", "TimberProductID")]
setcolorder(eu_ratios.tpr, c(1:2, (ncol(eu_ratios.tpr) - 1):ncol(eu_ratios.tpr), 3:(N.ITER + 2)))


#### Primary Products Ratio
ppr.yr.adj <- corr.param.periods.fcn(setDT(ppr.hwp), "PrimaryProdRatios", tpr.yr.adj$end.mcv, START.YEAR, END.YEAR)
ppr.yr.adj2 <- as.matrix(cbind(Year = years, ppr.yr.adj$yr.adj))                                              # Adding a year-column to the beginning.


# PPR iterations   (Did not perform this action using Data Table because it is not memory intensive)
ppr_tpr <- ratio_cat.hwp %>% select(PrimaryProductID, TimberProductID) %>%
  distinct() %>% select(TimberProductID)

ppr.iter <- matrix(0, N.PPR * N.ITER, N.YEARS + 3)                     # We need, for each iteration and year, a vector of the 40 TPR ratios,
ppr.matrix <- as.matrix(data.frame(iter = 0, ppr_tpr[,1], ppr.hwp))    #   adjusted by the appropriate MC value (at least the max of the 40 values is adjusted

tpr.id <- matrix(1:40, 40, 1)
tpr.which.fcn <- function(tprId, pprId) {which(pprId == tprId) }
ppr_sto <- apply(tpr.id, 1, tpr.which.fcn, pprId = ppr_tpr)     # Which PPR sets must sum to one?
ppr_sto_length <- lapply(ppr_sto, length)                       # Length of PPR sets that sum to one


for (q in 1:N.ITER) {                             #   Columns must sum to one.
  ppr.matrix2 <- ppr.matrix                       # q = iteration index.
  ppr.matrix2[,1] <- q
  
  for (r in 1:N.YEARS) {
    for (s in 1:40) {
      if (ppr_sto_length[[s]] == 1) next
      ppr.matrix2.1 <- ppr.matrix2[ppr_sto[[s]], (3 + r)]
      max.ppr <- which(ppr.matrix2.1 == max(ppr.matrix2.1))                    # What is the location of the max value in the column?
      if (ppr.matrix2.1[max.ppr] == 1) next                                                       # If, among a set of values, one is 1.0, it remains unchanged. 
      orig.rem.ppr <- 1 - ppr.matrix2.1[max.ppr]                                        # 1 - the max value (this is the sum of the remaining values). 
      ppr.matrix2.1[max.ppr] <- ppr.matrix2.1[max.ppr] * ppr.yr.adj2[r, q + 1]   # Adjusting the max value by the MC proportion
      if (ppr.matrix2.1[max.ppr] >= 1)  {                                               # If the max value > 1, need to truncate at 1
        ppr.matrix2.1[max.ppr] <- 1;                                                   #        and set all others to 0, start internal loop over.
        ppr.matrix2.1[-max.ppr] <- 0; next}
      ppr.rem <- 1 - ppr.matrix2.1[max.ppr]                                            # This is the new remaining sum of the vector (not counting adjusted max).
      max.ratio <- ppr.rem / orig.rem.ppr                                                     # The ratio of the new remainder / old remainder is what each remaining
      ppr.matrix2.1[-max.ppr] <- ppr.matrix2.1[-max.ppr] * max.ratio            #      value is adjusted by.  
      ppr.matrix2[ppr_sto[[s]], (3 + r)] <- ppr.matrix2.1
    }
  }
  ppr.iter[(q * N.PPR - (N.PPR - 1)):(q * N.PPR), ] <- ppr.matrix2                                                      # Stacking all 40 x years by iteration.
}

ppr.iter <- as.data.table(ppr.iter)
names(ppr.iter) <- c("iter", "TimberProductID", "PrimaryProductID", paste0("X", years))

ppr.iter1 <- melt(ppr.iter, id.vars = 1:3, measure = patterns("X"), variable.name = "Year", value.name = "ppr_adj")
ppr.iter2 <- dcast(ppr.iter1, Year + PrimaryProductID ~ iter, value.var = "ppr_adj")
ppr.iter2$Year <- as.numeric(substr(ppr.iter2$Year, 2, 5))

eu_ratios.ppr <- ppr.iter2[eu.mc_ratios[, .(EndUseID, PrimaryProductID, TimberProductID, Year)], on = c("Year", "PrimaryProductID")]
setcolorder(eu_ratios.ppr, c(1:2, (ncol(eu_ratios.ppr) - 1), ncol(eu_ratios.ppr),  3:(ncol(eu_ratios.ppr) - 2)))


## Variables to use
eur.fuel <- grep("fuel", ratio_cat.hwp$EndUseProduct)
eur.pulp <- grep("pulp", ratio_cat.hwp$EndUseProduct)      #End use ratio rows for wood pulp (100% used, no discard)

# Matrix where the products in use take into account the new products and the half-life of earlier products
PIU.LOSS <- matrix(rep(PIU.WOOD.LOSS, N.EUR), ncol = 1)
PIU.LOSS[eur.pulp,] <- PIU.PAPER.LOSS
nonPIU.loss <- 1.0 - PIU.LOSS
nonPIU.loss[eur.fuel] <- 1                 # Again, no PIU change for pulp or fuel wood



########## Results output #############
MCout <- array(0, c(N.YEARS, N.ITER, 4))


################ Begin MC #############################
for (x in 1:N.ITER) {
  
  ccf_mt.change <- eu.ratios.CCF.MT[, x + 4, with = FALSE]                 # Making the variable palatable for the "mutate" function in the next line
  Vol_cf.adj <- eu_ratios.harv[, x + 4, with = FALSE]                   # Harvest's contribution
  landfill.fx.ratio.adj <- lf.fx[, c(1, x + 1)]           # Landfill fixed ratio
  landfill.hl.adj <- lf.dd[, c(1, x + 1)]                 # Landfill decay half-life
  dump.hl.adj <- dmp.dd[, c(1, x + 1)]                    # Dump decay half-life
  recov.hl.adj <- rec.dd[, c(1, x + 1)]                   # Recovered/recycled decay half-life
  tpr.adj <- eu_ratios.tpr[,c(x + 4), with = FALSE]                     # Timber Product Ratios
  ppr.adj <- eu_ratios.ppr[,c(x + 4), with = FALSE]                     # Primary Product Ratios
  eur.adj <- eu_ratios.eur[,c(x + 4), with = FALSE]                     # End Use Product Ratios
  wp.adj <- paper.wood.iter[paper.wood.iter[,1] == x, 3:N.YEARS]
  
  # Note that MTC in this version of eu_ratios uses many of the adjusted parameters 
  eu_ratiosMC <- eu.mc_ratios[, MTC := ..eur.adj * ..tpr.adj * ..ppr.adj * ..ccf_mt.change * ..Vol_cf.adj]   
  
  # eu_matrix is the total annual C in the Total ownership category
  eu_matrix <- matrix(eu_ratiosMC[, MTC], nrow = N.EUR)      # previously eu_array
  
  #########  Emitted with energy capture array #############
  
  # Finding the End Use Product rows that correspond to fuel wood
  
  # Emitted with Energy Capture (eec) matrix for fuelwood
  fuel_matrix <- matrix(0, nrow = N.EUR, ncol = N.YEARS)                                # Previously fuel_array
  fuel_matrix[eur.fuel, ] <- eu_matrix[eur.fuel, ]  # Redefining using original version as a template
  
  # Products in Use matrix (pu_matrix)
  euhlMC <- matrix(c(1:N.EUR, hl3[,x]), nrow = N.EUR, byrow = F)                          ## Inserting the MC version of half lives (see above)
  
  # Need the discarded products matrix (dp_matrix), generated every year from new timber harvest
  dp_matrix <- sweep(eu_matrix, MARGIN = 1, PIU.LOSS, `*`)    # Previously dp_array
  dp_matrix[eur.fuel,  ] <- 0            # Removing fuel wood from the dp_matrix because it is assumed burned in the given year (no discard). 


  # Need to develop an array where the products in use take into account the new products and the half-life of earlier products
  eu.reduced_matrix <- sweep(eu_matrix, MARGIN = 1, nonPIU.loss, `*`) # This is the End Use matrix reduced by the Placed in Use Loss (1-PIU.loss) except for fuel
  eu.reduced_matrix[eur.fuel, ] <- 0                   # No end use for fuel (burned to create fuel_matrix)
  
  
  # Products in Use matrix (determining cumulative totals over time with decay)
  pu_matrix <- matrix(0, nrow = N.EUR, ncol = N.YEARS)   # Previously pu_array
  pu_matrix[, 1] <- eu.reduced_matrix[, 1]       # The first year = initial harvest values
  
  
  totals_decay <- decay.fcn2(empty.eu.mc, first.eu.mc, eu.reduced_matrix, euhlMC, N.YEARS, N.EUR)
  pu_matrix <- totals_decay[["decay"]]
  pu.discard_matrix <- totals_decay[["da"]]
  
  
  # Joining the 8% loss when placed into use with the half-life loss
  dp_matrix <- dp_matrix + pu.discard_matrix
  
  
  ######################################
  ## Initiating Discarded Products fates
  ######################################
  
  
  # First, creating matrices for calculating the proportion going to different fates
  
  discard.props <- data.table(Year = rep(years, 2), paper = rep(c(1, 0), each = N.YEARS), 
                              DEC = dec.iter[,x], Burned = burn.iter[,x], Recovered = recov.iter[,x], 
                              Composted = compost.iter[,x], Landfills = landfill.iter[,x], Dumps = dumps.iter[,x])
  #
  
  eu_ratiosMC$paper <- ifelse(eu_ratiosMC$EndUseID %in% eur.pulp, 1, 0)
  eu_ratiosMC2 <- discard.props[eu_ratiosMC, on = .(Year, paper)]
  
  #Creating the five immediate fates for Discarded Products (the sixth is Burned with Energy Capture, which will be a fraction of Burned)
  dec.input_matrix <- matrix(eu_ratiosMC2[, DEC], nrow = N.EUR) * dp_matrix                   # Previously dec.input_array
  burn.input_matrix <- matrix(eu_ratiosMC2[, Burned], nrow = N.EUR) * dp_matrix                 # Previously burn.input_array
  recov.input_matrix <- matrix(eu_ratiosMC2[, Recovered], nrow = N.EUR)  * dp_matrix                  # Previously recov.input_array      
  compost.input_matrix <- matrix(eu_ratiosMC2[, Composted], nrow = N.EUR)  * dp_matrix               # Previously compost.input_array      
  landfill.input_matrix <- matrix(eu_ratiosMC2[, Landfills], nrow = N.EUR) * dp_matrix               # Previously landfill.input_array      
  dumps.input_matrix <- matrix(eu_ratiosMC2[, Dumps], nrow = N.EUR) * dp_matrix                 # Previously dumps.input_array   
  
  
  # Carbon locked up in landfills (does not decay): Solid Waste Disposal Site Carbon (swdsC_matrix).  
  swdsC_matrix <- matrix(rep(as.numeric(landfill.fx.ratio.adj[,2]), N.YEARS), nrow = N.EUR) * landfill.input_matrix
  
  
  # The remaining carbon in the landfill available for decay:
  landfill.available_matrix <- landfill.input_matrix  - swdsC_matrix
  
  
  # Recycling:
  recov_decay <- decay.fcn2(empty.eu.mc, first.eu.mc, recov.input_matrix, recov.hl.adj, N.YEARS, N.EUR) 
  recov_matrix <- recov_decay[["decay"]]
  recov.discard_matrix <- recov_decay[["da"]]
  
  # Landfill (portion that decays): 
  landfill_decay <- decay.fcn2(empty.eu.mc, first.eu.mc, landfill.available_matrix, landfill.hl.adj, N.YEARS, N.EUR) 
  landfill_matrix <- landfill_decay[["decay"]]
  landfill.discard_matrix <- landfill_decay[["da"]]
  
  # Dumps
  dumps_decay <- decay.fcn2(empty.eu.mc, first.eu.mc, dumps.input_matrix, dump.hl.adj, N.YEARS, N.EUR) 
  dumps_matrix <- dumps_decay[["decay"]]
  dumps.discard_matrix <- dumps_decay[["da"]]
  
  
  ######## Final fates ###########
  
  # Emitted With Energy Capture (fuelwood plus discarded products * Burned with Energy Capture ratios)
  eec_matrix <- fuel_matrix + dec.input_matrix
  
  #Emitted Without Energy Capture: combination of decay output
  ewoec_matrix <- dumps.discard_matrix + landfill.discard_matrix + recov.discard_matrix + compost.input_matrix + burn.input_matrix
  
  
  # Combine materials remaining in the landfill with material that will not decay in the landfill
  #swdsC_matrix <- cumsum(swdsC_matrix)
  swdsC.cumsum_matrix <- swdsC_matrix
  for (i in 1:N.EUR) {
    swdsC.cumsum_matrix[i, ] <- as.numeric(cumsum(swdsC_matrix[i, ]))
  }
  
  swdsCtotal_matrix <- swdsC.cumsum_matrix + landfill_matrix + dumps_matrix
  
  
  # combining the products in use matrix with recycling to have a final products in use matrix
  pu.final_matrix <- pu_matrix + recov_matrix
  
  
  
  ### MC outputs
  # Cumulative carbon in HWP
  
  eec <- cumsum(apply(eec_matrix, 2, sum))
  ewoec <- cumsum(apply(ewoec_matrix, 2, sum))
  swdsC <- apply(swdsCtotal_matrix, 2, sum)
  pu <- apply(pu.final_matrix, 2, sum)
  
  
  # Emitted, cumulative (These are the results of the iteration)
  
  MCout[, x, 1:4] <- matrix(c(eec, ewoec, swdsC, pu), byrow = F)
  
}
################ End of MC loop ###########################



##### Short-cut for avoiding re-running the MC to produce output once MC has been run.  
##### If reading in the MCout file, it needs to be re-constructed as an array.  Use the following code to do so. #########
#sheetVector <- c('eec', 'ewoec', 'swdsC', 'pu')  # The names of the Excel worksheets for the file MC_All.xlsx
#
#MCout <- lapply(                     #  Reads the four worksheets and creates a list of matrices from them.
#  sheetVector,
#  function(sheet) {
#    read_xlsx(path = "MonteCarlo/MC_Output/MC_All.xlsx", sheet = sheet)  
#  }
#)

#MCout <- abind(MCout, along = 3)     # Creating an array out of the list using the abind function        
###########################################################

