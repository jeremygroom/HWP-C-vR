# Quality Assurance code #

# This code verifies features of the ten input files to ensure that the code runs correctly.  
# The output file will create a message that verifies if a feature passed the check or not. 
##################################



################################ Functions ##########################

# The following function is used for year-checking
#colnum.fcn <- function(rept, col.id) which(colnames(rept) == col.id) 

year.n.check.fcn <- function(true1, table.hwp, non.yr.cols) {
  if (true1) {
    if (ncol(table.hwp) - non.yr.cols == N.YEARS) {
      msg <- "Year number matches Harvest_BF year number."
      term <- 0
    } else {
      msg <- "Year number DOES NOT MATCH Harvest_BF year number."
      term <- 1
    }} else {
      msg <- 0; term <- 0}
  return(list(msg = msg, term = term))
}

# This function examines the column names for years and ensures that they can be changed to numeric values.
yr.name.check.fcn <- function(true1, table.hwp, start.col) {
  if (true1) {
    if (any(is.na(suppressWarnings((as.numeric(colnames(table.hwp)[start.col:ncol(table.hwp)])))))) {
      msg <- "Year columns ARE NOT numeric"
      term <- 1 } else {
        msg <- "Year columns are numeric" 
        term <- 0  }} else {
          msg <- 0; term <- 0}
  return(list(msg = msg, term = term))
}

# Checking that all values are numeric
vals.numeric.check.fcn <- function(true1, table.hwp, start.col) {
  if (true1) {
    if (all(apply(data.frame(table.hwp[,start.col:ncol(table.hwp)]), 2, is.numeric))) {
      msg <- "Values are numeric" 
      term <- 0 } else {
        msg <- "Values ARE NOT numeric"
        term <- 1  }} else {
          msg <- 0; term <- 0}
  return(list(msg = msg, term = term)) 
    }
  


# This function may be used for checking row names or column names.  
name.check.fcn <- function(true1, target.vector, targ.names, english.num, col.row) {
  if (true1) { 
    if (suppressWarnings(all(target.vector == targ.names))) {
      msg <- paste("First", english.num, col.row, "correctly labeled")
      term <- 0 } else if (true1) {
        msg <- paste("First", english.num, col.row, "NOT labeled", paste(targ.names, collapse = ", "))
        term <- 1 }} else {
          msg <- 0
          term <- 0}
  return(list(msg = msg, term = term))
}


# Function to see if files can be read / are there.  
tC.read <- function(dataframe.name) {
  out <- tryCatch( {hwp.data[[which(names(hwp.data) == dataframe.name)]]}, 
                   error = function(cond) {NULL} )
  out2 <- if (is.data.frame(out)) list(tble = out, msg = paste("Worksheet", dataframe.name, "found"), term = 0) else {
    list(tble = out, msg = paste("File", dataframe.name, "NOT found.  Verify Excel worksheet is correctly named."), term = 1)}
  return(out2)
}


#####################################################################################################
## Setting up tables and recording error messages.  
#####################################################################################################

harv.hwp <- hwp.data$Harvest_MBF   # harvest data, units = 1000 board feet. Used to obtain number of years (N.YEARS) and ownership names
bfcf.hwp <- hwp.data$BFCF  # Board feet to cubic feet
tpr.hwp <- hwp.data$TimberProdRatios     # Timber product ratios  (n = 40)
ppr.hwp <- hwp.data$PrimaryProdRatio      # Primary product ratios (n = 64) 
eur.hwp <- hwp.data$EndUseRatios       # Loading End Use Ratios (n = N.EUR)
ratio_cat.hwp <- hwp.data$RatioCategories    # Associates TPR, EUR, and PPR values
ccf_conversion.hwp <- hwp.data$CCF_MT_Conversion     # getting hundred cubic feet (CCF) to metric ton (MT) carbon conversion factors
eu_half.lives.hwp <- hwp.data$EU_HalfLives    # Half-lives of End Use Products
discard.fates.hwp <- hwp.data$DiscardFates    # Fraction of discarded C to different fates (dumps, compost, landfill, burned with energy capture, etc.)
discard.hl.hwp <- hwp.data$Discard_HalfLives  # Half-lives of discards in different SWDS categories (dumps, landfills) plus fraction of landfill discard that doesn't decay
mc.adj.hwp <- hwp.data$MonteCarloValues 


## Setting up the data frames "terminate" and "error report".  The data frame "terminate" saves instances that should stop the process from proceeding.
#   The data frame "error report" will contain hopefully useful messages for users so that they know what to fix. 
terminate <- error.report <- data.frame(item.id = 1:7, harv_hwp = rep(0, 7), bfcf_hwp = rep(0, 7), tpr_hwp = rep(0, 7), ppr_hwp = rep(0, 7), 
                                        ratio_cat_hwp = rep(0, 7), ccf_conversion_hwp = rep(0, 7), eur_hwp = rep(0, 7), eu_half_lives_hwp = rep(0, 7),
                                        discard_fates_hwp = rep(0, 7), discard_hl_hwp = rep(0, 7), mc_adj_hwp = rep(0, 7))

# QA checks for harv.hwp.  Three checks.

harv_hwp <- tC.read("Harvest_MBF"); harv.hwp <- harv_hwp$tble; terminate$harv_hwp[1] <- harv_hwp$term; error.report$harv_hwp[1] <- harv_hwp$msg
harv_true1 <- is.data.frame(harv.hwp)

if (harv_true1) {
  if (all(names(harv.hwp)[c(1, ncol(harv.hwp))] == c("Year", "Total")) == TRUE) {
    error.report$harv_hwp[2] <- "First and last column names are Year and Total" } else {
      error.report$harv_hwp[2] <- "First and last column names ARE NOT Year and Total"
      terminate$harv_hwp[2] <- 1}}

if (terminate$harv_hwp[2] == 0) {
  N.YEARS <- nrow(harv.hwp)
  FIRST.YEAR <- harv.hwp$Year[1]
  LAST.YEAR <- harv.hwp$Year[N.YEARS]
  harv_true1 <- is.data.frame(harv.hwp)
}
# Checking that all values are numeric
harv.vals.numeric.check <- vals.numeric.check.fcn(harv_true1, harv.hwp, 2)
error.report$harv_hwp[3] <- harv.vals.numeric.check$msg ; terminate$harv_hwp[3] <- harv.vals.numeric.check$term

harv.col <- ncol(harv.hwp)  
if (harv_true1 & terminate$harv_hwp[2] == 0 & terminate$harv_hwp[3] == 0) {
  if (ncol(harv.hwp) > 2) {   # This check only applies to harvest files with ownership values.  It skips those that only have Year and Total
    harv.hwp.test2 <- harv.hwp[which(apply(as.matrix(harv.hwp[,2:(ncol(harv.hwp) - 1)]), 1, function(x) all(is.na(x)) == F)), ] # Complex call to find out which ownership rows have non-NA values
    if (all(apply(as.matrix(harv.hwp.test2[,2:(ncol(harv.hwp.test2) - 1)]), 1, sum) - harv.hwp.test2$Total < 0.001)) {
      error.report$harv_hwp[4] <- "Sum of ownership harvest values equals Total values"} else {
        error.report$harv_hwp[4] <- "Ownership value sums DO NOT all equal Total values"
        terminate$harv_hwp[4] <- 1} 
  }
}

# QA checks for bfcf.hwp
bfcf_hwp <- tC.read("BFCF"); bfcf.hwp <- bfcf_hwp$tble; terminate$bfcf_hwp[1] <- bfcf_hwp$term; error.report$bfcf_hwp[1] <- bfcf_hwp$msg
bfcf_true1 <- is.data.frame(bfcf.hwp)

bfcf_namecheck <- name.check.fcn(bfcf_true1, colnames(bfcf.hwp), c("Conversion", "StartYear", "EndYear"), "three", "columns")
error.report$bfcf_hwp[2] <- bfcf_namecheck$msg; terminate$bfcf_hwp[2] <- bfcf_namecheck$term

# Double-if statement.  As long as harv.hwp checks out, we want to see if the final BFCF year equals the final harv.hwp year
if (terminate$harv_hwp[1] != 1 & terminate$harv_hwp[2] != 1 & bfcf_true1 & terminate$bfcf_hwp[2] == 0) {
  if (bfcf.hwp[nrow(bfcf.hwp), 3] == LAST.YEAR) {
    error.report$bfcf_hwp[3] <- "BFCF final year equals harvest final year"} else {
      error.report$bfcf_hwp[3] <- "BFCF final year DOES NOT equal harvest final year"
      terminate$bfcf_hwp[3] <- 1}
}

# Checking that all values are numeric
bfcf.vals.numeric.check <- vals.numeric.check.fcn(bfcf_true1, bfcf.hwp, 1)
error.report$bfcf_hwp[4] <- bfcf.vals.numeric.check$msg ; terminate$bfcf_hwp[4] <- bfcf.vals.numeric.check$term

# QA checks for tpr.hwp
tpr_hwp <- tC.read("TimberProdRatios"); tpr.hwp <- tpr_hwp$tble; terminate$tpr_hwp[1] <- tpr_hwp$term; error.report$tpr_hwp[1] <- tpr_hwp$msg
tpr_true1 <- is.data.frame(tpr.hwp)

# checking that number of years is correct
tpr.yr.check <- year.n.check.fcn(tpr_true1, tpr.hwp, 1)
error.report$tpr_hwp[2] <- tpr.yr.check$msg; terminate$tpr_hwp[2] <- tpr.yr.check$term

# Ensuring column-name year values can be numeric
tpr.yr.name.check <- yr.name.check.fcn(tpr_true1, tpr.hwp, 2)
error.report$tpr_hwp[3] <- tpr.yr.name.check$msg ; terminate$tpr_hwp[3] <- tpr.yr.name.check$term

# Checking that first column name = "TimberProductID"
tpr_namecheck <- name.check.fcn(tpr_true1, colnames(tpr.hwp)[1], c("TimberProductID"), "", "column")
error.report$tpr_hwp[4] <- tpr_namecheck$msg; terminate$tpr_hwp[4] <- tpr_namecheck$term

# Checking that all values are numeric
tpr.vals.numeric.check <- vals.numeric.check.fcn(tpr_true1, tpr.hwp, 1)
error.report$tpr_hwp[5] <- tpr.vals.numeric.check$msg ; terminate$tpr_hwp[5] <- tpr.vals.numeric.check$term

# Verifying that TPR columns sum to 1.0
if (all(terminate$tpr_hwp[1:5] == 0)) {
  if (all(apply(tpr.hwp[,2:ncol(tpr.hwp)], 2, sum) == 1)) {
    error.report$tpr_hwp[6] <- "All year columns total 1.0"} else {
      terminate$tpr_hwp[6] <- 1
      error.report$tpr_hwp[6] <- "ERROR: NOT ALL year columns total 1.0"}
}

# QA checks for ppr.hwp
ppr_hwp <- tC.read("PrimaryProdRatios"); ppr.hwp <- ppr_hwp$tble; terminate$ppr_hwp[1] <- ppr_hwp$term ; error.report$ppr_hwp[1] <- ppr_hwp$msg
ppr_true1 <- is.data.frame(ppr.hwp)

# checking that number of years is correct
ppr.yr.check <- year.n.check.fcn(ppr_true1, ppr.hwp, 1)
error.report$ppr_hwp[2] <- ppr.yr.check$msg; terminate$ppr_hwp[2] <- ppr.yr.check$term

# Ensuring year values can be numeric
ppr.yr.name.check <- yr.name.check.fcn(ppr_true1, ppr.hwp, 2)
error.report$ppr_hwp[3] <- ppr.yr.name.check$msg ; terminate$ppr_hwp[3] <- ppr.yr.name.check$term

# Checking that first column name = "PrimaryProductID"
ppr_namecheck <- name.check.fcn(ppr_true1, colnames(ppr.hwp)[1], c("PrimaryProductID"), "", "column")
error.report$ppr_hwp[4] <- ppr_namecheck$msg; terminate$ppr_hwp[4] <- ppr_namecheck$term

# Checking that all values are numeric
ppr.vals.numeric.check <- vals.numeric.check.fcn(ppr_true1, ppr.hwp, 1)
error.report$ppr_hwp[5] <- ppr.vals.numeric.check$msg ; terminate$ppr_hwp[5] <- ppr.vals.numeric.check$term


# Verifying that PPR columns sum to the number of TPR columns
if (all(terminate$ppr_hwp[1:5] == 0) & tpr_true1) {
  if (all(apply(ppr.hwp[,2:ncol(ppr.hwp)], 2, sum) == nrow(tpr.hwp))) {
    error.report$ppr_hwp[6] <- paste0("All year columns total ", nrow(tpr.hwp), ", the number of TPR categories.")} else {
      terminate$ppr_hwp[6] <- 1
      error.report$ppr_hwp[6] <- paste("ERROR: NOT ALL year columns total", nrow(tpr.hwp))}
}



# QA checks for CCF to MTC conversion
ccf_conversion_hwp <- tC.read("CCF_MT_Conversion"); ccf_conversion.hwp <- ccf_conversion_hwp$tble; terminate$ccf_conversion_hwp[1] <- ccf_conversion_hwp$term; error.report$ccf_conversion_hwp[1] <- ccf_conversion_hwp$msg
ccf_true1 <- is.data.frame(ccf_conversion.hwp)

# Verifying that the columns are labeled correctly
ccf_conversion_namecheck <- name.check.fcn(ccf_true1, colnames(ccf_conversion.hwp), c("PrimaryProductID", "CCFtoMTconv"), "two", "columns")
error.report$ccf_conversion_hwp[2] <- ccf_conversion_namecheck$msg; terminate$ccf_conversion_hwp[2] <- ccf_conversion_namecheck$term

# Checking that all values are numeric
ccf_conversion.vals.numeric.check <- vals.numeric.check.fcn(ccf_true1, ccf_conversion.hwp, 1)
error.report$ccf_conversion_hwp[3] <- ccf_conversion.vals.numeric.check$msg ; terminate$ccf_conversion_hwp[3] <- ccf_conversion.vals.numeric.check$term

# Verifying that the correct number of primary product values have values
if (ccf_true1 & ppr_true1) {
  if (nrow(ppr.hwp) == nrow(ccf_conversion.hwp)) {
    error.report$ccf_conversion_hwp[4] <- "Number of Primary Product IDs equal to those in PrimaryProductRatios" } else {
      error.report$ccf_conversion_hwp[4] <- "Number of Primary Product IDs NOT EQUAL to those in PrimaryProductRatios"
      terminate$ccf_conversion_hwp[4] <- 1 }  }


# QA checks for eur.hwp
eur_hwp <- tC.read("EndUseRatios"); eur.hwp <- eur_hwp$tble; terminate$eur_hwp[1] <- eur_hwp$term; error.report$eur_hwp[1] <- eur_hwp$msg
eur_true1 <- is.data.frame(eur.hwp)

# checking that number of years is correct
eur.yr.check <- year.n.check.fcn(eur_true1, eur.hwp, 1)
error.report$eur_hwp[2] <- eur.yr.check$msg; terminate$eur_hwp[2] <- eur.yr.check$term

# Ensuring year values can be numeric
eur.yr.name.check <- yr.name.check.fcn(eur_true1, eur.hwp, 2)
error.report$eur_hwp[3] <- eur.yr.name.check$msg ; terminate$eur_hwp[3] <- eur.yr.name.check$term

# Checking that first column name = "EndUseID"
eur_namecheck <- name.check.fcn(eur_true1, colnames(eur.hwp)[1], "EndUseID", "", "column")
error.report$eur_hwp[4] <- eur_namecheck$msg; terminate$eur_hwp[4] <- eur_namecheck$term

# Checking that all values are numeric
eur.vals.numeric.check <- vals.numeric.check.fcn(eur_true1, eur.hwp, 1)
error.report$eur_hwp[5] <- eur.vals.numeric.check$msg ; terminate$eur_hwp[5] <- eur.vals.numeric.check$term

# Verifying that EUR columns sum to the number of PPR columns
if (all(terminate$eur_hwp[1:5] == 0) & eur_true1) {
  if (all(apply(eur.hwp[,2:ncol(eur.hwp)], 2, sum) == nrow(ppr.hwp))) {
    error.report$eur_hwp[6] <- paste0("All year columns total ", nrow(ppr.hwp), ", the number of PPR categories.")} else {
      terminate$eur_hwp[6] <- 1
      error.report$eur_hwp[6] <- paste("ERROR: NOT ALL year columns total", nrow(ppr.hwp))}
}



# QA checks for ratio_cat.hwp
ratio_cat_hwp <- tC.read("RatioCategories"); ratio_cat.hwp <- ratio_cat_hwp$tble; terminate$ratio_cat_hwp[1] <- ratio_cat_hwp$term; error.report$ratio_cat_hwp[1] <- ratio_cat_hwp$msg
ratio_cat_true1 <- is.data.frame(ratio_cat.hwp) 

# Checking that column names correct
ratio_cat_namecheck <- name.check.fcn(ratio_cat_true1, colnames(ratio_cat.hwp), c("TimberProductID", "PrimaryProductID", "EndUseID", "TimberProduct", "PrimaryProduct", "EndUseProduct"), "six", "columns")  
error.report$ratio_cat_hwp[2] <- ratio_cat_namecheck$msg; terminate$ratio_cat_hwp[2] <- ratio_cat_namecheck$term

# Ensuring that some EUR rows are labeled as "fuel"
if (ratio_cat_true1) {
  if (length(grep("fuel", ratio_cat.hwp$EndUseProduct)) > 0) {
    error.report$ratio_cat_hwp[3] <- "The word 'fuel' appears in column EndUseProduct"} else {
      error.report$ratio_cat_hwp[3] <- "The word 'fuel' DOES NOT APPEAR in column EndUseProduct"
      terminate$ratio_cat_hwp[3] <- 1}}

# Ensuring that some EUR rows are labeled as "pulp"  
if (ratio_cat_true1) {
  if (length(grep("pulp", ratio_cat.hwp$EndUseProduct)) > 0) {
    error.report$ratio_cat_hwp[4] <- "The word 'pulp' appears in column EndUseProduct"} else {
      error.report$ratio_cat_hwp[4] <- "The word 'pulp' DOES NOT APPEAR in column EndUseProduct"
      terminate$ratio_cat_hwp[4] <- 1} }

# Verifying that TPR IDs are the same number in this file as original TimberProdRatios file
if (ratio_cat_true1 & tpr_true1) {
  if (nrow(tpr.hwp) == length(unique(ratio_cat.hwp$TimberProductID))) {
    error.report$ratio_cat_hwp[5] <- "Number of Timber Product IDs equal to those in TimberProdRatios" } else {
      error.report$ratio_cat_hwp[5] <- "Number of Timber Product IDs NOT EQUAL to those in TimberProdRatios"
      terminate$ratio_cat_hwp[5] <- 1 } }

# Verifying that PP IDs are the same number in this file as original PrimaryProductRatios file
if (ratio_cat_true1 & ppr_true1) {
  if (nrow(ppr.hwp) == length(unique(ratio_cat.hwp$PrimaryProductID))) {
    error.report$ratio_cat_hwp[6] <- "Number of Primary Product IDs equal to those in PrimaryProductRatios" } else {
      error.report$ratio_cat_hwp[6] <- "Number of Primary Product IDs NOT EQUAL to those in PrimaryProductRatios"
      terminate$ratio_cat_hwp[6] <- 1 } }

# Verifying that EUR IDs are the same number in this file as original EndUseRatios file
if (ratio_cat_true1 & eur_true1) {
  if (nrow(eur.hwp) == length(unique(ratio_cat.hwp$EndUseID))) {
    error.report$ratio_cat_hwp[7] <- "Number of End Use IDs equal to those in EndUseRatios" } else {
      error.report$ratio_cat_hwp[7] <- "Number of End Use IDs NOT EQUAL to those in EndUseRatios"
      terminate$ratio_cat_hwp[7] <- 1 } }



# QA checks for eu_half.lives.hwp
eu_half_lives_hwp <- tC.read("EU_HalfLives"); eu_half.lives.hwp <- eu_half_lives_hwp$tble; terminate$eu_half_lives_hwp[1] <- eu_half_lives_hwp$term; 
error.report$eu_half_lives_hwp[1] <- eu_half_lives_hwp$msg
eu_half_lives_true1 <- all( all(terminate$eu_half_lives_hwp != 1) & is.data.frame(eur.hwp))

# Checking that column names are correct
eu_half_lives_namecheck <- name.check.fcn(eu_half_lives_true1, colnames(eu_half.lives.hwp), c("EndUseID", "EU_HalfLife"), "two", "columns")  
error.report$eu_half_lives_hwp[2] <- eu_half_lives_namecheck$msg; terminate$eu_half_lives_hwp[2] <- eu_half_lives_namecheck$term

# Verifying that number of EUR categories equals those in EndUseRatios
if (eu_half_lives_true1) {
  if (nrow(eur.hwp) == length(unique(eu_half.lives.hwp$EndUseID))) {
    error.report$eu_half_lives_hwp[3] <- "Number of End Use IDs equal to those in EndUseRatios" } else {
      error.report$eu_half_lives_hwp[3] <- "Number of End Use IDs NOT EQUAL to those in EndUseRatios"
      terminate$eu_half_lives_hwp[3] <- 1 }  }

# Checking that all values are numeric
eu_half_lives.vals.numeric.check <- vals.numeric.check.fcn(eu_half_lives_true1, eu_half.lives.hwp, 1)
error.report$eu_half_lives_hwp[4] <- eu_half_lives.vals.numeric.check$msg ; terminate$eu_half_lives_hwp[4] <- eu_half_lives.vals.numeric.check$term

# QA checks for discard.fates.hwp
discard_fates_hwp <- tC.read("DiscardFates"); discard.fates.hwp <- discard_fates_hwp$tble; terminate$discard_fates_hwp[1] <- 
  discard_fates_hwp$term; error.report$discard_fates_hwp[1] <- discard_fates_hwp$msg
discard_fates_true1 <- is.data.frame(discard.fates.hwp)

# Making sure first to column names are correct
discard_fates_namecheck <- name.check.fcn(discard_fates_true1, colnames(discard.fates.hwp[, 1:2]), c("DiscardType", "DiscardDestination"), "two", "columns")  
error.report$discard_fates_hwp[2] <- discard_fates_namecheck$msg; terminate$discard_fates_hwp[2] <- discard_fates_namecheck$term


# Are there the right number of year columns?
discard.fates.yr.n <- year.n.check.fcn(discard_fates_true1, discard.fates.hwp, 2) 
error.report$discard_fates_hwp[3] <- discard.fates.yr.n$msg
terminate$discard_fates_hwp[3] <- discard.fates.yr.n$term

# Are the year column names convertable to numerics?
discard.fates.yr.name <- yr.name.check.fcn(discard_fates_true1, discard.fates.hwp, 3)
error.report$discard_fates_hwp[4] <- discard.fates.yr.name$msg ; terminate$discard_fates_hwp[4] <- discard.fates.yr.name$term

# Checking that all values are numeric
discard.fates.vals.numeric.check <- vals.numeric.check.fcn(discard_fates_true1, discard.fates.hwp, 3)
error.report$discard_fates_hwp[5] <- discard.fates.vals.numeric.check$msg ; terminate$discard_fates_hwp[5] <- discard.fates.vals.numeric.check$term

# Do all column (year) values sum to 2?
if (all(discard_fates_true1, discard.fates.yr.n$term == 0, discard.fates.vals.numeric.check$term == 0)) {
  if (all(apply(discard.fates.hwp[, 3:(N.YEARS + 2)], 2, sum) == 2)) {
    error.report$discard_fates_hwp[5] <- "Year columns correctly sum to 2"} else {
      error.report$discard_fates_hwp[5] <- "Year columns DO NOT sum to 2. Check values."
      terminate$discard_fates_hwp[5] <- 1} }

# QA checks for discard.hl.hwp
discard_hl_hwp <- tC.read("Discard_HalfLives"); discard.hl.hwp <- discard_hl_hwp$tble; terminate$discard_hl_hwp[1] <- 
  discard_hl_hwp$term; error.report$discard_hl_hwp[1] <- discard_hl_hwp$msg
discard_hl_true1 <- is.data.frame(discard.hl.hwp)

# Checking that column names are correct
discard_hl_namecheck <- name.check.fcn(discard_hl_true1, colnames(discard.hl.hwp), c("Type",  "Dumps",  "Landfills_fixed",  "Landfills_decay",  "Recovered"), "five", "columns")  
error.report$discard_hl_hwp[2] <- discard_hl_namecheck$msg
terminate$discard_hl_hwp[2] <- discard_hl_namecheck$term

# Need first two rows to = 'paper', then 'wood'
discard_hl.rows_namecheck <- name.check.fcn(discard_hl_true1, discard.hl.hwp[1:2, 1], c("paper", "wood"), "two", "rows")  
error.report$discard_hl_hwp[3] <- discard_hl.rows_namecheck$msg
terminate$discard_hl_hwp[3] <- discard_hl.rows_namecheck$term

# Checking that all values are numeric
discard_hl.vals.numeric.check <- vals.numeric.check.fcn(discard_hl_true1, discard.hl.hwp, 2)
error.report$discard_hl_hwp[4] <- discard_hl.vals.numeric.check$msg ; terminate$discard_hl_hwp[4] <- discard_hl.vals.numeric.check$term


# QA checks for mc.adj.hwp
mc_adj_hwp <- tC.read("MonteCarloValues"); mc.adj.hwp <- mc_adj_hwp$tble; terminate$mc_adj_hwp[1] <- mc_adj_hwp$term; error.report$mc_adj_hwp[1] <- mc_adj_hwp$msg
mc_adj_true1 <- is.data.frame(mc.adj.hwp)

# Column name check
mc_adj_namecheck <- name.check.fcn(mc_adj_true1, colnames(mc.adj.hwp), c("Parameter_ID", "Parameter_Name", "Paper", "First_Year", "Last_Year", "MinCI", "Peak_Value", "MaxCI", "CI"), "nine", "columns")  
error.report$mc_adj_hwp[2] <- mc_adj_namecheck$msg; terminate$mc_adj_hwp[2] <- mc_adj_namecheck$term

# Row name check
mc_adj.row_namecheck <- name.check.fcn(mc_adj_true1, unique(mc.adj.hwp$Parameter_Name), c("CCFtoMTC", "EndUse_HalfLives", "EndUseRatios", "DiscardedDispositionRatios", 
                                                                                          "LandfillDecayLimits", "Landfill_HalfLives", "Dump_HalfLives", "Recovered_HalfLives", 
                                                                                          "Harvest", "TimberProdRatios", "PrimaryProdRatios"), "eleven", "row name types")  
error.report$mc_adj_hwp[3] <- mc_adj.row_namecheck$msg; terminate$mc_adj_hwp[3] <- mc_adj.row_namecheck$term

# Checking that all values are numeric
mc_adj.vals.numeric.check <- vals.numeric.check.fcn(mc_adj_true1, mc.adj.hwp, 6)
error.report$mc_adj_hwp[4] <- mc_adj.vals.numeric.check$msg ; terminate$mc_adj_hwp[4] <- mc_adj.vals.numeric.check$term

# Verify that distributions are symmetric
if (mc_adj_true1 & terminate$mc_adj_hwp[4] == 0) {
  if (all(round(mc.adj.hwp$Peak_Value - mc.adj.hwp$MinCI, 4) == round(mc.adj.hwp$MaxCI - mc.adj.hwp$Peak_Value, 4))) {
    error.report$mc_adj_hwp[5] <- "All confidence interval boundaries are symmetric around 1.0" } else {
      error.report$mc_adj_hwp[5] <- "NOT ALL confidence interval boundaries are symmetric around 1.0"
      terminate$mc_adj_hwp[5] <- 1 } }

# Within parameters that have First and Last Year values, the first and last years for each variable type must be in complete (no gaps) sequential order
if (mc_adj_true1 & terminate$mc_adj_hwp[4] == 0) {
  mc.adj.hwp2 <- mc.adj.hwp[14:nrow(mc.adj.hwp),]  # Truncated version of MC variable table
  
  param.num.fun <- function(x) {           # Function to find the parameter names with multiple values
    sapply(x, grep, mc.adj.hwp2$Parameter_Name)
  }
  # Running the function, creating a data frame of the results, and creating a null results variable
  yr.series.check <- param.num.fun(unique(mc.adj.hwp2$Parameter_Name))
  ysc.colnames <- colnames(yr.series.check)
  ysc.results <- first.yr.results <- last.yr.results <- rep(FALSE, length(ysc.colnames))

  
  for(i in 1:length(ysc.colnames)) {  # Running a for-loop to examine years listed for each of the parameters.
    fy <- mc.adj.hwp2$First_Year[mc.adj.hwp2$Parameter_Name == ysc.colnames[i]]  # First years
    ly <- mc.adj.hwp2$Last_Year[mc.adj.hwp2$Parameter_Name == ysc.colnames[i]]   # Last years
    ysc.results[i] <- all(suppressWarnings(unlist(mapply(seq, fy, ly)) == seq(fy[1], max(ly), 1))) # Do the sequences of all years provided equal a sequence of the very first with very last year?
    first.yr.results[i] <- min(fy) == FIRST.YEAR
    last.yr.results[i] <- LAST.YEAR %in% seq(ly[length(ly) - 1], ly[length(ly)], 1)
  } 
  
 
  if (all(ysc.results)) {
    error.report$mc_adj_hwp[6] <- "All year sequences are in order" } else {
      error.report$mc_adj_hwp[6] <- "NOT ALL year sequences are in order or without gaps or overlaps"
      terminate$mc_adj_hwp[6] <- 1 } 
  
  if (all(first.yr.results) & all(last.yr.results)) {
    error.report$mc_adj_hwp[7] <- "Time series begins on first year; last year within final time series set" } else {
      error.report$mc_adj_hwp[7] <- "MAKE SURE time series begins on first year and the last year is within the final time series set"
      terminate$mc_adj_hwp[7] <- 1 } 
  }
  
  



###### Consolidating results, testing to see if process should be halted (any "terminate" codes of 1),saving the Error_Report.csv file
file.xwalk <- tibble(QA.name = c("bfcf_hwp", "ccf_conversion_hwp", "discard_fates_hwp", "discard_hl_hwp", "eu_half_lives_hwp", 
                                 "eur_hwp", "harv_hwp", "mc_adj_hwp", "ppr_hwp", "ratio_cat_hwp", "tpr_hwp"),
                     "Worksheet Name" = c("BFCF", "CCF_MT_Conversion", "DiscardFates", "Discard_HalfLives", "EU_HalfLives", "EndUseRatios", 
                                          "Harvest_BF", "MonteCarloValues", "PrimaryProdRatios", "RatioCategories", "TimberProdRatios"))

#error.report[, 2:ncol(error.report)] <- as.character(error.report[, 2:ncol(error.report)])
error.report.save <- error.report %>% pivot_longer(ends_with("hwp") , names_to = "File Type", values_to = "Comments") %>%
  arrange(`File Type`) %>% 
  left_join(file.xwalk, by = c("File Type" = "QA.name"))

terminate.save <- terminate %>% pivot_longer(ends_with("hwp"), names_to = "File Type", values_to = "Termination Points") %>%
  arrange(`File Type`)

joint.err.term.rept <- left_join(terminate.save, error.report.save, by = c("item.id", "File Type")) %>%
  select(-`File Type`) %>%
  relocate(`Worksheet Name`, .after = item.id) %>%
  filter(Comments != "0")


QA_PASS <- if (sum(terminate[, 2:12]) > 0) FALSE else TRUE

#write_csv(joint.err.term.rept, "QA/Error_Report.csv")


# Initial halt function in case files are not loading or QA detects issues
#.Last <- function() {
#  graphics.off()
#  print("Terminating HWP script. HWP files incorrectly loaded or contained formatting errors.  See QA Error Report.")
#}

#if (QA_PASS == FALSE) quit(save = "ask", runLast = TRUE)  # If any terminate = 1, quit R



