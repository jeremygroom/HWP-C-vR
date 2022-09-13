hwp.model.options <- hwp.data$HWP_MODEL_OPTIONS
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

N.OWNERSHIP <- ncol(harv.hwp) - 1  # Number of ownership classes
N.YEARS <- nrow(harv.hwp)   # Number of years in the data set
N.TPR <- nrow(tpr.hwp)
N.PPR <- nrow(ppr.hwp)
N.EUR <- nrow(eur.hwp)            # Number of End Use Ratios
OWNERSHIP_STARTYEAR <- {if (ncol(harv.hwp) > 2)   # Finding the minimum start year for ownership details. If the data do not include ownership, then only the Totals column start year is used.
  min(harv.hwp$Year[apply(as.matrix(harv.hwp[,2:(ncol(harv.hwp) - 1)]), 2, function(x) min(which(x > 0)))]) else
    min(harv.hwp$Year) }
STARTYR.POSITION <- which(harv.hwp$Year == OWNERSHIP_STARTYEAR)
START.YEAR <- min(harv.hwp$Year)
END.YEAR <- max(harv.hwp$Year)
SWDS_COLOR <- "#B52F8CFF"  # These colors control SWDS & PIU colors in Plots 2, 3, and 4
PIU_COLOR <-  "#6E00A8FF"  
MIN.PLOT.YR <- floor(min(harv.hwp$Year / 10)) * 10   # For plotting.  Determining, by 10s, where to set the maximum x-axis year range
MAX.PLOT.YR <- ceiling(max(harv.hwp$Year / 10)) * 10     # For plotting.  Determining, by 10s, where to set the minimum x-axis year range
MIN.PLOT.OWNR.YR <- floor(min(OWNERSHIP_STARTYEAR/10)) * 10     # For plotting.  Determining, by 10s, where to set the minimum x-axis year range for data with ownership values

SHIFTYEAR <- hwp.model.options$SHIFTYEAR    # Should output on tables/figures for everything but TPO be shifted one year later (e.g., timber harvested in year X, distributed to different pools by Jan 1 X+1)
PIU.WOOD.LOSS <- hwp.model.options$PIU.WOOD.LOSS    # This is the Placed in Use Loss for all End Use products except for wood pulp and fuel wood. 
PIU.PAPER.LOSS <- hwp.model.options$PIU.PAPER.LOSS    # This is the Placed in Use Loss for all End Use products except for wood pulp and fuel wood. 

# Scraping some values from the CSV files
ownership.names <- names(harv.hwp)[2:ncol(harv.hwp)]  
years <- harv.hwp$Year

# Monte Carlo related values
N.ITER <- hwp.model.options$N.ITER[1]    # Number of MC iterations.  Select fewer (e.g., 10) for trouble-shooting. (default = 2000)
#BURN.IN <- hwp.model.options$BURN.IN[1]
R <- hwp.model.options$R[1]              #  Target (Pearson's) correlation coefficient  (default = 0.5)
OPT.START.VAL <- hwp.model.options$OPT.START.VAL[1]  # Optimization starting value for determining triangular distribution endpoints.  (default = 0.5)
MC.ARRAY.OUT <- hwp.model.options$MC.ARRAY.OUT[1]   # True/False: Should the MC run arrays be saved? (default = TRUE)
