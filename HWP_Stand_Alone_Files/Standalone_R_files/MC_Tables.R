### Monte Carlo figures and CSV files

#Function for saving the array.  If MC.ARRAY.OUT is FALSE, this step will be skipped
mc.save.array.fcn <- function() {
  MCarray.fcn <- function(arrayX, array_name) {
    x1 <- data.frame(arrayX[, , 1])
    x2 <- data.frame(arrayX[, , 2])
    x3 <- data.frame(arrayX[, , 3])
    x4 <- data.frame(arrayX[, , 4])
    
    x <- list(x1, x2, x3, x4)
    names(x) <- c("eec", "ewoec", "swdsC",  "pu")
    
    write_xlsx(x, path = paste0(TABLELOC, array_name))
  }
  
  # Saving arrays clumped by summary arrays
  MCarray.fcn(MCout, "MC_All.xlsx")
}


## Evaluating convergence of last year's results with the given number of iterations and burn-in period
MCout.lastyr <- MCout[N.YEARS, , 3] + MCout[N.YEARS, , 4]


mean_MC <- ci05_MC <- ci95_MC <- data.frame(eec = rep(0, N.YEARS), ewoec = rep(0, N.YEARS), 
                                            swdsC = rep(0, N.YEARS), pu = rep(0, N.YEARS))
for (i in 1:4) {
  mean_MC[,i] <- apply(MCout[, , i], 1, mean)
  ci05_MC[, i] <- apply(MCout[, , i], 1, quantile, probs = 0.05)  # Obtaining the 90% CI empirically
  ci95_MC[, i] <- apply(MCout[, , i], 1, quantile, probs = 0.95)
}

mean_MC$Year <- years
mean_MC2 <- mean_MC %>% pivot_longer(cols = 1:4, names_to = "Type.M", values_to = "Means")
ci05_MC2 <- ci05_MC %>% pivot_longer(cols = 1:4, names_to = "Type.lci", values_to = "lci")
ci95_MC2 <- ci95_MC %>% pivot_longer(cols = 1:4, names_to = "Type.uci", values_to = "uci")


mc_plot <- cbind(mean_MC2, ci05_MC2[, 2], ci95_MC2[, 2]) %>%
  mutate(
    pct_lci = lci/Means,
    pct_uci = uci/Means)

write_csv(mc_plot, paste0(TABLELOC, "MC_ComponentsSummary.csv"))  # Used to produce "Cumulative carbon in individual storage and emission pools"




## MMC total
MC_MMTC <- MCout[, , 3] + MCout[, , 4]


mean_MMTC <- apply(MC_MMTC, 1, mean)
ci05_MMTC <- apply(MC_MMTC, 1, quantile, probs = 0.05)  # Obtaining the 90% CI empirically
ci95_MMTC <- apply(MC_MMTC, 1, quantile, probs = 0.95)

mc_PoolsTotalPlot <- tibble(Year = START.YEAR:END.YEAR, Mean = mean_MMTC/1e6, lci = ci05_MMTC/1e6, uci = ci95_MMTC/1e6)
write_csv(mc_PoolsTotalPlot, paste0(TABLELOC, "MC_PIU_Plus_SWDS.csv")) # Used to produce "Cumulative carbon in stored pools combined (PIU + SWDS)"



