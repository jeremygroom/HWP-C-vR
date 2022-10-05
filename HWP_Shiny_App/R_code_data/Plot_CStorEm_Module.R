## Plot module: Carbon Storage and Emissions

plot_CStorEm_UI <- function(id) {
  
  ns <- NS(id)  
  
  tagList(
    fluidRow(box(width = 7,
      #status = "success",   # makes the top of the box green.  
      column(12, offset = 1, h1(id = "main-header", "Carbon Storage and Emissions")))),
    box(width = 12,
        fluidRow(column(9, plotOutput(NS(id, "pltCSEm"), height = 600)), 
                 column(3, radioButtons(NS(id, "filltype"), label = h3("Display Options"),
                                        choices = list("Summary pool and emissions categories" = 1,
                                                       "Pool and emission category components" = 2,
                                                       "Pools and emissions by halflife category" = 3)),
                        fluidRow(column(12, #conditionalPanel(
                                        #condition = "input.filltype == '3'",
                                        #ns = ns,
                                        checkboxGroupInput(ns("select.hl"), label = h3("Select Pool/Emission Categories"),
                                                           choices = list("Solid Waste Disposal Site Categories" = 1,
                                                                          "Products in Use Categories" = 2,
                                                                          "Emissions Categories" = 3),
                                                           selected = c("1", "2", "3")))), #),
                        fluidRow(column(width = 12, radioButtons(NS(id, "metrictype"), label = h3("Select Metric"),
                                                                 choices = list("Tg C" = 1, "Tg CO2e" = 2), selected = 1))),
                        fluidRow(column(12,  checkboxInput(NS(id, "s.e.labs"),
                                                                      label = "Include Storage and Emissions Labels", 
                                                                      value = TRUE))),
                        fluidRow(column(width = 12,   
                                        actionButton(NS(id, "action"), label = "Change figure title"))),
                        fluidRow(column(width = 12, uiOutput(NS(id, "change_title_csem")))),
                        fluidRow(column(width = 12, dl_moduleUI(NS(id, "dl_figure_csem"))))))
    ),
    fluidRow(column(width = 12, offset = 0, style = 'padding-left:0px; padding-right:1px; padding-top:15px; padding-bottom:15px')), # Adding a blank row.
    fluidRow(
      column(8, offset = 2, 
             box(width = 12,
                 h3("Carbon storage and emissions"),
                 p("Using the buttons on the right-side of the screen, users can explore the cumulative harvested wood product 
                   carbon or carbon dioxide equivalent in products in use and at solid waste disposal site storage pools or emitted 
                   with or without energy capture. These storage pools and emissions categories can be displayed individually or 
                   together. ", style = "font-size:20px"),
                 p("Data can also be displayed for the components of each storage pool (e.g., products in use, landfill stock subject 
                   to decay, etc.) and emissions category (eg., fuelwood, landfill decay, etc.).", style = "font-size:20px"),
                 p("Lastly, the portion of the stock or emissions contributed by immediate oxidation, short, medium, or long-lasting 
                   uses can be displayed for each storage pool or emissions category. ", style = "font-size:20px"),
                 strong("Emissions are for the carbon content of the harvested wood only and do not include carbon emitted in the 
                        form of other carbon-containing greenhouse gasses, such as methane.", style = "font-size:20px")
             )     
      ))
    
  )
}


plot_CStorEm_Server <- function(id, hwp.dt) {
  
  moduleServer(id, function(input, output, session) {
    
    ns = session$ns
    
    
    counterTR <- reactiveValues(countervalue = 0)  # Setting a counter to register if plots are switched. If they are, the titles revert to original titles
    
    # Code for activating box that allows for a figure name change
    observeEvent(input$action, {         # Text widget will not appear if use ' NS(id, input$action) '. Instead is already there        
      if (counterTR$countervalue == 0) {
        counterTR$countervalue <- 1
        
        insertUI(                                    # NOT renderUI.  This is better for having added and then taken away again (see below).
          selector = paste0("#", NS(id, "action")),
          where = "afterEnd",
          textInput(NS(id, "change_title_csem"), label = h3("Enter figure title"), 
                    value = switch(as.numeric(input$filltype),
                                   "Cumulative total carbon in harvested wood product storage pools (positive values) and emission categories",
                                   "Cumulative total carbon in harvested wood product storage pools (positive values) and emission categories",
                                   paste0("Distribution of carbon from products with short (1-6 years), ",
                                          "medium (7-30 years) and long (31+ years) term Products in Use (PIU) half", 
                                          "lives and the carbon's fate in Solid Waste Disposal Sites (SWDS) and Emissions (E)")), 
                    width = "500px"))
      }
    })
    
    observeEvent(input$filltype, {  # If the user changes figures the counter resets and the original title reappears
      if (counterTR$countervalue == 1) {
        removeUI(
          selector = paste0("div:has(> #", NS(id, "change_title_csem"), ")")  # If just "#change_titleMC" it fails to remove the title.  If "div:has(>> #change_titleMC)"
        )                                                                  #   it removes the UI permanently, button and all.
      }
      counterTR$countervalue <- 0
    })
    
    
    
    ## -- Data Preparation -- ##
    plots.data <- reactive({
      
      
      Tg.scale <- as.numeric(input$metrictype)                   # For switching between Tg C and Tg CO2e
      
      hwp.data <- state.data[[which(state.choices == hwp.dt())]]     # Note: This is passed to module server from main UI, not module UI
      
      eec <- cumsum(apply(hwp.data$eec_array[, hwp.data$N.OWNERSHIP, ], 2, sum))/1e6
      ewoec <- cumsum(apply(hwp.data$ewoec_array[, hwp.data$N.OWNERSHIP, ], 2, sum))/1e6
      swdsC <- apply(hwp.data$swdsCtotal_array[, hwp.data$N.OWNERSHIP, ], 2, sum)/1e6
      pu.all <- apply(hwp.data$pu.final_array[, hwp.data$N.OWNERSHIP, ], 2, sum)/1e6    # Products in Use and Recovered
      p1G <- data.frame(years = hwp.data$yearsUse, eec = -eec, ewoec = -ewoec, pu.all, swdsC, row.names = NULL) # for Plotting 1G
      p1G.names <- names(p1G)[c(5, 4, 2, 3)]
      p1G.leg.names <- c("Solid Waste\n Disposal Site", "Products in Use\n", "Emissions without\n Energy Capture", "Emissions with\n Energy Capture")
      p1G.breaks <- names(p1G)[c(5, 4, 3, 2)]
      p1G.colors <- c( hwp.data$SWDS_COLOR , hwp.data$PIU_COLOR, "#424242", "#A6A6A6")
      if (Tg.scale == 2) p1G[, 2:5] <- p1G[, 2:5] * 44/12                            # Transforming to Tg CO2e value
      
      
      # Data for Totals
      p1G.1 <- p1G %>% pivot_longer(cols = -1, names_to = "cFates", values_to = "C")
      p1g.rem <- NULL                                               # Starting some code to track what categories need to be removed from the figure.
      if (!("1" %in% input$select.hl)) p1g.rem <- c(p1g.rem, 1)
      if (!("2" %in% input$select.hl)) p1g.rem <- c(p1g.rem, 2)
      if (!("3" %in% input$select.hl)) p1g.rem  <- c(p1g.rem, 3:4)
      
      if (is.null(p1g.rem) == F) {  # Reducing inputs if user selected to not show categories
        p1G.names <- p1G.names[-p1g.rem]
        p1G.breaks <- p1G.breaks[-p1g.rem]
        p1G.leg.names <- p1G.leg.names[-p1g.rem]
        p1G.colors <- p1G.colors[-p1g.rem]
        p1G.1 <- p1G.1 %>% filter(cFates %in% p1G.names)
      }
      p1G.1$cFates <- factor(p1G.1$cFates, levels = p1G.names)
      
#browser()      
      max.y.1 <- p.e.y.fcn(p1G.1, "C")  # Generating y-values from positive and negative values. See PlotFunctions1.r   

      ##### Data for Pool/Emissions Components
      # Pool
      pu <- apply(hwp.data$pu_array[, hwp.data$N.OWNERSHIP, ], 2, sum)/1e6                 # Products in Use alone (no Recovered)
      lf.fx <- apply(hwp.data$lf.fixed.cumsum_array[, hwp.data$N.OWNERSHIP, ], 2, sum)/1e6  # Landfill, inert
      lf <- apply(hwp.data$landfill_array[, hwp.data$N.OWNERSHIP, ], 2, sum)/1e6           # Landfill, decaying
      dmp <- apply(hwp.data$dumps_array[, hwp.data$N.OWNERSHIP, ], 2, sum)/1e6             # Dump, decaying
      recov <- apply(hwp.data$recov_array[, hwp.data$N.OWNERSHIP, ], 2, sum)/1e6           # Recovered, decaying
      # Emissions
      comp <- cumsum(apply(hwp.data$compost.input_array[, hwp.data$N.OWNERSHIP, ], 2, sum))/1e6 # Compost (becomes emissions immediately)
      dec <- cumsum(apply(hwp.data$dec.input_array[, hwp.data$N.OWNERSHIP, ], 2, sum))/1e6  # Discard energy capture (burned with energy capture)
      fuel <- cumsum(apply(hwp.data$fuel_array[, hwp.data$N.OWNERSHIP, ], 2, sum))/1e6          # Fuel, burned with energy capture
      bwoec <- cumsum(apply(hwp.data$bwoec.input_array[, hwp.data$N.OWNERSHIP, ], 2, sum))/1e6  # Discarded, burned without energy capture
      recov.decay <- cumsum(apply(hwp.data$recov.discard_array[, hwp.data$N.OWNERSHIP, ], 2, sum))/1e6 # Recovered decay
      dmp.decay <- cumsum(apply(hwp.data$dumps.discard_array[, hwp.data$N.OWNERSHIP, ], 2, sum))/1e6   # Dump decay
      lf.decay <- cumsum(apply(hwp.data$landfill.discard_array[, hwp.data$N.OWNERSHIP, ], 2, sum))/1e6 # Landfill decay
      # Data frame
      pool.emit <- data.frame(years = hwp.data$yearsUse, pu, lf.fx, lf, dmp, recov, comp = -comp, dec = -dec, fuel = -fuel, 
                              bwoec = -bwoec, recov.decay = -recov.decay, dmp.decay = -dmp.decay, lf.decay = -lf.decay) 
      if (Tg.scale == 2) pool.emit[, 2:ncol(pool.emit)] <- pool.emit[, 2:ncol(pool.emit)] * 44/12                            # Transforming to Tg CO2e value
      
      #pool.check <- apply(pool.emit[, 2:6], 1, sum)
      #emit.check <- apply(pool.emit[, 7:13], 1, sum)
      
      p.e.1 <- pool.emit %>% pivot_longer(cols = 2:13, names_to = "cFates", values_to = "C")
      p.e.1$cFates <- factor(p.e.1$cFates, levels = c("dmp", "lf", "lf.fx", "recov", "pu", "fuel", "dec", "dmp.decay", "lf.decay", "recov.decay", "comp", "bwoec"))
      if (sum(p.e.1$C[p.e.1$cFates == "dec"]) == 0) {    # If discarded carbon was not burned for energy then the dec category is removed.
        p.e.1 <- p.e.1 %>% filter(cFates != "dec")
        p.e.1$cFates <- p.e.1$cFates[drop = TRUE]
      }
      
      p.e.1_pos <- p.e.1 %>% filter(cFates %in% c("pu", "lf.fx", "lf", "dmp", "recov"))
      p.e.1_pos$cFates <- p.e.1_pos$cFates[drop = TRUE]  # Drops unused levels
      p.e.1_neg <- p.e.1 %>% filter(cFates %in% c("comp", "dec", "fuel", "bwoec", "recov.decay", "dmp.decay", "lf.decay"))
      p.e.1_neg$cFates <- p.e.1_neg$cFates[drop = TRUE]  # Drops unused levels
      
      n.levs <- length(c(levels(p.e.1_pos$cFates), levels(p.e.1_neg$cFates)))
      p.e.levels <- c(levels(p.e.1_pos$cFates), levels(p.e.1_neg$cFates))    # Needed below for finalizing data set sent to plotting.
      
      hex_codes.pos.p.e <- viridis_pal(option = "C")(n.levs * 4)  # Setting up Viridis colors to use in figure, flexible by number of groups.
      hex_codes.pos.p.e.use <- hex_codes.pos.p.e[seq(2, (n.levs * 4), 4)]  #  Trying to get colors that are somewhat different from one another by spacing them out a bit.
      
      legend.labels <- c("S:Dump", "S:Landfill, decaying", "S:Landfill, inert",  "S:Recovered", "S:Products in Use",  
                         "E:Burned without\nenergy capture", "E:Compost decay", "E:Recovery decay", "E:Landfill decay", "E:Dump decay", 
                         "E:Discard energy capture",  "E:Fuel wood")
      legend.breaks <- c("dmp", "lf", "lf.fx", "recov", "pu", "bwoec", "comp", "recov.decay", "lf.decay",  "dmp.decay", "dec",  "fuel" ) 
      if (n.levs == 11) {   # If there is no "burned with energy capture" then it is removed from the mix
        legend.labels <- legend.labels[-11]
        legend.breaks <- legend.breaks[-11]
      }
      
      p.e.rem <- NULL                                          # Starting some code to track what categories need to be removed from the figure.
      if (!("1" %in% input$select.hl)) p.e.rem <- c(p.e.rem, 1:3)
      if (!("2" %in% input$select.hl)) p.e.rem <- c(p.e.rem, 4:5)
      if (!("3" %in% input$select.hl)) p.e.rem  <- c(p.e.rem, 6:length(legend.breaks))  # We don't care what order the emissions are in, the same values will be removed either way.
      
      if (is.null(p.e.rem) == F) {  # Reducing inputs if user selected to not show categories
        p.e.levels <- p.e.levels[-p.e.rem]
        legend.breaks <- legend.breaks[-p.e.rem]
        legend.labels <- legend.labels[-p.e.rem]
        hex_codes.pos.p.e.use <- hex_codes.pos.p.e.use[-p.e.rem]
        p.e.1 <- p.e.1 %>% filter(cFates %in% legend.breaks)
      }
      
      p.e.1$cFates <- factor(p.e.1$cFates, levels = p.e.levels)

      max.y.2 <- p.e.y.fcn(p.e.1, "C") 

      ### Data for Pools and Emissions by Halflife Category
      sml <- hwp.data$eu_half.lives.hwp %>%
        mutate(ShortMedLong = ifelse(EU_HalfLife > 0 & EU_HalfLife <= 6, "Short", 
                                     ifelse(EU_HalfLife > 6 & EU_HalfLife <= 30, "Medium", 
                                            ifelse(EU_HalfLife > 30, "Long", "Fuel"))))
      
      st <- which(sml$ShortMedLong == "Short")
      md <- which(sml$ShortMedLong == "Medium")
      lng <- which(sml$ShortMedLong == "Long")
      # Products in Use
      pu.st <- apply(hwp.data$pu.final_array[st, hwp.data$N.OWNERSHIP, ], 2, sum)/1e6     # Products in Use with short halflife
      pu.md <- apply(hwp.data$pu.final_array[md, hwp.data$N.OWNERSHIP, ], 2, sum)/1e6     # Products in Use with medium halflife
      pu.lng <- apply(hwp.data$pu.final_array[lng, hwp.data$N.OWNERSHIP, ], 2, sum)/1e6     # Products in Use with long halflife
      
      # SWDS
      swds.st <- apply(hwp.data$swdsCtotal_array[st, hwp.data$N.OWNERSHIP, ], 2, sum)/1e6
      swds.md <- apply(hwp.data$swdsCtotal_array[md, hwp.data$N.OWNERSHIP, ], 2, sum)/1e6
      swds.lng <- apply(hwp.data$swdsCtotal_array[lng, hwp.data$N.OWNERSHIP, ], 2, sum)/1e6
      
      # Fuel + DEC (EEC), 1 category

      # EWOEC 
      ewoec.st <- cumsum(apply(hwp.data$ewoec_array[st, hwp.data$N.OWNERSHIP, ], 2, sum))/1e6
      ewoec.md <- cumsum(apply(hwp.data$ewoec_array[md, hwp.data$N.OWNERSHIP, ], 2, sum))/1e6
      ewoec.lng <- cumsum(apply(hwp.data$ewoec_array[lng, hwp.data$N.OWNERSHIP, ], 2, sum))/1e6
      
      hl.data <- tibble(years = hwp.data$yearsUse, swds.st, swds.md, swds.lng, pu.st, pu.md, pu.lng, ewoec.st = -ewoec.st, 
                        ewoec.md = -ewoec.md, ewoec.lng = -ewoec.lng, eec = -eec)
      
      dec.pres <- if (sum(dec) > 0) 1 else 2  # If Discard Energy Capture is present
      
      hl.data2 <- hl.data %>%
        pivot_longer(cols = 2:ncol(hl.data), names_to = "se.hl.type", values_to = "MMTC")
      hl.names <- names(hl.data)[c(2:7, 11:8)]
      hl.data2$se.hl.type <- factor(hl.data2$se.hl.type, levels = hl.names)
      hl.leg.names <- c("SWDS: Short Term", "SWDS: Medium Term", "SWDS: Long Term",  # halflife legend names
                        "PIU: Short Term", "PIU: Medium Term", "PIU: Long Term",
                        "E: Short Term", "E: Medium Term", "E: Long Term",
                        switch(dec.pres, "EEC: Fuel Wood + Discard Energy Capture", "EEC: Fuel Wood"))
      if (Tg.scale == 2) hl.data2$MMTC <- hl.data2$MMTC * 44/12                            # Transforming to Tg CO2e value
      
      hex_codes.hl <- viridis_pal(option = "H")(10 * 4)  # Setting up Viridis colors to use in figure, flexible by number of groups.
      hex_codes.hl.use <- hex_codes.hl[seq(2, (10 * 4), 4)]  #  Trying to get colors that are somewhat different from one another by spacing them out a bit.
      hex_codes.hl.use <- hex_codes.hl.use[c(1:6, 10:7)]
      #browser()
      rm.opts <- NULL
      if (!("1" %in% input$select.hl)) rm.opts <- c(rm.opts, 1:3)
      if (!("2" %in% input$select.hl)) rm.opts <- c(rm.opts, 4:6)
      if (!("3" %in% input$select.hl)) rm.opts <- c(rm.opts, 7:10)

      hl.vec <- c(1:6, 10:7)

      # The following if-function is ugly. I tried to match it to the above code but ggplot is very finicky with color schemes
      #   above and below y = 0.  
      if (is.null(rm.opts) == FALSE) {
        hl.names <- hl.names[-rm.opts]
        hl.leg.names <- hl.leg.names[-rm.opts]
        hex_codes.hl.use <- hex_codes.hl.use[-rm.opts]
        hl.data2 <- hl.data2 %>% filter(se.hl.type %in% hl.names)
        hl.data2$se.hl.type <- hl.data2$se.hl.type[drop = TRUE]
        if (all(7:10 %in% rm.opts) == FALSE) {
          hl.vec <- c(1:(10 - length(rm.opts)))
          hl.vec[grep("e", hl.names)] <- hl.vec[rev(grep("e", hl.names))]
        } else if (all(7:10 %in% rm.opts) == TRUE) {
          hl.vec <- hl.vec[-rm.opts]
        }
        hl.vec <- rank(hl.vec) # Order matters, position must be adjusted to the number of items in this vector.
      }

      max.y.3 <- p.e.y.fcn(hl.data2, "MMTC") 

      out.data <- list(hwp.data, p1G.1, p1G.leg.names, p1G.breaks, p1G.colors, max.y.1, p.e.1, max.y.2, legend.labels, legend.breaks, hex_codes.pos.p.e.use,
                       hl.data2, hl.leg.names, hl.names, hex_codes.hl.use, hl.vec, max.y.3)
      names(out.data) <- c("hwp.data", "p1G.1", "p1G.leg.names", "p1G.breaks", "p1G.colors",  "max.y.1", 
                           "p.e.1", "max.y.2", "legend.labels", "legend.breaks", "hex_codes.pos.p.e.use", 
                           "hl.data2", "hl.leg.names", "hl.names", "hex_codes.hl.use", "hl.vec", "max.y.3" )
      return(out.data)
      
    })
    ## -- End Data Preparation -- ##    
    
    
    plotForOutput <- reactive({
      
      hwp.dataX <- plots.data()$hwp.data
      
      y.lab.cc <- "Tg C"   # Setting the y-axis label 
      
      if (input$metrictype == "2") {  # Changes to the plot if metric is Tg CO2e
        y.lab.cc <- expression("Tg C"*O[2]*e)
      }  
      
      # Plotting
      if (input$filltype == "1") {
        y.lim.min <- if (plots.data()$max.y.1$min.y == -1) 0 else plots.data()$max.y.1$min.y 
        
        gg.x <- ggplot() +
          geom_area(data = plots.data()$p1G.1, aes(years, C, fill = cFates)) + 
          geom_hline(yintercept = 0) +
          scale_x_continuous(breaks = seq(hwp.dataX$MIN.PLOT.YR, hwp.dataX$MAX.PLOT.YR, by = 10), limits = c(hwp.dataX$MIN.PLOT.YR, hwp.dataX$MAX.PLOT.YR), minor_breaks = NULL) +
          scale_y_continuous(breaks = seq(plots.data()$max.y.1$min.y, plots.data()$max.y.1$max.y, by = plots.data()$max.y.1$breaks.y), 
                             limits = c(y.lim.min, plots.data()$max.y.1$max.y), minor_breaks = NULL, expand = c(0, 0)) +
          scale_fill_manual(labels = plots.data()$p1G.leg.names,
                            breaks = plots.data()$p1G.breaks,
                            values = plots.data()$p1G.colors,
                            drop = FALSE) +  
          guides(fill = guide_legend(title = element_blank(),
                                     #nrow = 3, byrow = T, 
                                     override.aes = list(size = 1), reverse = F)) +
          labs(y = y.lab.cc, x = NULL, 
               title = if (input$action == 0) wrapper("Cumulative total carbon in harvested wood product storage pools (positive values) and emission categories (negative values)", 70) else wrapper(input$change_title_csem, 70)) + 
          theme_bw()  + 
          theme(text = element_text(size = 20), 
                legend.position = "right", legend.text = element_text(size = 15), legend.spacing.y = unit(0, 'cm'))
        
      } else if (input$filltype == "2") {
          gg.x <- ggplot() +
          geom_area(data = plots.data()$p.e.1, aes(years, C, fill = cFates)) + #, levels = c("dmp", "lf", "lf.fx",  "recov", "pu")) + 
          #geom_area(data = p.e.1_neg, aes(years, C, fill = cFates))+#, levels = c("fuel", "dec", "bwoec", "dmp.decay", "lf.decay", "recov.decay", "comp")) + 
          geom_hline(yintercept = 0) +
          scale_x_continuous(breaks = seq(hwp.dataX$MIN.PLOT.YR, hwp.dataX$MAX.PLOT.YR, by = 10), limits = c(hwp.dataX$MIN.PLOT.YR, hwp.dataX$MAX.PLOT.YR), minor_breaks = NULL) +
          scale_y_continuous(breaks = seq(plots.data()$max.y.2$min.y, plots.data()$max.y.2$max.y, by = plots.data()$max.y.2$breaks.y), 
                             limits = c(plots.data()$max.y.2$min.y, plots.data()$max.y.2$max.y), minor_breaks = NULL, expand = c(0, 0)) +
          scale_fill_manual(labels = plots.data()$legend.labels,
                            breaks = plots.data()$legend.breaks, 
                            values = plots.data()$hex_codes.pos.p.e.use, drop = FALSE) +  
          guides(fill = guide_legend(title = "(S)torage and (E)mission\nCategories"#element_blank()#,
          ),#nrow = 3, byrow = T), 
          override.aes = list(size = 1), reverse = T) +
          labs(y = y.lab.cc, x = NULL, 
               title = if (input$action == 0) wrapper("Cumulative total carbon in individual harvested wood product storage pools (positive values) and emissions categories", 70) else wrapper(input$change_title_csem, 70)) + 
          theme_bw()  + 
          theme(text = element_text(size = 20), 
                legend.position = "right", legend.text = element_text(size = 15), legend.spacing.y = unit(0, 'cm'))
        
      } else if (input$filltype == "3") { 
     # if (length(plots.data()$hl.vec) == 3) browser()
                gg.x <- ggplot() + 
          geom_area(data = plots.data()$hl.data2, aes(years, MMTC, fill = se.hl.type)) +
          geom_hline(yintercept = 0) +
          scale_x_continuous(breaks = seq(hwp.dataX$MIN.PLOT.YR, hwp.dataX$MAX.PLOT.YR, by = 10), limits = c(hwp.dataX$MIN.PLOT.YR, hwp.dataX$MAX.PLOT.YR), minor_breaks = NULL) +
          scale_y_continuous(breaks = seq(plots.data()$max.y.3$min.y, plots.data()$max.y.3$max.y, by = plots.data()$max.y.3$breaks.y), 
                             limits = c(plots.data()$max.y.3$min.y, plots.data()$max.y.3$max.y), minor_breaks = NULL, expand = c(0, 0)) +
          scale_fill_manual(labels = plots.data()$hl.leg.names,
                            breaks = plots.data()$hl.names[plots.data()$hl.vec], 
                            values = plots.data()$hex_codes.hl.use[plots.data()$hl.vec],
                            drop = FALSE) +
          guides(fill = guide_legend(title = "Product Carbon Fate"), 
                 override.aes = list(size = 1), reverse = T) + 
          labs(title = wrapper("Distribution of carbon from products with short (1-6 years), medium (7-30 years) and long (31+ years)
                                     term Products in Use (PIU) half lives and the carbon's fate in Solid Waste Disposal Sites (SWDS) and 
                                     Emissions (E)", 70),
               x = NULL, y = y.lab.cc) +
          theme_bw()  + 
          theme(text = element_text(size = 20), 
                legend.position = "right", legend.text = element_text(size = 15), legend.spacing.y = unit(0, 'cm'))
        
      }
      #      browser()
      if (input$s.e.labs == TRUE) {  # Adds the "Storage" and "Emissions" text to the plot
        gg.x.yrange <- layer_scales(gg.x)$y$range$range   # Extracting the plot y range
        ann.y.min <- gg.x.yrange[1]
        ann.y.max <- gg.x.yrange[2]
        
        if (gg.x.yrange[1] == 0) ann.y.min <- -200
        if (gg.x.yrange[2] == 0) ann.y.max <- 200
        
        gg.x <- gg.x + 
          annotate("text", x = hwp.dataX$MIN.PLOT.YR + 10, y = 0.65 * ann.y.min, label = "Emissions", size = 8) +
          annotate("text", x = hwp.dataX$MIN.PLOT.YR + 10, y = 0.65 * ann.y.max, label = "Storage", size = 8)
      }
      
      gg.x2 <- gg.x + theme(legend.text = element_text(size = 8))
      
      list(gg.x, gg.x2)    
      
    })
    
    output$pltCSEm <- renderPlot(plotForOutput()[[1]]) 
    
    # The output for the Plot4, when saved as a png file, makes the legend image and font size too large.  The gg.x2 version corrects this, and is called here.
    plot.for.printing <- eventReactive(plotForOutput(), {
      plotForOutput()[[2]]})
    
    
    # Server module for saving the file.
    callModule(id = "dl_figure_csem", module = dl_module_server, plot_output = plot.for.printing, plot_name = "Plot_CStorEm.png", plot_height = 5, plot_width = 8)
    
    
  })
}
























