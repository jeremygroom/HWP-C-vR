######################
##  Private, USFS, BLM  C in HWP & SWDS cumulatively.  Currently figure 7

# Populating two matrices with the sum across PIU categories by year (i.e., the total)


plot_CStorOwn_UI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(box(
     # status = "success",   # makes the top of the box green.  
      column(12, offset = 1, h1(id = "main-header", "Carbon Storage by Ownership")))),
    box(width = 12,
        fluidRow(column(9, plotOutput(ns("Plot_cso"), height = 600)),
                 column(3, 
                        fluidRow(radioButtons(ns("pools_cso"), label = h3("Carbon Pools to Display"), 
                                                        choices = list("Products in Use & SWDS" = 1, "Products in Use" = 2, "SWDS" = 3),
                                                        selected = 1)),
                        fluidRow(uiOutput(ns("ownerships"))),
                        fluidRow(radioButtons(NS(id, "metrictype"), label = h3("Select Metric"),
                                              choices = list("Tg C" = 1, "Tg CO2e" = 2), selected = 1)),
                        fluidRow(actionButton(NS(id, "action"), label = "Change figure title")),
                        fluidRow(uiOutput(NS(id, "change_title_cso"))),
                        fluidRow(dl_moduleUI(NS(id, "dl_figure_cso"))))
        )
    ),
    fluidRow(column(width = 12, offset = 0, style = 'padding-left:0px; padding-right:1px; padding-top:15px; padding-bottom:15px')), # Adding a blank row.
    fluidRow(
      column(8, offset = 2, 
             box(width = 12,
                 h3("Carbon storage by ownership"),
                 p("Using the buttons on the right-side of the screen, users can explore the cumulative harvested 
                   wood product carbon or carbon dioxide equivalent stored in products in use and at solid waste 
                   disposal site pools individually or together, by individual or multiple owners. ", style = "font-size:20px"),
             )     
      ))
  )
}


plot_CStorOwn_Server <- function(id, hwp.dt, file.loc) {
  
  moduleServer(id, function(input, output, session) {

    # a reactive UI that adjusts to the number of ownerships in a given data set
    output$ownerships <- renderUI({
      
      hwp.data <- state.data[[which(state.choices == hwp.dt())]]     # Note: This is passed to module server from main UI, not module UI
      ownership.names <- unique(hwp.data$harv_cf1$Source) 
      ownership.names <- ownership.names[ownership.names != "Total"]
      tagList(
        checkboxGroupInput(session$ns("ownerCheck"), label = h3("Select Ownerships"), 
                           choices = ownership.names, selected = c(ownership.names[1], ownership.names[2])) 
      )
    })
    
    
    #Plot title
    title.complete <- reactive({
      if (input$pools_cso == "1") { 
        title.pool <- "products in use and solid waste disposal sites"
      } else if (input$pools_cso == "2") {
        title.pool <- "products in use"} else {
          title.pool <- "solid waste disposal sites"}
      paste0("Cumulative carbon stored in ", title.pool, " by ownership")
    })
    
    
    # Code for activating box that allows for a figure name change
    observeEvent(input$action, {         # Text widget will not appear if use ' NS(id, input$action) '. Instead is already there        
      output$change_title_cso = renderUI(
        textInput(NS(id, "change_title_cso"), label = h3("Enter figure title"), 
                  value = title.complete()))#,
                  #width = "700px"))
    })
    
    # The selected data are used for plotting and for controlling the UI; thus, defined early.
    state.data.sel <- reactive(state.data[[which(state.choices == hwp.dt())]])   
    
    observe({  # If no ownership data present, the "plotselect" and download buttons are disabled. 
         #### NOTE: The ID used for disabling the download button required a combination of two IDs, one for the 
      ###       dl_moduleUI (dl_figure_cso) and one for the ID within the module (figure_download). 
      ##         See: https://stackoverflow.com/questions/64867941/rshiny-enable-disable-ui-modules
      toggleState("dl_figure_cso-figure_download", condition = ncol(state.data.sel()$harv.hwp) != 2)
      toggleState("action", condition = ncol(state.data.sel()$harv.hwp) != 2)
    })  
    
    # If there are no ownership data, this picture is presented.  
    altPlotForOutput <- reactive({
      img <- readPNG(paste0(file.loc, "ShrubPic3.png"))  # length 1.9 * height
      # plot with picture as layer
      ggplot_pic(img)
    })
    
    
    plotForOutput <- reactive({           # Provided as a reactive so that it may be used both for plotting on webpage and saving as file  

      hwp.data <- state.data.sel()      # Note: This is passed to module server from main UI, not module UI
      ownership.names <- unique(hwp.data$harv_cf1$Source)         # All ownership names in the data set selected
      ownership.names <- ownership.names[ownership.names != "Total"]  # ... except "Total"
      pools_num <- input$pools_cso                                  # Pools = all (1), PIU only (2), and SWDS only (3)
      req(input$ownerCheck)
      owner.list <- input$ownerCheck                             # Checked boxes for owners

      f4.piu <- f4.swds <- data.frame(matrix(0, length(hwp.data$yearsUse), (length(ownership.names) + 1)))  # Creating data frame of PIU and SWDS for each ownership
      f4.piu[,1] <- f4.swds[,1] <- hwp.data$yearsUse
      names(f4.piu) <- names(f4.swds) <- c("years", ownership.names)
      for (i in 1:length(ownership.names)) {
        f4.piu[,i + 1] <- apply(hwp.data$pu.final_array[, i, ], 2, sum)
        f4.swds[,i + 1] <- apply(hwp.data$swdsCtotal_array[, i, ], 2, sum)
      }
      

      f4.groups <- data.frame(hwp.data$years, f4.piu[,-1], f4.swds[,-1])   # The main dataset, raw.
      names(f4.groups) <- c("years", ownership.names, paste0(ownership.names, "_SWDS"))
      
      hex_codes1 <- viridis_pal(option = "C")(length(ownership.names) * 4)  # Setting up Viridis colors to use in figure, flexible by number of groups.
      hex_codes.use <- hex_codes1[seq(2, (length(ownership.names) * 4), 2)]  #  Trying to get colors that are somewhat different from one another by spacing them out a bit.
      
      ## Need to have adjustable plot and label order to groups.  Creating a data frame of ranks, names, colors, etc. to maintain consistency 
      #    as different groups are selected.
      half.f4 <- hwp.data$N.OWNERSHIP - 1                                                  
      f4.add <- f4.groups[,2:(half.f4 + 1)] + f4.groups[, (2 + half.f4):ncol(f4.groups)]  # Adding PIU, SWDS for each ownership to help rank & determine y axis
      f4.rank <- if (hwp.data$N.OWNERSHIP > 2) (1 + length(ownership.names)) - rank(f4.add[hwp.data$N.YEARS,]) else 1          # Reverse-ranking of the totals

      f4.rank.df <- data.frame(ranks = c(f4.rank, f4.rank - 0.5), 
                               cnames = names(f4.groups[2:length(f4.groups)]), 
                               labs = c(paste0(ownership.names[1:half.f4], " products in use"), 
                                 paste0(ownership.names[1:half.f4], " in SWDS")),
                             swds = c(rep(0, half.f4), rep(1, half.f4)),
                             group.name = rep(ownership.names, 2)) %>% 
        arrange(ranks) %>%
        mutate(hex_codes.for.use = hex_codes1[seq(2, (length(ownership.names) * 4), 2)])  # Colors are added after the sort (arrange) to keep colors for ownership together. Otherwise
      # the figures are color mayhem
      
      # Reducing the data set by the ownerships selected in the check boxes
      f4.groups2 <- f4.groups %>% select(years, f4.rank.df$cnames[f4.rank.df$group.name %in% owner.list])
      
     # browser()
      # Pivoting the now-reduced data set
      start.year <- hwp.data$OWNERSHIP_STARTYEAR
      f4.1 <- f4.groups2 %>% filter(hwp.data$years >= start.year) %>% 
        pivot_longer(cols = 2:ncol(f4.groups2), names_to = "OwnerGroup", values_to = "C") %>%
        mutate(swds = 0)#,
               #general.group = 0)
      f4.1$swds[grep("SWDS", f4.1$OwnerGroup)] <- 1

      
            #f4.1$general.group <- rep(1:half.f4, nrow(f4.1)/half.f4)
      max.y <- y.axis.fcn(apply(f4.groups2[,2:ncol(f4.groups2)], 1, sum), T)  # Here the code determines y-axis height.  I keep this constant across "pool number" for comparison's sake.

       # The values for SWDS & Products in Use
      f4.rank.df2 <- f4.rank.df %>% filter(group.name %in% owner.list)
      hex_codes.use <- f4.rank.df2$hex_codes.for.use
      ownership.order <- f4.rank.df2$cnames
      f4.labels <- f4.rank.df2$labs
      
      
  if (pools_num == 2) {  # Products in Use Only
    f4.1 <- f4.1 %>% filter(swds == 0)
    hex_codes.use <- f4.rank.df2$hex_codes.for.use[f4.rank.df2$swds == 0]
    f4.labels <- f4.rank.df2$labs[f4.rank.df2$swds == 0]
    ownership.order <- f4.rank.df2$cnames[f4.rank.df2$swds == 0]
  }
  if (pools_num == 3) {  # SWDS only
    f4.1 <- f4.1 %>% filter(swds == 1)
    hex_codes.use <- f4.rank.df2$hex_codes.for.use[f4.rank.df2$swds == 1]
    f4.labels <- f4.rank.df2$labs[f4.rank.df2$swds == 1]
    ownership.order <- f4.rank.df2$cnames[f4.rank.df2$swds == 1]
    }

  y.lab.cc <- "Tg C"   # Setting the y-axis label 
      
    if (input$metrictype == "2") {  # Changes to the plot if metric is Tg CO2e
      f4.1$C <- f4.1$C * 44/12
      y.lab.cc <- expression("Tg C"*O[2]*e)
      max.y <- y.axis.fcn(apply(f4.groups2[,2:ncol(f4.groups2)], 1, sum) * 44/12, T)
    }  
      
      # Plotting
      gg.x <- ggplot(data = f4.1, aes(years, C/1e6, fill = factor(OwnerGroup, levels = ownership.order))) + 
        geom_area() + 
        geom_hline(yintercept = 0) +
        scale_x_continuous(breaks = seq(hwp.data$MIN.PLOT.OWNR.YR, hwp.data$MAX.PLOT.YR, by = 10), limits = c(hwp.data$MIN.PLOT.OWNR.YR, hwp.data$MAX.PLOT.YR), minor_breaks = NULL) +
        scale_y_continuous(breaks = seq(0, max.y$max.y ,by = max.y$breaks.y), limits = c(0, max.y$max.y), expand = c(0, 0)) +
        scale_fill_manual(name = "", labels = f4.labels,
                          values = hex_codes.use) + 
        guides(fill = guide_legend(labels = f4.labels, 
                                   title = element_blank(), nrow = 3, byrow = T, override.aes = list(size = 1))) +
        labs(y = y.lab.cc, x = "Harvest Year", 
             title = if (input$action == 0) wrapper(title.complete(), 70) else wrapper(input$change_title_cso, 70)) + # See Functions for 'wrapper' 
        theme_bw()  + 
        theme(text = element_text(size = 20), 
              legend.position = "bottom", legend.text = element_text(size = 15), legend.spacing.y = unit(0, 'cm'))
     
      gg.x2 <- gg.x + theme(legend.text = element_text(size = 8))
      
      list(gg.x, gg.x2)  # gg.x is for output$Plot_cso and gg.x2 is for saving to file.
      
    })

    
    output$Plot_cso <- renderPlot(if (ncol(state.data.sel()$harv.hwp) != 2) plotForOutput()[[1]] else altPlotForOutput())  

      # The output for the Plot_cso, when saved as a png file, makes the legend image and font size too large.  The gg.x2 version corrects this, and is called here.
    plot.for.printing <- eventReactive(plotForOutput(), {
      plotForOutput()[[2]]})
      
      # Server module for saving the file.
    callModule(id = "dl_figure_cso", module = dl_module_server, plot_output = plot.for.printing, plot_name = "Plot_CStorOwn.png", plot_height = 5, plot_width = 8)

  })
}















