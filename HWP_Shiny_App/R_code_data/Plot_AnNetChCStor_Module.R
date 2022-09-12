## Plot Annual Net Change in Carbon Storage module


plot_AnNetChCStor_UI <- function(id) {
  tagList(
    fluidRow(box(width = 7,
      status = "success",   # makes the top of the box green.  
      column(12, offset = 1, h1(id = "main-header", "Annual Net Change in Carbon Storage"),
             tags$style(HTML("#main-header{color: blue;}"))))),
    box(width = 4,
        fluidRow(column(10, offset = 1,  selectInput(NS(id, "fig.select"), label = h3("Select IPCC change figure"), 
                                                    choices = list("IPCC production approach - stock change" = 1,
                                                                   "IPCC simple decay approach - net change" = 2), 
                                                    selected = 1)))),
    fluidRow(box(column(9, plotOutput(NS(id, "Plot_anccs.1"), height = 800)),
                 column(3, h2("Plotting Options"), 
                        fluidRow(checkboxInput(NS(id, "lines"), label = "Net", value = FALSE)),
                        fluidRow(radioButtons(NS(id, "metrictype"), label = h4("Select Metric"),
                                              choices = list("Tg C" = 1, "Tg CO2e" = 2), selected = 1)),
                        fluidRow(column(width = 12,  actionButton(NS(id, "action1"), label = "Change figure title"))),
                        fluidRow(column(width = 12,  uiOutput(NS(id, "change_title_anccs.1")))),
                        fluidRow(column(width = 12,  dl_moduleUI(NS(id, "dl_figure_anccs.1"))))), width = 12)),
    fluidRow(column(width = 12, offset = 0, style = 'padding-left:0px; padding-right:1px; padding-top:15px; padding-bottom:15px')), # Adding a blank row.
    fluidRow(
      column(8, offset = 2, 
             box(width = 12,
                 h3("Annual net change in carbon storage"),
                 p("This HWP C estimation tool was designed to implement the Intergovernmental Panel on Climate Change (IPCC) Production 
               Approach (", a("IPCC 2006b", href = "https://www.ipcc-nggip.iges.or.jp/public/2006gl/pdf/4_Volume4/V4_12_Ch12_HWP.pdf"), ").  
               The IPCC Production Approach for HWP C can be paired with the IPCC Stock-change Approach for 
               the forest ecosystem. Annual net changes in carbon stocks in the forest ecosystem pools and of the HWP C storage pools 
               are provided to get a net carbon sequestration value for the Forest Sector. In this way, carbon that is cut and removed 
               from the forest is considered an instant oxidation, unless it enters HWP storage pools. The HWP 
               contribution to the Forest sector is the sum of the annual change in the HWP C stock for products in use and the annual 
               change in HWP C stock at solid waste disposal sites, produced from domestic harvest. In other words, the HWP contribution 
               is the stock change in the storage pools from one year to the next, or the annual net change. Carbon from wood that is 
               cut and removed from the forest ecosystem pools that is associated with wood that is immediately burned for energy never 
               enters HWP storage pools. Though these emissions are not explicitly quantified under this approach, they are accounted 
               for by not being present in either the forest ecosystem or the HWP storage pools. ", style = "font-size:20px"),
                 p("Users can view the IPCC Production Approach net change figure using the dropdown menu in the top left portion of the screen. 
               The dark purple and magenta bars display the annual change in both the products in use and the solid waste disposal site 
               storage pools, respectively. Negative values indicate that the carbon in those pools is decreasing, while positive values 
               indicate that those pools are growing. For example, in 2010 the Products in Use pool is losing more carbon to solid waste 
               disposal sites than is being added to this pool through new harvest. The green \"net\" line displays the sum of the annual 
               change in both storage pools and is what is reported under the IPCC Production Approach (i.e, the annual net change). 
               Negative values indicate that overall carbon is being emitted from the HWP pools (as in 2010), while positive values indicate 
               that carbon is being accrued to the HWP pools and these pools are functioning as a net sink.", style = "font-size:20px"),
                 p("Under the IPCC Simple Decay Approach, the HWP pool is considered to be related to activities in the forest and hence does 
               not assume instant oxidation of wood from the forest ecosystem in the year of harvest with separate HWP pools acting as a 
               sink or source, as in the Production Approach. The HWP contribution is determined as the difference in the annual harvest 
               and the annual release of carbon to the atmosphere from HWPs where wood came from domestic harvest. In other words, the HWP 
               contribution is the difference in annual harvest and annual emissions, or the net annual balance. Timber harvest is represented 
               as a lateral transfer of carbon from the forest ecosystem to the product sector, where carbon will be stored and subsequently 
               released through burning (typically in the year of harvest) or decay (again, with carbon retention times depending on the type of 
               product produced). Both the IPCC Production 
               Approach and the Simple Decay Approach are limited to domestic harvest only, therefore the HWP C estimates from this tool 
               can also be displayed to demonstrate the Simple Decay Approach, provided the correct outputs are used.", style = "font-size:20px"),
                 p("Users can view the IPCC Simple Decay Approach net balance figure using the dropdown menu in the top left portion of the 
               screen. The teal bars display annual harvest, while the orange and yellow bars display the annual emissions with and without 
               energy capture. Because annual harvest is displayed as a positive value and annual emissions are displayed as negative values 
               for visualization purposes, the green \"net\" line is the sum of the annual harvest and annual emissions bars (rather than the 
               difference, as described above), and is what is reported under the IPCC Simple Decay Approach (i.e., the net balance).", style = "font-size:20px"),
                 strong("Whether the IPCC Production Approach or the Simple Decay Approach is used, the green \"net\" line is the same, as the HWP contribution to the Forest Sector under these two approaches is the same.", style = "font-size:20px"),
                 br(),
                 br(),
                 strong("Emissions are for the carbon content of the harvested wood only and do not include carbon emitted in the form of other carbon-containing greenhouse gasses, such as methane.", style = "font-size:20px"),
                 br(),
                 br(),
                 p("Though we can display the annual change in each of the storage pools along with the annual emissions with and without energy 
               capture, it is important for users to understand that these values are not additive to reach the net change or net balance. 
               The annual change in the solid waste disposal site pool includes the carbon that enters this pool from the products in use pool, 
               as well as the carbon that is emitted from this pool through decay or combustion. These emissions are the emissions without 
               energy capture category, so if we add the annual change in the storage pools to the annual emissions categories, we double 
               count emissions without energy capture. The details provided above for IPCC Production or Simple Decay Approaches correctly 
               describes how each of these datasets interact (along with annual harveset) to provide the HWP contribution to the Forest 
               Sector. ", style = "font-size:20px"),
                 p("The overall height of annual change in each of the storage pools and the annual emissions with and without energy capture is 
               equivalent to the harvested carbon from the previous year, demonstrating that mass balance is maintained in these estimates.", style = "font-size:20px"),
                 p("*Note: the model provides cumulative emissions with a one-year lag. In order to calculate annual emissions, the change in 
               the cumulative emissions from one year to the next must be calculated. Additionally, to match up the correct harvest year 
               with the correct emissions, the previous year's harvested carbon must be matched up with the subsequent year's annual 
               emissions.  For example, the 2019 harvest must be matched up with the difference in the 2020 and 2019 cumulative emissions. ", style = "font-size:20px"),
             )     
      ))
    
  )
}


plot_AnNetChCStor_Server <- function(id, hwp.dt) {
  
  moduleServer(id, function(input, output, session) {
    
    
    # Code for activating box that allows for a figure name change
    observeEvent(input$action1, {         # Text widget will not appear if use ' NS(id, input$action) '. Instead is already there        
      output$change_title_anccs.1 = renderUI(
        textInput(NS(id, "change_title_anccs.1"), label = NULL, #h5("Enter figure title"), 
                  value = "Annual net change in carbon for storage pools and emission categories",
                  width = "700px"))
    })
    
    plotForOutput1 <- reactive({    
      
      hwp.data <- state.data[[which(state.choices == hwp.dt())]]     # Note: This is passed to module server from main UI, not module UI
      
      
      
      #pool.list <- as.numeric(c(input$pools1, input$pools2))
      pu.all <- apply(hwp.data$pu.final_array[, hwp.data$N.OWNERSHIP, ], 2, sum)/1e6    # Products in Use and Recovered
      swdsC <- apply(hwp.data$swdsCtotal_array[, hwp.data$N.OWNERSHIP, ], 2, sum)/1e6
      swdsC_change <- swdsC[2:length(swdsC)] - swdsC[1:(length(swdsC) - 1)]
      pu_change <- pu.all[2:length(pu.all)] - pu.all[1:(length(pu.all) - 1)]
      ewoec_change <- apply(hwp.data$ewoec_array[, hwp.data$N.OWNERSHIP, ], 2, sum)/1e6
      eec_change <- apply(hwp.data$eec_array[, hwp.data$N.OWNERSHIP, ], 2, sum)/1e6
      
      t1 <- data.frame(Year = hwp.data$yearsUse[2:length(hwp.data$yearsUse)], SWDSchange = swdsC_change, 
                       PUchange = pu_change, EECchange = -eec_change[2:length(eec_change)], 
                       EWOECchange = -ewoec_change[2:length(ewoec_change)]) %>% 
        mutate(Net = SWDSchange + PUchange)
      t1$Harvest <- t1$PUchange + t1$SWDSchange - t1$EECchange - t1$EWOECchange
      
      t2 <- t1 %>% pivot_longer(2:7, names_to = "pool", values_to = "mmtc") 
      t2$pool <- factor(t2$pool, levels = c("Net", "EECchange", "EWOECchange", "SWDSchange", "PUchange", "Harvest"))
      
      y.lab.cc <- "Tg C"   # Setting the y-axis label 
      
      if (input$metrictype == "2") {  # Changes to the plot if metric is Tg CO2e
        t2$mmtc <- t2$mmtc * 44/12
        y.lab.cc <- expression("Tg C"*O[2]*e)
        t1$Net <- t1$Net * 44/12
      }  
      
      
      t2.labels <- c("Net", "Emitted with\nEnergy Capture", "Emitted without\nEnergy Capture", "Solid Waste\nDisposal Sites", "Products in Use", "Harvest")
      t2.labels <- factor(t2.labels, as.ordered(t2.labels))
      #    hex_codes1 <- viridis_pal(option = "C")(18)  # Setting up Viridis colors to use in figure, flexible by number of groups.
      #    hex_codes.use <- hex_codes1[seq(2, 17, 3)]  #  Trying to get colors that are somewhat different from one another by spacing them out a bit.
      
      #    browser()
      
      t2.sel <- data.frame(labs = t2.labels, pools = factor(levels(t2$pool), level = levels(t2$pool)))
      
      net.line.color <- "#3CB371"#008B00"
      
      if (input$fig.select == "1") {
        
        t3 <- t2 %>% dplyr::filter(pool == "SWDSchange" | pool == "PUchange") %>% 
          left_join(t1[, c(1, 6)], by = "Year") 
        
        t3.leg <- data.frame(labs.sel = c("Solid Waste\nDisposal Sites", "Products in Use"),
                             pool.sel  = c("SWDSchange", "PUchange"),
                             hex.sel  = c("#7801A8FF", "#B42E8DFF"))
        
        
        t.y.t3 <- t3 %>% group_by(Year) %>% summarize(mmtc.y.pos = sum(mmtc[mmtc > 0]), mmtc.y.neg = sum(mmtc[mmtc <= 0])) %>%
          pivot_longer(cols = 2:3, names_to = "type", values_to = "mmtc.y")
        if (input$lines == TRUE) { # Ensuring the y-axis includes the Net line if the Net line is selected.
          t2.y.net <- t2 %>% filter(pool == "Net") %>% rename(type = pool, mmtc.y = mmtc)
          t.y.t3 <- bind_rows(t.y.t3, t2.y.net)
        }
        t.y <- y.axis.fcn(t.y.t3$mmtc.y, F)
        
        
        p <- ggplot(data = t3, aes(Year, mmtc, fill = pool)) + 
          geom_col(position = "stack") +  
          geom_hline(yintercept = 0) +
          scale_x_continuous(breaks = seq(hwp.data$MIN.PLOT.YR, hwp.data$MAX.PLOT.YR, by = 10), minor_breaks = NULL) +
          scale_y_continuous(breaks = seq(t.y$min.y, t.y$max.y ,by = t.y$breaks.y), limits = c(t.y$min.y, t.y$max.y), expand = c(0, 0)) +
          scale_fill_manual(values = t3.leg$hex.sel, breaks = t3.leg$pool.sel, labels = t3.leg$labs.sel, name = element_blank()) +  
          labs(y = y.lab.cc, x = NULL, 
               title = if (input$action1 == 0) "IPCC production approach - stock change" else input$change_title_anccs.1) + 
          expand_limits(y = 0) +
          theme_bw(base_size = 20)  + 
          theme( #text=element_text(size=20), 
            #legend.key.size = grid::unit(3.5, "lines"),
            legend.position = "bottom", legend.spacing.y = unit(0, 'cm'))#legend.text = element_text(size=15), 
        
        
        if (input$lines == TRUE) {
          p <-  p + geom_line(aes(Year, Net, color = net.line.color), size = 2) +
            scale_color_manual(values = net.line.color, name = NULL, labels = "Net") } 
        
        
        
      } else {
        
        t3 <- t2 %>% dplyr::filter(pool == "Harvest" | pool == "EWOECchange" | pool == "EECchange") %>% 
          left_join(t1[, c(1, 6)], by = "Year") 
        
        t3.leg <- data.frame(labs.sel = c("Emitted without\nEnergy Capture", "Emitted with\nEnergy Capture", "Annual\nHarvest"),
                             pool.sel  = c("EWOECchange", "EECchange", "Harvest"),
                             hex.sel  = c("#F99A3EFF", "#F8DF25FF", "#00CED1"))
        
        
        t.y.t3 <- t3 %>% group_by(Year) %>% summarize(mmtc.y.pos = sum(mmtc[mmtc > 0]), mmtc.y.neg = sum(mmtc[mmtc <= 0])) %>%
          pivot_longer(cols = 2:3, names_to = "type", values_to = "mmtc.y")
        if (input$lines == TRUE) { # Ensuring the y-axis includes the Net line if the Net line is selected.
          t2.y.net <- t2 %>% filter(pool == "Net") %>% rename(type = pool, mmtc.y = mmtc)
          t.y.t3 <- bind_rows(t.y.t3, t2.y.net)
        }
        t.y <- y.axis.fcn(t.y.t3$mmtc.y, F)
        
        
        p <- ggplot(data = t3, aes(Year, mmtc, fill = pool)) + 
          geom_col(position = "stack") +  
          geom_hline(yintercept = 0) +
          scale_x_continuous(breaks = seq(hwp.data$MIN.PLOT.YR, hwp.data$MAX.PLOT.YR, by = 10), minor_breaks = NULL) +
          scale_y_continuous(breaks = seq(t.y$min.y, t.y$max.y ,by = t.y$breaks.y), limits = c(t.y$min.y, t.y$max.y), expand = c(0, 0)) +
          scale_fill_manual(values = t3.leg$hex.sel, breaks = t3.leg$pool.sel, labels = t3.leg$labs.sel, name = element_blank()) +  
          labs(y = y.lab.cc, x = NULL, 
               title = if (input$action1 == 0) "IPCC simple decay approach - net change" else input$change_title_anccs.1) + 
          expand_limits(y = 0) +
          theme_bw(base_size = 20)  + 
          theme( #text=element_text(size=20), 
            #legend.key.size = grid::unit(3.5, "lines"),
            legend.position = "bottom", legend.spacing.y = unit(0, 'cm'))#legend.text = element_text(size=15), 
        
        
        if (input$lines == TRUE) {
          p <-  p + geom_line(aes(Year, Net, color = net.line.color), size = 2) +
            scale_color_manual(values = net.line.color, name = NULL, labels = "Net") } 
        
        
        
        
      }
      
      p 
      
      
      
    }) 
    
    output$Plot_anccs.1 <- renderPlot(plotForOutput1())  
    
    # Server module for saving the file.
    callModule(id = "dl_figure_anccs.1", module = dl_module_server, plot_output = plotForOutput1, plot_name = "Plot_AnNetChCStor.png", plot_height = 5, plot_width = 8)
    
    
    
  })
}


