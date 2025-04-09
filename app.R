#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
#


#renv::restore()   # Begin here to download appropriate package versions.  This line can be removed or commented out after being run the first time.

library(packrat)
library(rsconnect)
library(tidyverse)
library(shiny)
library(shinyjs)
library(shinydashboard)
library(viridis)
library(networkD3)   # Sankey diagram
library(htmlwidgets)
library(htmltools)
library(openxlsx)
library(writexl)
library(data.table)
library(abind)      # Combine matrices into an array
library(triangle)   # for generating random variables from triangular distributions for the Monte Carlo
library(lhs)        # Latin Hypercube Sampling
library(png)
library(jpeg)
library(shinybusy)   # Busy signal for Monte Carlo simulation run (ModelRunPage.R)
library(vroom)

#renv::snapshot()     # If any of the libraries are updated and the project works, run this code to preserve the new library changes.


# Load data and general functions
file.loc <- "HWP_Shiny_App/R_code_data/"
source(paste0(file.loc, "global.r"))
HWP.VERSION <- "HWP Version 1.1.1"

#

HWPapp <- function() {
#  file.loc <- "HWP_Shiny_App/R_code_data/"
  source(paste0(file.loc, "Plot_HomePage.R"))
    # Plotting function for determining Y-axis extent and breaks
  source(paste0(file.loc, "Plot_AnnTimHarv_Module.R"))
  source(paste0(file.loc, "Plot_CStorOwn_Module.R"))
  source(paste0(file.loc, "Plot_CStorEm_Module.R"))
  source(paste0(file.loc, "Plot_AnNetChCStor_Module.R"))
  source(paste0(file.loc, "Plot_FateHarvC_Module.R"))
  source(paste0(file.loc, "Plot_HarvFuncLS_Module.R"))
  source(paste0(file.loc, "Plot_MCest_Module.R"))
  source(paste0(file.loc, "ModelRunPage.R"))
  source(paste0(file.loc, "ContactUsPage.R"))
  source(paste0(file.loc, "dl_module.R"))

  
#addResourcePath(prefix = 'www', directoryPath = 'HWP_Shiny_App/')
  
    # Define UI for application that draws a histogram
  
  ui <- dashboardPage(
    dashboardHeader(title = "HWP Data Visualization", # Will only work when deployed to shinyapps.io 
                    tags$li(a(href = 'http://www.groomanalytics.com',   
                              img(src = 'GroomAnalyticsH.jpg',
                                  title = "Groom Analytics Home", height = "40px"),
                              style = "margin-right:10px; padding-top:5px; padding-bottom:5px;"),
                            class = "dropdown"
                    ), 
                    titleWidth = "250px"
                    ),
    
    dashboardSidebar(
      sidebarMenu(
        menuItem(h5(HWP.VERSION)),
        selectInput("Dataset", selected = "California",
                    label = "Select a data set", choices = state.choices),
        menuItem(h5("Home"), tabName = "home"),
        menuItem(h5("Timber Harvest Summaries"),
                 menuSubItem("Annual Timber Harvest", tabName = "plot_ath", icon = icon("angles-right") ),
                 menuSubItem("Fate of Harvested Carbon", tabName = "fhc", icon = icon("angles-right")),
                 menuSubItem("Harvest by Functional Lifespan", tabName = "hf_ls", icon = icon("angles-right"))),
        menuItem(h5("Carbon Storage and Emissions"),
                 menuSubItem("Carbon Storage by Ownership", tabName = "cso", icon = icon("angles-right")),
                 menuSubItem("Carbon Storage and Emissions", tabName = "csem", icon = icon("angles-right")),
                 #        menuItem("Figure 7", icon = icon("tree-conifer", lib = "glyphicon"), tabName = "fig7"),
                 menuSubItem("Annual Net Change in Carbon Storage", tabName = "anccs", icon = icon("angles-right")),
                 menuSubItem("Monte Carlo Estimates", tabName = "mc_est", icon = icon("angles-right"))),
        menuItem(h5("Documentation and Data Upload"),
                 menuSubItem("Documentation", href = "https://jeremygroom.github.io/HWP-vR-Documentation/", icon = icon("angles-right")),
                 menuSubItem("Files and Code", icon = icon("github"), href = 'https://github.com/jeremygroom/HWP-C-vR'),
                 menuSubItem("Data Templates", icon = icon("github"), href = 'https://github.com/jeremygroom/HWP-C-vR/raw/main/HWP%20Data/HWP%20Data.zip'),
                 menuSubItem("Upload Data", icon = icon("cloud-upload", lib = "glyphicon"), tabName = "input1")),
        menuItem(h5("Contact Us"), tabName = "contact_us"),
        style = "white-space: normal;" # Allows for text wrapping in sidebar.
      )),
    dashboardBody(
      tags$head(         # CSS to make the header have a consistent color
        tags$style(HTML(' 
                /* Upper left corner (logo) */
        .skin-blue .main-header .logo {
          background-color: #154360 ;
        }
                /* Upper left corner (logo) when hovered */
        .skin-blue .main-header .logo:hover {
          background-color: #154360;
        }
                /* navbar (rest of the header) */
        .skin-blue .main-header .navbar {
                              background-color: #154360;
        }
                              
        
        .skin-blue .sidebar-menu > li.active > a {
        color: #F1C40F;
        }
        .skin-blue main-sidebar .sidebar a {
            color: #F1C40F;
        }
        
        .skin-blue .main-sidebar .sidebar .sidebar-menu .treeview-menu li.active a {
        color: #F1C40F !important;
        }
        .skin-blue .main-sidebar .sidebar .sidebar-menu .treeview-menu a {
        color: #F9E79F !important;
        }

      '))),
      tags$html(lang = "en"),   # Set the page language for accessibility.
      tags$head(tags$style(HTML("a {color: #4A235A; font-weight: bold;}"))), # Set hyperlink font color and make bold to make accessible.
      tags$style(HTML(".js-irs-0 .irs-single, .js-irs-0 .irs-bar-edge, .js-irs-0 .irs-bar {background: purple}")), # Making first (only) slider background purple.
      tags$style(HTML(".irs-from {background-color: black !important; font-weight: bold;}")), 
      tags$style(HTML(".irs-to {background-color: black !important; font-weight: bold;}")), 
      tags$style(HTML("#main-header{color: #154360;}")),
      tabItems(
        
        tabItem(tabName = "home",
                fluidRow(
                  plot_HomePage_UI("homeP")
                )),
        tabItem(tabName = "plot_ath",
                fluidRow(
                  plot_AnnTimHarv_UI("AnnTimHarv")
                )),
        
        tabItem(tabName = "cso",
                fluidRow(
                  plot_CStorOwn_UI("CStorOwn")
                )),
        tabItem(tabName = "csem",
                fluidRow(
                  plot_CStorEm_UI("CStorEm")
                )),
        tabItem(tabName = "anccs",
                fluidRow(
                  plot_AnNetChCStor_UI("AnNetChCStor")
                )),
        tabItem(tabName = "fhc",
                fluidRow(
                  plot_FateHarvC_UI("FateHarvC")
                )),
        tabItem(tabName = "hf_ls",
                fluidRow(
                  plot_HarvFuncLS_UI("HarvFuncLS")
                )),
        tabItem(tabName = "mc_est",
                fluidRow(
                  plot_MCest_UI("MCest")
                )),
        tabItem(tabName = "input1",
                fluidRow(
                  input1UI("Input_Page")
                )),
        tabItem(tabName = "contact_us",
                fluidRow(
                  plot_ContactUs_UI("ContactUs")
                ))
      )
    )
  )
  
  # Shiny server function:
  server <- function(input, output, session) {
    mc_trigger <- reactiveVal(0)  # Allows Plot_MCest_Module to know that the data have been updated.
    
    output$homeP <- plot_HomePage_Server("homeP", file.loc)
    output$AnnTimHarv <- plot_AnnTimHarv_Server("AnnTimHarv", reactive(input$Dataset))
    output$CStorOwn <- plot_CStorOwn_Server("CStorOwn", reactive(input$Dataset), file.loc)
    output$CStorEm <- plot_CStorEm_Server("CStorEm", reactive(input$Dataset))
    output$AnNetChCStor <- plot_AnNetChCStor_Server("AnNetChCStor", reactive(input$Dataset))
    output$FateHarvC <- plot_FateHarvC_Server("FateHarvC", reactive(input$Dataset), file.loc)
    output$HarvFuncLS <- plot_HarvFuncLS_Server("HarvFuncLS", reactive(input$Dataset))
    output$MCest <- plot_MCest_Server("MCest", reactive(input$Dataset), file.loc, mc_trigger)
    output$ContactUs <- plot_ContactUs_Server("ContactUs", file.loc)
    new.hwp.model <- input1Server("Input_Page", reactive(input$Dataset), file.loc, mc_trigger) # Must pass global file.loc variable to module for it to be seen in shinyapps.io server
    output$Input_Page <- new.hwp.model   
    
    
  ## When new HWP data are uploaded ("ModelRunPage.R") this code takes the "return_new_name" value from that module and adds it to the Dataset list of options  
   observeEvent(new.hwp.model(), {
      hwp.name <- new.hwp.model()
      #print(paste0("main> New HWP name is: ", hwp.name))  # QA check that this portion succeeded to this point
      state.choices <<- c(state.choices[1:2], hwp.name)
      updateSelectInput(session, "Dataset", choices = state.choices, selected = hwp.name)
    })
    
    
   
   
  }
  # Run the application 
  shinyApp(ui = ui, server = server)
}

HWPapp()



