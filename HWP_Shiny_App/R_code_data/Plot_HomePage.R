### App home page ###

plot_HomePage_UI <- function(id) {
  ns <- NS(id)
  tagList(
    
    useShinyjs(),
    fluidRow(box(width = 12,
      #status = "success",   # makes the top of the box green.  
      column(10, offset = 1, align = "center", h1(id = "hp-title", "Harvested Wood Products Carbon Model, v.R"),
             tags$style(HTML("#hp-title{color: #154360;
                             font-size: 64px;
                                 font-style: italic;}"))))),
    fluidRow(column(4, offset = 1, align = "center", 
                    imageOutput(ns("chair"))),
  #  fluidRow(column(width = 12, offset = 0, style = 'padding-left:0px; padding-right:1px; padding-top:15px; padding-bottom:15px')), # Adding a blank row.
    column(width = 5, offset = 1,
                    box(width = 12, h3("Summary"),
                        p("This web-based tool, HWP C vR, calculates cumulative carbon stocks and emissions through time for 
                          harvested wood products (HWP) using the Tier 3 Production Approach carbon estimation guidelines 
                          developed by the", a("Intergovernmental Panel on Climate Change", href = "https://www.ipcc.ch/"), ". This tool was created through a partnership 
                          between the California Department of Forestry and Fire Protection (CAL FIRE), the Oregon Department of 
                          Forestry (ODF), Oregon State University's Institute for Natural Resources, and Groom Analytics, LLC.  This tool expands upon one originally created by the U.S. Forest Service (USFS) 
                          for National Forest System regional HWP carbon inventories (e.g., Stockmann et al. 2014) and later modified by the USFS for use in the California 
                          Forest Ecosystem and Harvested Wood Product Carbon Inventory (Loeffler et al. 2018).", style = "font-size:20px"),
                        p("Users can explore California and Oregon data, as well as run their own data. Instructions are provided ", 
                          a("here.", href = "https://jeremygroom.github.io/HWP-vR-Documentation/"), style = "font-size:20px"),
                        br(),
                        p("Stockmann, Keith; Anderson, Nathaniel; Young, Jesse; Skog, Ken; Healey, Sean; Loeffler, Dan; Butler, Edward; Jones, J. Greg; Morrison, 
                          James. 2014. Estimates of carbon stored in harvested wood products from United States Forest Service Intermountain Region, 1911-2012. 
                          Unpublished report. Missoula, MT: U.S. Department of Agriculture, Forest Service, Rocky Mountain Research Station, Forestry Sciences 
                          Laboratory. 28 p.", style = "font-size:12px"),
                        p("Loeffler, D., Anderson, N., Stockmann, K., Morgan, T.A., Tase, N.A. 2019. Harvested wood product carbon chapters 5 & 6 in Christensen, 
                          G.A.; Gray, A.N.; Kuegler, O.; Tase, N.A.; Rosenberg. M; Loeffler, D.; Anderson, N.; Stockmann, K.; Morgan, T.A. AB 1504 California 
                          Forest Ecosystem and Harvested Wood Product Carbon Inventory: 2017 Reporting Period. Final Report. U.S. Forest Service agreement no. 
                          18-CO-11052021-214, 17-CO-11261979-086, California Department of Forestry and Fire Protection agreement no. 8CA04056 and 8CA03714 and 
                          the University of Montana. Sacramento, CA: California Department of Forestry and Fire Protection and California Board of Forestry and Fire Protection. 539 p. ",
                          a("PDF Link", href = "https://bof.fire.ca.gov/media/8026/4-final_1504_forest_ecosys_hwp_c_2017_13feb19_full.pdf"), style = "font-size:12px"),
                        br(),
                        p("Sketch by Farren Groom, 2022", style = "font-size:20px")),
  fluidRow(column(12, align = "center", style = "padding-top:50px",
                  imageOutput(ns("logo_strip") )))))
  )
}

plot_HomePage_Server <- function(id, file.loc) {
  moduleServer(id, function(input, output, session) {
#   output$chair <- renderImage({
#     list(src = "chair.png",
#          width = "1949px",
#          height = "3424px")
     
     
     output$chair <- renderImage({
       return(list(
         src = paste0(file.loc, "chair.jpg"),
         width = "569px",
         height = "913px",
         contentType = "image/jpg",
         alt = "Chair picture"
       ))
#       img <- readJPEG(paste0(file.loc, "chair.jpg"))  # length 1.9 * height
       # plot with picture as layer
 #      ggplot_pic(img)
     },
deleteFile = FALSE)
     
     output$logo_strip <- renderImage({
       return(list(
         src = paste0(file.loc, "logo_strip.png"),
         width = "627px",
         height = "200px",
         contentType = "image/png",
         alt = "Logo strip"
       ))
       #logos <- readPNG(paste0(file.loc, "logo_strip.png")) 
       #ggplot_pic(logos)
     },
     deleteFile = FALSE)
  })
}