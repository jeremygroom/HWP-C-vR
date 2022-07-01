# Download module
dl_moduleUI <- function(id) {
  downloadButton(NS(id, "figure_download"), label = "Save Figure")
}


#dl_module_server <- function(id, plot_output, plot_name) {
  
#  moduleServer(id, function(input, output, session) {

dl_module_server <- function(input, output, session, plot_output, plot_name, plot_height, plot_width) {#}, figure_name)  {
  
  output$figure_download <-  downloadHandler(
    filename = plot_name, 
    content = function(file) {
      ggsave(file,
             plot = {
               y <- plot_output()
               y <- y + theme(text=element_text(size=12)) 
               y
             }, 
             device = "png", height = plot_height, width = plot_width, units = "in", dpi = 300)
    })
}



