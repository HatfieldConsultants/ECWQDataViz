# Run the application 
run_app <- function(){
#' @export
  shiny::shinyApp(ui = ecviz::ui, server = ecviz::server)}