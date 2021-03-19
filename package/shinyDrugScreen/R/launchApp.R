#' @title launchApp
#'
#' @description Launches the drug screening Shiny app.
#'
#' @export
launchApp <- function(){
  shinyApp(ui = ui, server = server)
}