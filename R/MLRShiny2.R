#' Start MLRShiny2
#' @title Launch MLRShiny2 Interface
#' @return Nothing
#' @description MLRShiny2() loads interactive user interface built using R shiny.
#' @details The interactive user interface is to provide an easy way for people who are learning Multiple Linear Regression. Includes example data for testing out a few example analysis.
#' @keywords MLRShiny2
#' @export MLRShiny2:: MLRShiny2()
#' @examples
#' if(interactive()){
#' library(shiny)
#' MLRShiny2()
#' }
 MLRShiny2 <- function() {
  shiny::runApp(appDir = system.file("shiny-examples", "myapp", package = "MLRShiny2"))
  Sys.setenv("R_TESTS" = "")
}
