#' Run the Shiny Application
#'
#' @param ... arguments to pass to golem_opts.
#' See `?golem::get_golem_options` for more details.
#' @inheritParams shiny::shinyApp
#'
#' @export
#' @importFrom shiny shinyApp
#' @importFrom golem with_golem_options
#' @importFrom bsplus use_bs_tooltip use_bs_popover %>% bs_accordion bs_append shinyInput_label_embed shiny_iconlink bs_embed_tooltip bs_button
#' @importFrom shinyjs useShinyjs
#' @importFrom shinyWidgets switchInput updateSwitchInput
#' @importFrom reshape2 melt
#' @import miaSim
#' @import miaViz
#' @import ggplot2
#' @importFrom stats dbeta rbeta rgamma runif
run_app <- function(
  onStart = NULL,
  options = list(),
  enableBookmarking = NULL,
  uiPattern = "/",
  ...
) {
  with_golem_options(
    app = shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}
