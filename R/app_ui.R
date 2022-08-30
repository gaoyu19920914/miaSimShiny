#' The application User-Interface
#'
#' @param request Internal parameter for `{shiny}`.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_ui <- function(request) {
  tagList(
    # Leave this function for adding external resources
    golem_add_external_resources(),
    # Your application UI logic
    navbarPage(
      title ="miaSimShiny",
      # tab1 Consumer-Resource Model ####
      tabPanel(
        title = "Consumer-Resource Model",
        titlePanel("Consumer-Resource Model"),
        # enable bsplus tooltips and popovers
        use_bs_tooltip(),
        use_bs_popover(),
        # enable Shinyjs
        useShinyjs(),
        ## CRM Model ####
        bs_accordion(id = "CRMcontents") %>%
          bs_append(
            title = "Model",
            content =
              fluidRow(
                column(
                  width = 5,
                  wellPanel(
                    tabsetPanel(
                      header = tags$style(
                        HTML(
                          "
                            /* add a border for tabpanes */
                            .tabbable > .tab-content > .tab-pane {
                                border-left: 1px solid #ddd;
                                border-right: 1px solid #ddd;
                                border-bottom: 1px solid #ddd;
                                border-radius: 0px 0px 5px 5px;
                                padding: 10px;
                            }
                            .nav-tabs {
                                margin-bottom: 0;
                            }
                          "
                        )
                      ),
                      ### Basic ####
                      tabPanel(
                        "Basic",
                        sliderInput(
                          "nSpeciesCRM",
                          "number of species",
                          value = 2,
                          min = 1,
                          max = 100) %>%
                          shinyInput_label_embed(
                            shiny_iconlink("circle-info") %>%
                              bs_embed_tooltip(
                                title =  "Number of species in the simulation",
                                placement = "right",
                                container = "body"
                              )
                          ),
                        sliderInput(
                          "nResourcesCRM",
                          "number of compounds",
                          value = 4,
                          min = 1,
                          max = 100) %>%
                          shinyInput_label_embed(
                            shiny_iconlink("circle-info") %>%
                              bs_embed_tooltip(
                                title =  "Number of compounds in the simulation",
                                placement = "right",
                                container = "body"
                              )
                          ),
                        tags$hr(),
                        switchInput(
                          "CustomCRM",
                          strong("custom names/times in simulation"),
                          value = FALSE,
                          labelWidth = "100%"
                        ),
                        conditionalPanel(
                          condition = "input.CustomCRM",
                          helpText("Custom names separate by ',' or ';' (and spaces) will replace default names."),
                          textInput("namesSpeciesCRM", "names of species"),
                          textInput("namesResourcesCRM", "names of compounds"),
                        ),
                        conditionalPanel(
                          condition = "input.CustomCRM",
                          tags$hr(),
                          numericInput("tStartCRM", "start time of the simulation", value = 0, min = 0, max = 10000, step = 100),
                        ),
                        numericInput("tEndCRM", "final time of the simulation", value = 1000, min = 100, max = 10000, step = 100),
                        conditionalPanel(
                          condition = "input.CustomCRM",
                          numericInput("tStepCRM", "time step of the simulation", value = 0.1, min = 0.01, max = 10, step = 0.01),
                          numericInput("tStoreCRM", "stored time points of the simulation", value = 1000, min = 100, max = 10000, step = 100),
                        ),
                      ),
                      ### Compounds ####
                      tabPanel(
                        "Compounds",
                        sliderInput(
                          "resourcesConcentrationCRM",
                          "mean initial concentration of compounds",
                          min = 0,
                          max = 1000,
                          value = 100,
                          step = 1) %>%
                          shinyInput_label_embed(
                            shiny_iconlink("circle-info") %>%
                              bs_embed_tooltip(
                                title =  "initial average concentration of each compound",
                                placement = "right",
                                container = "body"
                              )
                          ),
                        sliderInput(
                          "resourcesEvennessCRM",
                          "evenness of compounds",
                          min = 0.1,
                          max = 100,
                          value = 10,
                          step = 0.1) %>%
                          shinyInput_label_embed(
                            shiny_iconlink("circle-info") %>%
                              bs_embed_tooltip(
                                title =  "higher evenness leads to similar concentrations of initial compounds",
                                placement = "right",
                                container = "body"
                              )
                          ),
                        textInput(
                          "resourcesCustomCRM",
                          "initial concentrations of compounds") %>%
                          shinyInput_label_embed(
                            shiny_iconlink("circle-info") %>%
                              bs_embed_tooltip(
                                title =  "If the given initial concentrations of compounds are not enough, random values will be added.",
                                placement = "right",
                                container = "body"
                              )
                          ),
                        verbatimTextOutput("resourcesOutputCRM"),

                        plotOutput("resourcesCRMPlot"),

                        tags$hr(),
                        sliderInput(
                          "meanConsumptionCRM",
                          "consumption weight",
                          value = 0.4,
                          min = 0,
                          max = 1) %>%
                          shinyInput_label_embed(
                            shiny_iconlink("circle-info") %>%
                              bs_embed_tooltip(
                                title =  "Mean proportion of compounds consumed by each species.",
                                placement = "right",
                                container = "body"
                              )
                          ),
                        sliderInput(
                          "meanProductionCRM",
                          "production weight",
                          value = 0.2,
                          min = 0,
                          max = 0.5) %>%
                          shinyInput_label_embed(
                            shiny_iconlink("circle-info") %>%
                              bs_embed_tooltip(
                                title =  "Mean proportion of compounds produced by each species.",
                                placement = "right",
                                container = "body"
                              )
                          ),
                        sliderInput("maintenanceCRM", "maintenance weight", value = 0.5, min = 0, max = 1) %>%
                          shinyInput_label_embed(
                            shiny_iconlink("circle-info") %>%
                              bs_embed_tooltip(
                                title =  "How much compounds were used to maintain the microbial community (not involved in flux of compounds).",
                                placement = "right",
                                container = "body"
                              )
                          ),

                        sliderInput(
                          "inflowRateCRM",
                          "inflow rate",
                          min = 0,
                          max = 10,
                          value = 0,
                          step = 0.1) %>%
                          shinyInput_label_embed(
                            shiny_iconlink("circle-info") %>%
                              bs_embed_tooltip(
                                title =  "inflow rate of additional nutrients to the cultivator. volume per unit time",
                                placement = "right",
                                container = "body"
                              )
                          ),

                        conditionalPanel(
                          condition = "input.inflowRateCRM > 0",
                          textInput("resourcesDilutionCRM", "resources concentration in dilution")  %>%
                            shinyInput_label_embed(
                              shiny_iconlink("circle-info") %>%
                                bs_embed_tooltip(
                                  title =  "concentrations of resources in continuous flow, by default equal to initial concentrations of compounds",
                                  placement = "right",
                                  container = "body"
                                )
                            ),
                          verbatimTextOutput("resourcesDilutionCRMOutput"),

                          sliderInput(
                            "outflowRateCRM",
                            "outflow rate",
                            min = 0,
                            max = 10,
                            value = 0,
                            step = 0.1) %>%
                            shinyInput_label_embed(
                              shiny_iconlink("circle-info") %>%
                                bs_embed_tooltip(
                                  title =  "outflow rate from the bioreactor. volume per unit time",
                                  placement = "right",
                                  container = "body"
                                )
                            ),

                          sliderInput(
                            "volumeCRM",
                            "volume of the reactor",
                            min = 100,
                            max = 10000,
                            value = 1000,
                            step = 100)  %>%
                            shinyInput_label_embed(
                              shiny_iconlink("circle-info") %>%
                                bs_embed_tooltip(
                                  title =  "volume of the reactor",
                                  placement = "right",
                                  container = "body"
                                )
                            ),
                        ),

                        tags$hr(),
                        tags$div(
                          tags$label("Compounds Stochiometry"),
                          tags$div(
                            class = "pull-right",
                            shiny_iconlink("circle-info") %>%
                              bs_embed_tooltip(
                                title =  "Stochiometric values of consumption and production of compounds by each cell. Positive efficiencies indicate the consumption of resources, whilst negatives indicate that the species would produce the resource.",
                                placement = "right",
                                container = "body"
                              ),
                          )
                        ),
                        DT::dataTableOutput("tableECRM", width = "100%"),
                      ),
                      ### Growth rates ####
                      tabPanel(
                        "Growth rates",
                        textInput("x0CRM", "initial abundances of species")  %>%
                          shinyInput_label_embed(
                            shiny_iconlink("circle-info") %>%
                              bs_embed_tooltip(
                                title =  "If the given initial abundances of species is not enough, random initial abundances will be added.",
                                placement = "right",
                                container = "body"
                              )
                          ),
                        verbatimTextOutput("x0CRMOutput"),
                        tags$hr(),
                        tags$div(
                          tags$label("Distribution of Growth Rates"),
                        ),
                        bs_button(
                          label = "-",
                          button_type = "primary",
                          id = "buttonBetaEven",
                          style = "width:13%",
                          class = "btn-default action-button shiny-bound-input"
                        ) %>% bs_embed_tooltip(title = "even distribution"),
                        bs_button(
                          label = "/\\",
                          button_type = "primary",
                          id = "buttonBetaRidge",
                          style = "width:13%",
                          class = "btn-default action-button shiny-bound-input"
                        ) %>% bs_embed_tooltip(title = "normal-alike distribution"),
                        bs_button(
                          label = "\\/",
                          button_type = "primary",
                          id = "buttonBetaValley",
                          style = "width:13%",
                          class = "btn-default action-button shiny-bound-input"
                        ) %>% bs_embed_tooltip(title = "U shape distribution"),
                        bs_button(
                          label = "\\_",
                          button_type = "primary",
                          id = "buttonBetaLeft",
                          style = "width:13%",
                          class = "btn-default action-button shiny-bound-input"
                        ) %>% bs_embed_tooltip(title = "left skewed distribution"),
                        bs_button(
                          label = "_/",
                          button_type = "primary",
                          id = "buttonBetaRight",
                          style = "width:13%",
                          class = "btn-default action-button shiny-bound-input"
                        ) %>% bs_embed_tooltip(title = "right skewed distribution"),
                        bs_button(
                          label = "\\",
                          button_type = "primary",
                          id = "buttonBetaLeftTriangle",
                          style = "width:13%",
                          class = "btn-default action-button shiny-bound-input"
                        ) %>% bs_embed_tooltip(title = "left-triangle distribution"),
                        bs_button(
                          label = "/",
                          button_type = "primary",
                          id = "buttonBetaRightTriangle",
                          style = "width:13%",
                          class = "btn-default action-button shiny-bound-input"
                        ) %>% bs_embed_tooltip(title = "right-triangle distribution"),

                        sliderInput(
                          "alphaCRM",
                          "alpha",
                          value = 1,
                          min = 0,
                          max = 10,
                          step = 0.1
                        )  %>%
                          shinyInput_label_embed(
                            shiny_iconlink("circle-info") %>%
                              bs_embed_tooltip(
                                title =  "first parameter of beta distribution",
                                placement = "right",
                                container = "body"
                              )
                          ),
                        sliderInput(
                          "betaCRM",
                          "beta",
                          value = 1,
                          min = 0,
                          max = 10,
                          step = 0.1
                        )  %>%
                          shinyInput_label_embed(
                            shiny_iconlink("circle-info") %>%
                              bs_embed_tooltip(
                                title =  "second parameter of beta distribution",
                                placement = "right",
                                container = "body"
                              )
                          ),
                        textInput(
                          "growthRatesCRM",
                          "maximum growth rates of species"
                        ) %>%
                          shinyInput_label_embed(
                            shiny_iconlink("circle-info") %>%
                              bs_embed_tooltip(
                                title =  "If the given growth rates are not enough, random values will be added.",
                                placement = "right",
                                container = "body"
                              )
                          ),
                        verbatimTextOutput("growthRatesCRMOutput"),

                        plotOutput("growthRatesCRMDist"),
                        tags$label("Monod Constant"),
                        DT::dataTableOutput("tableMonodCRM", width = "100%"),
                      ),
                      ### Perturbations ####
                      tabPanel(
                        "Perturbations",
                        sliderInput(
                          "errorVarianceCRM",
                          "variance of measurement error",
                          value = 0,
                          min = 0,
                          max = 1,
                          step = 0.01)  %>%
                          shinyInput_label_embed(
                            shiny_iconlink("circle-info") %>%
                              bs_embed_tooltip(
                                title =  "The variance of measurement error. By default it equals to 0, indicating that the result won't contain any measurement error.",
                                placement = "right",
                                container = "body"
                              )
                          ),
                        switchInput(
                          "stochasticCRM",
                          strong("use stochasticity"),
                          value = TRUE,
                          labelWidth = "100%"
                        ),
                        conditionalPanel(
                          condition = "input.stochasticCRM",
                          sliderInput("sigmaDriftCRM", "strength of drift", value = 0, min = 0, max = 1, step = 0.001)  %>%
                            shinyInput_label_embed(
                              shiny_iconlink("circle-info") %>%
                                bs_embed_tooltip(
                                  title =  "drift happens on each step of simulation",
                                  placement = "right",
                                  container = "body"
                                )
                            ),
                          tags$hr(),
                          sliderInput("epochPCRM", "probability of random periodic (epoch) changes", value = 0.001, min = 0, max = 1, step = 0.001) %>%
                            shinyInput_label_embed(
                              shiny_iconlink("circle-info") %>%
                                bs_embed_tooltip(
                                  title =  "microbial epoch perturbations happens by chance",
                                  placement = "right",
                                  container = "body"
                                )
                            ),
                          conditionalPanel(
                            condition = "input.epochPCRM >0",
                            sliderInput("sigmaEpochCRM", "strength of microbial epoch perturbation", value = 0.001, min = 0, max = 1, step = 0.001),

                          ),
                          tags$hr(),
                          sliderInput("sigmaExternalCRM", "strength of external perturbations", value = 0.3, min = 0, max = 1, step = 0.001),
                          conditionalPanel(
                            condition = "input.sigmaExternalCRM >0",
                            textInput("tExternalEventsCRM", "starting time of external events"),
                            verbatimTextOutput("tExternalEventsCRMOutput"),
                            textInput("tExternalDurationsCRM", "durations of external events"),
                            verbatimTextOutput("tExternalDurationsCRMOutput"),
                          ),
                        ),
                        tags$hr(),
                        sliderInput("migrationPCRM", "probability/frequency of migration from metacommunity", value = 0.01, min = 0, max = 1),
                        conditionalPanel(
                          condition = "input.migrationPCRM >0",
                          sliderInput("sigmaMigrationCRM", "intensity of migration", value = 0.01, min = 0, max = 1, step = 0.001),
                        ),
                        textInput(
                          "metacommunityProbabilityCRM",
                          "metacommunity") %>%
                          shinyInput_label_embed(
                            shiny_iconlink("circle-info") %>%
                              bs_embed_tooltip(
                                title =  "Normalized probability distribution of the likelihood that species from the metacommunity can enter the community during the simulation.",
                                placement = "right",
                                container = "body"
                              )
                          ),
                        verbatimTextOutput("metacommunityProbabilityCRM"),
                      ),
                    ),
                    tags$hr(),
                    switchInput(
                      "normCRM",
                      strong("returns normalized abundances"),
                      value = FALSE,
                      labelWidth = "100%"
                    ),
                    # actionButton("buttonSimulateCRM", "Run the consumer-resource model", class = "btn btn-primary", width = "100%")
                  )
                ),
                ### Display Panel ####
                column(
                  width = 7,
                  #### example buttons ####
                  fluidRow(
                    style = "padding-left: 15px; padding-right: 15px;",
                    tags$div(
                      class = "panel panel-default",
                      tags$div(
                        class = "panel-heading",
                        tags$h3(
                          class = "panel-title",
                          "Examples",
                        ),
                      ),
                      tags$div(
                        class = "panel-body",
                        actionButton(
                          inputId = "CRMEX1",
                          label = "Ex.1",
                          style = "width:12%",
                          class = "btn-primary"
                        ),
                        actionButton(
                          inputId = "CRMEX2",
                          label = "Ex.2",
                          style = "width:12%",
                          class = "btn-primary"
                        ),
                        actionButton(
                          inputId = "CRMEX3",
                          label = "Ex.3",
                          style = "width:12%",
                          class = "btn-primary"
                        ),
                        actionButton(
                          inputId = "CRMEX4pre",
                          label = "Pre.4",
                          style = "width:7%",
                          class = "btn-info"
                        ),
                        actionButton(
                          inputId = "CRMEX4",
                          label = "Ex.4",
                          style = "width:7%",
                          class = "btn-primary",
                          disabled = "disabled"
                        ) %>% bs_embed_tooltip(title = "Please run the Pre.4 First") ,
                        actionButton(
                          inputId = "CRMEX5pre",
                          label = "Pre.5",
                          style = "width:7%",
                          class = "btn-info"
                        ),
                        actionButton(
                          inputId = "CRMEX5",
                          label = "Ex.5",
                          style = "width:7%",
                          class = "btn-primary",
                          disabled = "disabled"
                        ) %>% bs_embed_tooltip(title = "Please run the Pre.5 First") ,
                        actionButton(
                          inputId = "CRMEX6pre",
                          label = "Pre.6",
                          style = "width:7%",
                          class = "btn-info"
                        ),
                        actionButton(
                          inputId = "CRMEX6",
                          label = "Ex.6",
                          style = "width:7%",
                          class = "btn-primary",
                          disabled = "disabled"
                        ) %>% bs_embed_tooltip(title = "Please run the Pre.6 First") ,
                        actionButton(
                          inputId = "CRMEX7pre",
                          label = "Pre.7",
                          style = "width:7%",
                          class = "btn-info"
                        ),
                        actionButton(
                          inputId = "CRMEX7",
                          label = "Ex.7",
                          style = "width:7%",
                          class = "btn-primary",
                          disabled = "disabled"
                        ) %>% bs_embed_tooltip(title = "Please run the Pre.7 First") ,
                      ),
                    ),
                  ),
                  #### Species Change ####
                  tags$br(),
                  fluidRow(
                    style = "padding-left: 15px; padding-right: 15px;",
                    tags$div(
                      class = "panel panel-default",
                      tags$div(
                        class = "panel-heading",
                        tags$h3(
                          class = "panel-title",
                          "Species Change",
                        ),
                      ),
                      tags$div(
                        class = "panel-body",
                        plotOutput("CRMSpecies"),
                      ),
                    ),
                    #### Compounds Change ####
                    tags$br(),
                    tags$div(
                      class = "panel panel-default",
                      tags$div(
                        class = "panel-heading",
                        tags$h3(
                          class = "panel-title",
                          "Compounds Change",
                        ),
                      ),
                      tags$div(
                        class = "panel-body",
                        plotOutput("CRMResources"),

                      ),
                    ),
                    #### Volume Change (conditional) ####
                    conditionalPanel(
                      condition = "input.inflowRateCRM != input.outflowRateCRM",
                      tags$br(),
                      tags$div(
                        class = "panel panel-default",
                        tags$div(
                          class = "panel-heading",
                          tags$h3(
                            class = "panel-title",
                            "Volume Change",
                          ),
                        ),
                        tags$div(
                          class = "panel-body",
                          plotOutput("CRMVolume"),
                        ),
                      ),
                    ),

                    #### Matrix of Efficiency ####
                    tags$br(),
                    tags$div(
                      class = "panel panel-default",
                      tags$div(
                        class = "panel-heading",
                        tags$h3(
                          class = "panel-title",
                          "Matrix of Efficiency",
                          tags$div(
                            class = "pull-right",
                            shiny_iconlink("circle-info") %>%
                              bs_embed_tooltip(
                                title =  "Positive values indicate consumption, and negative values indicate production",
                                placement = "left",
                                container = "body"
                              ),
                          ),
                        ),
                      ),
                      tags$div(
                        class = "panel-body",
                        plotOutput("CRMPlotE"),
                      ),
                    ),
                  ),
                ),
              )
          ) %>%
          bs_append(
            ## Description ####
            title = "Description",
            content =
              fluidRow(
                column(
                  width = 12,
                  withMathJax(includeMarkdown(app_sys("app/www/crm.Rmd"))),
                ),
              )
          ) %>%
          bs_append(
            ## Inputs ####
            title = "Inputs",
            content =
              fluidRow(
                column(width = 12,
                       withMathJax(includeMarkdown(app_sys("app/www/crm_parms.Rmd"))),
                ),
              )
          ) %>%
          bs_append(
            ## References ####
            title = "References",
            content = "Panel of refs."
          )
      ),

      # tab2 Generalized Lotka-Volterra (GLV) Model ####
      tabPanel(
        title = "Generalized Lotka-Volterra (GLV)",
        titlePanel("Generalized Lotka-Volterra (GLV)"),
        ## GLV Model ####
        bs_accordion(id = "GLVcontents") %>%
          bs_append(
            title = "Model",
            content =
              fluidRow(
                column(
                  width = 5,
                  wellPanel(
                    tabsetPanel(
                      ### Interspecies interactions ####
                      tabPanel(
                        "Interspecies interactions",
                        sliderInput(
                          "nSpeciesGLV",
                          "number of species",
                          value = 2,
                          min = 2,
                          max = 50),
                        switchInput(
                          "advancedRandomA",
                          strong("show advanced parameters"),
                          labelWidth = "100%"
                        ),
                        conditionalPanel(
                          condition = "input.advancedRandomA",
                          helpText("Custom names separate by ',' or ';' (and spaces) will replace default names."),
                          textInput("namesSpeciesGLV", "names of species"),
                          sliderInput("diagonalGLV", "diagonal values of matrix A", value = -0.5, min = -2, max = 0, step = 0.1),
                          sliderInput("connectanceGLV", "connectance of matrix A", value = 0.2, min = 0, max = 1),
                          sliderInput("scaleGLV", "scale of off-diagonal values", value = 0.1, min = 0, max = 1),
                          tags$hr(),
                          numericInput("mutualismGLV", "relative proportion of mutualism in matrix A", value = 1, min = 0, max = 10,step = 0.05),
                          numericInput("commensalismGLV", "relative proportion of commensalism in matrix A", value = 1, min = 0, max = 10,step = 0.05),
                          numericInput("parasitismGLV", "relative proportion of parasitism in matrix A", value = 1, min = 0, max = 10,step = 0.05),
                          numericInput("amensalismGLV", "relative proportion of amensalism in matrix A", value = 1, min = 0, max = 10,step = 0.05),
                          numericInput("competitionGLV", "relative proportion of competition in matrix A", value = 1, min = 0, max = 10,step = 0.05),

                          textInput("interactionsCustomGLV", "user-defined interactions between species") %>%
                            shinyInput_label_embed(
                              shiny_iconlink("circle-info") %>%
                                bs_embed_tooltip(
                                  title =  "if the given interactions between species are not enough, random values will be added",
                                  placement = "right",
                                  container = "body"
                                )
                            ),
                          verbatimTextOutput("interactionsOutputGLV"),
                          bs_button(
                            label = "\\/",
                            button_type = "primary",
                            id = "buttonInteractionsU",
                            style = "width:30%",
                            class = "btn-default action-button shiny-bound-input"
                          ) %>% bs_embed_tooltip(title = "rbeta(n, shape1 = 0.5, shape2 = 0.5)"),
                          bs_button(
                            label = "/\\",
                            button_type = "primary",
                            id = "buttonInteractionsN",
                            style = "width:30%",
                            class = "btn-default action-button shiny-bound-input"
                          ) %>% bs_embed_tooltip(title = "rbeta(n, shape1 = 2, shape2 = 2)"),
                          bs_button(
                            label = "-",
                            button_type = "primary",
                            id = "buttonInteractionsEven",
                            style = "width:30%",
                            class = "btn-default action-button shiny-bound-input"
                          ) %>% bs_embed_tooltip(title = "runif(n, min = 0, max = 1)"),

                          tags$hr(),
                          switchInput(
                            "symmetricGLV",
                            strong("matrix A"),
                            labelWidth = "100%",
                            onLabel = "symmetric",
                            offLabel = "non-symmetric"
                          ),
                          # textInput("list_AGLV", "a list of previous generated matrix"),
                        ),
                        # actionButton("buttonRandomA", "generate random matrix A of interspecies interactions", class = "btn btn-primary"),
                        conditionalPanel(
                          condition = "input.advancedRandomA",
                          tags$hr(),
                          numericInput("tStartGLV", "start time of the simulation", value = 0, min = 0, max = 10000, step = 100),
                        ),
                        numericInput("tEndGLV", "final time of the simulation", value = 100, min = 100, max = 10000, step = 100),
                        conditionalPanel(
                          condition = "input.advancedRandomA",
                          numericInput("tStepGLV", "time step of the simulation", value = 0.1, min = 0.01, max = 10, step = 0.01),
                          numericInput("tStoreGLV", "stored time points of the simulation", value = 200, min = 100, max = 10000, step = 100),
                        ),
                      ),
                      ### Growth rates ####
                      tabPanel(
                        "Growth rates",
                        textInput("x0GLV", "initial abundances of species"),
                        verbatimTextOutput("x0GLVOutput"),
                        textInput("growthRatesGLV", "maximum growth rates of species"),
                        verbatimTextOutput("growthRatesGLVOutput"),
                      ),
                      ### Perturbations ####
                      tabPanel(
                        "Perturbations",
                        sliderInput(
                          "errorVarianceGLV",
                          "variance of measurement error",
                          value = 0,
                          min = 0,
                          max = 10,
                          step = 0.1) %>%
                          shinyInput_label_embed(
                            shiny_iconlink("circle-info") %>%
                              bs_embed_tooltip(
                                title =  "The variance of measurement error. By default it equals to 0, indicating that the result won't contain any measurement error.",
                                placement = "right",
                                container = "body"
                              )
                          ),

                        switchInput(
                          "stochasticGLV",
                          strong("use stochasticity"),
                          value = TRUE,
                          labelWidth = "100%"),

                        conditionalPanel(
                          condition = "input.stochasticGLV",
                          sliderInput(
                            "sigmaDriftGLV",
                            "strength of drift",
                            value = 0.001,
                            min = 0,
                            max = 1,
                            step = 0.001
                          ) %>%
                            shinyInput_label_embed(
                              shiny_iconlink("circle-info") %>%
                                bs_embed_tooltip(
                                  title =  "drift happens on each step of simulation",
                                  placement = "right",
                                  container = "body"
                                )
                            ),
                          tags$hr(),

                          sliderInput(
                            "epochPGLV",
                            "probability of random periodic (epoch) changes",
                            value = 0.001,
                            min = 0,
                            max = 1,
                            step = 0.001
                          ) %>%
                            shinyInput_label_embed(
                              shiny_iconlink("circle-info") %>%
                                bs_embed_tooltip(
                                  title =  "microbial epoch perturbations happens by chance",
                                  placement = "right",
                                  container = "body"
                                )
                            ),
                          conditionalPanel(
                            condition = "input.epochPGLV > 0",
                            sliderInput("sigmaEpochGLV", "strength of microbial epoch perturbation", value = 0.001, min = 0, max = 1, step = 0.001),
                          ),
                          tags$hr(),
                          sliderInput("sigmaExternalGLV", "strength of external perturbations", value = 0.3, min = 0, max = 1, step = 0.001),
                          conditionalPanel(
                            condition = "input.sigmaExternalGLV > 0",
                            textInput("tExternalEventsGLV", "timepoints of external events"),
                            verbatimTextOutput("tExternalEventsGLVOutput"),
                            textInput("tExternalDurationsGLV", "time durations of external events"),
                            verbatimTextOutput("tExternalDurationsGLVOutput"),
                          ),
                          tags$hr()
                        ),
                        sliderInput("migrationPGLV", "probability/frequency of migration from metacommunity", value = 0.01, min = 0, max = 1, step = 0.01),
                        conditionalPanel(
                          condition = "input.migrationPGLV >0",
                          sliderInput("sigmaMigrationGLV", "intensity of migration", value = 0.01, min = 0, max = 1, step = 0.001),
                        ),
                        textInput(
                          "metacommunityProbabilityGLV",
                          "metacommunity"
                        ) %>%
                          shinyInput_label_embed(
                            shiny_iconlink("circle-info") %>%
                              bs_embed_tooltip(
                                title =  "Normalized probability distribution of the likelihood that species from the metacommunity can enter the community during the simulation.",
                                placement = "right",
                                container = "body"
                              )
                          ),
                        verbatimTextOutput("metacommunityProbabilityGLV"),
                      ),
                    ),

                    tags$hr(),
                    switchInput(
                      "normGLV",
                      strong("returns normalized abundances"),
                      value = FALSE,
                      labelWidth = "100%"
                    ),

                    # tags$hr(),
                    # tags$h4(
                    #     "Press the following button to run the model",
                    #     tags$div(
                    #         class = "pull-right",
                    #         shiny_iconlink("circle-info") %>%
                    #             bs_embed_tooltip(
                    #                 title =  "GLV model was not designed responsive to reduce the calculation",
                    #                 placement = "left",
                    #                 container = "body"
                    #             ),
                    #     ),
                    # ),
                    # actionButton("buttonSimulateGLV", "Run the GLV Model", class = "btn btn-primary", width = "100%"),
                  )
                ),
                ### Display Panel ####
                column(
                  width = 7,
                  #### example buttons ####
                  fluidRow(
                    style = "padding-left: 15px; padding-right: 15px;",
                    tags$div(
                      class = "panel panel-default",
                      tags$div(
                        class = "panel-heading",
                        tags$h3(
                          class = "panel-title",
                          "Examples",
                          # tags$div(
                          #     class = "pull-right",
                          #     shiny_iconlink("circle-info") %>%
                          #         bs_embed_tooltip(
                          #             title =  'Don\'t forget to click on the "Run the GLV model" button on the left',
                          #             placement = "left",
                          #             container = "body"
                          #         ),
                          # ),
                        ),
                      ),
                      tags$div(
                        class = "panel-body",
                        # strong('Please click on the "Run the GLV model" button afterwards'),
                        # tags$br(),
                        actionButton(
                          inputId = "GLVEX1",
                          label = "Ex.1",
                          style = "width:24%",
                          class = "btn-primary"
                        ),
                        actionButton(
                          inputId = "GLVEX2",
                          label = "Ex.2",
                          style = "width:24%",
                          class = "btn-primary"
                        ),
                        actionButton(
                          inputId = "GLVEX3",
                          label = "Ex.3",
                          style = "width:24%",
                          class = "btn-primary"
                        ),
                        actionButton(
                          inputId = "GLVEX4",
                          label = "Ex.4",
                          style = "width:24%",
                          class = "btn-primary"
                        ),
                      ),
                    ),
                  ),


                  #### Matrix A ####
                  tags$br(),
                  fluidRow(
                    style = "padding-left: 15px; padding-right: 15px;",
                    tags$div(
                      class = "panel panel-default",
                      tags$div(
                        class = "panel-heading",
                        tags$h3(
                          class = "panel-title",
                          "Matrix of interspecies interactions",
                        ),
                      ),
                      tags$div(
                        class = "panel-body",
                        DT::dataTableOutput("TableAGLV", width = "100%"),
                        plotOutput("GLVPlotA", height = "600px"),
                      ),
                    ),
                  ),
                  #### Species Change ####
                  tags$br(),
                  fluidRow(
                    style = "padding-left: 15px; padding-right: 15px;",
                    tags$div(
                      class = "panel panel-default",
                      tags$div(
                        class = "panel-heading",
                        tags$h3(
                          class = "panel-title",
                          "Species Change",
                        ),
                      ),
                      tags$div(
                        class = "panel-body",
                        plotOutput("GLVSpecies"),
                      ),
                    ),
                  ),
                ),
              )

          ) %>%
          bs_append(
            ## Description ####
            title = "Description",
            content =
              fluidRow(
                column(
                  width = 12,
                  withMathJax(includeMarkdown(app_sys("app/www/glv.Rmd"))),
                ),
              )
          ) %>%
          bs_append(
            ## Inputs ####
            title = "Inputs",
            content =
              fluidRow(
                column(width = 12,
                       withMathJax(includeMarkdown(app_sys("app/www/glv_parms.Rmd"))),
                ),
              )
          ) %>%
          bs_append(
            ## References ####
            title = "References",
            content = "Panel of refs."
          )
      ),
      # tab3 Hubbell Model ####
      tabPanel(
        title = "Hubbell Model (with growth rates)",
        titlePanel("Hubbell Model (with growth rates)"),
        ## Hubbell Model ####
        bs_accordion(id = "HUBcontents") %>%
          bs_append(
            title = "Model",
            content =
              fluidRow(
                column(
                  width = 5,
                  wellPanel(
                    tabsetPanel(
                      ### Hubbell initial states ####
                      tabPanel(
                        "Basic",
                        sliderInput(
                          "nSpeciesHUB",
                          "number of species",
                          value = 5,
                          min = 2,
                          max = 50),
                        textInput(
                          "x0HUB",
                          "initial species composition (counts of individuals)",
                          value = "",
                        ) %>%
                          shinyInput_label_embed(
                            shiny_iconlink("circle-info") %>%
                              bs_embed_tooltip(
                                title =  "separated by comma or semicolon (and space), indicating the number of species, too. If not enough, 100 will be added.",
                                placement = "right",
                                container = "body"
                              )
                          ),
                        verbatimTextOutput("x0HUBOutput"),
                        switchInput(
                          "CustomHUB",
                          strong("custom names/times in simulation"),
                          value = FALSE,
                          labelWidth = "100%"
                        ),
                        conditionalPanel(
                          condition = "input.CustomHUB",
                          tags$hr(),
                          textInput("namesSpeciesHUB", "names of species"),
                          tags$hr(),
                          numericInput("tStartHUB", "start time of the simulation", value = 0, min = 0, max = 10000, step = 100),
                        ),
                        numericInput("tEndHUB", "final time of the simulation", value = 100, min = 100, max = 10000, step = 100),
                        conditionalPanel(
                          condition = "input.CustomHUB",
                          numericInput("tStepHUB", "time step of the simulation", value = 0.1, min = 0.01, max = 10, step = 0.01),
                          numericInput("tStoreHUB", "stored time points of the simulation", value = 1000, min = 100, max = 10000, step = 100),
                        ),


                      ),

                      ### Perturbations ####
                      tabPanel(
                        "Perturbations",
                        sliderInput(
                          "errorVarianceHUB",
                          "variance of measurement error",
                          value = 0,
                          min = 0,
                          max = 100,
                          step = 0.1) %>%
                          shinyInput_label_embed(
                            shiny_iconlink("circle-info") %>%
                              bs_embed_tooltip(
                                title =  "The variance of measurement error. By default it equals to 0, indicating that the result won't contain any measurement error.",
                                placement = "right",
                                container = "body"
                              )
                          ),

                        tags$hr(),
                        sliderInput("kEventsHUB", "number of events to simulate before updating the sampling distributions", value = 1, min = 1, max = 10),

                        tags$hr(),
                        sliderInput("migrationPHUB", "probability/frequency of migration from metacommunity", value = 0.01, min = 0, max = 1),
                        conditionalPanel(
                          condition = "input.migrationPHUB >0",
                          textInput(
                            "metacommunityProbabilityHUB",
                            "metacommunity") %>%
                            shinyInput_label_embed(
                              shiny_iconlink("circle-info") %>%
                                bs_embed_tooltip(
                                  title =  "Normalized probability distribution of the likelihood that species from the metacommunity can enter the community during the simulation.",
                                  placement = "right",
                                  container = "body"
                                )
                            ),
                          verbatimTextOutput("metacommunityProbabilityHUB"),
                        ),
                      ),
                      ### Growth Rates ####
                      tabPanel(
                        "Growth Rates",
                        textInput("growthRatesHUB", "growth rates of species"),
                        verbatimTextOutput("growthRatesHUBOutput"),
                        tags$hr(),
                      ),

                    ),

                    tags$hr(),
                    switchInput(
                      "normHUB",
                      strong("returns normalized abundances"),
                      value = FALSE,
                      labelWidth = "100%"
                    ),

                    # tags$hr(),
                    # tags$h4(
                    #     "Press the following button to run the model",
                    #     tags$div(
                    #         class = "pull-right",
                    #         shiny_iconlink("circle-info") %>%
                    #             bs_embed_tooltip(
                    #                 title =  "Hubbell neutral model was not designed responsive to reduce the calculation",
                    #                 placement = "left",
                    #                 container = "body"
                    #             ),
                    #     ),
                    # ),
                    # actionButton("buttonSimulateHUB", "Run the Hubbell Model", class = "btn btn-primary", width = "100%"),
                  )
                ),
                ### Display Panel ####
                column(
                  width = 7,
                  #### example buttons ####
                  fluidRow(
                    style = "padding-left: 15px; padding-right: 15px;",
                    tags$div(
                      class = "panel panel-default",
                      tags$div(
                        class = "panel-heading",
                        tags$h3(
                          class = "panel-title",
                          "Examples",
                          # tags$div(
                          #     class = "pull-right",
                          #     shiny_iconlink("circle-info") %>%
                          #         bs_embed_tooltip(
                          #             title =  'Don\'t forget to click on the "Run the Hubbell model" button on the left',
                          #             placement = "left",
                          #             container = "body"
                          #         ),
                          # ),
                        ),
                      ),
                      tags$div(
                        class = "panel-body",
                        # strong('Please click on the "Run the Hubbell model" button afterwards'),
                        # tags$br(),
                        actionButton(
                          inputId = "HUBEX1",
                          label = "Ex.1",
                          style = "width:19%",
                          class = "btn-primary"
                        ),
                        actionButton(
                          inputId = "HUBEX2",
                          label = "Ex.2",
                          style = "width:19%",
                          class = "btn-primary"
                        ),
                        actionButton(
                          inputId = "HUBEX3",
                          label = "Ex.3",
                          style = "width:19%",
                          class = "btn-primary"
                        ),
                        actionButton(
                          inputId = "HUBEX4",
                          label = "Ex.4",
                          style = "width:19%",
                          class = "btn-primary"
                        ),
                        actionButton(
                          inputId = "HUBEX5",
                          label = "Ex.5",
                          style = "width:19%",
                          class = "btn-primary"
                        ),

                      ),
                    ),
                  ),


                  #### Species Change ####
                  tags$br(),
                  fluidRow(
                    style = "padding-left: 15px; padding-right: 15px;",
                    tags$div(
                      class = "panel panel-default",
                      tags$div(
                        class = "panel-heading",
                        tags$h3(
                          class = "panel-title",
                          "Species Change",
                        ),
                      ),
                      tags$div(
                        class = "panel-body",
                        plotOutput("HUBSpecies"),
                      ),
                    ),
                  ),
                )
              )
          )  %>%
          bs_append(
            ## Description ####
            title = "Description",
            content =
              fluidRow(
                column(
                  width = 12,
                  withMathJax(includeMarkdown(app_sys("app/www/hub.Rmd"))),
                ),
              )
          ) %>%
          bs_append(
            ## Inputs ####
            title = "Inputs",
            content =
              fluidRow(
                column(
                  width = 12,
                  withMathJax(includeMarkdown(app_sys("app/www/hub_parms.Rmd"))),
                ),
              )
          ) %>%
          bs_append(
            ## References ####
            title = "References",
            content =
              fluidRow(
                "Panel of refs.",
                "https://en.wikipedia.org/wiki/Gillespie_algorithm"
              )
          )
      ),

      # tab4 Logistic Model (with stochasticity) ####
      tabPanel(
        title = "Logistic Model (with stochasticity)",
        titlePanel("Logistic Model (with stochasticity)"),
        ## Logistic Model ####
        bs_accordion(id = "LOGcontents") %>%
          bs_append(
            title = "Model",
            content =
              fluidRow(
                column(
                  width = 5,
                  wellPanel(
                    tabsetPanel(
                      ### Basics ####
                      tabPanel(
                        "Basics",
                        sliderInput(
                          "nSpeciesLOG",
                          "number of species",
                          value = 2,
                          min = 2,
                          max = 50),
                        tags$hr(),
                        switchInput(
                          "CustomLOG",
                          strong("custom names/times in simulation"),
                          value = FALSE,
                          labelWidth = "100%"
                        ),
                        conditionalPanel(
                          condition = "input.CustomLOG",
                          helpText("Custom names separate by ',' or ';' (and spaces) will replace default names."),
                          textInput("namesSpeciesLOG", "names of species"),
                        ),
                        conditionalPanel(
                          condition = "input.CustomLOG",
                          tags$hr(),
                          numericInput("tStartLOG", "start time of the simulation", value = 0, min = 0, max = 10000, step = 100),
                        ),
                        numericInput("tEndLOG", "final time of the simulation", value = 1000, min = 100, max = 10000, step = 100),
                        conditionalPanel(
                          condition = "input.CustomLOG",
                          numericInput("tStepLOG", "time step of the simulation", value = 0.1, min = 0.01, max = 10, step = 0.01),
                          numericInput("tStoreLOG", "stored time points of the simulation", value = 1000, min = 100, max = 10000, step = 100),
                        ),
                      ),
                      ### Growth rates ####
                      tabPanel(
                        "Growth and death rates",
                        textInput("x0LOG", "initial abundances of species"),
                        verbatimTextOutput("x0LOGOutput"),
                        textInput("growthRatesLOG", "growth rates of species"),
                        verbatimTextOutput("growthRatesLOGOutput"),
                        textInput("deathRatesLOG", "death rates of species"),
                        verbatimTextOutput("deathRatesLOGOutput"),
                        textInput("carryingCapacitiesLOG", "carrying capacity of species"),
                        verbatimTextOutput("carryingCapacitiesLOGOutput"),

                      ),
                      ### Perturbations ####
                      tabPanel(
                        "Perturbations",
                        sliderInput(
                          "errorVarianceLOG",
                          "variance of measurement error",
                          value = 0,
                          min = 0,
                          max = 1000,
                          step = 10) %>%
                          shinyInput_label_embed(
                            shiny_iconlink("circle-info") %>%
                              bs_embed_tooltip(
                                title =  "The variance of measurement error. By default it equals to 0, indicating that the result won't contain any measurement error.",
                                placement = "right",
                                container = "body"
                              )
                          ),

                        switchInput(
                          "stochasticLOG",
                          strong("use stochasticity"),
                          value = TRUE,
                          labelWidth = "100%"),

                        conditionalPanel(
                          condition = "input.stochasticLOG",
                          sliderInput(
                            "sigmaDriftLOG",
                            "strength of drift",
                            value = 0.001,
                            min = 0,
                            max = 1,
                            step = 0.001
                          ) %>%
                            shinyInput_label_embed(
                              shiny_iconlink("circle-info") %>%
                                bs_embed_tooltip(
                                  title =  "drift happens on each step of simulation",
                                  placement = "right",
                                  container = "body"
                                )
                            ),
                          tags$hr(),

                          sliderInput(
                            "epochPLOG",
                            "probability of random periodic (epoch) changes",
                            value = 0.001,
                            min = 0,
                            max = 1,
                            step = 0.001
                          ) %>%
                            shinyInput_label_embed(
                              shiny_iconlink("circle-info") %>%
                                bs_embed_tooltip(
                                  title =  "microbial epoch perturbations happens by chance",
                                  placement = "right",
                                  container = "body"
                                )
                            ),
                          conditionalPanel(
                            condition = "input.epochPLOG > 0",
                            sliderInput("sigmaEpochLOG", "strength of microbial epoch perturbation", value = 0.001, min = 0, max = 1, step = 0.001),
                          ),
                          tags$hr(),
                          sliderInput("sigmaExternalLOG", "strength of external perturbations", value = 0.3, min = 0, max = 1, step = 0.001),
                          conditionalPanel(
                            condition = "input.sigmaExternalLOG > 0",
                            textInput("tExternalEventsLOG", "timepoints of external events"),
                            verbatimTextOutput("tExternalEventsLOGOutput"),
                            textInput("tExternalDurationsLOG", "time durations of external events"),
                            verbatimTextOutput("tExternalDurationsLOGOutput"),
                          ),
                          tags$hr()
                        ),
                        sliderInput("migrationPLOG", "probability/frequency of migration from metacommunity", value = 0.01, min = 0, max = 1, step = 0.01),
                        conditionalPanel(
                          condition = "input.migrationPLOG >0",
                          sliderInput("sigmaMigrationLOG", "intensity of migration", value = 0.01, min = 0, max = 1, step = 0.001),
                        ),
                        textInput(
                          "metacommunityProbabilityLOG",
                          "metacommunity"
                        ) %>%
                          shinyInput_label_embed(
                            shiny_iconlink("circle-info") %>%
                              bs_embed_tooltip(
                                title =  "Normalized probability distribution of the likelihood that species from the metacommunity can enter the community during the simulation.",
                                placement = "right",
                                container = "body"
                              )
                          ),
                        verbatimTextOutput("metacommunityProbabilityLOG"),
                      ),
                    ),

                    tags$hr(),
                    switchInput(
                      "normLOG",
                      strong("returns normalized abundances"),
                      value = FALSE,
                      labelWidth = "100%"
                    ),

                    # tags$hr(),
                    # tags$h4(
                    #     "Press the following button to run the model",
                    #     tags$div(
                    #         class = "pull-right",
                    #         shiny_iconlink("circle-info") %>%
                    #             bs_embed_tooltip(
                    #                 title =  "Logistic model was not designed responsive to reduce the calculation",
                    #                 placement = "left",
                    #                 container = "body"
                    #             ),
                    #     ),
                    # ),
                    # actionButton("buttonSimulateLOG", "Run the Logistic Model", class = "btn btn-primary", width = "100%"),
                  )
                ),

                ### Display Panel ####
                column(
                  width = 7,
                  #### example buttons ####
                  fluidRow(
                    style = "padding-left: 15px; padding-right: 15px;",
                    tags$div(
                      class = "panel panel-default",
                      tags$div(
                        class = "panel-heading",
                        tags$h3(
                          class = "panel-title",
                          "Examples",
                        ),
                      ),
                      tags$div(
                        class = "panel-body",
                        actionButton(
                          inputId = "LOGEX1",
                          label = "Ex.1",
                          style = "width:24%",
                          class = "btn-primary"
                        ),
                        actionButton(
                          inputId = "LOGEX2",
                          label = "Ex.2",
                          style = "width:24%",
                          class = "btn-primary"
                        ),
                        actionButton(
                          inputId = "LOGEX3",
                          label = "Ex.3",
                          style = "width:24%",
                          class = "btn-primary"
                        ),
                        actionButton(
                          inputId = "LOGEX4",
                          label = "Ex.4",
                          style = "width:24%",
                          class = "btn-primary",
                        ),
                      ),
                    ),
                  ),



                  #### Species Change ####
                  tags$br(),
                  fluidRow(
                    style = "padding-left: 15px; padding-right: 15px;",
                    tags$div(
                      class = "panel panel-default",
                      tags$div(
                        class = "panel-heading",
                        tags$h3(
                          class = "panel-title",
                          "Species Change",
                        ),
                      ),
                      tags$div(
                        class = "panel-body",
                        plotOutput("LOGSpecies"),
                      ),
                    ),
                  ),
                )
              )
          ) %>%
          bs_append(
            ## Description ####
            title = "Description",
            content =
              fluidRow(
                column(
                  width = 12,
                  withMathJax(includeMarkdown(app_sys("app/www/log.Rmd"))),
                ),
              )
          ) %>%
          bs_append(
            ## Inputs ####
            title = "Inputs",
            content =
              fluidRow(
                column(
                  width = 12,
                  withMathJax(includeMarkdown(app_sys("app/www/log_parms.Rmd"))),
                ),
              )
          ) %>%
          bs_append(
            ## References ####
            title = "References",
            content = "Panel of refs."
          )
      )
    )

  )
}

#' Add external Resources to the Application
#'
#' This function is internally used to add external
#' resources inside the Shiny application.
#'
#' @import shiny
#' @importFrom golem add_resource_path activate_js favicon bundle_resources
#' @noRd
golem_add_external_resources <- function() {
  add_resource_path(
    "www",
    app_sys("app/www")
  )

  tags$head(
    favicon(),
    bundle_resources(
      path = app_sys("app/www"),
      app_title = "miaSimShiny"
    )
    # Add here other external resources
    # for example, you can add shinyalert::useShinyalert()
  )
}
