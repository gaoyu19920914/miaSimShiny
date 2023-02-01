#' The application server-side
#'
#' @param input,output,session Internal parameters for {shiny}.
#'     DO NOT REMOVE.
#' @import shiny
#' @noRd
app_server <- function(input, output, session) {
  # model1 simulate consumer resource model ####
  ## basic ####
  n_species_crm <- reactive(input$nSpeciesCRM)
  n_resources_crm <- reactive(input$nResourcesCRM)
  names_species_crm <- reactive(text2chars(input$namesSpeciesCRM, len = n_species_crm(), prefix = "sp"))
  names_resources_crm <- reactive(text2chars(input$namesResourcesCRM, len = n_resources_crm(), prefix = "res"))
  t_start_crm <- reactive(input$tStartCRM)
  t_end_crm <- reactive(input$tEndCRM)
  observeEvent(input$tStartCRM | input$tEndCRM, {
    updateNumericInput(inputId = "tStartCRM", max = input$tEndCRM)
    updateNumericInput(inputId = "tEndCRM", min = input$tStartCRM)
  })

  t_step_crm <- reactive(input$tStepCRM)
  t_store_crm <- reactive(input$tStoreCRM)
  observeEvent(input$tStartCRM | input$tEndCRM | input$tStepCRM | input$tStoreCRM, {
    updateNumericInput(inputId = "tStepCRM", max = (input$tEndCRM-input$tStartCRM)/input$tStoreCRM)
    updateNumericInput(inputId = "tStoreCRM", max = (input$tEndCRM-input$tStartCRM)/input$tStepCRM)
  })

  ## compounds ####
  res_conc_crm <- reactive(input$resourcesConcentrationCRM)
  res_even_crm <- reactive(input$resourcesEvennessCRM)
  resources_dist_crm <- reactive(rdirichlet(1, rep(1, n_resources_crm())*res_even_crm())*res_conc_crm()*n_resources_crm())
  res_custom_crm <- reactive(as.numeric(as.vector(text2char(input$resourcesCustomCRM))))
  resources_crm <- reactive({
    if (length(res_custom_crm()) < n_resources_crm()){
      return(c(res_custom_crm(), as.vector(resources_dist_crm())[(length(res_custom_crm())+1):n_resources_crm()]))
    } else {
      return(utils::head(res_custom_crm(), n_resources_crm()))
    }
  })
  output$resourcesOutputCRM <- renderPrint(resources_crm())

  ### dilution/influx and outflux ####
  inflow_rate_crm <- reactive(input$inflowRateCRM)
  outflow_rate_crm <- reactive(input$outflowRateCRM)
  volume_crm <- reactive(input$volumeCRM)
  res_dilu_crm <- reactive(as.numeric(as.vector(text2char(input$resourcesDilutionCRM))))
  resources_dilution_crm <- reactive({
    if (length(res_dilu_crm()) < n_resources_crm()){
      return(c(res_dilu_crm(), resources_crm()[(length(res_dilu_crm())+1):n_resources_crm()]))
    } else {
      return(utils::head(res_dilu_crm(), n_resources_crm()))
    }
  })
  output$resourcesDilutionCRMOutput <- renderPrint(resources_dilution_crm())

  output$resourcesCRMPlot <- renderPlot(makePiePlot(resources_crm(), label = 'concentration', title='compounds'))
  mean_consumption_crm <- reactive(input$meanConsumptionCRM)
  mean_production_crm <- reactive(input$meanProductionCRM)
  observeEvent(input$meanConsumptionCRM | input$meanProductionCRM, {
    updateSliderInput(inputId = "meanProductionCRM", max = 1 - input$meanConsumptionCRM)
    updateSliderInput(inputId = "meanConsumptionCRM", max = 1 - input$meanProductionCRM)
  })
  maintenance_crm <- reactive(input$maintenanceCRM)

  ### editable matrixECRM and matrixMonodCRM ####
  RV_crm <- reactiveValues(matrixECRM = NULL, matrixMonodCRM = NULL)
  observe({
    roundECRM <- round(
      randomE(
        n_species = n_species_crm(),
        n_resources = n_resources_crm(),
        names_species = names_species_crm(),
        names_resources = names_resources_crm(),
        mean_consumption = as.integer(mean_consumption_crm() * n_resources_crm()),
        mean_production = as.integer(mean_production_crm() * n_resources_crm()),
        maintenance = maintenance_crm()
      ),
      digits = 3
    )
    RV_crm$matrixECRM <- roundECRM
  })
  output$tableECRM <- DT::renderDataTable(RV_crm$matrixECRM, editable = 'all', selection = 'none', server = TRUE, options = list(scrollX = TRUE))
  output$CRMPlotE <- renderPlot(makeHeatmap(RV_crm$matrixECRM, 'Consumption/production matrix'), res = 96)

  observeEvent(input$tableECRM_cell_edit, {
    RV_crm$matrixECRM <<- DT::editData(RV_crm$matrixECRM, input$tableECRM_cell_edit, 'tableECRM')
  })


  ## growth rates ####
  x0_crm <- reactive(as.numeric(text2chars(input$x0CRM, len = n_species_crm(), expr = paste0("runif(n = ", n_species_crm() ,", min = 0.1, max = 10)"))))
  output$x0CRMOutput <- renderPrint(x0_crm())


  alpha_crm <- reactive(input$alphaCRM)
  beta_crm <- reactive(input$betaCRM)
  # listening to the changes in nSpeciesCRM, alphaCRM, and betaCRM
  observeEvent(input$nSpeciesCRM | input$alphaCRM | input$betaCRM, {
    growth_rates_crm <- reactive(as.numeric(text2chars(input$growthRatesCRM, len = n_species_crm(), expr = paste0('round(rbeta(',n_species_crm(), ',' ,alpha_crm(), ',' , beta_crm(),'), digits = 3)'))))
  })
  observeEvent(input$nSpeciesCRM, {
    x0_crm <- reactive(as.numeric(text2chars(input$x0CRM, len = n_species_crm(), expr = paste0("runif(n = ", n_species_crm() ,", min = 0.1, max = 10)"))))
  })
  observeEvent(input$buttonBetaEven, {
    updateSliderInput(inputId = "alphaCRM", value = 1)
    updateSliderInput(inputId = "betaCRM", value = 1)
  })
  observeEvent(input$buttonBetaRidge, {
    updateSliderInput(inputId = "alphaCRM", value = 4)
    updateSliderInput(inputId = "betaCRM", value = 4)
  })
  observeEvent(input$buttonBetaValley, {
    updateSliderInput(inputId = "alphaCRM", value = 0.5)
    updateSliderInput(inputId = "betaCRM", value = 0.5)
  })
  observeEvent(input$buttonBetaLeft, {
    updateSliderInput(inputId = "alphaCRM", value = 0.5)
    updateSliderInput(inputId = "betaCRM", value = 1)
  })
  observeEvent(input$buttonBetaRight, {
    updateSliderInput(inputId = "alphaCRM", value = 1)
    updateSliderInput(inputId = "betaCRM", value = 0.5)
  })
  observeEvent(input$buttonBetaLeftTriangle, {
    updateSliderInput(inputId = "alphaCRM", value = 1)
    updateSliderInput(inputId = "betaCRM", value = 2)
  })
  observeEvent(input$buttonBetaRightTriangle, {
    updateSliderInput(inputId = "alphaCRM", value = 2)
    updateSliderInput(inputId = "betaCRM", value = 1)
  })

  growth_rates_crm <- reactive(as.numeric(text2chars(input$growthRatesCRM, len = n_species_crm(), expr = paste0('round(rbeta(',n_species_crm(), ',' ,alpha_crm(), ',' , beta_crm(),'), digits = 3)'))))
  output$growthRatesCRMOutput <- renderPrint(growth_rates_crm())

  output$growthRatesCRMDist <- renderPlot({
    ggplot(data.frame(x = c(-4, 4)), aes(x = x)) +
      stat_function(fun = dbeta, args = list(shape1=alpha_crm(), shape2=beta_crm())) +
      xlim(0,1) + theme_linedraw()
  }, res = 96)
  observe({
    roundMonodCRM <- round(
      matrix(
        rgamma(
          n = n_species_crm()*n_resources_crm(),
          shape = 50*max(resources_crm()),
          rate = 1),
        nrow = n_species_crm(),
        dimnames = list(names_species_crm(), names_resources_crm())
      ),
      digits = 3)
    RV_crm$matrixMonodCRM <- roundMonodCRM
  })
  output$tableMonodCRM <- DT::renderDataTable(RV_crm$matrixMonodCRM, editable = 'all', selection = 'none', server = TRUE, options = list(scrollX = TRUE))
  observeEvent(input$tableMonodCRM_cell_edit, {
    RV_crm$matrixMonodCRM <<- DT::editData(RV_crm$matrixMonodCRM, input$tableMonodCRM_cell_edit, 'tableMonodCRM')
  })
  ## perturbation ####
  error_variance_crm <- reactive(input$errorVarianceCRM)
  stochastic_crm <- reactive(input$stochasticCRM)
  sigma_drift_crm <- reactive(input$sigmaDriftCRM)
  sigma_epoch_crm <- reactive(input$sigmaEpochCRM)
  sigma_external_crm <- reactive(input$sigmaExternalCRM)
  sigma_migration_crm <- reactive(input$sigmaMigrationCRM)
  epoch_p_crm <- reactive(input$epochPCRM)
  t_external_events_crm <- reactive(as.numeric(text2char(input$tExternalEventsCRM)))
  output$tExternalEventsCRMOutput <- renderPrint(t_external_events_crm())
  t_external_durations_crm <- reactive(as.numeric(text2char(input$tExternalDurationsCRM)))
  output$tExternalDurationsCRMOutput <- renderPrint(t_external_durations_crm())
  migration_p_crm <- reactive(input$migrationPCRM)
  metacommunity_probability_crm <- reactive(as.numeric(text2chars(input$metacommunityProbabilityCRM, len = n_species_crm(), expr = paste0("rdirichlet(1, alpha = rep(1,", n_species_crm(), "))"))))
  output$metacommunityProbabilityCRM <- renderPrint(metacommunity_probability_crm())


  norm_crm <- reactive(input$normCRM)

  ## examples CRM ####
  observeEvent(input$CRMEX1, {
    updateSliderInput(inputId = "nSpeciesCRM", value = 5)
    updateSliderInput(inputId = "nResourcesCRM", value = 5)
  })
  observeEvent(input$CRMEX2, {
    updateNumericInput(inputId = "tEndCRM", value = 2000)
    updateNumericInput(inputId = "tStoreCRM", value = 500)
    updateSliderInput(inputId = "migrationPCRM", value = 0)
    updateCheckboxInput(inputId = "stochasticCRM", value = FALSE)
    updateSliderInput(inputId = "dilutionRateCRM", value = 0.001)
  })
  observeEvent(input$CRMEX3, {
    updateSliderInput(inputId = "nSpeciesCRM", value = 1)
    updateSliderInput(inputId = "nResourcesCRM", value = 10)
    updateSliderInput(inputId = "maintenanceCRM", value = 0.1)
  })
  observeEvent(input$CRMEX4pre, {
    updateSliderInput(inputId = "nSpeciesCRM", value = 3)
    updateSliderInput(inputId = "nResourcesCRM", value = 4)
    updateTextInput(inputId = "growthRatesCRM", value = "2, 4.5, 2.6")
    updateTextInput(inputId = "x0CRM", value = "1, 2, 1")
    updateTextInput(inputId = "resourcesCustomCRM", value = "10, 0, 0, 0")
    updateTextInput(inputId = "namesSpeciesCRM", value = "homoacetogenic, homofermentative, butyrateProducer")
    updateTextInput(inputId = "namesResourcesCRM", value = "glucose, acetate, lactate, butyrate")
    # updateButton(session, "CRMEX4", disabled = !input$CRMEX4pre)
    shinyjs::enable("CRMEX4")
  })
  observeEvent(input$CRMEX4, {
    # auto update matrixECRM, then take changes after it.
    matrixExample4 <- matrix(c(1, -3, 0, 0, 1, 0, -2, 0, 0, 0, 4, -3), nrow = 3, byrow = TRUE)*c(4.3/4, 2/4, 1/4)
    RV_crm$matrixECRM <- matrixExample4
  })
  observeEvent(input$CRMEX5pre, {
    updateSliderInput(inputId = "nSpeciesCRM", value = 10)
    updateSliderInput(inputId = "nResourcesCRM", value = 10)
    # updateButton(session, "CRMEX5", disabled = !input$CRMEX5pre)
    shinyjs::enable("CRMEX5")
  })
  observeEvent(input$CRMEX5, {
    RV_crm$matrixECRM <- randomE(n_species = 10, n_resources = 10, mean_consumption = 3, mean_production = 1,
                                 maintenance = 0.5, trophic_preferences = list(c(5,3,1,1,1,1,1,1,1,1)))
  })
  observeEvent(input$CRMEX6pre, {
    updateSliderInput(inputId = "nSpeciesCRM", value = 20)
    updateSliderInput(inputId = "nResourcesCRM", value = 20)
    # updateButton(session, "CRMEX6", disabled = !input$CRMEX6pre)
    shinyjs::enable("CRMEX6")
  })
  observeEvent(input$CRMEX6, {
    RV_crm$matrixECRM <- randomE(n_species = 20, n_resources = 20, mean_consumption = 3, mean_production = 2, maintenance = 0.0, trophic_levels = c(7, 13))
  })
  observeEvent(input$CRMEX7pre, {
    updateSliderInput(inputId = "nSpeciesCRM", value = 4)
    updateSliderInput(inputId = "nResourcesCRM", value = 11)
    updateTextInput(inputId = "namesSpeciesCRM", value = "A, B, C, D")
    updateTextInput(inputId = "x0CRM", value = "1, 1, 1, 1")
    updateTextInput(inputId = "resourcesCustomCRM", value = "1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5, 1.5")
    updateCheckboxInput(inputId = "stochasticCRM", value = FALSE)
    updateSliderInput(inputId = "migrationPCRM", value = 0)
    updateSliderInput(inputId = "dilutionRateCRM", value = 0)
    # updateButton(session, "CRMEX7", disabled = !input$CRMEX7pre)
    shinyjs::enable("CRMEX7")
  })
  observeEvent(input$CRMEX7, {
    #secretion of C
    sec.C <- rdirichlet(1, c(1,1,1))*.5
    #The metabolic preferences of A are set to the secretion products of C
    pref.A.D <- list(c(sec.C*1000, rep(1,8)))
    em.A <- randomE(n_species = 1, n_resources = 11, names_species = 'A', trophic_preferences= pref.A.D, mean_production = 3, mean_consumption = 3)
    #secretion of A
    sec.A <- abs(em.A*(em.A<0))
    #The metabolic preferences of D are set to the secretion products of A
    em.D <- randomE(n_species = 1, n_resources = 11, names_species = 'D', trophic_preferences= pref.A.D, mean_production = 3, mean_consumption = 3)
    #secretion of D
    sec.D <- abs(em.D*(em.D<0))
    pref.B <- 1000*((sec.A + sec.D)/(sum(sec.A)+sum(sec.D)))
    pref.B[pref.B==0] <- 1
    pref.B <- list(pref.B[4:11])
    em.B <- randomE(n_species = 1, n_resources = 8, names_species = 'B', trophic_preferences= pref.B, mean_production = 3, mean_consumption = 3)
    #secretion of B
    sec.B <- abs(em.B*(em.B<0))
    #The metabolic preferences of C are set to the secretion products B
    pref.C <- sec.B*1000
    pref.C[pref.C==0] <- 1
    em.B <-t(as.matrix(c(rep(0,3),em.B)))
    row.names(em.B) = 'B'
    em.C <- randomE(n_species = 1, n_resources = 8, names_species = 'C', trophic_preferences= list(pref.C), mean_production = 0, mean_consumption = 3)
    em.C <- cbind(-sec.C, em.C)
    RV_crm$matrixECRM <- rbind(em.A, em.B, em.C, em.D)
  })

  ## runCRM ####
  runCRM <- reactive({
    # runCRM <- eventReactive(input$buttonSimulateCRM, {
    simulateConsumerResource(
      n_species = n_species_crm(),
      n_resources = n_resources_crm(),
      names_species = names_species_crm(),
      names_resources = names_resources_crm(),
      E = RV_crm$matrixECRM,
      x0 = x0_crm(),
      resources = resources_crm(),
      resources_dilution = resources_dilution_crm(),
      inflow_rate = inflow_rate_crm(),
      outflow_rate = outflow_rate_crm(),
      volume = volume_crm(),
      growth_rates = growth_rates_crm(),
      monod_constant = RV_crm$matrixMonodCRM,
      sigma_drift = sigma_drift_crm(),
      sigma_epoch = sigma_epoch_crm(),
      sigma_external = sigma_external_crm(),
      sigma_migration = sigma_migration_crm(),
      epoch_p = epoch_p_crm(),
      t_external_events = t_external_events_crm(),
      t_external_durations = t_external_durations_crm(),
      stochastic = stochastic_crm(),
      migration_p = migration_p_crm(),
      metacommunity_probability = metacommunity_probability_crm(),
      error_variance = error_variance_crm(),
      norm = norm_crm(),
      t_start = t_start_crm(),
      t_end = t_end_crm(),
      t_step = t_step_crm(),
      t_store = t_store_crm()
    )}
  )

  output$CRMSpecies <- renderPlot(plotSeries(runCRM(), x = "time"))
  output$CRMResources <- renderPlot(makePlotRes(metadata(runCRM())$resources, "quantity of compounds by time"),  res = 96)
  output$CRMVolume <- renderPlot(makePlot(metadata(runCRM())$volume, "volume changes of the reactor by time"), res = 96)


  # model2 simulate generalized Lotka-Volterra Model ####
  ## interspecies interactions ####
  n_species_glv <- reactive(input$nSpeciesGLV)
  names_species_glv <- reactive(text2chars(input$namesSpeciesGLV, len = n_species_glv(), prefix = "sp"))
  diagonal_glv <- reactive(input$diagonalGLV)
  connectance_glv <- reactive(input$connectanceGLV)
  scale_glv <- reactive(input$scaleGLV)

  mutualism_glv <- reactive(input$mutualismGLV)
  commensalism_glv <- reactive(input$commensalismGLV)
  parasitism_glv <- reactive(input$parasitismGLV)
  amensalism_glv <- reactive(input$amensalismGLV)
  competition_glv <- reactive(input$competitionGLV)

  observeEvent(input$buttonInteractionsU, {
    updateTextInput(inputId = "interactionsCustomGLV", value = round(rbeta(n_species_glv()^2, 0.5, 0.5), digits = 3))
  })
  observeEvent(input$buttonInteractionsN, {
    updateTextInput(inputId = "interactionsCustomGLV", value = round(rbeta(n_species_glv()^2, 2, 2), digits = 3))
  })
  observeEvent(input$buttonInteractionsEven, {
    updateTextInput(inputId = "interactionsCustomGLV", value = round(runif(n_species_glv()^2, 0, 1), digits = 3))
  })
  interactions_dist_glv <- reactive(round(runif(n_species_glv()^2, 0, 1), digits = 3))
  inter_custom_glv <- reactive(as.numeric(as.vector(text2char(input$interactionsCustomGLV))))
  interactions_glv <- reactive({
    if (length(inter_custom_glv()) < n_species_glv()^2){
      return(
        c(
          inter_custom_glv(),
          as.vector(
            interactions_dist_glv()[(length(inter_custom_glv())+1):(n_species_glv()^2)]
          )
        )
      )
    } else {
      return(utils::head(inter_custom_glv(), n_species_glv()^2))
    }
  })
  output$interactionsOutputGLV <- renderPrint(interactions_glv())
  symmetric_glv <- reactive(input$symmetricGLV)
  list_A_glv <- reactive(text2char(input$list_AGLV)) # TODO: convert list_A

  RV_glv <- reactiveValues(matrixAGLV = NULL)
  # replace generateA() by RV_glv$matrixAGLV

  #### editable matrixA ####
  observe({
    roundACRM <- round(
      randomA(
        n_species = n_species_glv(),
        names_species = names_species_glv(),
        diagonal = diagonal_glv(),
        connectance = connectance_glv(),
        scale_off_diagonal = scale_glv(),
        mutualism = mutualism_glv(),
        commensalism = commensalism_glv(),
        parasitism = parasitism_glv(),
        amensalism = amensalism_glv(),
        competition = competition_glv(),
        interactions = interactions_glv(),
        symmetric = symmetric_glv()
      ),
      digits = 3
    )
    RV_glv$matrixAGLV <- roundACRM
  })
  output$TableAGLV <- DT::renderDataTable(RV_glv$matrixAGLV, editable = 'all', selection = 'none', server = TRUE, options = list(scrollX = TRUE))
  output$GLVPlotA <- renderPlot(makeHeatmap(RV_glv$matrixAGLV, "interspecies interaction matrix"), res = 96)

  observeEvent(input$tableAGLV_cell_edit, {
    RV_glv$matrixAGLV <<- DT::editData(RV_glv$matrixAGLV, input$tableAGLV_cell_edit, 'tableAGLV')
  })

  ## growth rates ####
  x0_glv <- reactive(as.numeric(text2chars(input$x0GLV, len = n_species_glv(), expr = paste0("runif(n =", n_species_glv(), ", min = 0, max = 1)"))))
  output$x0GLVOutput <- renderPrint(x0_glv())
  growth_rates_glv <- reactive(as.numeric(text2chars(input$growthRatesGLV, len = n_species_glv(), expr = paste0("runif(n =", n_species_glv(), ", min = 0, max = 1)"))))
  output$growthRatesGLVOutput <- renderPrint(growth_rates_glv())
  ## perturbations ####
  stochastic_glv <- reactive(input$stochasticGLV)
  sigma_drift_glv <- reactive(input$sigmaDriftGLV)
  sigma_epoch_glv <- reactive(input$sigmaEpochGLV)
  sigma_external_glv <- reactive(input$sigmaExternalGLV)
  epoch_p_glv <- reactive(input$epochPGLV)
  sigma_migration_glv <- reactive(input$sigmaMigrationGLV)
  t_external_events_glv <- reactive(as.numeric(text2char(input$tExternalEventsGLV)))
  output$tExternalEventsGLVOutput <- renderPrint(t_external_events_glv())
  t_external_durations_glv <- reactive(as.numeric(text2char(input$tExternalDurationsGLV)))
  output$tExternalDurationsGLVOutput <- renderPrint(t_external_durations_glv())
  migration_p_glv <- reactive(input$migrationPGLV)
  metacommunity_probability_glv <- reactive(as.numeric(text2chars(input$metacommunityProbabilityGLV, len = n_species_glv(), expr = paste0("rdirichlet(1, alpha = rep(1,", n_species_glv(), "))"))))
  output$metacommunityProbabilityGLV <- renderPrint(metacommunity_probability_glv())

  error_variance_glv <- reactive(input$errorVarianceGLV)
  t_start_glv <- reactive(input$tStartGLV)
  t_end_glv <- reactive(input$tEndGLV)
  t_step_glv <- reactive(input$tStepGLV)
  t_store_glv <- reactive(input$tStoreGLV)

  norm_glv <- reactive(input$normGLV)

  ## examples GLV ####
  observeEvent(input$GLVEX1, {
    updateSliderInput(inputId = "nSpeciesGLV", value = 5)
  })
  observeEvent(input$GLVEX2, {
    updateSliderInput(inputId = "nSpeciesGLV", value = 4)
    updateSliderInput(inputId = "diagonalGLV", value = -1)
    updateSliderInput(inputId = "connectanceGLV", value = 0.5)
    updateSliderInput(inputId = "scaleGLV", value = 0.5)
    updateSwitchInput(inputId = "symmetricGLV", value = TRUE)
    updateSwitchInput(inputId = "stochasticGLV", value = FALSE)
  })
  observeEvent(input$GLVEX3, {
    updateSliderInput(inputId = "nSpeciesGLV", value = 4)
    updateSliderInput(inputId = "diagonalGLV", value = -1)
    updateSliderInput(inputId = "connectanceGLV", value = 0.5)
    updateSliderInput(inputId = "scaleGLV", value = 0.5)
    updateSwitchInput(inputId = "symmetricGLV", value = TRUE)
    updateSwitchInput(inputId = "stochasticGLV", value = FALSE)
    updateSliderInput(inputId = "migrationPGLV", value = 0)
  })
  observeEvent(input$GLVEX4, {
    updateSliderInput(inputId = "nSpeciesGLV", value = 4)
    updateSliderInput(inputId = "diagonalGLV", value = -1)
    updateSliderInput(inputId = "connectanceGLV", value = 0.5)
    updateSliderInput(inputId = "scaleGLV", value = 0.5)
    updateSwitchInput(inputId = "symmetricGLV", value = TRUE)
    updateSwitchInput(inputId = "stochasticGLV", value = FALSE)
    updateSliderInput(inputId = "migrationPGLV", value = 0)
    updateSliderInput(inputId = "errorVarianceGLV", value = 0.1)
  })

  ## runGLV ####

  # use button or not?
  runGLV <- reactive({
    # runGLV <- eventReactive(input$buttonSimulateGLV, {
    simulateGLV(
      n_species = n_species_glv(),
      names_species = names_species_glv(),
      A = RV_glv$matrixAGLV,
      x0 = x0_glv(),
      growth_rates = growth_rates_glv(),
      sigma_drift = sigma_drift_glv(),
      sigma_epoch = sigma_epoch_glv(),
      sigma_external = sigma_external_glv(),
      sigma_migration = sigma_migration_glv(),
      epoch_p = epoch_p_glv(),
      t_external_events = t_external_events_glv(),
      t_external_durations = t_external_durations_glv(),
      stochastic = stochastic_glv(),
      migration_p = migration_p_glv(),
      metacommunity_probability = metacommunity_probability_glv(),
      error_variance = error_variance_glv(),
      norm = norm_glv(),
      t_start = t_start_glv(),
      t_end = t_end_glv(),
      t_step = t_step_glv(),
      t_store = t_store_glv()
    )
  })
  output$GLVSpecies <- renderPlot(plotSeries(runGLV(), x = "time"))

  # model3 simulate Hubbell neutral model with growth rates ####
  ## basic ####
  n_species_hub <- reactive(input$nSpeciesHUB)
  x0_hub <- reactive(as.numeric(text2chars(input$x0HUB, len = n_species_hub(), expr = paste0("rep(100, ", n_species_hub() ,")"))))
  output$x0HUBOutput <- renderPrint(x0_hub())
  names_species_hub <- reactive(text2chars(input$namesSpeciesHUB, len = n_species_hub(), prefix = "sp"))
  growth_rates_hub <- reactive(as.numeric(text2chars(input$growthRatesHUB, len = n_species_hub(), expr = paste0('rep(1, ',n_species_hub(), ')'))))
  output$growthRatesHUBOutput <- renderPrint(growth_rates_hub())

  t_start_hub <- reactive(input$tStartHUB)
  t_end_hub <- reactive(input$tEndHUB)
  observeEvent(input$tStartHUB | input$tEndHUB, {
    updateNumericInput(inputId = "tStartHUB", max = input$tEndHUB)
    updateNumericInput(inputId = "tEndHUB", min = input$tStartHUB)
  })

  t_step_hub <- reactive(input$tStepHUB)
  t_store_hub <- reactive(input$tStoreHUB)
  observeEvent(input$tStartHUB | input$tEndHUB | input$tStepHUB | input$tStoreHUB, {
    updateNumericInput(inputId = "tStepHUB", max = (input$tEndHUB-input$tStartHUB)/input$tStoreHUB)
    updateNumericInput(inputId = "tStoreHUB", max = (input$tEndHUB-input$tStartHUB)/input$tStepHUB)
  })

  ## perturbations ####
  error_variance_hub <- reactive(input$errorVarianceHUB)
  k_events_hub <- reactive(input$kEventsHUB)
  migration_p_hub <- reactive(input$migrationPHUB)
  metacommunity_probability_hub <- reactive(as.numeric(text2chars(input$metacommunityProbabilityHUB, len = n_species_hub(), expr = paste0("rdirichlet(1, alpha = rep(1,", n_species_hub(), "))"))))
  output$metacommunityProbabilityHUB <- renderPrint(metacommunity_probability_hub())

  norm_hub <- reactive(input$normHUB)

  ## examples HUB ####
  observeEvent(input$HUBEX1, {
    updateSliderInput(inputId = "nSpeciesHUB", value = 5)
  })
  observeEvent(input$HUBEX2, {
    updateSliderInput(inputId = "nSpeciesHUB", value = 5)
    updateSliderInput(inputId = "migrationPHUB", value = 0)
  })
  observeEvent(input$HUBEX3, {
    updateSliderInput(inputId = "nSpeciesHUB", value = 5)
    updateSliderInput(inputId = "migrationPHUB", value = 1)
    updateTextInput(inputId = "metacommunityProbabilityHUB", value = "0.1, 0.15, 0.2, 0.25, 0.3")
    updateTextInput(inputId = "tEndHUB", value = 20)
    updateTextInput(inputId = "tStoreHUB", value = 200)
  })
  observeEvent(input$HUBEX4, {
    updateSliderInput(inputId = "nSpeciesHUB", value = 5)
    updateSliderInput(inputId = "migrationPHUB", value = 1)
    updateTextInput(inputId = "metacommunityProbabilityHUB", value = "0.1, 0.15, 0.2, 0.25, 0.3")
    updateTextInput(inputId = "tEndHUB", value = 20)
    updateTextInput(inputId = "tStoreHUB", value = 200)
    updateSliderInput(inputId = "errorVarianceHUB", value = 100)
  })
  observeEvent(input$HUBEX5, {
    updateSliderInput(inputId = "nSpeciesHUB", value = 5)
    updateSliderInput(inputId = "migrationPHUB", value = 0.1)
    updateTextInput(inputId = "metacommunityProbabilityHUB", value = "0.1, 0.15, 0.2, 0.25, 0.3")
    updateTextInput(inputId = "tEndHUB", value = 20)
    updateTextInput(inputId = "tStoreHUB", value = 200)
    updateSliderInput(inputId = "kEventsHUB", value = 5)
    updateTextInput(inputId = "growthRatesHUB", value = "1.1, 1.05, 1, 0.95, 0.9")
  })

  ## runHUB ####
  runHUB <- reactive({
    # runHUB <- eventReactive(input$buttonSimulateHUB, {
    simulateHubbellRates(
      n_species = n_species_hub(),
      x0 = x0_hub(),
      names_species = names_species_hub(),
      migration_p = migration_p_hub(),
      metacommunity_probability = metacommunity_probability_hub(),
      k_events = k_events_hub(),
      growth_rates = growth_rates_hub(),
      error_variance = error_variance_hub(),
      norm = norm_hub(),
      t_start = t_start_hub(),
      t_end = t_end_hub(),
      t_step = t_step_hub(),
      t_store = t_store_hub()
    )
  })
  output$HUBSpecies <- renderPlot(plotSeries(runHUB(), x = "time"))

  # model4 simulate stochastic logistic model ####
  ## basic ####
  n_species_log <- reactive(input$nSpeciesLOG)
  names_species_log <- reactive(text2chars(input$namesSpeciesLOG, len = n_species_log(), prefix = "sp"))
  t_start_log <- reactive(input$tStartLOG)
  t_end_log <- reactive(input$tEndLOG)
  observeEvent(input$tStartLOG | input$tEndLOG, {
    updateNumericInput(inputId = "tStartLOG", max = input$tEndLOG)
    updateNumericInput(inputId = "tEndLOG", min = input$tStartLOG)
  })

  t_step_log <- reactive(input$tStepLOG)
  t_store_log <- reactive(input$tStoreLOG)
  observeEvent(input$tStartLOG | input$tEndLOG | input$tStepLOG | input$tStoreLOG, {
    updateNumericInput(inputId = "tStepLOG", max = (input$tEndLOG-input$tStartLOG)/input$tStoreLOG)
    updateNumericInput(inputId = "tStoreLOG", max = (input$tEndLOG-input$tStartLOG)/input$tStepLOG)
  })

  ## growth and death rates ####
  x0_log <- reactive(as.numeric(text2chars(input$x0LOG, len = n_species_log(), expr = paste0("rep(100, ", n_species_log() ,")"))))
  output$x0LOGOutput <- renderPrint(x0_log())
  growth_rates_log <- reactive(as.numeric(text2chars(input$growthRatesLOG, len = n_species_log(), expr = paste0("runif(n = ", n_species_log() ,", min = 0.1, max = 0.2)"))))
  output$growthRatesLOGOutput <- renderPrint(growth_rates_log())
  death_rates_log <- reactive(as.numeric(text2chars(input$deathRatesLOG, len = n_species_log(), expr = paste0("runif(n = ", n_species_log() ,", min = 0.0005, max = 0.0025)"))))
  output$deathRatesLOGOutput <- renderPrint(death_rates_log())
  carrying_capacities_log <- reactive(as.numeric(text2chars(input$carryingCapacitiesLOG, len = n_species_log(), expr = paste0("runif(n = ", n_species_log() ,", min = 1000, max = 2000)"))))
  output$carryingCapacitiesLOGOutput <- renderPrint(carrying_capacities_log())

  ## perturbations ####
  error_variance_log <- reactive(input$errorVarianceLOG)
  stochastic_log <- reactive(input$stochasticLOG)
  sigma_drift_log <- reactive(input$sigmaDriftLOG)
  epoch_p_log <- reactive(input$epochPLOG)
  sigma_epoch_log <- reactive(input$sigmaEpochLOG)
  sigma_external_log <- reactive(input$sigmaExternalLOG)
  t_external_events_log <- reactive(as.numeric(text2char(input$tExternalEventsLOG)))
  output$tExternalEventsLOGOutput <- renderPrint(t_external_events_log())
  t_external_durations_log <- reactive(as.numeric(text2char(input$tExternalDurationsLOG)))
  output$tExternalDurationsLOGOutput <- renderPrint(t_external_durations_log())
  migration_p_log <- reactive(input$migrationPLOG)
  sigma_migration_log <- reactive(input$sigmaMigrationLOG)
  metacommunity_probability_log <- reactive(as.numeric(text2chars(input$metacommunityProbabilityLOG, len = n_species_log(), expr = paste0("rdirichlet(1, alpha = rep(1,", n_species_log(), "))"))))
  output$metacommunityProbabilityLOG <- renderPrint(metacommunity_probability_log())

  norm_log <- reactive(input$normLOG)


  ## examples LOG ####
  observeEvent(input$LOGEX1, {
    updateSliderInput(inputId = "nSpeciesLOG", value = 5)
    updateSwitchInput(inputId = "stochasticLOG", value = FALSE)
  })

  observeEvent(input$LOGEX2, {
    updateSliderInput(inputId = "nSpeciesLOG", value = 5)
    updateSwitchInput(inputId = "stochasticLOG", value = FALSE)
    updateTextInput(inputId = "deathRatesLOG", value = "0,0,0,0,0")
  })

  observeEvent(input$LOGEX3, {
    updateSliderInput(inputId = "nSpeciesLOG", value = 5)
    updateSwitchInput(inputId = "stochasticLOG", value = FALSE)
    updateTextInput(inputId = "deathRatesLOG", value = "0,0,0,0,0")
    updateTextInput(inputId = "growthRatesLOG", value = "0.1, 0.2, 0.3, 0.4, 0.5")
    updateTextInput(inputId = "deathRatesLOG", value = "0.001, 0.0008, 0.0006, 0.0004, 0.0002")
    updateTextInput(inputId = "carryingCapacitiesLOG", value = "1000, 1200, 1400, 1600, 1800")

  })

  observeEvent(input$LOGEX4, {
    updateSliderInput(inputId = "nSpeciesLOG", value = 5)
    updateTextInput(inputId = "deathRatesLOG", value = "0,0,0,0,0")
    updateTextInput(inputId = "growthRatesLOG", value = "0.1, 0.2, 0.3, 0.4, 0.5")
    updateTextInput(inputId = "deathRatesLOG", value = "0.001, 0.0008, 0.0006, 0.0004, 0.0002")
    updateTextInput(inputId = "carryingCapacitiesLOG", value = "1000, 1200, 1400, 1600, 1800")
    updateSliderInput(inputId = "errorVarianceLOG", value = 500)
    updateSwitchInput(inputId = "normLOG", value = TRUE)
  })



  ## runLOG ####
  runLOG <- reactive({
    # runLOG <- eventReactive(input$buttonSimulateLOG, {
    simulateStochasticLogistic(
      n_species = n_species_log(),
      names_species = names_species_log(),
      growth_rates = growth_rates_log(),
      carrying_capacities = carrying_capacities_log(),
      death_rates = death_rates_log(),
      x0 = x0_log(),
      sigma_drift = sigma_drift_log(),
      sigma_epoch = sigma_epoch_log(),
      sigma_external = sigma_external_log(),
      sigma_migration = sigma_migration_log(),
      epoch_p = epoch_p_log(),
      t_external_events = t_external_events_log(),
      t_external_durations = t_external_durations_log(),
      migration_p = migration_p_log(),
      metacommunity_probability = metacommunity_probability_log(),
      stochastic = stochastic_log(),
      error_variance = error_variance_log(),
      norm = norm_log(),
      t_start = t_start_log(),
      t_end = t_end_log(),
      t_step = t_step_log(),
      t_store = t_store_log()
    )
  })

  output$LOGSpecies <- renderPlot(plotSeries(runLOG(), x = "time"))
}
