library(shiny)

shinyServer(function(input, output) {
  
  # ----- data source -----
  
  dt_react <- reactive({
    
    if (input$dt_mis_condition == "dt_null"){
      dt[[input$dt_mis_condition]][condition == input$true_model,]
    } else {
      dt[[input$dt_mis_condition]][condition == input$true_model & n_node_factor == input$n_nodes,]
    }
    
  })
  
  
  # ----- boxplots -----
  
  boxplot_react <- eventReactive(input$draw_box_plot, {
    
    if (input$dt_mis_condition == "dt_null") {
      box_plotter(dt_react(), metric = input$fit_index, 
                  x_facet = "n_node_factor", y_facet = "n_time_factor", 
                  color = "type") + guides(fill=guide_legend(title="Model type"))
    } else if (input$dt_mis_condition == "dt_mean") {
      box_plotter(dt_react(), metric = input$fit_index, 
                  x_facet = "type", y_facet = "n_time_factor", 
                  color = "mean_trend_factor") + guides(fill=guide_legend(title="Mean trends"))
    } else if (input$dt_mis_condition %in% c("dt_rt", "dt_nt")) {
      box_plotter(dt_react(), metric = input$fit_index, 
                  x_facet = "type", y_facet = "n_time_factor", 
                  color = "rewire_temporal_factor") + guides(fill=guide_legend(title="Rewiring probability"))
    } else {
      box_plotter(dt_react(), metric = input$fit_index, 
                  x_facet = "type", y_facet = "n_time_factor", 
                  color = "rewire_contemporaneous_factor") + guides(fill=guide_legend(title="Rewiring probability"))
    }
    
  })
  
  output$boxplot <- renderPlot({boxplot_react()})
  
  

  # ----- line plots -----
  
  # *--- calculate rejection rates ---
  rej_react <- eventReactive(input$draw_line_plot, {
    
    # obtain data  
    data <- dt_react()
    
    # calculate rejection rate of absolute indices
    rej <- data[, (sum(RMSEA > input$RMSEA_cut) / .N), by = c(conditions_ls[[input$dt_mis_condition]])]
    setnames(rej, "V1", paste0("RMSEA>", input$RMSEA_cut))
    
    # cutoffs for incremental indices
    cutoffs <- c(0.05, input$NFI_cut, input$PNFI_cut, input$TLI_cut,
                 input$RFI_cut, input$IFI_cut, input$RNI_cut, input$CFI_cut)
    
    # var names to identify rej rates
    rej_var <- paste0(metrics[-9], "<", cutoffs)
    
    # calculate rejection rates of incremental indices
    for (i in 1:length(rej_var)) {
      rej[[rej_var[i]]] <- data[, (sum(get(metrics[i]) < cutoffs[i]) / .N), by = c(conditions_ls[[input$dt_mis_condition]])][, V1]
    }
    
    rej <- rej[type == input$model_type,]
    
    # make long-format
    longer <- melt(rej, id.vars = conditions_ls[[input$dt_mis_condition]])
    
    return(longer)
    
  })
  
  # *--- draw plot --- 
  
  line_plot_react <- eventReactive(input$draw_line_plot, {
    
    if (input$dt_mis_condition == "dt_null") {
      line_plotter(data = rej_react(), x = "n_person_factor", y = "value", color = line_colors,
                   color_var = "variable", xfacet = "n_node_factor", yfacet = "n_time_factor")
    } else if (input$dt_mis_condition == "dt_mean") {
      line_plotter(data = rej_react(), x = "n_person_factor", y = "value", color = line_colors,
                   color_var = "variable", xfacet = "mean_trend_factor", yfacet = "n_time_factor")
    } else if (input$dt_mis_condition %in% c("dt_rt", "dt_nt")) {
      line_plotter(data = rej_react(), x = "n_person_factor", y = "value", color = line_colors,
                   color_var = "variable", xfacet = "rewire_temporal_factor", yfacet = "n_time_factor")
    } else {
      line_plotter(data = rej_react(), x = "n_person_factor", y = "value", color = line_colors,
                   color_var = "variable", xfacet = "rewire_contemporaneous_factor", yfacet = "n_time_factor")
    }
    
  })
  
  # output the line plot
  output$line_plot <- renderPlot({line_plot_react()})
  
  
  # ----- ANCOVA table -----

  # *--- data source for ANCOVA ---

  dt_ancova_react <- reactive({
      if (input$dt_mis_condition_ancova %in% c("dt_rt", "dt_rc")) {
        dt[[input$dt_mis_condition_ancova]][condition == input$true_model & type == "Confirmatory",][, type := NULL]
      } else {
        dt[[input$dt_mis_condition_ancova]][condition == input$true_model,]
      }
  })
  
  # *--- ANCOVA ---
  # conditions
  tab_react <- eventReactive(input$make_ANOVA_table, {
    
    # load data
    data <- dt_ancova_react()
    
    data[[1]] <- data[[1]] %>% as.character %>% 
      sub(pattern = ".* = ", replacement = "") %>% as.numeric
    
    # conditions (mismatch conditions don't have type as predictor)
    if (input$dt_mis_condition_ancova %in% c("dt_rt", "dt_rc")) {
      conditions <- conditions_ls[[input$dt_mis_condition_ancova]][-2]
    } else {
      conditions <- conditions_ls[[input$dt_mis_condition_ancova]]
    }

    # formulas
    formulas <- paste(metrics[-1], "~", paste(conditions, collapse = " + "))
    
    # result list
    ANCOVA = list()
    
    # ancova series
    ANCOVA <- lapply(formulas, function(f) {
      f <- as.formula(f)
      aov.fit <- sjstats::anova_stats(aov(f, data = data)) %>% as.data.table
      aov.fit <- aov.fit[, c("partial.etasq", "term")]
    })
    
    # rename list dimensions
    names(ANCOVA) <- metrics[-1]
    
    # bind results
    ANCOVA <- rbindlist(ANCOVA, idcol = "Metrics")
    
    # just keep the first identifier of each table
    if (input$dt_mis_condition_ancova %in% c("dt_rt", "dt_rc")) {
      
      ANCOVA <- ANCOVA[, lapply(.SD, as.character)][
        # round numbers with roundif, except for the residual row
        (1:nrow(ANCOVA))[-seq(5, nrow(ANCOVA), by = 5)], 
        partial.etasq := lapply(.SD, roundif), .SDcols = "partial.etasq"][
          # mark p < .001
          , partial.etasq := lapply(.SD, function(x) {
            ifelse(as.numeric(x) < .001, "< .001", x)
          }), .SDcols = "partial.etasq"]
                       
      
    } else {
      
      ANCOVA <- ANCOVA[, lapply(.SD, as.character)][
        # round numbers with roundif, except for the residual row
        (1:nrow(ANCOVA))[-seq(6, nrow(ANCOVA), by = 6)], 
        partial.etasq := lapply(.SD, roundif), .SDcols = "partial.etasq"][
          # mark p < .001
          , partial.etasq := lapply(.SD, function(x) {
            ifelse(as.numeric(x) < .001, "< .001", x)
          }), .SDcols = "partial.etasq"]
      
    }
    
    # store the partial eta squares
    p.etasq <- dcast(ANCOVA, formula = Metrics ~ term, value.var = "partial.etasq")     
  
    # remove unncessary columns & order to find the index that was the most sensitive to mean_trends and least to covariates
    if (input$dt_mis_condition_ancova == "dt_mean") {
      
      p.etasq <- p.etasq[, Residuals := NULL][
        order(-mean_trend_factor,
              n_node_factor, 
              n_person_factor, 
              n_time_factor)] 
        
    } else if (input$dt_mis_condition_ancova %in% c("dt_rt", "dt_nt")) {
      
      p.etasq <- p.etasq[, Residuals := NULL][
        order(-rewire_temporal_factor,
              n_node_factor, 
              n_person_factor, 
              n_time_factor)]
      
    } else {

      p.etasq <- p.etasq[, Residuals := NULL][
        order(-rewire_contemporaneous_factor,
              n_node_factor, 
              n_person_factor, 
              n_time_factor)]
      
    }
    
    # reorder
    if (input$dt_mis_condition_ancova == "dt_rt") {
      p.etasq <- p.etasq[,c("Metrics","rewire_temporal_factor", "n_node_factor", 
                 "n_person_factor", "n_time_factor")]
    } else if (input$dt_mis_condition_ancova == "dt_rc") {
      p.etasq <- p.etasq[,c("Metrics","rewire_contemporaneous_factor", "n_node_factor", 
                 "n_person_factor", "n_time_factor")]
    } else if (input$dt_mis_condition_ancova == "dt_nt") {
      p.etasq <- p.etasq[,c("Metrics","rewire_temporal_factor", "n_node_factor", 
                            "n_person_factor", "n_time_factor", "type")]
    } else if (input$dt_mis_condition_ancova == "dt_nc") {
      p.etasq <- p.etasq[,c("Metrics","rewire_contemporaneous_factor", "n_node_factor", 
                            "n_person_factor", "n_time_factor", "type")]
    } else {
      p.etasq <- p.etasq[,c("Metrics","mean_trend_factor", "n_node_factor", 
                            "n_person_factor", "n_time_factor", "type")]
    }
    
    # assign more informative colnames 
    if (input$dt_mis_condition_ancova == "dt_mean") {
      colNames <- c("Mean trend", "Number of nodes", "Sample size", "Number of waves", "Model type")
    } else if (input$dt_mis_condition_ancova %in% c("dt_rt", "dt_rc")) {
      colNames <- c("Rewire probability", "Number of nodes", "Sample size", "Number of waves")
    } else {
      colNames <- c("Rewire probability", "Number of nodes", "Sample size", "Number of waves", "Model type")
    }

    colnames(p.etasq)[-1] <- colNames
    
    return(p.etasq)
    
  }) # end eventReact make_ancova_table
  
  output$ancova_table <- renderTable({tab_react()})
  
}) # end server

