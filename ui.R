library(shiny)

# define ui
navbarPage("Confirmatory Network Analysis Fit Measures", theme = shinytheme("cosmo"),
           
           # ----- introduction ----- 
           tabPanel("Introduction",
                    fluidPage(
                      
                      # ----- title -----
                      
                      fluidRow(
                        column(width = 12,
                               tags$div(
                                 tags$strong("Confirmatory Network Analysis Fit Measures",
                                             style = "font-size:24px;")
                               ),
                               tags$div(
                                 HTML(paste(
                                   "This app is to display the results for paper ",
                                   "Du, X., Skjerdingstad, N., Freichel, R., ",
                                   "Ebrahimi, O. V., Hoekstra, R. H. A., & Epskamp, S. (",
                                   tags$a(
                                     href = "https://osf.io/d76ab",
                                     "under review"),
                                   "). Moving from exploratory to confirmatory network analysis: ",
                                   "An evaluation of SEM fit indices and cutoff values in network psychometrics.",
                                   sep = ""))
                               )
                        ) # end column  
                      ), # end fluidRow for title
                      
                      
                      
                      # ----- instructions -----
                      
                      h4("How to use this app"),
                      
                      # *--- three options --- 
                      tags$div("You can choose from"),
                      tags$ul(
                        tags$li(tags$b("Boxplots:"),
                                "Displays how fit measures change in response to", 
                                "different levels of misspecifications",
                                "under various sample characteristics."),
                        tags$li(tags$b("Lineplots:"), "Displays the rejection rates of", 
                                "different cutoff values in response to different levels",
                                "of misspecifications under various sample characteristics."),
                        tags$li(tags$b("ANCOVA tables:"),
                                "Display the effect sizes that indicate the sensitivity",
                                "of fit measures to different types of misspecifications",
                                "and their vulnerability to sample characteristics.")
                      ), # end tab introduction
                      
                      # *--- true model setups --- 
                      tags$div(
                        HTML(paste("Then you choose for which true model setup you display the results.",
                                   "The default is to display the results of the original study.",
                                   "In response to the reviewer's feedback,",
                                   "we also display the results for two alternative true model setups",
                                   "to showcase the robustness of our results.",
                                   "In total, there are three setups:"))),
                      tags$ul(
                        tags$li(tags$b("Original (propPos = 0.5 & nei = 1)"), 
                                "In the original study,",
                                "we set the proportion of positive edges to be 0.5", 
                                "and number of neighbors for each node to be 1"),
                        tags$li(tags$b("Setting propPos = 0.8:"), 
                                "A reviewer suggested to check the results",
                                "if we set the proportion of positive edges in the networks to be 0.8"),
                        tags$li(tags$b("Setting nei = 2:"), 
                                "Check the results if we change number of neighbors for each node to be 2.")
                      )
                    ),
                    
                    # *--- contact information --- 
                    hr(),
                    HTML(paste(
                      "This app is made by Xinkai Du. For questions or error reporting, please contact via:",
                      tags$a(href = "mailto:xinkai.du.xd@gmail.com",
                             "xinkai.du.xd@gmail.com")
                    ))
                      
           ), # end tab: Introduction
           
           
           # ----- boxplots ----- 
           
           tabPanel("Boxplots",
                    sidebarLayout(
                      sidebarPanel(
                        actionButton("draw_box_plot", "Create plot"),
                        hr(),
                        
                        # *--- choose dataset --- 
                        selectInput("dt_mis_condition", "Choose the misspecification condition", 
                                    choices = c("Simulation Study (1): Null model" = "dt_null", 
                                                "Simulation Study (2): Mean trends" = "dt_mean", 
                                                "Simulation Study (3): Misspecified temporal networks" = "dt_rt",
                                                "Simulation Study (4): Misspecified contemporaneous networks" = "dt_rc",
                                                "Simulation Study (5): Non-stationary temporal networks" = "dt_nt",
                                                "Simulation Study (6): Non-stationary contemporaneous networks" = "dt_nc")),
                        
                        # *--- choose true model ---
                        selectInput("true_model", "Choose the true model setup",
                                    choices = c("nei = 1 & propPos = 0.5",
                                                "nei = 1 & propPos = 0.8",
                                                "nei = 2 & propPos = 0.5")),
                        
                        # *--- fit measure ---
                        selectInput("fit_index", "The fit measure you wish to display results for",
                                    choices = metrics[-1]),
                        
                        # *--- choose sample characteristics --- 
                        conditionalPanel(
                          condition = "input.dt_mis_condition != 'dt_null'",
                          selectInput("n_nodes", "Choose the number of nodes",
                                      choices = c("6 nodes" = "P = 6",
                                                  "12 nodes" = "P = 12"))
                        )
                        # hr(),
                        # numericInput('width_boxplot', 'Width', value = 15, min = 1),
                        # numericInput('height_boxplot', 'Height', value = 10, min = 1),
                        
                      ), # end sidebar panel
                      
                      # *--- plots display ---
                      mainPanel(
                        plotOutput("boxplot")
                      ) # end main panel
                    ) # end sidebar layout
           ), # end tab: Boxplots
           
           
           # ----- line plots -----
           
           tabPanel("Line Plots",
                    sidebarLayout(
                      sidebarPanel(
                        actionButton("draw_line_plot", "Create plot"),
                        hr(),
                        
                        # *--- choose dataset --- 
                        selectInput("dt_mis_condition", "Choose the misspecification condition", 
                                    choices = c("Simulation Study (1): Null model" = "dt_null", 
                                                "Simulation Study (2): Mean trends" = "dt_mean", 
                                                "Simulation Study (3): Misspecified temporal networks" = "dt_rt",
                                                "Simulation Study (4): Misspecified contemporaneous networks" = "dt_rc",
                                                "Simulation Study (5): Non-stationary temporal networks" = "dt_nt",
                                                "Simulation Study (6): Non-stationary contemporaneous networks" = "dt_nc")),
                        
                        # *--- choose true model ---
                        selectInput("true_model", "Choose the true model setup",
                                    choices = c("nei = 1 & propPos = 0.5",
                                                "nei = 1 & propPos = 0.8",
                                                "nei = 2 & propPos = 0.5")),
                        
                        # * --- choose model type --- 
                        selectInput("model_type", "Choose the type of model",
                                      choices = c("Confirmatory", "Saturated", "Pruned")),
                        
                        # *--- choose sample characteristics --- 
                        conditionalPanel(
                          condition = "input.dt_mis_condition != 'dt_null'",
                          selectInput("n_nodes", "Choose the number of nodes",
                                      choices = c("6 nodes" = "P = 6",
                                                  "12 nodes" = "P = 12"))),
                        
                        # *--- select the cutoff values ---
                        tags$div(tags$b("Set cutoff values:")),
                        
                        slider_set_up("NFI", .95), # inputId is paste0(id, "_cut)
                        slider_set_up("PNFI", .80),
                        slider_set_up("TLI", .95),
                        slider_set_up("RFI", .95),
                        slider_set_up("IFI", .95),
                        slider_set_up("RNI", .95),
                        slider_set_up("CFI", .95),
                        slider_set_up("RMSEA", .05),
                        
                      ), # end sidebar panel
                      
                      # *--- plots display ---
                      mainPanel(
                        plotOutput("line_plot")
                      ) # end main panel
                    ) # end sidebar layout
           ), # end tab Line Plots
           
           tabPanel("ANCOVA tables",
                    sidebarLayout(
                      sidebarPanel(
                        actionButton("make_ANOVA_table", "Create table"),
                        hr(),

                        # *--- choose dataset ---
                        selectInput("dt_mis_condition_ancova", "Choose the misspecification condition",
                                    choices = c("Simulation Study (2): Mean trends" = "dt_mean",
                                                "Simulation Study (3): Misspecified temporal networks" = "dt_rt",
                                                "Simulation Study (4): Misspecified contemporaneous networks" = "dt_rc",
                                                "Simulation Study (5): Non-stationary temporal networks" = "dt_nt",
                                                "Simulation Study (6): Non-stationary contemporaneous networks" = "dt_nc")),

                        # *--- choose true model ---
                        selectInput("true_model", "Choose the true model setup",
                                    choices = c("nei = 1 & propPos = 0.5",
                                                "nei = 1 & propPos = 0.8",
                                                "nei = 2 & propPos = 0.5")),

                      ), # end sidebar panel

                      # *--- plots display ---
                      mainPanel(
                        tableOutput("ancova_table")
                      ) # end main panel
                    ) # end sidebar layout
           ) # end tab ANCOVA tables

)