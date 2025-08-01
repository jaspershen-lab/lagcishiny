laggedcor_ui <- shinydashboard::dashboardPage(
  skin = "black",
  
  shinydashboard::dashboardHeader(title = "laggedcor"),
  
  shinydashboard::dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem(text = "Home", tabName = "home", icon = icon("info-circle")),
      menuItem(text = "Data Upload", tabName = "upload_data", icon = icon("upload")),
      menuItem(text = "Calculate", tabName = "calculate", icon = icon("desktop")),
      menuItem(text = "Result Plots", tabName = "result_plot", icon = icon("images")),
      menuItem(text = "Results & Report", tabName = "report", icon = icon("clipboard-list"))
    )
  ),
  
  dashboardBody(
    tabItems(
      ## Home tab ====
      tabItem(tabName = "home",
              fluidPage(
                titlePanel("Introduction of laggedcor"),
                fluidRow(
                  column(12,
                         p("Welcome to the LaggedCor App.")
                  )
                )
              )),
      
      ## Data Upload tab ====
      tabItem(tabName = "upload_data",
              fluidPage(
                titlePanel("Data Upload"),
                fluidRow(
                  column(4,
                         upload_file_ui("upload_file"),
                         time_plot_parameter_ui("time_plot_parameters"),
                         actionButton("preview", "Preview", class = "btn-primary")
                  ),
                  column(8,
                         uiOutput("preview_outputs")
                  )
                )
              )),
      
      ## Calculate tab ====
      tabItem(tabName = "calculate",
              fluidPage(
                titlePanel("Calculation"),
                fluidRow(
                  column(4,
                         calculation_parameter_ui("calculation"),
                         actionButton("run", "Run Calculation", class = "btn-primary")
                  ),
                  column(8,
                         uiOutput("cor_summary")
                  )
                )
              )),
      ## Result Plots tab ====
      tabItem(tabName = "result_plot",
              fluidPage(
                titlePanel("Result Plots"),
                fluidRow(
                  column(4,
                         scatter_plot_parameter_ui("scatter_plot"),
                         alignment_plot_parameter_ui("alignment_plot"),
                         actionButton("get_plot", label = "Get Plots Now!", class = "btn-primary")
                  ),
                  column(8,
                         uiOutput("plot_result_ui")
                  )
                )
              )),
      
      ## Report tab ====
      tabItem(tabName = "report",
              fluidPage(
                titlePanel("Report"),
                fluidRow(
                  column(4,
                         downloadButton("download_report", "Download")
                  ),
                  column(8,
                         p("Results will be shown here.")
                  )
                )
              ))
    )
  )
)
