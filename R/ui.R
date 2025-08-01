#' LaggedCor Shiny UI
#'
#' Defines the user interface layout for the LaggedCor Shiny application using the `shinydashboard` framework.
#' The UI is organized into five tabs: Home, Data Upload, Calculate, Result Plots, and Results & Report.
#'
#' @format A `shinydashboard::dashboardPage` object defining the full app layout.
#'
#' @section Tabs Overview:
#' \describe{
#'   \item{Home}{Displays a brief welcome message and app introduction.}
#'   \item{Data Upload}{
#'     Allows the user to upload two data files and configure time plot parameters.
#'     Includes preview functionality with interactive plot export.
#'   }
#'   \item{Calculate}{
#'     Enables configuration of lagged correlation parameters and triggers computation.
#'     Outputs a structured correlation result summary.
#'   }
#'   \item{Result Plots}{
#'     Provides controls for generating alignment and scatter plots (max/global),
#'     and displays them with export buttons.
#'   }
#'   \item{Results & Report}{
#'     Presents all analysis results for review, and enables full report generation
#'     via \code{\link{report_download_ui}}.
#'   }
#' }
#'
#' @note This UI is designed to be paired with \code{\link{laggedcor_server}}.
#'
#' @seealso \code{\link{laggedcor_server}}, \code{\link{upload_file_ui}}, \code{\link{report_download_ui}}
#'
#' @importFrom shinydashboard dashboardPage dashboardHeader dashboardSidebar sidebarMenu menuItem dashboardBody tabItems tabItem
#' @importFrom shiny fluidPage fluidRow column titlePanel actionButton uiOutput
#' @importFrom shinyjs useShinyjs
#'
#' @keywords internal
#' @noRd
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
    shinyjs::useShinyjs(),
    tabItems(
      ## Home tab ====
      tabItem(tabName = "home",
              fluidPage(
                titlePanel("Introduction of laggedcor"),
                fluidRow(
                  column(12
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
                         report_download_ui("report_download")
                  ),
                  column(8,
                         uiOutput("preview_outputs_report"),
                         uiOutput("cor_summary_report"),
                         uiOutput("plot_result_ui_report")
                  )
                )
              ))
    )
  )
)
