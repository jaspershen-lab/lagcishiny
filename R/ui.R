# intro_cleaned_content <- grep("<(/?(html|head|body))>", 
#                               readLines(system.file("app/www/introduction.html", 
#                                                     package = "lagcishiny")),
#                               invert = TRUE, value = TRUE)

#' lagci Shiny UI
#'
#' Defines the user interface layout for the lagci Shiny application using the `shinydashboard` framework.
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
#' @note This UI is designed to be paired with \code{\link{lagci_server}}.
#'
#' @seealso \code{\link{lagci_server}}, \code{\link{upload_file_ui}}, \code{\link{report_download_ui}}
#'
#' @import shinydashboard
#' @importFrom shiny fluidPage fluidRow column titlePanel actionButton uiOutput
#' @importFrom shinyjs useShinyjs
#' @importFrom shiny HTML
#'
#' @keywords internal
#' @noRd
lagci_ui <- shinydashboard::dashboardPage(
  skin = "black",
  
  shinydashboard::dashboardHeader(title = "lagci"),
  
  shinydashboard::dashboardSidebar(
    shinydashboard::sidebarMenu(
      id = "tabs",
      shinydashboard::menuItem(text = "Home", tabName = "home", icon = icon("info-circle")),
      shinydashboard::menuItem(text = "Data Upload", tabName = "upload_data", icon = icon("upload")),
      shinydashboard::menuItem(text = "Calculate", tabName = "calculate", icon = icon("desktop")),
      shinydashboard::menuItem(text = "Result Plots", tabName = "result_plot", icon = icon("images")),
      shinydashboard::menuItem(text = "Results & Report", tabName = "report", icon = icon("clipboard-list"))
    )
  ),
  
  shinydashboard::dashboardBody(
    shinyjs::useShinyjs(),
    
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "/www/custom.css")
    ),
    
    
    shinydashboard::tabItems(
      ## Home tab ====
      shinydashboard::tabItem(tabName = "home",
              fluidPage(
                titlePanel(" "),
                fluidRow(
                  column(12,
                         # shiny::HTML(intro_cleaned_content),
                         # shiny::includeHTML(system.file("app/www/introduction.html",
                         #                                package = "lagcishiny")),
                         # tags$iframe(
                         #   src = "/www/introduction.html",
                         #   width = "100%",
                         #   height = "600px",
                         #   frameborder = 0,
                         #   style = "border: none;"
                         # ),
                         intro_ui,
                         citation_ui("cite")
                  )
                )
              )),
      
      ## Data Upload tab ====
      shinydashboard::tabItem(tabName = "upload_data",
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
      shinydashboard::tabItem(tabName = "calculate",
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
      shinydashboard::tabItem(tabName = "result_plot",
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
      shinydashboard::tabItem(tabName = "report",
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
    ),
    ### footer ====
    tags$footer(
      div(
        class = "app-footer",
        tags$img(
          src = "/www/shen_lab_logo.png",
          class = "footer-logo"
        ),
        div(
          class = "footer-content",
          HTML("The Shen Lab at Nanyang Technological University Singapore"),
          HTML("<br>"),
          tags$a(
            href = "http://www.shen-lab.org",
            target = "_blank",
            class = "footer-link",
            tags$i(class = "fa fa-house footer-icon"),
            " Shen Lab"
          ),
          tags$a(
            href = "https://www.shen-lab.org/#contact",
            target = "_blank",
            class = "footer-link",
            tags$i(class = "fa fa-envelope footer-icon"),
            " Email"
          ),
          tags$a(
            href = "https://github.com/jaspershen-lab/lagci",
            target = "_blank",
            class = "footer-link",
            tags$i(class = "fa fa-github footer-icon"),
            " GitHub"
          )
        ),
        tags$img(
          src = "/www/lagci-logo.png",
          class = "footer-logo-right"
        )
      )
    )
  )
)
