


bibtex_entry <- "@ARTICLE{Shen2019-xv,
  title     = \"Metabolic reaction network-based recursive metabolite annotation
               for untargeted metabolomics\",
  author    = \"Shen, Xiaotao and Wang, Ruohong and Xiong, Xin and Yin, Yandong
               and Cai, Yuping and Ma, Zaijun and Liu, Nan and Zhu, Zheng-Jiang\",
  journal   = \"Nat. Commun.\",
  publisher = \"Springer Science and Business Media LLC\",
  volume    =  10,
  number    =  1,
  pages     = \"1516\",
  month     =  apr,
  year      =  2019,
  copyright = \"https://creativecommons.org/licenses/by/4.0\",
  language  = \"en\"
}"


ui <- fluidPage(
  shiny::titlePanel("LaggedCor"),
  tabsetPanel(
    id = "tabs",
    tabPanel(
      title = "1.Import Data",
      fluidRow(
        column(
          width = 6,
          "First column of your file should be time,",
          br(),
          "Second column of your file should be value.",
          fileInput(inputId = "file_1",
                    label = "Upload First File (.csv or .rda)",
                    accept = c(".csv", ".rda")),
          fileInput(inputId = "file_2",
                    label = "Upload Second File (.csv or .rda)",
                    accept = c(".csv", ".rda")),
          actionButton("submit_btn", "Submit", class = "btn-primary")
        ),
        column(
          width = 6,
          h4("Time Plot Parameters"),
          textInput("color", "Line color(Hexadecimal color code or color name)", value = "blue"),
          textInput("y_axis_name_1", "Y-axis label of file 1", value = "Value_1"),
          textInput("y_axis_name_2", "Y-axis label of file 2", value = "Value_2"),
          textInput("sun_rise_time", "Sunrise time (hh:mm:ss)", value = "6:00:00"),
          textInput("sun_set_time", "Sunset time (hh:mm:ss)", value = "18:00:00"),
          numericInput("time_gap", "Time gap (hour)", value = 12, min = 1),
          checkboxInput("add_point", "Add points", value = FALSE),
          checkboxInput("facet", "Facet by day", value = FALSE)
        )
      )
    ),
    
    tabPanel(
      title = "2.Preview & Set Parameters",
      shiny::conditionalPanel(
        condition = "output.dataUploaded === true",
        fluidRow(
          column(6,
                 div(uiOutput("file1_title"), style = "text-align:center;"),
                 DT::dataTableOutput("head_data1")
          ),
          column(6,
                 div(uiOutput("file2_title"), style = "text-align:center;"),
                 DT::dataTableOutput("head_data2")
          )
        ),
        hr(),
        fluidRow(
          column(6,
                 div(uiOutput("plot1_title"), style = "text-align:center;"),
                 plotOutput("time_plot1")
          ),
          column(6,
                 div(uiOutput("plot2_title"), style = "text-align:center;"),
                 plotOutput("time_plot2")
          )
        ),
        shiny::br(),
        shiny::hr()
      ),
      shiny::numericInput(inputId = "time_tol",
                          "Time tolerance (hour)",
                          value = 0.5,min = 0),
      shiny::numericInput(inputId = "step", 
                          "Step size (hour)", 
                          value = 0.2, min = 0),
      shiny::numericInput(inputId = "min_match", 
                          "Min matched samples", 
                          value = 10, min = 0),
      shiny::selectInput(inputId = "cor_method", 
                         "Correlation method", 
                         choices = c("spearman", "pearson"),
                         selected = "spearman"),
      shiny::selectInput(inputId = "align_method",
                         label = "Select interpolation method for alignment:",
                         choices = c("linear", "constant"),
                         selected = "linear"),
      shiny::actionButton(inputId = "start", 
                          "Start Analysis",
                          class = "btn-primary")
    ),
    tabPanel(
      title = "3.Results & Report",
      shiny::h4("Correlation Summary"),
      shiny::verbatimTextOutput("cor_summary"),
      shiny::h4("Plots"),
      shiny::uiOutput("eva_lagged_cor_plot"),
      shiny::uiOutput("max_scatter_plot"),
      shiny::uiOutput("global_scatter_plot"),
      shiny::uiOutput("max_alignment_plot"),
      shiny::uiOutput("global_alignment_plot"),
      selectInput("report_fmt", "Select report format:",
                  choices = c("HTML", "PDF"),
                  selected = "HTML"),
      
      downloadButton("download_report", "Download Report")
    ),
    navbarMenu(
      title = "MORE",
      tabPanel(
        title = "citation",
        useShinyjs(),
        tags$script(HTML("
    function copyBibtex() {
      var text = document.getElementById('bibtex_output').innerText;
      navigator.clipboard.writeText(text).then(function() {
        alert('Copied!');
      });
    }
  ")),
        actionButton("copy_btn", "Copy BibTeX"),
        verbatimTextOutput("bibtex_output")
      )
    )
  )
)

server <- function(input, output, session) {
  
  options(shiny.error = function() traceback(3))
  
  
  data_1 <- reactiveVal(NULL)
  data_2 <- reactiveVal(NULL)
  
  
  #action button at Import Data
  observeEvent(input$submit_btn, {
    req(input$file_1, input$file_2)  # 检查是否上传了文件
    
    # 读取文件函数（支持 .csv 和 .rda）
    read_data <- function(file) {
      ext <- tools::file_ext(file$datapath)
      if (ext == "csv") {
        read.csv(file$datapath, stringsAsFactors = FALSE)
      } else if (ext == "rda") {
        e <- new.env()
        load(file$datapath, envir = e)
        obj <- e[[ls(e)[1]]]
        return(obj)
      } else {
        stop("Unsupported file type.")
      }
    }
    
    tryCatch({
      dat1 <- read_data(input$file_1)
      dat2 <- read_data(input$file_2)
      
      # 检查是否都为数据框且只有两列
      if (!(is.data.frame(dat1) && ncol(dat1) == 2)) stop("First file must have exactly two columns.")
      if (!(is.data.frame(dat2) && ncol(dat2) == 2)) stop("Second file must have exactly two columns.")
      
      # 检查是否存在能转为 POSIXct 的时间列
      check_time_col <- function(df) {
        any(sapply(df, function(col) {
          suppressWarnings(!all(is.na(as.POSIXct(col, origin = "1970-01-01", tz = "UTC"))))
        }))
      }
      
      if (!check_time_col(dat1)) stop("First file must contain a valid time column.")
      if (!check_time_col(dat2)) stop("Second file must contain a valid time column.")
      
      # 转换第一列时间列为 POSIXct 格式
      dat1[[1]] <- as.POSIXct(dat1[[1]], origin = "1970-01-01", tz = "UTC")
      dat2[[1]] <- as.POSIXct(dat2[[1]], origin = "1970-01-01", tz = "UTC")
      
      data_1(dat1)
      data_2(dat2)
      
      # 通知用户
      showNotification("Files loaded successfully", type = "message")
      
      # 跳转到 tab2
      updateTabsetPanel(session, "tabs", selected = "2.Preview & Set Parameters")
      
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error")
    })
  })
  
  output$head_data1 <- DT::renderDataTable({
    req(data_1())
    head(data_1(), 5)
  }, options = list(
    dom = 't',
    pageLength = 5
  ))
  
  output$head_data2 <- DT::renderDataTable({
    req(data_2())
    head(data_2(), 5)
  }, options = list(
    dom = 't',
    pageLength = 5
  ))
  
  output$time_plot1 <- renderPlot({
    req(data_1())
    laggedcor::time_plot(
      x = data_1()[[2]],
      time = data_1()[[1]],
      color = input$color,
      y_axis_name = input$y_axis_name_1,
      sun_rise_time = input$sun_rise_time,
      sun_set_time = input$sun_set_time,
      time_gap = input$time_gap,
      add_point = input$add_point,
      facet = input$facet
    )
  })
  
  output$time_plot2 <- renderPlot({
    req(data_2())
    laggedcor::time_plot(
      x = data_2()[[2]],
      time = data_2()[[1]],
      color = input$color,
      y_axis_name = input$y_axis_name_2,
      sun_rise_time = input$sun_rise_time,
      sun_set_time = input$sun_set_time,
      time_gap = input$time_gap,
      add_point = input$add_point,
      facet = input$facet
    )
  })
  
  output$file1_title <- renderUI({
    req(input$file_1)
    h5(paste0(input$file_1$name, " Preview"))
  })
  
  output$file2_title <- renderUI({
    req(input$file_2)
    h5(paste0(input$file_2$name, " Preview"))
  })
  
  output$plot1_title <- renderUI({
    req(input$file_1)
    h5(paste0(input$file_1$name, " Time Plot"))
  })
  
  output$plot2_title <- renderUI({
    req(input$file_2)
    h5(paste0(input$file_2$name, " Time Plot"))
  })
  
  
  
  
  output$dataUploaded <- reactive({
    !is.null(data_1()) && !is.null(data_2())
  })
  outputOptions(output, "dataUploaded", suspendWhenHidden = FALSE)
  
  #点击Start analysis之后，开始计算
  result <- eventReactive(input$start, {
    
    file_nrow_1 <- nrow(data_1())
    file_nrow_2 <- nrow(data_2())
    
    if (file_nrow_1 <= file_nrow_2){
      x <- data_1()[[2]]
      time1 <- data_1()[[1]]
      y <- data_2()[[2]]
      time2 <- data_2()[[1]]
    }else{
      x <- data_2()[[2]]
      time1 <- data_1()[[1]]
      y <- data_1()[[2]]
      time2 <- data_1()[[1]]
    }
    
    laggedcor::calculate_lagged_correlation(x = x,
                                            y = y,
                                            time1 = time1, 
                                            time2 = time2, 
                                            time_tol = input$time_tol, 
                                            step = input$step,
                                            min_matched_sample = input$min_match,
                                            align_method = input$align_method, 
                                            cor_method = input$cor_method)
  })
  
  #计算结果之后跳转tab
  observeEvent(result(), { 
    updateTabsetPanel(session, "tabs", selected = "3.Results & Report")
  })
  
  output$cor_summary <- renderPrint({
    req(result())
    list(
      max_cor = laggedcor::extract_max_cor(result()),
      global_cor = laggedcor::extract_global_cor(result()),
      all_cor = laggedcor::extract_all_cor(result()),
      all_cor_p = laggedcor::extract_all_cor_p(result()),
      shift_time_numeric = laggedcor::extract_shift_time(result(), numeric = TRUE),
      shift_time_no_numeric = laggedcor::extract_shift_time(result(), numeric = FALSE),
      evaluated_lagged_cor = laggedcor::evaluate_lagged_cor(object = result, plot = FALSE)
    )
  })

  output$eva_lagged_cor_plot <- renderPlot({
    req(result())
    evaluated_lagged_cor_plot <- laggedcor::evaluate_lagged_cor(result(),
                                   plot = TRUE)
    evaluated_lagged_cor_plot$plot
  })
  
  observeEvent(result(), {
    print(length(result()@x))
    print(length(result()@y))
    print(range(result()@time1))
    print(range(result()@time2))
  })


  output$max_scatter_plot <- renderPlot({
    req(result())
    laggedcor::lagged_scatter_plot(result(),
                                   x_name = "x",
                                   y_name = "y",
                                   which = "max",
                                   hex = TRUE)
  })

  output$global_scatter_plot <- renderPlot({
    req(result())
    laggedcor::lagged_scatter_plot(result(),
                                   x_name = "x",
                                   y_name = "y",
                                   which = "global",
                                   hex = TRUE)
  })

  output$max_alignment_plot <- renderPlot({
    req(result())
    laggedcor::lagged_alignment_plot(result(),
                                     x_color = "blue",
                                     y_color = "red",
                                     x_name = "x",
                                     y_name = "y",
                                     which = "max",
                                     x_limit = c(1:1000),
                                     non_matched_point_size = 0.1,
                                     y_point_size = 1,
                                     x_point_size = 3,
                                     integrated = FALSE,
                                     add_connect_line = FALSE,
                                     add_point = FALSE,
                                     time_gap = 4)
  })

  output$global_alignment_plot <- renderPlot({
    req(result())
    laggedcor::lagged_alignment_plot(result(),
                                     x_color = "blue",
                                     y_color = "red",
                                     x_name = "x",
                                     y_name = "y",
                                     which = "global",
                                     x_limit = c(1:1000),
                                     non_matched_point_size = 0.1,
                                     y_point_size = 1,
                                     x_point_size = 3,
                                     integrated = FALSE,
                                     add_connect_line = FALSE,
                                     add_point = FALSE,
                                     time_gap = 4)
  })

  output$bibtex_output <- renderPrint({
    cat(bibtex_entry)
  })
  
  observeEvent(input$copy_btn, {
    runjs("copyBibtex();")
  })
}

shinyApp(ui,server)

