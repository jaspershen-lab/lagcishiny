intro_ui <- shiny::tagList(
  shiny::tags$h1("Introduction"),
  shiny::tags$p(
    shiny::tags$code("laggedcor"),
    " is an R package designed for calculating lagged correlations between two time-series datasets, ",
    "with a particular focus on wearable and omics data analysis. Developed by Dr. Xiaotao Shen, ",
    "this tool is available on GitHub at ",
    shiny::tags$a("https://github.com/jaspershen-lab/laggedcor",
                  href = "https://github.com/jaspershen-lab/laggedcor",
                  target = "_blank"
    ),
    "."
  ),
  shiny::tags$p(
    shiny::tags$code("laggedcor"),
    " is part of the larger tidywearable ecosystem, which provides a comprehensive suite of tools ",
    "for wearable data analysis. This integration ensures compatibility and workflow continuity ",
    "for researchers working with multiple types of temporal health data."
  ),
  shiny::tags$p(
    "The package includes demonstration datasets that illustrate typical use cases: ",
    shiny::tags$code("heart_data"), " containing heart rate measurements and ",
    shiny::tags$code("step_data"), " containing step count measurements. ",
    "These datasets exemplify the type of wearable device data that researchers commonly need ",
    "to analyze for temporal relationships."
  ),
  
  shiny::tags$h1("Contacts"),
  shiny::tags$p(
    shiny::tags$i(class = "fa fa-envelope"),
    " ",
    shiny::tags$a("xiaotao.shen@outlook.com", href = "mailto:xiaotao.shen@outlook.com")
  ),
  shiny::tags$p(
    shiny::tags$i(class = "fa-brands fa-square-x-twitter"), " ",
    shiny::tags$a("X", href = "https://x.com/xiaotaoshen1990", target = "_blank")
  ),
  shiny::tags$p(
    shiny::tags$i(class = "fa fa-map-marker-alt"), " ",
    shiny::tags$a(
      "Experimental Medicine Building (EMB), 59 Nanyang Dr, Singapore 636921",
      href = "https://www.google.com/maps/place/Experimental+Medicine+Building+(EMB)/@1.3446589,103.6759014,17z/data=!3m2!4b1!5s0x31da0fa062fb6e8b:0x7a7422f5cbf93df8!4m6!3m5!1s0x31da0f74c11959dd:0x1a2e60ac1237b269!8m2!3d1.3446535!4d103.6784763!16s%2Fg%2F11bwpz7vzw?entry=ttu&g_ep=EgoyMDI1MDczMC4wIKXMDSoASAFQAw%3D%3D",
      target = "_blank"
    )
  )
)
