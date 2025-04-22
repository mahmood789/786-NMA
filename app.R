# ---------------------------
# 1. Load Packages & Setup
# ---------------------------
library(shiny)
library(bs4Dash)            # For bs4Dash UI
library(shinycssloaders)    # For spinners on outputs
library(promises)           # For asynchronous processing
library(future)             # For asynchronous tasks
library(netmeta)            # Frequentist network meta-analysis
library(visNetwork)         # For interactive network visualization
library(DT)                 # For interactive tables
library(readr)              # For fast CSV reading
library(igraph)             # For static network plot generation
library(meta)               # For additional meta-analysis functions

plan(multisession)          # Set up asynchronous processing

# ---------------------------
# 2. Instructions Module
# ---------------------------
instructionsUI <- function(id) {
  ns <- NS(id)
  tagList(
    h2("NMA Shiny App Instructions"),
    p("This app supports two types of data formats for network meta-analysis (NMA) with binary outcomes and relative effect measures (Risk Ratio or Odds Ratio):"),
    tags$ul(
      tags$li(strong("Arm-level data:"), " Each row represents a study arm. Required columns are:"),
      tags$ul(
        tags$li(code("study")),
        tags$li(code("treatment")),
        tags$li(code("event")),
        tags$li(code("n")),
        tags$li(code("covariate"))
      ),
      tags$li(strong("Contrast-level data:"), " Each row represents a direct comparison between two treatments. Required columns are:"),
      tags$ul(
        tags$li(code("studlab")),
        tags$li(code("treat1")),
        tags$li(code("treat2")),
        tags$li(code("TE")),
        tags$li(code("seTE")),
        tags$li(code("covariate"))
      )
    ),
    p("Optional columns:"),
    tags$ul(
      tags$li(code("year"), " - useful for additional analyses.")
    ),
    p("To prepare your data:"),
    tags$ol(
      tags$li("Decide whether your data is at the arm level or contrast level."),
      tags$li("Ensure that all required columns are present and that numeric columns contain valid numbers."),
      tags$li("For meta‑regression, include at least one covariate column that has non‑missing numeric values."),
      tags$li("If you want additional analyses, consider including a column named ", code("year"), ".")
    ),
    p("You can download multiple sample CSV files from the ", strong("Example CSV Files"), " tab to see examples of how your data should be formatted.")
  )
}

instructionsServer <- function(id) {
  moduleServer(id, function(input, output, session) {})
}

# ---------------------------
# 3. Data Upload Module
# ---------------------------
dataUploadUI <- function(id) {
  ns <- NS(id)
  tagList(
    fileInput(ns("file"), "Upload CSV Data", accept = ".csv"),
    radioButtons(ns("format"), "Data Format:",
                 choices = c("Arm-level (one row per arm)" = "arm",
                             "Contrast-level (one row per comparison)" = "contrast")),
    radioButtons(ns("measure"), "Outcome Measure:",
                 choices = c("Risk Ratio (RR)" = "RR",
                             "Odds Ratio (OR)" = "OR"),
                 selected = "RR"),
    checkboxInput(ns("logTrans"), "Log-transform TE (if not already on the log scale)", FALSE),
    DTOutput(ns("dataPreview"))
  )
}

dataUploadServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    dataset <- reactiveVal(NULL)
    observeEvent(input$file, {
      req(input$file)
      raw <- read_csv(input$file$datapath, show_col_types = FALSE)
      
      if (input$format == "arm") {
        req_cols <- c("study", "treatment", "event", "n", "covariate")
        if (!all(req_cols %in% names(raw))) {
          showNotification(paste("Arm-level data must contain:",
                                 paste(req_cols, collapse = ", ")), type = "error")
          return(NULL)
        }
        # For binary outcomes, use event counts and sample sizes.
        pw <- pairwise(treat = treatment, event = event, n = n,
                       studlab = study, data = raw, sm = input$measure)
        contrast <- as.data.frame(pw)
        processed <- list(raw = raw, contrast = contrast, format = "arm")
      } else {
        req_cols <- c("studlab", "treat1", "treat2", "TE", "seTE", "covariate")
        if (!all(req_cols %in% names(raw))) {
          showNotification(paste("Contrast-level data must contain:",
                                 paste(req_cols, collapse = ", ")), type = "error")
          return(NULL)
        }
        processed <- list(raw = raw, contrast = raw, format = "contrast")
      }
      
      if (input$logTrans && "TE" %in% names(processed$contrast)) {
        if(all(processed$contrast$TE > 0, na.rm = TRUE)) {
          processed$contrast$TE <- log(processed$contrast$TE)
        } else {
          showNotification("Cannot log-transform TE because some values are non-positive", type = "error")
        }
      }
      
      dataset(processed)
      output$dataPreview <- renderDT({
        datatable(head(processed$contrast, 10))
      })
    })
    return(reactive({ dataset() }))
  })
}

# ---------------------------
# 4. Example CSV Files Module (Multiple Samples)
# ---------------------------
exampleCSVUI <- function(id) {
  ns <- NS(id)
  tagList(
    h3("Example CSV Files"),
    h4("Contrast-level Data Example (with covariate and year)"),
    verbatimTextOutput(ns("contrastCSV1")),
    downloadButton(ns("dlContrastCSV1"), "Download Contrast-level CSV (with year)"),
    br(), br(),
    h4("Arm-level Data Example (with covariate and year)"),
    verbatimTextOutput(ns("armCSV1")),
    downloadButton(ns("dlArmCSV1"), "Download Arm-level CSV (with year)"),
    br(), br(),
    h4("Extended Contrast-level Data Example (with two covariates and year)"),
    verbatimTextOutput(ns("contrastCSV2")),
    downloadButton(ns("dlContrastCSV2"), "Download Extended Contrast-level CSV")
  )
}

exampleCSVServer <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Updated contrast-level data sample for OR/RR
    sampleContrastCSV1 <- "studlab,treat1,treat2,TE,seTE,covariate,year
Study1,DrugA,DrugB,1.20,0.15,1.0,2010
Study2,DrugA,DrugC,0.85,0.12,1.2,2011
Study3,DrugB,DrugC,1.50,0.20,1.5,2012
Study4,DrugA,DrugB,1.10,0.18,1.0,2013
Study5,DrugC,DrugB,0.95,0.22,1.3,2014
Study6,DrugA,DrugC,1.30,0.16,1.1,2015"
    
    sampleArmCSV1 <- "study,treatment,event,n,covariate,year
Study1,DrugA,10,30,1.0,2010
Study1,DrugB,5,30,1.0,2010
Study2,DrugA,12,25,1.5,2011
Study2,DrugB,8,25,1.5,2011
Study2,DrugC,9,25,1.5,2011
Study3,DrugA,20,40,2.0,2012"
    
    sampleContrastCSV2 <- "studlab,treat1,treat2,TE,seTE,covariate,covariate2,year
Study1,DrugA,DrugB,1.20,0.15,1.0,0.8,2010
Study2,DrugA,DrugC,0.85,0.12,1.2,0.9,2011
Study3,DrugB,DrugC,1.50,0.20,1.5,1.1,2012
Study4,DrugA,DrugB,1.10,0.18,1.0,0.7,2013
Study5,DrugC,DrugB,0.95,0.22,1.3,1.0,2014
Study6,DrugA,DrugC,1.30,0.16,1.1,0.95,2015"
    
    output$contrastCSV1 <- renderText({ sampleContrastCSV1 })
    output$dlContrastCSV1 <- downloadHandler(
      filename = function() { "sample_contrast_data_with_year.csv" },
      content = function(file) {
        writeLines(sampleContrastCSV1, con = file)
      }
    )
    
    output$armCSV1 <- renderText({ sampleArmCSV1 })
    output$dlArmCSV1 <- downloadHandler(
      filename = function() { "sample_arm_data_with_year.csv" },
      content = function(file) {
        writeLines(sampleArmCSV1, con = file)
      }
    )
    
    output$contrastCSV2 <- renderText({ sampleContrastCSV2 })
    output$dlContrastCSV2 <- downloadHandler(
      filename = function() { "sample_extended_contrast_data.csv" },
      content = function(file) {
        writeLines(sampleContrastCSV2, con = file)
      }
    )
  })
}

# ---------------------------
# 5. Network Plot Module (with extra layout "Circle")
# ---------------------------
networkUI <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("focus"), "Focus on Treatment:", choices = NULL),
    visNetworkOutput(ns("networkPlot")) %>% withSpinner(),
    br(),
    h4("Customize Static Network Plot (igraph)"),
    fluidRow(
      column(4,
             selectInput(ns("layoutOption"), "Layout:",
                         choices = c("Fruchterman-Reingold" = "layout_with_fr",
                                     "Kamada-Kawai" = "layout_with_kk",
                                     "Reingold-Tilford" = "layout_as_tree",
                                     "Random" = "layout_randomly",
                                     "Circle" = "layout_in_circle"),
                         selected = "layout_with_fr")
      ),
      column(4,
             numericInput(ns("vertexSize"), "Vertex Size:", value = 30, min = 10, max = 100)
      ),
      column(4,
             selectInput(ns("vertexColor"), "Vertex Color:",
                         choices = c("skyblue", "red", "green", "blue", "orange",
                                     "purple", "gray", "black", "yellow", "pink"),
                         selected = "skyblue")
      )
    ),
    fluidRow(
      column(4,
             numericInput(ns("vertexLabelCex"), "Vertex Label Size:", value = 1.2, min = 0.5, max = 3)
      ),
      column(4,
             selectInput(ns("edgeColor"), "Edge Color:",
                         choices = c("gray", "black", "red", "blue", "green", "orange", "purple"),
                         selected = "gray")
      ),
      column(4,
             numericInput(ns("edgeWidth"), "Edge Width:", value = 1, min = 0.5, max = 5)
      )
    ),
    br(),
    h4("Static Network Plot Preview"),
    plotOutput(ns("staticNetworkPlot")) %>% withSpinner(),
    br(),
    downloadButton(ns("dlNetworkIgraphPNG"), "Download Static Network Plot")
  )
}

networkServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    observeEvent(data(), {
      req(data())
      d <- data()
      if (!is.null(d$contrast)) {
        updateSelectInput(session, "focus", 
                          choices = as.character(unique(c(d$contrast$treat1, d$contrast$treat2))))
      }
    })
    
    networkData <- reactive({
      req(data())
      d <- data()
      if (!is.null(d$contrast) && all(c("treat1", "treat2") %in% names(d$contrast))) {
        treatments <- as.character(unique(c(d$contrast$treat1, d$contrast$treat2)))
        if (length(treatments) == 0) return(NULL)
        nodes <- data.frame(
          id = treatments,
          label = treatments,
          title = treatments,
          stringsAsFactors = FALSE
        )
        edges <- data.frame(
          from = as.character(d$contrast$treat1),
          to   = as.character(d$contrast$treat2),
          stringsAsFactors = FALSE
        )
        if (!is.null(input$focus) && input$focus != "") {
          edges <- edges[edges$from == input$focus | edges$to == input$focus, , drop = FALSE]
        }
        nodes[] <- lapply(nodes, as.character)
        edges[] <- lapply(edges, as.character)
        list(nodes = nodes, edges = edges)
      }
    })
    
    output$networkPlot <- renderVisNetwork({
      net <- networkData()
      if (is.null(net) || nrow(net$nodes) == 0) {
        dummyNodes <- data.frame(id = "No data", label = "No data", title = "No data", stringsAsFactors = FALSE)
        dummyEdges <- data.frame(from = character(0), to = character(0), stringsAsFactors = FALSE)
        return(visNetwork(dummyNodes, dummyEdges, height = "500px", width = "100%"))
      }
      visNetwork(net$nodes, net$edges, height = "500px", width = "100%") %>% 
        visNodes(shape = "dot", size = 20) %>% 
        visEdges(smooth = FALSE)
    })
    
    output$staticNetworkPlot <- renderPlot({
      req(data())
      d <- data()
      if (!is.null(d$contrast) && all(c("treat1", "treat2") %in% names(d$contrast))) {
        edges <- d$contrast[, c("treat1", "treat2")]
        g <- igraph::graph_from_data_frame(edges, directed = FALSE)
        layoutFunc <- switch(input$layoutOption,
                             layout_with_fr = igraph::layout_with_fr,
                             layout_with_kk = igraph::layout_with_kk,
                             layout_as_tree = igraph::layout_as_tree,
                             layout_randomly = igraph::layout_randomly,
                             layout_in_circle = igraph::layout_in_circle)
        lay <- layoutFunc(g)
        plot(g, layout = lay,
             vertex.size = input$vertexSize,
             vertex.color = input$vertexColor,
             vertex.label.cex = input$vertexLabelCex,
             edge.color = input$edgeColor,
             edge.width = input$edgeWidth,
             main = "Static Network Plot (igraph)")
      }
    })
    
    output$dlNetworkIgraphPNG <- downloadHandler(
      filename = function() { "NetworkPlot_Static.png" },
      content = function(file) {
        png(file, width = 800, height = 600, units = "px", res = 96)
        req(data())
        d <- data()
        if (!is.null(d$contrast) && all(c("treat1", "treat2") %in% names(d$contrast))) {
          edges <- d$contrast[, c("treat1", "treat2")]
          g <- igraph::graph_from_data_frame(edges, directed = FALSE)
          layoutFunc <- switch(input$layoutOption,
                               layout_with_fr = igraph::layout_with_fr,
                               layout_with_kk = igraph::layout_with_kk,
                               layout_as_tree = igraph::layout_as_tree,
                               layout_randomly = igraph::layout_randomly,
                               layout_in_circle = igraph::layout_in_circle)
          lay <- layoutFunc(g)
          plot(g, layout = lay,
               vertex.size = input$vertexSize,
               vertex.color = input$vertexColor,
               vertex.label.cex = input$vertexLabelCex,
               edge.color = input$edgeColor,
               edge.width = input$edgeWidth,
               main = "Static Network Plot (igraph)")
          dev.off()
        } else {
          showNotification("No network data available for download.", type = "error")
        }
      }
    )
    
  })
}

# ---------------------------
# 6. Analysis Module (Netmeta + Optional Meta-regression)
# ---------------------------
analysisUI <- function(id) {
  ns <- NS(id)
  tagList(
    radioButtons(ns("method"), "Analysis Method:",
                 choices = c("Frequentist (netmeta)" = "freq")),
    radioButtons(ns("effectModel"), "Effect Model:",
                 choices = c("Fixed-effect" = "fixed", "Random-effects" = "random"),
                 selected = "random"),
    sliderInput(ns("confLevel"), "Confidence Level:",
                min = 0.80, max = 0.99, value = 0.95, step = 0.01),
    checkboxInput(ns("runMetareg"), "Run Meta-regression", FALSE),
    uiOutput(ns("covariateSelector")),
    verbatimTextOutput(ns("status"))
  )
}

analysisServer <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    resultContainer <- reactiveVal(NULL)
    
    observeEvent(data(), {
      req(data())
      d_all <- data()
      localEffectModel <- isolate(input$effectModel)
      localConfLevel <- isolate(input$confLevel)
      output$status <- renderText("Running frequentist analysis...")
      future({
        netfit <- netmeta(
          TE, seTE, treat1, treat2, studlab, data = d_all$contrast,
          common = (localEffectModel == "fixed"),
          random = (localEffectModel == "random"),
          level = localConfLevel,
          details.chkmultiarm = TRUE
        )
        netfit$method <- "freq"
        nma_text <- capture.output(print(netfit))
        list(model = netfit, nma_text = paste(nma_text, collapse = "\n"))
      }) %...>% resultContainer
    }, ignoreNULL = FALSE)
    
    observe({
      req(data())
      d_all <- data()
      mandatory <- c("study", "treatment", "event", "n",
                     "studlab", "treat1", "treat2", "TE", "seTE")
      availableCov <- setdiff(names(d_all$raw), mandatory)
      if (length(availableCov) > 0) {
        output$covariateSelector <- renderUI({
          selectInput(session$ns("covariate"), "Select Covariate(s) for Meta-regression:",
                      choices = availableCov, multiple = TRUE)
        })
      } else {
        output$covariateSelector <- renderUI({ NULL })
      }
    })
    
    combinedResult <- reactive({
      res <- resultContainer()
      req(res)
      metaRes <- NULL
      if (!is.null(input$runMetareg) && input$runMetareg &&
          !is.null(data()$raw) && length(input$covariate) > 0) {
        cov_formula <- as.formula(paste("~", paste(input$covariate, collapse = " + ")))
        metaRes <- tryCatch({
          netmetareg(res$model, covariate = cov_formula)
        }, error = function(e) { NULL })
      }
      res$metareg <- metaRes
      res
    })
    
    return(combinedResult)
  })
}

# ---------------------------
# 7. Diagnostics Module
# ---------------------------
diagnosticsUI <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("runDiagnostics"), "Run Diagnostics"),
    verbatimTextOutput(ns("diagText")),
    plotOutput(ns("funnelPlot")) %>% withSpinner(),
    br(),
    downloadButton(ns("dlFunnelPNG"), "Download Funnel Plot as PNG")
  )
}

diagnosticsServer <- function(id, analysisResult) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$runDiagnostics, {
      req(analysisResult())
      res <- analysisResult()
      if (res$model$method != "freq") {
        output$diagText <- renderText("Diagnostics are only available for frequentist analysis.")
      } else {
        diag_res <- decomp.design(res$model)
        output$diagText <- renderPrint({ diag_res })
      }
    })
    output$funnelPlot <- renderPlot({
      req(analysisResult())
      res <- analysisResult()
      if (res$model$method == "freq" &&
          !is.null(res$model$trts) && length(res$model$trts) >= 2) {
        funnel(res$model, order = res$model$trts)
      } else {
        plot.new()
        text(0.5, 0.5, "Funnel plot not available (insufficient treatment comparisons)", cex = 1.3)
      }
    })
    output$dlFunnelPNG <- downloadHandler(
      filename = function() { "FunnelPlot.png" },
      content = function(file) {
        png(file, width = 800, height = 600, units = "px", res = 96)
        res <- analysisResult()
        if (res$model$method == "freq" &&
            !is.null(res$model$trts) && length(res$model$trts) >= 2) {
          funnel(res$model, order = res$model$trts)
        } else {
          plot.new()
          text(0.5, 0.5, "Funnel plot not available (insufficient treatment comparisons)", cex = 1.3)
        }
        dev.off()
      }
    )
  })
}

# ---------------------------
# 8. Results Module (with additional meta‑regression outputs)
# ---------------------------
resultsUI <- function(id) {
  ns <- NS(id)
  tabsetPanel(
    tabPanel("Forest Plot", 
             fluidRow(
               column(3, textInput(ns("forestTitle"), "Plot Title:", value = "Forest Plot")),
               column(3, textInput(ns("forestXlab"), "X-axis Label:", value = "Treatment Effect")),
               column(3, sliderInput(ns("forestXlim"), "X-axis Range:",
                                     min = -10, max = 10, value = c(-5, 5), step = 0.5)),
               column(3, numericInput(ns("forestCex"), "Label Size (cex):", value = 1, min = 0.5, max = 3, step = 0.1))
             ),
             fluidRow(
               column(3, numericInput(ns("forestDigits"), "Digits (TE):", value = 2, min = 0, max = 5, step = 1)),
               column(3, numericInput(ns("forestDigitsSE"), "Digits (SE):", value = 2, min = 0, max = 5, step = 1)),
               column(3, numericInput(ns("forestDigitsPval"), "Digits (p-val):", value = 3, min = 0, max = 5, step = 1))
             ),
             fluidRow(
               column(3, selectInput(ns("forestSquareColor"), "Square Color:",
                                     choices = c("black", "blue", "red", "green", "purple"), selected = "black")),
               column(3, selectInput(ns("forestDiamondColor"), "Diamond Color:",
                                     choices = c("black", "blue", "red", "green", "purple"), selected = "blue"))
             ),
             br(),
             plotOutput(ns("forestPlot")) %>% withSpinner(),
             br(),
             downloadButton(ns("dlForestPNG"), "Download Forest Plot as PNG")
    ),
    tabPanel("Relative Effects",
             verbatimTextOutput(ns("relativeEffectsText")),
             br(),
             downloadButton(ns("dlRelativeEffects"), "Download Relative Effects Summary")
    ),
    tabPanel("NMA Results", 
             verbatimTextOutput(ns("nmaResults"))
    ),
    tabPanel("Extra Analysis",
             fluidRow(
               column(6, plotOutput(ns("scatterPlot")) %>% withSpinner()),
               column(6, verbatimTextOutput(ns("extraText")))
             )
    ),
    tabPanel("Additional Plots",
             fluidRow(
               column(12, verbatimTextOutput(ns("studySummary")))
             )
    ),
    tabPanel("Excluded Studies",
             verbatimTextOutput(ns("excludedText"))
    ),
    tabPanel("Network Graph",
             fluidRow(
               column(6, textInput(ns("netGraphTitle"), "Graph Title:", value = "Network Graph")),
               column(6, textInput(ns("longLabels"), "Full Treatment Labels (comma separated):",
                                   value = "DrugA,DrugB,DrugC,DrugD"))
             ),
             plotOutput(ns("netGraphPlot")) %>% withSpinner(),
             br(),
             downloadButton(ns("dlNetGraphPNG"), "Download Network Graph as PNG")
    ),
    tabPanel("Effect Estimate Table",
             DTOutput(ns("effectTable")),
             br(),
             downloadButton(ns("dlEffectTable"), "Download Effect Table as CSV")
    ),
    tabPanel("Treatment Ranking",
             DTOutput(ns("treatmentRanking")),
             br(),
             downloadButton(ns("dlTreatmentRanking"), "Download Treatment Ranking as CSV")
    ),
    tabPanel("Net Heat Plot",
             plotOutput(ns("netHeatPlot")) %>% withSpinner(),
             br(),
             downloadButton(ns("dlNetHeatPNG"), "Download Net Heat Plot as PNG")
    ),
    tabPanel("Net Splitting",
             plotOutput(ns("netSplitPlot")) %>% withSpinner(),
             br(),
             downloadButton(ns("dlNetSplitPNG"), "Download Net Split Plot as PNG")
    ),
    tabPanel("Comparison-Adjusted Funnel Plot",
             plotOutput(ns("funnelPlotAdj")) %>% withSpinner(),
             br(),
             downloadButton(ns("dlFunnelAdjPNG"), "Download Funnel Plot as PNG")
    ),
    tabPanel("Leave-One-Out Analysis",
             DTOutput(ns("leaveOneOut")),
             br(),
             downloadButton(ns("dlLeaveOneOut"), "Download Leave-One-Out Table as CSV")
    ),
    tabPanel("Detailed Model Summary",
             verbatimTextOutput(ns("detailedModelSummary")),
             br(),
             downloadButton(ns("dlDetailedModelSummary"), "Download Model Summary as TXT")
    ),
    tabPanel("Study-Level Analysis",
             fluidRow(
               plotOutput(ns("studyLevelPlot"), width = "800px", height = "600px") %>% withSpinner()
             ),
             fluidRow(
               verbatimTextOutput(ns("studyLevelSummary"))
             ),
             br(),
             downloadButton(ns("dlStudyLevel"), "Download Study-Level Analysis as TXT")
    ),
    tabPanel("Meta-regression",
             uiOutput(ns("metaRegressionUI")),
             h4("Meta-regression Residual Plot"),
             plotOutput(ns("metaResidualPlot")) %>% withSpinner(),
             br(),
             downloadButton(ns("dlMetaResidualPNG"), "Download Meta-regression Residual Plot as PNG")
    ),
    tabPanel("Treatment Frequency",
             fluidRow(
               column(6, textInput(ns("freqPlotTitle"), "Plot Title:", value = "Treatment Frequency")),
               column(6, numericInput(ns("freqLabelSize"), "Label Size:", value = 1, min = 0.5, max = 3, step = 0.1))
             ),
             plotOutput(ns("treatmentFreqPlot")) %>% withSpinner(),
             br(),
             downloadButton(ns("dlTreatmentFreqPNG"), "Download Treatment Frequency Plot as PNG")
    ),
    tabPanel("Raw Data Summary",
             verbatimTextOutput(ns("rawDataSummary")),
             br(),
             downloadButton(ns("dlRawDataSummary"), "Download Raw Data Summary as TXT")
    ),
    tabPanel("Extended Text Summary",
             verbatimTextOutput(ns("extendedTextSummary")),
             br(),
             downloadButton(ns("dlExtendedText"), "Download Extended Text Summary as TXT")
    ),
    tabPanel("Additional Text Results",
             verbatimTextOutput(ns("textResults")),
             br(),
             downloadButton(ns("dlTextResults"), "Download Text Results as TXT")
    )
  )
}

resultsServer <- function(id, analysisResult) {
  moduleServer(id, function(input, output, session) {
    
    output$metaRegressionUI <- renderUI({
      req(analysisResult())
      res <- analysisResult()
      if (is.null(res$metareg)) {
        h4("Meta-regression was not run or no covariate(s) selected.")
      } else {
        tagList(
          h4("Meta-regression Summary:"),
          verbatimTextOutput(session$ns("metaregSummary")),
          h4("Meta-regression Plot:"),
          plotOutput(session$ns("metaregPlot")) %>% withSpinner(),
          br(),
          downloadButton(session$ns("dlMetaregPNG"), "Download Meta-regression Plot as PNG")
        )
      }
    })
    
    output$metaregSummary <- renderPrint({
      req(analysisResult())
      res <- analysisResult()
      if (!is.null(res$metareg)) {
        cat("Meta-regression Result Structure:\n")
        str(res$metareg)
        cat("\nMeta-regression Coefficients:\n")
        print(summary(res$metareg)$coefficients)
      } else {
        cat("Meta-regression was not run or no covariate(s) selected.")
      }
    })
    
    output$metaregPlot <- renderPlot({
      req(analysisResult())
      res <- analysisResult()
      if (!is.null(res$metareg)) {
        r <- residuals(res$metareg)
        f <- fitted(res$metareg)
        if(length(r) > 0 && length(f) > 0 && all(is.finite(r)) && all(is.finite(f))) {
          plot(f, r, xlab = "Fitted Values", ylab = "Residuals", main = "Meta-regression Residual Plot", pch = 16, col = "blue")
          abline(h = 0, col = "red", lty = 2)
        } else {
          plot.new()
          text(0.5, 0.5, "Meta-regression plot not available due to non-finite values.", cex = 1.3)
        }
      } else {
        plot.new()
        text(0.5, 0.5, "Meta-regression not available.", cex = 1.3)
      }
    })
    
    output$dlMetaregPNG <- downloadHandler(
      filename = function() { "MetaRegressionPlot.png" },
      content = function(file) {
        png(file, width = 800, height = 600, units = "px", res = 96)
        req(analysisResult())
        res <- analysisResult()
        if (!is.null(res$metareg))
          plot(res$metareg)
        else {
          plot.new()
          text(0.5, 0.5, "Meta-regression plot not available.", cex = 1.3)
        }
        dev.off()
      }
    )
    
    output$dlMetaResidualPNG <- downloadHandler(
      filename = function() { "MetaRegressionResidualPlot.png" },
      content = function(file) {
        png(file, width = 800, height = 600, units = "px", res = 96)
        req(analysisResult())
        res <- analysisResult()
        if (!is.null(res$metareg)) {
          r <- residuals(res$metareg)
          f <- fitted(res$metareg)
          if(length(r) > 0 && length(f) > 0 && all(is.finite(r)) && all(is.finite(f))) {
            plot(f, r, xlab = "Fitted Values", ylab = "Residuals", main = "Meta-regression Residual Plot", pch = 16, col = "blue")
            abline(h = 0, col = "red", lty = 2)
          } else {
            plot.new()
            text(0.5, 0.5, "Meta-regression plot not available due to non-finite values.", cex = 1.3)
          }
        } else {
          plot.new()
          text(0.5, 0.5, "Meta-regression not available.", cex = 1.3)
        }
        dev.off()
      }
    )
    
    output$forestPlot <- renderPlot({
      req(analysisResult())
      res <- analysisResult()
      if (!is.null(res$model) && res$model$method == "freq") {
        netmeta:::forest.netmeta(
          res$model,
          main = input$forestTitle,
          xlab = input$forestXlab,
          xlim = input$forestXlim,
          cex = input$forestCex,
          digits = input$forestDigits,
          digits.se = input$forestDigitsSE,
          digits.pval = input$forestDigitsPval,
          col.square = input$forestSquareColor,
          col.diamond = input$forestDiamondColor
        )
      } else {
        plot.new()
        text(0.5, 0.5, "Forest plot not available.", cex = 1.3)
      }
    })
    
    output$dlForestPNG <- downloadHandler(
      filename = function() { "NMA_ForestPlot.png" },
      content = function(file) {
        png(file, width = 800, height = 600, units = "px", res = 96)
        res <- analysisResult()
        if (!is.null(res$model) && res$model$method == "freq") {
          netmeta:::forest.netmeta(
            res$model,
            main = input$forestTitle,
            xlab = input$forestXlab,
            xlim = input$forestXlim,
            cex = input$forestCex,
            digits = input$forestDigits,
            digits.se = input$forestDigitsSE,
            digits.pval = input$forestDigitsPval,
            col.square = input$forestSquareColor,
            col.diamond = input$forestDiamondColor
          )
        } else {
          plot.new()
          text(0.5, 0.5, "Forest plot not available.", cex = 1.3)
        }
        dev.off()
      }
    )
    
    output$relativeEffectsText <- renderPrint({
      req(analysisResult())
      res <- analysisResult()
      if (res$model$method == "freq" && !is.null(res$model$trts) &&
          length(unique(res$model$trts)) >= 2) {
        league <- netleague(res$model)
        print(league)
      } else {
        cat("Relative effects summary not available (insufficient treatments).")
      }
    })
    
    output$dlRelativeEffects <- downloadHandler(
      filename = function() { "RelativeEffectsSummary.txt" },
      content = function(file) {
        res <- analysisResult()
        if (res$model$method == "freq" && !is.null(res$model$trts) &&
            length(unique(res$model$trts)) >= 2) {
          league <- netleague(res$model)
          txt <- capture.output(print(league))
          writeLines(txt, con = file)
        } else {
          writeLines("Relative effects summary not available (insufficient treatments).", con = file)
        }
      }
    )
    
    output$nmaResults <- renderPrint({
      req(analysisResult())
      res <- analysisResult()
      cat("=== NMA Analysis Summary ===\n")
      cat(res$nma_text, "\n")
      if (!is.null(res$model$Q)) cat("Q statistic: ", res$model$Q, "\n")
      if (!is.null(res$model$I2)) cat("I-squared: ", res$model$I2, "\n")
      if (!is.null(res$model$tau)) cat("Tau: ", res$model$tau, "\n")
      if (!is.null(res$model$tau^2)) cat("Tau^2: ", res$model$tau^2, "\n")
      if (!is.null(res$model$pval.Q)) cat("p-value (heterogeneity): ", res$model$pval.Q, "\n")
      if (!is.null(res$model$studies.excluded) && length(res$model$studies.excluded) > 0) {
        cat("Excluded studies:", paste(res$model$studies.excluded, collapse = ", "), "\n")
      }
    })
    
    output$scatterPlot <- renderPlot({
      req(analysisResult())
      model <- analysisResult()$model
      if (!is.null(model$data)) {
        dat <- model$data
        if (all(c("TE", "seTE") %in% names(dat))) {
          plot(dat$TE, dat$seTE, 
               xlab = "Treatment Effect", 
               ylab = "Standard Error", 
               main = "Scatter Plot of Effect Sizes")
          if ("studlab" %in% names(dat)) {
            text(dat$TE, dat$seTE, labels = dat$studlab, pos = 3, cex = 0.8)
          }
        } else {
          plot.new()
          text(0.5, 0.5, "TE and seTE not found in model$data.")
        }
      } else {
        plot.new()
        text(0.5, 0.5, "Original data not available.")
      }
    })
    
    output$extraText <- renderPrint({
      req(analysisResult())
      model <- analysisResult()$model
      cat("=== Extra Analysis Summary ===\n")
      if (!is.null(model$Q)) cat("Q statistic: ", model$Q, "\n")
      if (!is.null(model$I2)) cat("I-squared: ", model$I2, "\n")
      if (!is.null(model$tau)) cat("Tau: ", model$tau, "\n")
      if (!is.null(model$tau^2)) cat("Tau^2: ", model$tau^2, "\n")
      if (!is.null(model$pval.Q)) cat("p-value (heterogeneity): ", model$pval.Q, "\n")
      if (!is.null(model$data)) {
        cat("\nData summary:\n")
        print(summary(model$data))
      }
    })
    
    output$studySummary <- renderPrint({
      req(analysisResult())
      model <- analysisResult()$model
      if (!is.null(model$data)) {
        cat("Study Data Summary:\n")
        print(summary(model$data))
      } else {
        cat("No study data available for summary.")
      }
    })
    
    output$excludedText <- renderPrint({
      req(analysisResult())
      res <- analysisResult()
      if (res$model$method == "freq") {
        if (!is.null(res$model$studies.excluded) && length(res$model$studies.excluded) > 0) {
          cat("Excluded Studies:\n")
          print(res$model$studies.excluded)
        } else {
          cat("No excluded studies.")
        }
      } else {
        cat("Excluded studies not available for non-frequentist analysis.")
      }
    })
    
    output$netGraphPlot <- renderPlot({
      req(analysisResult())
      res <- analysisResult()
      long.labels <- trimws(unlist(strsplit(input$longLabels, split = ",")))
      if (length(long.labels) != length(res$model$trts))
        long.labels <- res$model$trts
      netgraph(res$model, labels = long.labels, main = input$netGraphTitle)
    })
    output$dlNetGraphPNG <- downloadHandler(
      filename = function() { "NetworkGraph.png" },
      content = function(file) {
        png(file, width = 800, height = 600, units = "px", res = 96)
        req(analysisResult())
        res <- analysisResult()
        long.labels <- trimws(unlist(strsplit(input$longLabels, split = ",")))
        if (length(long.labels) != length(res$model$trts))
          long.labels <- res$model$trts
        netgraph(res$model, labels = long.labels, main = input$netGraphTitle)
        dev.off()
      }
    )
    
    output$effectTable <- renderDT({
      req(analysisResult())
      res <- analysisResult()
      league <- netleague(res$model)
      datatable(league$fixed, options = list(pageLength = 10))
    })
    output$dlEffectTable <- downloadHandler(
      filename = function() { "EffectEstimateTable.csv" },
      content = function(file) {
        req(analysisResult())
        res <- analysisResult()
        league <- netleague(res$model)
        write.csv(league$fixed, file, row.names = TRUE)
      }
    )
    
    output$treatmentRanking <- renderDT({
      req(analysisResult())
      res <- analysisResult()
      ranking <- netrank(res$model, small.values = "good")
      ranking_df <- data.frame(Treatment = names(ranking$ranking.common),
                               PScore = as.numeric(ranking$ranking.common))
      datatable(ranking_df, options = list(pageLength = 10))
    })
    output$dlTreatmentRanking <- downloadHandler(
      filename = function() { "TreatmentRanking.csv" },
      content = function(file) {
        req(analysisResult())
        res <- analysisResult()
        ranking <- netrank(res$model, small.values = "good")
        ranking_df <- data.frame(Treatment = names(ranking$ranking.common),
                                 PScore = as.numeric(ranking$ranking.common))
        write.csv(ranking_df, file, row.names = FALSE)
      }
    )
    
    output$netHeatPlot <- renderPlot({
      req(analysisResult())
      res <- analysisResult()
      netheat(res$model, random = TRUE)
    })
    output$dlNetHeatPNG <- downloadHandler(
      filename = function() { "NetHeatPlot.png" },
      content = function(file) {
        png(file, width = 800, height = 600, units = "px", res = 96)
        req(analysisResult())
        res <- analysisResult()
        netheat(res$model, random = TRUE)
        dev.off()
      }
    )
    
    output$netSplitPlot <- renderPlot({
      req(analysisResult())
      res <- analysisResult()
      nsplit <- netsplit(res$model)
      plot(nsplit, main = "Net Splitting Analysis")
    })
    output$dlNetSplitPNG <- downloadHandler(
      filename = function() { "NetSplitPlot.png" },
      content = function(file) {
        png(file, width = 800, height = 600, units = "px", res = 96)
        req(analysisResult())
        res <- analysisResult()
        nsplit <- netsplit(res$model)
        plot(nsplit, main = "Net Splitting Analysis")
        dev.off()
      }
    )
    
    output$funnelPlotAdj <- renderPlot({
      req(analysisResult())
      res <- analysisResult()
      if (!is.null(res$model$trts) && length(res$model$trts) >= 2) {
        funnel(res$model, order = res$model$trts, pch = 19, method.bias = "Egger")
      } else {
        plot.new()
        text(0.5, 0.5, "Comparison-adjusted funnel plot not available (insufficient treatments)", cex = 1.3)
      }
    })
    output$dlFunnelAdjPNG <- downloadHandler(
      filename = function() { "ComparisonAdjustedFunnelPlot.png" },
      content = function(file) {
        png(file, width = 800, height = 600, units = "px", res = 96)
        req(analysisResult())
        res <- analysisResult()
        if (!is.null(res$model$trts) && length(res$model$trts) >= 2) {
          funnel(res$model, order = res$model$trts, pch = 19, method.bias = "Egger")
        } else {
          plot.new()
          text(0.5, 0.5, "Comparison-adjusted funnel plot not available (insufficient treatments)", cex = 1.3)
        }
        dev.off()
      }
    )
    
    output$leaveOneOut <- renderDT({
      req(analysisResult())
      res <- analysisResult()
      if (exists("leave1out", envir = asNamespace("netmeta"))) {
        loo <- netmeta:::leave1out(res$model)
      } else {
        loo <- data.frame(Message = "leave1out function not available.")
      }
      datatable(loo, options = list(pageLength = 10))
    })
    output$dlLeaveOneOut <- downloadHandler(
      filename = function() { "LeaveOneOutAnalysis.csv" },
      content = function(file) {
        req(analysisResult())
        res <- analysisResult()
        if (exists("leave1out", envir = asNamespace("netmeta"))) {
          loo <- netmeta:::leave1out(res$model)
          write.csv(loo, file, row.names = FALSE)
        } else {
          write.csv(data.frame(Message = "leave1out function not available."), file, row.names = FALSE)
        }
      }
    )
    
    output$detailedModelSummary <- renderPrint({
      req(analysisResult())
      res <- analysisResult()
      summary(res$model)
    })
    output$dlDetailedModelSummary <- downloadHandler(
      filename = function() { "DetailedModelSummary.txt" },
      content = function(file) {
        req(analysisResult())
        res <- analysisResult()
        txt <- capture.output(summary(res$model))
        writeLines(txt, con = file)
      }
    )
    
    output$studyLevelPlot <- renderPlot({
      req(analysisResult())
      model <- analysisResult()$model
      if (!is.null(model$data)) {
        if (all(c("TE", "seTE") %in% names(model$data))) {
          boxplot(TE ~ studlab, data = model$data, main = "Boxplot of Effect Sizes by Study",
                  xlab = "Study", ylab = "Effect Size (TE)")
        } else {
          studies <- unique(model$data$studlab)
          eff <- sapply(studies, function(s) {
            subset_data <- subset(model$data, studlab == s)
            mean(subset_data$TE, na.rm = TRUE)
          })
          eff <- eff[!is.na(eff)]
          if (length(eff) == 0) {
            plot.new()
            text(0.5, 0.5, "No study-level effect available")
          } else {
            barplot(eff, main = "Average Effect by Study", xlab = "Study", ylab = "Average TE", las = 2)
          }
        }
      } else {
        plot.new()
        text(0.5, 0.5, "Study-level plot not available.")
      }
    })
    output$studyLevelSummary <- renderPrint({
      req(analysisResult())
      model <- analysisResult()$model
      if (!is.null(model$data)) {
        cat("Study-level Summary:\n")
        print(summary(model$data))
      } else {
        cat("No study-level data available.")
      }
    })
    output$dlStudyLevel <- downloadHandler(
      filename = function() { "StudyLevelAnalysis.txt" },
      content = function(file) {
        model <- analysisResult()$model
        txt <- capture.output({
          if (!is.null(model$data)) {
            cat("Study-level Summary:\n")
            print(summary(model$data))
          } else {
            cat("Study-level analysis not available.")
          }
        })
        writeLines(txt, con = file)
      }
    )
    
    output$rawDataSummary <- renderPrint({
      req(analysisResult())
      res <- analysisResult()
      cat("Raw Data Summary:\n")
      print(summary(res$model$data))
      cat("\nStructure of Raw Data:\n")
      str(res$model$data)
    })
    output$dlRawDataSummary <- downloadHandler(
      filename = function() { "RawDataSummary.txt" },
      content = function(file) {
        req(analysisResult())
        res <- analysisResult()
        txt <- capture.output({
          cat("Raw Data Summary:\n")
          print(summary(res$model$data))
          cat("\nStructure of Raw Data:\n")
          str(res$model$data)
        })
        writeLines(txt, con = file)
      }
    )
    
    output$extendedTextSummary <- renderPrint({
      req(analysisResult())
      res <- analysisResult()
      cat("=== Extended Meta-Analysis Summary ===\n")
      cat("Model Summary:\n")
      print(summary(res$model))
      cat("\nTreatment Ranking Details:\n")
      ranking <- netrank(res$model, small.values = "good")
      print(ranking)
      cat("\nHeterogeneity Statistics:\n")
      cat("I-squared:", res$model$I2, "\n")
      cat("Q statistic:", res$model$Q, "\n")
      cat("Tau:", res$model$tau, "\n")
    })
    output$dlExtendedText <- downloadHandler(
      filename = function() { "ExtendedTextSummary.txt" },
      content = function(file) {
        req(analysisResult())
        res <- analysisResult()
        txt <- capture.output({
          cat("=== Extended Meta-Analysis Summary ===\n")
          cat("Model Summary:\n")
          print(summary(res$model))
          cat("\nTreatment Ranking Details:\n")
          ranking <- netrank(res$model, small.values = "good")
          print(ranking)
          cat("\nHeterogeneity Statistics:\n")
          cat("I-squared:", res$model$I2, "\n")
          cat("Q statistic:", res$model$Q, "\n")
          cat("Tau:", res$model$tau, "\n")
        })
        writeLines(txt, con = file)
      }
    )
    
    output$textResults <- renderPrint({
      req(analysisResult())
      res <- analysisResult()
      cat("=== Additional Text Summary ===\n\n")
      cat("Treatments:\n")
      print(res$model$trts)
      cat("\nNumber of included studies:", res$model$k, "\n")
      cat("Number of treatments:", res$model$nt, "\n")
      cat("\nFixed-Effect Estimate Matrix:\n")
      print(round(res$model$TE.fixed, 2))
      cat("\nHeterogeneity (I-squared):", res$model$I2, "\n")
      cat("Q statistic:", res$model$Q, "\n")
    })
    output$dlTextResults <- downloadHandler(
      filename = function() { "AdditionalTextResults.txt" },
      content = function(file) {
        req(analysisResult())
        res <- analysisResult()
        txt <- capture.output({
          cat("=== Additional Text Summary ===\n\n")
          cat("Treatments:\n")
          print(res$model$trts)
          cat("\nNumber of included studies:", res$model$k, "\n")
          cat("Number of treatments:", res$model$nt, "\n")
          cat("\nFixed-Effect Estimate Matrix:\n")
          print(round(res$model$TE.fixed, 2))
          cat("\nHeterogeneity (I-squared):", res$model$I2, "\n")
          cat("Q statistic:", res$model$Q, "\n")
        })
        writeLines(txt, con = file)
      }
    )
    
    output$treatmentFreqPlot <- renderPlot({
      req(analysisResult())
      res <- analysisResult()
      if (!is.null(res$model$data)) {
        dat <- res$model$data
        if(all(c("treat1", "treat2") %in% names(dat))) {
          treatments <- c(as.character(dat$treat1), as.character(dat$treat2))
        } else {
          treatments <- character(0)
        }
        if(length(treatments) > 0) {
          freq <- table(treatments)
          barplot(freq, main = input$freqPlotTitle, cex.names = input$freqLabelSize,
                  xlab = "Treatment", ylab = "Frequency", col = "skyblue")
        } else {
          plot.new()
          text(0.5, 0.5, "No treatment frequency data available.")
        }
      } else {
        plot.new()
        text(0.5, 0.5, "No data available.")
      }
    })
    output$dlTreatmentFreqPNG <- downloadHandler(
      filename = function() { "TreatmentFrequency.png" },
      content = function(file) {
        png(file, width = 800, height = 600, units = "px", res = 96)
        req(analysisResult())
        res <- analysisResult()
        if (!is.null(res$model$data)) {
          dat <- res$model$data
          if(all(c("treat1", "treat2") %in% names(dat))) {
            treatments <- c(as.character(dat$treat1), as.character(dat$treat2))
          } else {
            treatments <- character(0)
          }
          if(length(treatments) > 0) {
            freq <- table(treatments)
            barplot(freq, main = input$freqPlotTitle, cex.names = input$freqLabelSize,
                    xlab = "Treatment", ylab = "Frequency", col = "skyblue")
          } else {
            plot.new()
            text(0.5, 0.5, "No treatment frequency data available.")
          }
        } else {
          plot.new()
          text(0.5, 0.5, "No data available.")
        }
        dev.off()
      }
    )
    
  })
}

# ---------------------------
# 9. Inconsistency Module
# ---------------------------
inconsistencyUI <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Design-based Inconsistency (Decomposition of Q)"),
    verbatimTextOutput(ns("decompText")),
    br(),
    downloadButton(ns("dlDecompText"), "Download Inconsistency Summary as TXT")
  )
}

inconsistencyServer <- function(id, analysisResult) {
  moduleServer(id, function(input, output, session) {
    decomp_res <- reactive({
      req(analysisResult())
      res <- analysisResult()
      if (res$model$method == "freq") {
        decomp.design(res$model)
      } else {
        NULL
      }
    })
    output$decompText <- renderPrint({
      req(decomp_res())
      print(decomp_res())
    })
    output$dlDecompText <- downloadHandler(
      filename = function() { "InconsistencySummary.txt" },
      content = function(file) {
        res <- analysisResult()
        if (res$model$method == "freq") {
          decomp_out <- decomp.design(res$model)
          txt <- capture.output(print(decomp_out))
          writeLines(txt, con = file)
        } else {
          writeLines("Design-based inconsistency not available.", con = file)
        }
      }
    )
  })
}

# ---------------------------
# 10. Main App UI & Server (bs4Dash)
# ---------------------------
ui <- bs4DashPage(
  title = "786-MIII NMA Shiny App (Frequentist Only - RR/OR with Optional Meta-regression)",
  header = bs4DashNavbar(title = "786-MIII NMA Frequentist Shiny App"),
  sidebar = bs4DashSidebar(
    bs4SidebarMenu(
      bs4SidebarMenuItem("Instructions", tabName = "instructions", icon = icon("info-circle")),
      bs4SidebarMenuItem("Data Upload", tabName = "dataUpload", icon = icon("upload")),
      bs4SidebarMenuItem("Network Plot", tabName = "network", icon = icon("project-diagram")),
      bs4SidebarMenuItem("Analysis", tabName = "analysis", icon = icon("chart-line")),
      bs4SidebarMenuItem("Diagnostics", tabName = "diagnostics", icon = icon("stethoscope")),
      bs4SidebarMenuItem("Results", tabName = "results", icon = icon("table")),
      bs4SidebarMenuItem("Inconsistency", tabName = "inconsistency", icon = icon("exclamation-triangle")),
      bs4SidebarMenuItem("Example CSV Files", tabName = "exampleCSV", icon = icon("file-alt"))
    )
  ),
  body = bs4DashBody(
    bs4TabItems(
      bs4TabItem(tabName = "instructions", instructionsUI("instructions")),
      bs4TabItem(tabName = "dataUpload", dataUploadUI("dataUpload")),
      bs4TabItem(tabName = "network", networkUI("network")),
      bs4TabItem(tabName = "analysis", analysisUI("analysis")),
      bs4TabItem(tabName = "diagnostics", diagnosticsUI("diagnostics")),
      bs4TabItem(tabName = "results", resultsUI("results")),
      bs4TabItem(tabName = "inconsistency", inconsistencyUI("inconsistency")),
      bs4TabItem(tabName = "exampleCSV", exampleCSVUI("exampleCSV"))
    )
  )
)

server <- function(input, output, session) {
  dataset <- dataUploadServer("dataUpload")
  analysisRes <- analysisServer("analysis", data = dataset)
  networkServer("network", data = dataset)
  resultsServer("results", analysisResult = analysisRes)
  diagnosticsServer("diagnostics", analysisResult = analysisRes)
  inconsistencyServer("inconsistency", analysisResult = analysisRes)
  exampleCSVServer("exampleCSV")
  instructionsServer("instructions")
}

shinyApp(ui = ui, server = server)
