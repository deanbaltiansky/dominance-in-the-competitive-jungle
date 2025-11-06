# app.R — Mediation App (single mediator)
# Data expected at: ./data/df_s1_elg.csv and ./data/var_info.csv

suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
})

# ---- Load data ----
load_data <- function() {
  if (!file.exists("data/df_s1_elg.csv"))
    stop("Missing data/df_s1_elg.csv — please place your dataset there.")
  if (!file.exists("data/var_info.csv"))
    stop("Missing data/var_info.csv — please place your variable info file there.")
  
  df <- read.csv("data/df_s1_elg.csv", stringsAsFactors = FALSE)
  var_info <- read.csv("data/var_info.csv", stringsAsFactors = FALSE)
  list(df = df, var_info = var_info)
}

dat <- load_data()
df_s1_elg <- dat$df
var_info <- dat$var_info

# ---- Choices ----
stopifnot(all(c("var","label") %in% names(var_info)))
var_choices <- setNames(var_info$var, paste0(var_info$label, " (", var_info$var, ")"))

# ---- Helper: simple table render ----
render_results_table <- function(df) {
  tags$table(
    style = "border-collapse:collapse; font-family: system-ui, sans-serif;",
    tags$thead(
      tags$tr(lapply(names(df), function(nm)
        tags$th(style = "border:1px solid #ddd; padding:6px 10px; background:#f7f7f7;", nm)))
    ),
    tags$tbody(
      lapply(seq_len(nrow(df)), function(i)
        tags$tr(lapply(df[i,], function(val)
          tags$td(style = "border:1px solid #ddd; padding:6px 10px; text-align:center;", as.character(val)))))
    )
  )
}

# ---- Basic SVG path diagram ----
diagram_svg <- function(X, M, Y, a, b, cprime, ctotal) {
  a <- round(a, 3); b <- round(b, 3); cprime <- round(cprime, 3); ctotal <- round(ctotal, 3)
  as.character(
    tags$svg(width = 800, height = 220, viewBox = "0 0 800 220",
             tags$rect(x=40, y=60, width=160, height=60, rx=10, fill="#f2f2f2", stroke="#333"),
             tags$text(x=120, y=95, "text-anchor"="middle", style="font: 14px sans-serif;", X),
             tags$rect(x=320, y=60, width=160, height=60, rx=10, fill="#f2f2f2", stroke="#333"),
             tags$text(x=400, y=95, "text-anchor"="middle", style="font: 14px sans-serif;", M),
             tags$rect(x=600, y=60, width=160, height=60, rx=10, fill="#f2f2f2", stroke="#333"),
             tags$text(x=680, y=95, "text-anchor"="middle", style="font: 14px sans-serif;", Y),
             tags$line(x1=200, y1=90, x2=320, y2=90, stroke="#333", "marker-end"="url(#arrow)"),
             tags$text(x=260, y=75, "text-anchor"="middle", style="font: 13px sans-serif;", paste0("a = ", a)),
             tags$line(x1=480, y1=90, x2=600, y2=90, stroke="#333", "marker-end"="url(#arrow)"),
             tags$text(x=540, y=75, "text-anchor"="middle", style="font: 13px sans-serif;", paste0("b = ", b)),
             tags$line(x1=200, y1=120, x2=600, y2=120, stroke="#333", "marker-end"="url(#arrow)"),
             tags$text(x=400, y=140, "text-anchor"="middle", style="font: 13px sans-serif;", paste0("c' = ", cprime)),
             tags$line(x1=200, y1=150, x2=600, y2=150, stroke="#333", "stroke-dasharray"="6,5", "marker-end"="url(#arrow)"),
             tags$text(x=400, y=170, "text-anchor"="middle", style="font: 13px sans-serif;", paste0("c = ", ctotal)),
             tags$defs(
               tags$marker(id="arrow", viewBox="0 0 10 10", refX="10", refY="5",
                           markerWidth="8", markerHeight="8", orient="auto-start-reverse",
                           tags$path(d="M 0 0 L 10 5 L 0 10 z", fill="#333"))
             )
    )
  )
}

# ---- UI ----
ui <- fluidPage(
  tags$head(tags$title("Mediation App")),
  titlePanel("Mediation (single mediator)"),
  sidebarLayout(
    sidebarPanel(
      selectInput("predictor", "Predictor (X):", choices = var_choices),
      selectInput("mediator",  "Mediator (M):",  choices = var_choices),
      selectInput("outcome",   "Outcome (Y):",   choices = var_choices),
      selectizeInput("controls", "Controls (optional):", choices = var_choices, multiple = TRUE),
      numericInput("sims", "Bootstrap draws:", value = 5000, min = 100, step = 100),
      actionButton("run", "Run Mediation", class = "btn-primary")
    ),
    mainPanel(
      h4("Results"),
      uiOutput("results_table"),
      br(),
      h4("Path Diagram"),
      htmlOutput("diagram")
    )
  )
)

# ---- Server ----
server <- function(input, output, session) {
  results <- eventReactive(input$run, {
    req(input$predictor, input$mediator, input$outcome)
    
    data <- df_s1_elg
    X <- input$predictor; M <- input$mediator; Y <- input$outcome; C <- input$controls
    
    form_med <- as.formula(paste(M, "~", paste(c(X, C), collapse = " + ")))
    form_out <- as.formula(paste(Y, "~", paste(c(X, M, C), collapse = " + ")))
    form_tot <- as.formula(paste(Y, "~", paste(c(X, C), collapse = " + ")))
    
    fit_med <- lm(form_med, data = data)
    fit_out <- lm(form_out, data = data)
    fit_tot <- lm(form_tot, data = data)
    
    a <- unname(coef(fit_med)[X])
    b <- unname(coef(fit_out)[M])
    cprime <- unname(coef(fit_out)[X])
    ctotal <- unname(coef(fit_tot)[X])
    
    list(
      table = data.frame(
        Effect = c("ACME (indirect)", "ADE (direct)", "Total Effect", "Prop. Mediated"),
        Estimate = round(c(a*b, cprime, ctotal, (a*b)/ctotal), 3)
      ),
      a = a, b = b, cprime = cprime, ctotal = ctotal,
      X = X, M = M, Y = Y
    )
  })
  
  output$results_table <- renderUI({
    req(results())
    render_results_table(results()$table)
  })
  
  output$diagram <- renderUI({
    req(results())
    r <- results()
    HTML(diagram_svg(r$X, r$M, r$Y, r$a, r$b, r$cprime, r$ctotal))
  })
}

shinyApp(ui, server)
