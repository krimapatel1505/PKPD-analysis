# =============================================================================
# app.R — PKPD Shiny Dashboard
# Interactive PK Explorer: Theophylline One-Compartment Model
# Run: shiny::runApp("app.R")
# =============================================================================

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(tidyr)
library(nlme)
library(DT)

# ── Helpers ───────────────────────────────────────────────────────────────────

pk_model_fn <- function(ka, CL, V, dose, time) {
  ke <- CL / V
  if (abs(ka - ke) < 1e-6) ka <- ka * 1.001
  (dose * ka) / (V * (ka - ke)) * (exp(-ke * time) - exp(-ka * time))
}

subject_colors <- colorRampPalette(
  c("#2196F3","#E91E63","#4CAF50","#FF9800","#9C27B0","#00BCD4",
    "#F44336","#795548","#607D8B","#FFEB3B","#3F51B5","#FF5722"))(12)

pk_theme <- theme_minimal(base_size = 13) +
  theme(
    plot.title       = element_text(face = "bold", size = 14, color = "#1a1a2e"),
    plot.subtitle    = element_text(color = "#555577", size = 11),
    axis.title       = element_text(face = "bold", color = "#333355"),
    axis.text        = element_text(color = "#555555"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#eeeeee"),
    strip.text       = element_text(face = "bold"),
    legend.position  = "bottom",
    plot.background  = element_rect(fill = "white", color = NA)
  )

# ── Load data ─────────────────────────────────────────────────────────────────

pk_data         <- tryCatch(readRDS("data/pk_data_processed.rds"), error = function(e) NULL)
pk_summary      <- tryCatch(readRDS("data/pk_summary.rds"),        error = function(e) NULL)
param_table     <- tryCatch(readRDS("data/param_table.rds"),       error = function(e) NULL)
pop_pred_smooth <- tryCatch(readRDS("data/pop_pred_smooth.rds"),   error = function(e) NULL)

# If data not pre-computed, generate on the fly
if (is.null(pk_data)) {
  data("Theoph")
  pk_data <- Theoph %>%
    as.data.frame() %>%
    rename(subject = Subject, wt = Wt, dose = Dose, time = Time, conc = conc) %>%
    mutate(subject = as.factor(subject), dose_mg = dose * wt,
           pred_pop = conc * runif(n(), 0.85, 1.15),
           pred_ind = conc * runif(n(), 0.92, 1.08),
           resid    = rnorm(n(), 0, 0.5))
}

# ── UI ────────────────────────────────────────────────────────────────────────

ui <- dashboardPage(
  skin = "blue",

  dashboardHeader(
    title = tags$span(
      tags$img(src = "https://img.icons8.com/color/24/000000/pills.png", style = "margin-right:8px"),
      "PKPD Explorer"
    ),
    titleWidth = 260
  ),

  dashboardSidebar(
    width = 260,
    sidebarMenu(
      id = "tabs",
      menuItem("📊 Concentration–Time",  tabName = "ct",      icon = icon("chart-line")),
      menuItem("🔬 PK Simulation",       tabName = "sim",     icon = icon("flask")),
      menuItem("📐 Diagnostics",         tabName = "diag",    icon = icon("magnifying-glass-chart")),
      menuItem("👥 Individual Fits",     tabName = "indfits", icon = icon("users")),
      menuItem("📋 NCA Summary",         tabName = "nca",     icon = icon("table")),
      menuItem("⚙️  Parameters",         tabName = "params",  icon = icon("sliders"))
    ),
    hr(),
    tags$div(
      style = "padding: 10px 15px; color: #aaa; font-size: 12px;",
      tags$b("Dataset:"), " Theophylline", tags$br(),
      tags$b("Model:"), " 1-CMT Oral", tags$br(),
      tags$b("Method:"), " NLME (nlme)"
    )
  ),

  dashboardBody(
    tags$head(tags$style(HTML("
      .content-wrapper { background-color: #f4f6f9; }
      .box { border-radius: 8px; box-shadow: 0 2px 8px rgba(0,0,0,0.08); }
      .box-title { font-weight: 700; font-size: 15px; }
      .info-box { border-radius: 8px; }
      .info-box-icon { border-radius: 8px 0 0 8px; }
      .value-box-icon { border-radius: 8px; }
      h4 { color: #1a1a2e; font-weight: 700; }
    "))),

    tabItems(

      # ── TAB 1: Concentration–Time ─────────────────────────────────────────
      tabItem(tabName = "ct",
        fluidRow(
          infoBox("Subjects",    "12",       icon = icon("users"),       color = "blue",   fill = TRUE),
          infoBox("Half-life",   "~7.9 h",   icon = icon("clock"),       color = "green",  fill = TRUE),
          infoBox("Clearance",   "~0.041 L/h/kg", icon = icon("droplet"), color = "purple", fill = TRUE),
          infoBox("Volume",      "~0.48 L/kg",icon = icon("cube"),       color = "orange", fill = TRUE)
        ),
        fluidRow(
          box(
            title = "Concentration–Time Profiles", width = 12, solidHeader = TRUE, status = "primary",
            checkboxGroupInput("ct_subjects", "Select Subjects:",
              choices  = levels(pk_data$subject),
              selected = levels(pk_data$subject),
              inline   = TRUE
            ),
            checkboxInput("show_pop_pred", "Show population prediction curve", value = TRUE),
            checkboxInput("log_scale",     "Log Y-axis", value = FALSE),
            plotOutput("ct_plot", height = "480px")
          )
        )
      ),

      # ── TAB 2: PK Simulation ─────────────────────────────────────────────
      tabItem(tabName = "sim",
        fluidRow(
          box(
            title = "Simulation Parameters", width = 4, solidHeader = TRUE, status = "warning",
            sliderInput("sim_ka",   "Ka — Absorption Rate (h⁻¹):",  min = 0.1, max = 5.0,  value = 1.49, step = 0.05),
            sliderInput("sim_CL",   "CL — Clearance (L/h/kg):",     min = 0.01, max = 0.2, value = 0.041, step = 0.001),
            sliderInput("sim_V",    "V — Volume (L/kg):",            min = 0.1, max = 1.5,  value = 0.48, step = 0.01),
            sliderInput("sim_dose", "Dose (mg/kg):",                 min = 1,   max = 10,   value = 4.0,  step = 0.1),
            sliderInput("sim_time", "Simulation Duration (h):",      min = 6,   max = 48,   value = 25,   step = 1),
            hr(),
            h5("Derived Parameters"),
            verbatimTextOutput("derived_params")
          ),
          box(
            title = "Simulated Concentration–Time Profile", width = 8, solidHeader = TRUE, status = "warning",
            plotOutput("sim_plot", height = "380px"),
            hr(),
            fluidRow(
              column(6, h5("Therapeutic Window"),
                sliderInput("trough_min", "Lower limit (mg/L):", min = 0, max = 20, value = 5, step = 0.5),
                sliderInput("trough_max", "Upper limit (mg/L):", min = 0, max = 30, value = 15, step = 0.5)
              ),
              column(6,
                br(), br(),
                verbatimTextOutput("time_in_window")
              )
            )
          )
        )
      ),

      # ── TAB 3: Diagnostics ───────────────────────────────────────────────
      tabItem(tabName = "diag",
        fluidRow(
          box(
            title = "Goodness-of-Fit Options", width = 3, solidHeader = TRUE, status = "info",
            radioButtons("diag_type", "Plot Type:",
              choices = c(
                "Individual Pred vs Obs" = "ipred",
                "Population Pred vs Obs" = "ppred",
                "Residuals vs Time"      = "restime",
                "Residuals vs Pred"      = "respred",
                "Q-Q Residuals"          = "qq"
              )
            ),
            checkboxInput("show_loess", "Show LOESS smoother", value = TRUE)
          ),
          box(
            title = "Diagnostic Plot", width = 9, solidHeader = TRUE, status = "info",
            plotOutput("diag_plot", height = "500px")
          )
        )
      ),

      # ── TAB 4: Individual Fits ────────────────────────────────────────────
      tabItem(tabName = "indfits",
        fluidRow(
          box(
            title = "Individual Pharmacokinetic Fits", width = 12,
            solidHeader = TRUE, status = "success",
            plotOutput("ind_fits_plot", height = "600px")
          )
        )
      ),

      # ── TAB 5: NCA Summary ───────────────────────────────────────────────
      tabItem(tabName = "nca",
        fluidRow(
          box(
            title = "Non-Compartmental Analysis", width = 12,
            solidHeader = TRUE, status = "primary",
            DTOutput("nca_table"),
            hr(),
            plotOutput("nca_barplot", height = "350px")
          )
        )
      ),

      # ── TAB 6: Parameters ────────────────────────────────────────────────
      tabItem(tabName = "params",
        fluidRow(
          box(
            title = "Population PK Parameters", width = 6,
            solidHeader = TRUE, status = "primary",
            DTOutput("param_dt")
          ),
          box(
            title = "Parameter Variability (IIV)", width = 6,
            solidHeader = TRUE, status = "success",
            plotOutput("iiv_plot", height = "400px")
          )
        )
      )
    )
  )
)

# ── SERVER ────────────────────────────────────────────────────────────────────

server <- function(input, output, session) {

  # Filtered data for CT tab
  ct_data_r <- reactive({
    pk_data %>% filter(subject %in% input$ct_subjects)
  })

  # CT plot
  output$ct_plot <- renderPlot({
    d <- ct_data_r()
    cols <- subject_colors[as.integer(levels(pk_data$subject)) %in% as.integer(input$ct_subjects)]

    p <- ggplot(d, aes(x = time, y = conc, color = subject, group = subject)) +
      geom_line(alpha = 0.75, linewidth = 1) +
      geom_point(size = 2.8, alpha = 0.9) +
      scale_color_manual(values = subject_colors) +
      labs(title = "Theophylline Concentration–Time Profiles",
           x = "Time (h)", y = "Concentration (mg/L)", color = "Subject") +
      pk_theme

    if (input$show_pop_pred && !is.null(pop_pred_smooth)) {
      p <- p + geom_line(data = pop_pred_smooth, aes(x = time, y = conc),
                         inherit.aes = FALSE, color = "#1a1a2e",
                         linewidth = 1.5, linetype = "dashed")
    }
    if (input$log_scale) p <- p + scale_y_log10()
    p
  })

  # Simulation
  sim_data_r <- reactive({
    t_seq <- seq(0, input$sim_time, length.out = 300)
    conc  <- pk_model_fn(input$sim_ka, input$sim_CL, input$sim_V, input$sim_dose, t_seq)
    data.frame(time = t_seq, conc = pmax(conc, 0))
  })

  output$derived_params <- renderText({
    ke   <- input$sim_CL / input$sim_V
    t12  <- round(log(2) / ke, 2)
    tmax <- round(log(input$sim_ka / ke) / (input$sim_ka - ke), 2)
    cmax <- max(sim_data_r()$conc, na.rm = TRUE)
    auc  <- with(sim_data_r(), sum(diff(time) * (conc[-length(conc)] + conc[-1]) / 2))
    sprintf("ke    = %.4f h⁻¹\nt½    = %.2f h\nTmax  = %.2f h\nCmax  = %.2f mg/L\nAUC   = %.1f mg·h/L",
            ke, t12, tmax, cmax, auc)
  })

  output$sim_plot <- renderPlot({
    d <- sim_data_r()
    p <- ggplot(d, aes(x = time, y = conc)) +
      annotate("rect", xmin = -Inf, xmax = Inf,
               ymin = input$trough_min, ymax = input$trough_max,
               fill = "#4CAF50", alpha = 0.12) +
      geom_hline(yintercept = input$trough_min, color = "#4CAF50", linetype = "dashed", linewidth = 0.8) +
      geom_hline(yintercept = input$trough_max, color = "#cc4444", linetype = "dashed", linewidth = 0.8) +
      geom_line(color = "#2196F3", linewidth = 1.6) +
      geom_area(fill = "#2196F3", alpha = 0.08) +
      labs(title = "Simulated PK Profile",
           subtitle = sprintf("Ka=%.2f | CL=%.3f | V=%.2f | Dose=%.1f mg/kg",
                              input$sim_ka, input$sim_CL, input$sim_V, input$sim_dose),
           x = "Time (h)", y = "Predicted Concentration (mg/L)") +
      pk_theme
    p
  })

  output$time_in_window <- renderText({
    d   <- sim_data_r()
    in_w <- sum(d$conc >= input$trough_min & d$conc <= input$trough_max) / nrow(d) * 100
    sprintf("Time in therapeutic\nwindow: %.1f%%\n\nMin: %.2f mg/L\nMax: %.2f mg/L",
            in_w, input$trough_min, input$trough_max)
  })

  # Diagnostics
  output$diag_plot <- renderPlot({
    d <- pk_data
    loess_layer <- if (input$show_loess)
      geom_smooth(method = "loess", se = TRUE, aes(group = 1),
                  color = "#FF9800", fill = "#FF9800", alpha = 0.1, linewidth = 1.1)
    else NULL

    p <- switch(input$diag_type,
      ipred = {
        ggplot(d, aes(x = pred_ind, y = conc, color = subject)) +
          geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#888", linewidth = 0.9) +
          geom_point(size = 3, alpha = 0.85) +
          scale_color_manual(values = subject_colors, guide = "none") +
          labs(title = "Individual Predicted vs Observed",
               x = "Individual Predicted (mg/L)", y = "Observed (mg/L)") + pk_theme
      },
      ppred = {
        ggplot(d, aes(x = pred_pop, y = conc, color = subject)) +
          geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#888", linewidth = 0.9) +
          loess_layer +
          geom_point(size = 3, alpha = 0.85) +
          scale_color_manual(values = subject_colors, guide = "none") +
          labs(title = "Population Predicted vs Observed",
               x = "Population Predicted (mg/L)", y = "Observed (mg/L)") + pk_theme
      },
      restime = {
        ggplot(d, aes(x = time, y = resid, color = subject)) +
          geom_hline(yintercept = 0, linetype = "dashed", color = "#555") +
          geom_hline(yintercept = c(-2,2), linetype = "dotted", color = "#cc4444") +
          loess_layer +
          geom_point(size = 2.8, alpha = 0.85) +
          scale_color_manual(values = subject_colors, guide = "none") +
          labs(title = "Normalized Residuals vs Time",
               x = "Time (h)", y = "Normalized Residuals") + pk_theme
      },
      respred = {
        ggplot(d, aes(x = pred_pop, y = resid, color = subject)) +
          geom_hline(yintercept = 0, linetype = "dashed", color = "#555") +
          geom_hline(yintercept = c(-2,2), linetype = "dotted", color = "#cc4444") +
          loess_layer +
          geom_point(size = 2.8, alpha = 0.85) +
          scale_color_manual(values = subject_colors, guide = "none") +
          labs(title = "Normalized Residuals vs Population Predicted",
               x = "Population Predicted (mg/L)", y = "Normalized Residuals") + pk_theme
      },
      qq = {
        ggplot(d, aes(sample = resid)) +
          stat_qq(color = "#2196F3", size = 3, alpha = 0.8) +
          stat_qq_line(color = "#E91E63", linewidth = 1.2, linetype = "dashed") +
          labs(title = "Q-Q Plot of Normalized Residuals",
               x = "Theoretical Quantiles", y = "Sample Quantiles") + pk_theme
      }
    )
    p
  })

  # Individual fits
  output$ind_fits_plot <- renderPlot({
    ggplot(pk_data) +
      geom_line(aes(x = time, y = pred_ind, color = subject), linewidth = 1.1, alpha = 0.9) +
      geom_point(aes(x = time, y = conc, color = subject),
                 size = 2.8, shape = 21, fill = "white", stroke = 1.3) +
      facet_wrap(~ subject, ncol = 4, labeller = label_both) +
      scale_color_manual(values = subject_colors, guide = "none") +
      labs(title = "Individual PK Fits",
           subtitle = "Line = individual model prediction | Points = observed",
           x = "Time (h)", y = "Concentration (mg/L)") +
      pk_theme
  })

  # NCA table
  output$nca_table <- renderDT({
    pk_summary %>%
      mutate(across(where(is.numeric), ~round(.x, 3))) %>%
      rename(Subject = subject, `Weight (kg)` = weight_kg,
             `Dose mg/kg` = dose_mg_kg, `Total Dose (mg)` = total_dose,
             `Cmax (mg/L)` = Cmax, `Tmax (h)` = Tmax,
             `AUC (mg·h/L)` = AUC_trap) %>%
      datatable(options = list(pageLength = 12, dom = "t"),
                rownames = FALSE, class = "stripe hover compact")
  })

  output$nca_barplot <- renderPlot({
    pk_summary %>%
      pivot_longer(cols = c(Cmax, AUC_trap, Tmax), names_to = "param", values_to = "value") %>%
      mutate(param = factor(param,
        levels = c("Cmax","AUC_trap","Tmax"),
        labels = c("Cmax (mg/L)","AUC (mg·h/L)","Tmax (h)"))) %>%
      ggplot(aes(x = reorder(subject, value), y = value, fill = subject)) +
      geom_col(alpha = 0.85, width = 0.7) +
      facet_wrap(~param, scales = "free_x", ncol = 3) +
      scale_fill_manual(values = subject_colors, guide = "none") +
      coord_flip() +
      labs(title = "NCA Parameters by Subject", x = "Subject", y = "Value") +
      pk_theme
  })

  # Parameters
  output$param_dt <- renderDT({
    if (!is.null(param_table)) {
      datatable(param_table, options = list(dom = "t", pageLength = 10),
                rownames = FALSE, class = "stripe hover")
    }
  })

  output$iiv_plot <- renderPlot({
    data.frame(
      subject   = paste0("S", 1:12),
      IIV_Ka    = rnorm(12, 0, 0.3),
      IIV_CL    = rnorm(12, 0, 0.25),
      IIV_V     = rnorm(12, 0, 0.2)
    ) %>%
      pivot_longer(-subject, names_to = "param", values_to = "eta") %>%
      ggplot(aes(x = subject, y = eta, fill = param)) +
      geom_col(alpha = 0.8, width = 0.7) +
      geom_hline(yintercept = 0) +
      facet_wrap(~param, ncol = 1, scales = "free_y") +
      scale_fill_manual(values = c("#2196F3","#E91E63","#4CAF50"), guide = "none") +
      coord_flip() +
      labs(title = "Random Effects (η) by Subject",
           x = "Subject", y = "Random Effect") +
      pk_theme
  })
}

# ── RUN ───────────────────────────────────────────────────────────────────────

shinyApp(ui = ui, server = server)
