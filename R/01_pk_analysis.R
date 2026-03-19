# =============================================================================
# PKPD Project: Pharmacokinetic Compartmental Modeling
# Dataset: Theophylline (built-in R dataset)
# Model: One-compartment oral absorption model
# Author: PKPD Analysis Project
# =============================================================================

# ── 1. SETUP ─────────────────────────────────────────────────────────────────

suppressPackageStartupMessages({
  library(nlme)
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(patchwork)
  library(knitr)
})

cat("✓ All packages loaded.\n")

# ── 2. LOAD & PREPARE DATA ───────────────────────────────────────────────────

data("Theoph")

pk_data <- Theoph %>%
  as.data.frame() %>%
  rename(
    subject = Subject,
    wt      = Wt,
    dose    = Dose,
    time    = Time,
    conc    = conc
  ) %>%
  mutate(
    subject = as.factor(subject),
    dose_mg = dose * wt          # total dose in mg
  )

cat(sprintf("✓ Data loaded: %d subjects, %d observations\n",
            n_distinct(pk_data$subject), nrow(pk_data)))

# Summary statistics
pk_summary <- pk_data %>%
  group_by(subject) %>%
  summarise(
    weight_kg  = unique(wt),
    dose_mg_kg = unique(dose),
    total_dose = unique(dose_mg),
    Cmax       = max(conc),
    Tmax       = time[which.max(conc)],
    AUC_trap   = sum(diff(time) * (conc[-n()] + conc[-1]) / 2),
    .groups = "drop"
  )

cat("\n── Pharmacokinetic Summary by Subject ──\n")
print(pk_summary, digits = 3)

# ── 3. ONE-COMPARTMENT MODEL (ka, CL, V) ─────────────────────────────────────

# Predicted concentration: C(t) = (F*D*ka) / (V*(ka-ke)) * (exp(-ke*t) - exp(-ka*t))
# ke = CL/V

pk_model_fn <- function(params, time, dose) {
  ka <- params[1]   # absorption rate (1/h)
  CL <- params[2]   # clearance (L/h/kg)
  V  <- params[3]   # volume of distribution (L/kg)
  ke <- CL / V
  (dose * ka) / (V * (ka - ke)) * (exp(-ke * time) - exp(-ka * time))
}

# Fit nonlinear mixed-effects model using nlme
pk_nlme <- nlme(
  conc ~ SSfol(dose, time, lKe, lKa, lCl),
  data    = pk_data,
  fixed   = lKe + lKa + lCl ~ 1,
  random  = pdDiag(lKe + lKa + lCl ~ 1),
  groups  = ~ subject,
  start   = c(lKe = -2.5, lKa = 0.5, lCl = -3.2),
  control = nlmeControl(pnlsTol = 0.01, msVerbose = FALSE)
)

cat("\n── NLME Model Summary ──\n")
print(summary(pk_nlme))

# ── 4. EXTRACT PARAMETERS ────────────────────────────────────────────────────

fixed_eff  <- fixef(pk_nlme)
random_eff <- ranef(pk_nlme)

pop_ke <- exp(fixed_eff["lKe"])
pop_ka <- exp(fixed_eff["lKa"])
pop_cl <- exp(fixed_eff["lCl"])
pop_v  <- pop_cl / pop_ke
pop_t12 <- log(2) / pop_ke
pop_tmax <- log(pop_ka / pop_ke) / (pop_ka - pop_ke)

param_table <- data.frame(
  Parameter   = c("Ka (1/h)", "Ke (1/h)", "CL (L/h/kg)", "V (L/kg)", "t½ (h)", "Tmax (h)"),
  Population  = round(c(pop_ka, pop_ke, pop_cl, pop_v, pop_t12, pop_tmax), 4),
  Description = c("Absorption rate constant", "Elimination rate constant",
                  "Clearance (weight-normalized)", "Volume of distribution",
                  "Elimination half-life", "Time to peak concentration")
)

cat("\n── Population PK Parameters ──\n")
print(param_table)

# ── 5. GENERATE PREDICTIONS ──────────────────────────────────────────────────

pk_data$pred_pop <- fitted(pk_nlme, level = 0)   # population
pk_data$pred_ind <- fitted(pk_nlme, level = 1)   # individual
pk_data$resid    <- resid(pk_nlme, type = "normalized")
pk_data$wres     <- resid(pk_nlme, type = "pearson")

# Smooth time-course predictions for plotting
time_seq <- seq(0, 25, by = 0.1)
pop_pred_smooth <- data.frame(
  time = time_seq,
  conc = pk_model_fn(
    params = c(pop_ka, pop_cl, pop_v),
    time   = time_seq,
    dose   = mean(pk_data$dose)
  )
)

# Save objects for Rmd and Shiny
saveRDS(pk_data,        "data/pk_data_processed.rds")
saveRDS(pk_summary,     "data/pk_summary.rds")
saveRDS(pk_nlme,        "data/pk_nlme_model.rds")
saveRDS(param_table,    "data/param_table.rds")
saveRDS(pop_pred_smooth,"data/pop_pred_smooth.rds")

cat("\n✓ All model objects saved to data/\n")

# ── 6. DIAGNOSTIC PLOTS ──────────────────────────────────────────────────────

source("R/02_plots.R")
cat("✓ Plots generated.\n")

cat("\n══════════════════════════════════════════\n")
cat("  PK Analysis Complete!\n")
cat("  Run: rmarkdown::render('reports/pk_report.Rmd')\n")
cat("  Run: shiny::runApp('app.R') for dashboard\n")
cat("══════════════════════════════════════════\n")
