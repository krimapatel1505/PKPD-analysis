# 💊 PKPD Pharmacokinetic Analysis in R

> One-Compartment Oral Absorption Model • Theophylline Dataset • NLME Modeling

[![R](https://img.shields.io/badge/R-%3E%3D4.2-blue.svg)](https://www.r-project.org/)
[![nlme](https://img.shields.io/badge/nlme-NLME-green.svg)](https://cran.r-project.org/package=nlme)
[![Shiny](https://img.shields.io/badge/Shiny-Dashboard-orange.svg)](https://shiny.rstudio.com/)
[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](LICENSE)

---

## 📋 Overview

This project implements a **population pharmacokinetic (PopPK) analysis** using the classic Theophylline dataset. The analysis follows industry-standard NLME methodology to estimate population PK parameters, quantify inter-individual variability, and assess model adequacy.

### What's Included

| Component | Description |
|-----------|-------------|
| `R/01_pk_analysis.R` | Main PK analysis: data prep, NLME fitting, parameter extraction |
| `R/02_plots.R` | Diagnostic and summary plots (8 publication-ready figures) |
| `reports/pk_report.Rmd` | R Markdown report (renders to HTML) |
| `app.R` | Interactive Shiny dashboard |
| `data/` | Processed model objects (generated on first run) |
| `output/` | Saved diagnostic plots (PNG) |

---

## 🔬 Model Specification

**One-Compartment Model with First-Order Oral Absorption:**

$$C(t) = \frac{F \cdot D \cdot k_a}{V(k_a - k_e)} \left[ e^{-k_e t} - e^{-k_a t} \right]$$

| Parameter | Symbol | Units |
|-----------|--------|-------|
| Absorption rate constant | Ka | h⁻¹ |
| Elimination rate constant | Ke | h⁻¹ |
| Apparent oral clearance | CL | L/h/kg |
| Volume of distribution | V | L/kg |
| Elimination half-life | t½ | h |

**Method:** Nonlinear mixed-effects modeling (`nlme::nlme()`) with diagonal random effects on Ka, CL, and V (log-scale).

---

## 📦 Package Dependencies

```r
# Install all required packages
install.packages(c(
  "nlme",            # NLME modeling
  "ggplot2",         # Plotting
  "dplyr",           # Data wrangling
  "tidyr",           # Data reshaping
  "patchwork",       # Plot composition
  "knitr",           # Report knitting
  "kableExtra",      # Table formatting
  "rmarkdown",       # HTML report
  "shiny",           # Dashboard
  "shinydashboard",  # Dashboard UI
  "DT"               # Interactive tables
))
```

---

## 🚀 Quick Start

### 1. Clone the repository

```bash
git clone https://github.com/YOUR_USERNAME/pkpd-analysis-r.git
cd pkpd-analysis-r
```

### 2. Run the main PK analysis

```r
# In RStudio:
source("R/01_pk_analysis.R")
```

This will:
- Load and process the Theophylline dataset
- Fit the one-compartment NLME model
- Extract population PK parameters
- Save all model objects to `data/`
- Generate and save diagnostic plots to `output/`

### 3. Generate the HTML report

```r
rmarkdown::render("reports/pk_report.Rmd", output_dir = "reports/")
```

### 4. Launch the Shiny dashboard

```r
shiny::runApp("app.R")
```

---

## 📊 Outputs

### Diagnostic Plots

| Plot | Description |
|------|-------------|
| `01_concentration_time.png` | Individual + population concentration-time profiles |
| `02_ind_pred_obs.png` | Individual predicted vs. observed |
| `03_pop_pred_obs.png` | Population predicted vs. observed |
| `04_resid_time.png` | Normalized residuals vs. time |
| `05_resid_pred.png` | Normalized residuals vs. population prediction |
| `06_qq_residuals.png` | Q-Q plot of normalized residuals |
| `07_individual_fits.png` | Faceted individual PK fits |
| `08_nca_summary.png` | NCA parameters (Cmax, AUC, Tmax) by subject |
| `00_diagnostic_panel.png` | Combined diagnostic panel |

### Shiny Dashboard Tabs

- **Concentration–Time** — Interactive subject selection and profile visualization
- **PK Simulation** — Explore the effect of Ka, CL, V, and dose on PK profiles
- **Diagnostics** — Interactive goodness-of-fit plots with LOESS smoother
- **Individual Fits** — Faceted individual model fits
- **NCA Summary** — Searchable NCA table and bar charts
- **Parameters** — Population parameter estimates and IIV visualization

---

## 📁 Project Structure

```
pkpd-analysis-r/
├── R/
│   ├── 01_pk_analysis.R        # Main analysis
│   └── 02_plots.R              # Plot generation
├── data/
│   ├── pk_data_processed.rds   # Processed dataset with predictions
│   ├── pk_summary.rds          # NCA summary table
│   ├── pk_nlme_model.rds       # Fitted NLME model object
│   ├── param_table.rds         # Population PK parameters
│   └── pop_pred_smooth.rds     # Smooth population prediction
├── output/
│   └── *.png                   # Diagnostic plots
├── reports/
│   └── pk_report.Rmd           # R Markdown report
├── app.R                       # Shiny dashboard
├── .gitignore
├── LICENSE
└── README.md
```

---

## 📚 Dataset

The **Theophylline** dataset is included in base R (`datasets` package):

- **Reference:** Boeckmann, Sheiner & Beal (1994), NONMEM Users Guide: Part V
- **12 healthy subjects** with single oral doses of theophylline
- **Weight-adjusted dosing** (~4 mg/kg)
- **10–11 plasma samples per subject** over 25 hours

---

## 🧮 Population PK Results

| Parameter | Population Estimate |
|-----------|---------------------|
| Ka        | ~1.49 h⁻¹ |
| Ke        | ~0.085 h⁻¹ |
| CL        | ~0.041 L/h/kg |
| V         | ~0.48 L/kg |
| **t½**    | **~7.9 hours** |

---

## 🤝 Contributing

Pull requests are welcome. For major changes, please open an issue first to discuss what you would like to change.

---

## 📄 License

[MIT](LICENSE) © PKPD Analysis Project

---

*Built with ❤️ in R | nlme • ggplot2 • Shiny*
