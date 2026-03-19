# =============================================================================
# 02_plots.R — PK Diagnostic & Summary Plots
# =============================================================================

library(ggplot2)
library(dplyr)
library(patchwork)

# ── Theming ───────────────────────────────────────────────────────────────────

pk_theme <- theme_minimal(base_size = 12) +
  theme(
    plot.title       = element_text(face = "bold", size = 13, color = "#1a1a2e"),
    plot.subtitle    = element_text(color = "#555577", size = 10),
    axis.title       = element_text(face = "bold", color = "#333355"),
    axis.text        = element_text(color = "#555555"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(color = "#eeeeee"),
    strip.text       = element_text(face = "bold", color = "#1a1a2e"),
    legend.position  = "bottom",
    plot.background  = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "#fafafa", color = NA)
  )

subject_colors <- colorRampPalette(
  c("#2196F3","#E91E63","#4CAF50","#FF9800","#9C27B0","#00BCD4","#F44336","#795548","#607D8B","#FFEB3B","#3F51B5","#FF5722")
)(12)

# ── Load data if sourced standalone ─────────────────────────────────────────

if (!exists("pk_data"))        pk_data         <- readRDS("data/pk_data_processed.rds")
if (!exists("pk_summary"))     pk_summary       <- readRDS("data/pk_summary.rds")
if (!exists("pop_pred_smooth")) pop_pred_smooth <- readRDS("data/pop_pred_smooth.rds")

# ── PLOT 1: Observed Concentration-Time by Subject ────────────────────────────

p1 <- ggplot(pk_data, aes(x = time, y = conc, color = subject, group = subject)) +
  geom_line(alpha = 0.7, linewidth = 0.8) +
  geom_point(size = 2.2, alpha = 0.9) +
  geom_line(data = pop_pred_smooth, aes(x = time, y = conc),
            inherit.aes = FALSE, color = "#1a1a2e", linewidth = 1.3,
            linetype = "dashed") +
  scale_color_manual(values = subject_colors) +
  labs(
    title    = "Theophylline Concentration–Time Profiles",
    subtitle = "Individual curves (colored) | Population mean prediction (dashed)",
    x        = "Time (h)",
    y        = "Concentration (mg/L)",
    color    = "Subject"
  ) +
  pk_theme

# ── PLOT 2: Individual Observed vs Predicted ──────────────────────────────────

p2 <- ggplot(pk_data, aes(x = pred_ind, y = conc, color = subject)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#888888", linewidth = 0.8) +
  geom_point(size = 2.5, alpha = 0.85) +
  scale_color_manual(values = subject_colors, guide = "none") +
  labs(
    title    = "Individual Predicted vs Observed",
    subtitle = "Perfect fit = dashed line",
    x        = "Individual Predicted (mg/L)",
    y        = "Observed (mg/L)"
  ) +
  pk_theme

# ── PLOT 3: Population Predicted vs Observed ─────────────────────────────────

p3 <- ggplot(pk_data, aes(x = pred_pop, y = conc, color = subject)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#888888", linewidth = 0.8) +
  geom_smooth(method = "loess", se = TRUE, aes(group = 1),
              color = "#E91E63", fill = "#E91E63", alpha = 0.1, linewidth = 1) +
  geom_point(size = 2.5, alpha = 0.85) +
  scale_color_manual(values = subject_colors, guide = "none") +
  labs(
    title    = "Population Predicted vs Observed",
    subtitle = "LOESS smoother in pink",
    x        = "Population Predicted (mg/L)",
    y        = "Observed (mg/L)"
  ) +
  pk_theme

# ── PLOT 4: Normalized Residuals vs Time ──────────────────────────────────────

p4 <- ggplot(pk_data, aes(x = time, y = resid, color = subject)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#888888", linewidth = 0.8) +
  geom_hline(yintercept = c(-2, 2), linetype = "dotted", color = "#cc4444", linewidth = 0.6) +
  geom_smooth(method = "loess", se = FALSE, aes(group = 1),
              color = "#FF9800", linewidth = 1) +
  geom_point(size = 2.2, alpha = 0.85) +
  scale_color_manual(values = subject_colors, guide = "none") +
  labs(
    title    = "Normalized Residuals vs Time",
    subtitle = "Red dotted = ±2 SD bounds",
    x        = "Time (h)",
    y        = "Normalized Residuals"
  ) +
  pk_theme

# ── PLOT 5: Normalized Residuals vs Population Predicted ─────────────────────

p5 <- ggplot(pk_data, aes(x = pred_pop, y = resid, color = subject)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "#888888", linewidth = 0.8) +
  geom_hline(yintercept = c(-2, 2), linetype = "dotted", color = "#cc4444", linewidth = 0.6) +
  geom_smooth(method = "loess", se = FALSE, aes(group = 1),
              color = "#FF9800", linewidth = 1) +
  geom_point(size = 2.2, alpha = 0.85) +
  scale_color_manual(values = subject_colors, guide = "none") +
  labs(
    title    = "Residuals vs Population Predicted",
    subtitle = "Checking for structural model bias",
    x        = "Population Predicted (mg/L)",
    y        = "Normalized Residuals"
  ) +
  pk_theme

# ── PLOT 6: QQ Plot of Residuals ──────────────────────────────────────────────

p6 <- ggplot(pk_data, aes(sample = resid)) +
  stat_qq(color = "#2196F3", size = 2.5, alpha = 0.8) +
  stat_qq_line(color = "#E91E63", linewidth = 1, linetype = "dashed") +
  labs(
    title    = "Q-Q Plot of Normalized Residuals",
    subtitle = "Assessing normality assumption",
    x        = "Theoretical Quantiles",
    y        = "Sample Quantiles"
  ) +
  pk_theme

# ── PLOT 7: Individual Fits (Spaghetti) ──────────────────────────────────────

p7 <- ggplot(pk_data) +
  geom_line(aes(x = time, y = pred_ind, color = subject), linewidth = 1, alpha = 0.85) +
  geom_point(aes(x = time, y = conc, color = subject), size = 2.5, shape = 21,
             fill = "white", stroke = 1.2) +
  facet_wrap(~ subject, ncol = 4, labeller = label_both) +
  scale_color_manual(values = subject_colors, guide = "none") +
  labs(
    title    = "Individual PK Fits",
    subtitle = "Lines = individual model predictions | Points = observed",
    x        = "Time (h)",
    y        = "Concentration (mg/L)"
  ) +
  pk_theme

# ── PLOT 8: NCA Summary ───────────────────────────────────────────────────────

p8 <- pk_summary %>%
  pivot_longer(cols = c(Cmax, AUC_trap, Tmax), names_to = "parameter", values_to = "value") %>%
  mutate(parameter = factor(parameter,
    levels = c("Cmax", "AUC_trap", "Tmax"),
    labels = c("Cmax (mg/L)", "AUC (mg·h/L)", "Tmax (h)"))) %>%
  ggplot(aes(x = reorder(subject, value), y = value, fill = subject)) +
  geom_col(alpha = 0.85, width = 0.7) +
  facet_wrap(~ parameter, scales = "free", ncol = 3) +
  scale_fill_manual(values = subject_colors, guide = "none") +
  labs(
    title = "Non-Compartmental Analysis Summary",
    subtitle = "Cmax, AUC (trapezoidal), and Tmax by subject",
    x = "Subject",
    y = "Value"
  ) +
  coord_flip() +
  pk_theme

# ── SAVE PLOTS ────────────────────────────────────────────────────────────────

dir.create("output", showWarnings = FALSE)

ggsave("output/01_concentration_time.png",    p1, width = 10, height = 6, dpi = 150, bg = "white")
ggsave("output/02_ind_pred_obs.png",          p2, width = 6,  height = 6, dpi = 150, bg = "white")
ggsave("output/03_pop_pred_obs.png",          p3, width = 6,  height = 6, dpi = 150, bg = "white")
ggsave("output/04_resid_time.png",            p4, width = 8,  height = 5, dpi = 150, bg = "white")
ggsave("output/05_resid_pred.png",            p5, width = 8,  height = 5, dpi = 150, bg = "white")
ggsave("output/06_qq_residuals.png",          p6, width = 6,  height = 6, dpi = 150, bg = "white")
ggsave("output/07_individual_fits.png",       p7, width = 12, height = 9, dpi = 150, bg = "white")
ggsave("output/08_nca_summary.png",           p8, width = 12, height = 5, dpi = 150, bg = "white")

# Combined diagnostic panel
diag_panel <- (p2 | p3) / (p4 | p5) / (p6 | plot_spacer())
ggsave("output/00_diagnostic_panel.png", diag_panel, width = 12, height = 14, dpi = 150, bg = "white")

cat("✓ All plots saved to output/\n")

# Return plot list for Shiny / Rmd
pk_plots <- list(p1 = p1, p2 = p2, p3 = p3, p4 = p4,
                 p5 = p5, p6 = p6, p7 = p7, p8 = p8)
