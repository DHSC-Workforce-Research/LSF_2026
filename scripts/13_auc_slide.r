# =====================================================================
# 13_auc_slide.r  -  ONE widescreen DHSC slide: the survey cannot predict
# who leaves. Decile chart of predicted risk vs actual leaving + AUC box.
# Reuses repo slide helpers. Writes ONE 16:9 PNG to outputs_dir().
# Run:  source("scripts/13_auc_slide.r")
# =====================================================================
suppressWarnings(suppressMessages({
  library(dplyr); library(readr); library(ggplot2)
  purrr::walk(list.files("functions", full.names = TRUE), source)
}))
set.seed(1); K <- 5

# ---- load the cached analysis sample (script 11's cache) ----
cache_path <- file.path(derived_dir(), "lsf_3spec_sample.rds")
if (file.exists(cache_path)) {
  smp <- as.data.frame(readRDS(cache_path))
} else {
  long <- read_csv(file.path(derived_dir(), "lsf_panel_long_2020_2026.csv"), show_col_types = FALSE)
  traj <- read_csv(file.path(derived_dir(), "lsf_trajectories_classified_2020_2026.csv"), show_col_types = FALSE)
  smp  <- as.data.frame(build_funding_leaving_sample(long, traj) |> define_leaving_outcomes())
}

# ---- model: five entry funding answers -> left before finishing ----
out   <- "left_before_finish"
terms <- c("fund_availability", "grant_influence",
           "I(funding_imp_crse >= 4)", "I(funding_imp_uni >= 4)", "grant_helps_stay")
raw   <- c("fund_availability", "grant_influence", "funding_imp_crse", "funding_imp_uni", "grant_helps_stay")
d <- smp[, c(out, raw)]
d[[out]] <- as.integer(as.logical(d[[out]]))
d <- d[stats::complete.cases(d), ]

form <- as.formula(paste(out, "~", paste(terms, collapse = " + ")))
n <- nrow(d); fold <- sample(rep(1:K, length.out = n)); oos <- rep(NA_real_, n)
for (k in 1:K) {                                   # 5-fold out-of-sample scores
  te <- which(fold == k)
  m  <- suppressWarnings(glm(form, data = d[-te, ], family = binomial))
  oos[te] <- predict(m, newdata = d[te, ], type = "response")
}

auc <- function(score, y) {                        # numeric-safe (no overflow)
  ok <- !is.na(score) & !is.na(y); score <- score[ok]; y <- as.integer(y[ok])
  r <- rank(score); n1 <- as.numeric(sum(y == 1)); n0 <- as.numeric(sum(y == 0))
  if (n1 == 0 || n0 == 0) return(NA_real_)
  (sum(r[y == 1]) - n1 * (n1 + 1) / 2) / (n1 * n0)
}
a <- auc(oos, d[[out]]); base <- mean(d[[out]])

# ---- actual leaving rate within each predicted-risk tenth ----
ok  <- !is.na(oos)
dec <- data.frame(y = d[[out]][ok], dcl = dplyr::ntile(oos[ok], 10)) |>
  dplyr::group_by(dcl) |>
  dplyr::summarise(rate = 100 * mean(y), .groups = "drop") |>
  dplyr::mutate(top = dcl == 10)

# ---- the slide ----
lab_auc <- sprintf(paste0(
  "AUC %.2f\n",
  "Pick one student who left and one who didn't, at random.\n",
  "The model rates the leaver as higher-risk just %.0f%% of the\n",
  "time. A coin toss is 50%%; a useful test scores 70%% or more."), a, 100 * a)

p <- ggplot(dec, aes(dcl, rate)) +
  geom_hline(yintercept = 100 * base, linetype = "dashed",
             colour = dcol("midgrey", "#6F777B"), linewidth = .6) +
  geom_col(aes(fill = top), width = .78, show.legend = FALSE) +
  geom_text(aes(label = sprintf("%.0f%%", rate)), vjust = -0.6, size = 4.2,
            colour = dcol("ink", "#0B0C0C")) +
  annotate("label", x = 0.55, y = 56, hjust = 0, vjust = 1, label = lab_auc,
           fill = dcol("gridgrey", "#E6E6E6"), colour = dcol("ink", "#0B0C0C"),
           label.size = 0, size = 4.1, lineheight = 1.03) +
  annotate("text", x = 10.45, y = 100 * base + 2.4, hjust = 1,
           label = sprintf("Average leaving rate, %.0f%%", 100 * base),
           colour = dcol("midgrey", "#6F777B"), size = 4.2) +
  scale_fill_manual(values = c(`FALSE` = dcol("af_teal", "#28A197"),
                               `TRUE`  = dcol("af_orange", "#F46A25"))) +
  scale_x_continuous(breaks = 1:10,
    labels = c("Lowest\npredicted\nrisk", 2:9, "Highest\npredicted\nrisk"),
    expand = expansion(add = .6)) +
  scale_y_continuous(limits = c(0, 60), breaks = seq(0, 50, 10),
                     labels = function(z) paste0(z, "%")) +
  labs(
    title = "We cannot predict which students will leave, only which groups are most at risk",
    subtitle = paste0(
      "Ranked by every funding answer students give at entry, the model separates future leavers from stayers barely better than a coin toss.\n",
      "Students who worry, or who say they depend on the grant, do leave a little more often, but even the highest-risk tenth leave at only\n",
      "just above the average. The survey is a good guide to at-risk groups, not to individuals."),
    x = "Students sorted into ten equal groups by the model's predicted risk of leaving (lowest to highest)",
    y = "Share who actually left before finishing",
    caption = paste0(
      "Logistic model of the five entry funding items, five-fold cross-validated. Outcome: left before expected completion. ",
      "n = ", format(n, big.mark = ","), " fully observed students.  Source: NHS Learning Support Fund longitudinal panel 2020-2026, DHSC analysis.")) +
  theme_dhsc_slide(base = 15) +
  theme(panel.grid.major.x = element_blank())

save_slide(p, file.path(outputs_dir(), "lsf_cannot_predict_slide.png"))
message("saved -> ", file.path(outputs_dir(), "lsf_cannot_predict_slide.png"))