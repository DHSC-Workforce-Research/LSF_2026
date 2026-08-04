# ===========================================================================
# functions/analysis_leaver_prediction.r
#
# analysis_leaver_prediction()  -  a PREDICTIVE benchmark, not a decomposition.
# The three Shapley decompositions (functions/analysis_importance.r) all show
# the additive logit model explains a small share of the variation in leaving
# (McFadden pseudo-R2 in the low single digits to a few tens of a percent).
# This function asks whether that low ceiling is a property of the DATA or of
# the MODEL CLASS: does a more flexible model (a tree, a forest) do
# meaningfully better than the additive logit at telling apart who leaves and
# who stays, out of sample?
#
# Three frames, one per stage of the triptych, built with the SAME code paths
# as the corresponding decomposition (rv_entry_sample() / importance_build_
# wave_panel(), functions/analysis_importance.r):
#   entry_ever   Y1 students,        outcome left_before_finish
#   y1_next      Y1 at-risk rows,    outcome left_next
#   cont_next    continuing at-risk, outcome left_next (plus financial
#                confidence and considered-leaving as predictors, since those
#                questions exist at this stage)
#
# Predictors, per frame, are a SIMPLER set than the corresponding decomposition
# fits (no course_family / grant-components block; the decompositions exist to
# answer a different question - shares of explained variation by BLOCK - this
# benchmark exists to answer a single number - can a test-set AUC be raised at
# all): course, region, cohort/year, real LSF value (rv), the five entry
# funding answers; confidence and leave_course are added for cont_next only.
#
# Three models per frame, in increasing flexibility:
#   logit_main  glm, main effects (course/region/cohort as ordinary factors,
#               same additive form the decomposition's full model uses)
#   tree        rpart::rpart(method = "class"), pruned to the 1-SE rule off
#               its own cross-validated cptable, minbucket 200
#   forest      randomForest::randomForest, 200 trees, mtry default, sampsize
#               capped at 50000 rows for runtime - ONLY if the package is
#               installed (requireNamespace() guard); if it is not, the row is
#               written with auc/tjur/base_rate = NA and note = "package not
#               installed". Never stop() for a missing optional package.
#
# rpart cannot use a 100+-level factor well and randomForest refuses factors
# with more than 53 levels outright, so `course` (which can run to that many
# distinct programmes) is NOT a predictor for tree/forest. In its place:
#   (a) course_family (nursing / dental / ahp / other, 4 levels)
#   (b) course_te - TARGET ENCODING: the mean outcome per course computed ON
#       THE TRAINING SPLIT ONLY, then looked up for every row (train and
#       test) by its course. This is not a leak: the encoding map is a
#       function of train-split outcomes alone, and is applied to test rows
#       exactly as a fitted coefficient would be. A course with zero rows in
#       the training split falls back to the training split's overall mean.
# logit_main keeps the raw `course` factor (glm handles any number of levels;
# it is exactly what fixest::feglm's fixed-effects absorption in the
# decompositions is doing, just without the FE trick).
#
# SPLIT. Train/test is GROUPED by UniqueID - a student's rows (one for
# entry_ever and y1_next, possibly several for cont_next) are wholly in train
# or wholly in test, never split across both, so no model sees a student at
# test time it was partly trained on. 70/30, set.seed(1) taken immediately
# before the split, the SAME split reused for every model within a frame (the
# three models are compared on identical held-out rows).
#
# CAVEATS.
#   - PREDICTIVE benchmark, not causal: a higher test AUC says a model class
#     can tell leavers from stayers better out of sample, not that any
#     predictor here causes leaving.
#   - the grouped split is what prevents STUDENT LEAKAGE (the same student's
#     rows appearing in both train and test, which would let a model
#     "memorise" a student rather than generalise); a plain random row split
#     would inflate cont_next's apparent accuracy, where one student
#     contributes several rows.
#   - target encoding for `course_te` is fit on the training split only,
#     exactly as any other trained parameter; see the design note above.
#
# Writes to tables_dir():
#   tbl_leaver_prediction.csv   frame, model, auc, tjur, base_rate, n_train,
#                                n_test, note  (9 rows = 3 frames x 3 models;
#                                forest rows are NA if randomForest is absent)
# ===========================================================================

# ---- constants (this file only; scripts/00_config.r is not touched here) --
LP_TRAIN_FRAC          <- 0.70    # grouped train share
LP_SEED                <- 1L      # set immediately before every split
LP_TREE_MINBUCKET      <- 200L    # rpart minimum leaf size
LP_TREE_CP_INIT        <- 0.0001  # small starting cp so the cptable has room to prune from
LP_TREE_XVAL           <- 10L     # rpart's internal cross-validation folds for the cptable
LP_FOREST_NTREE        <- 200L
LP_FOREST_SAMPSIZE_CAP <- 50000L  # capped for runtime, per spec

# ===========================================================================
# small shared helpers
# ===========================================================================

# GROUPED 70/30 split by id: returns "train"/"test" aligned to `ids`. Called
# once per frame (not once per model), so all three models in a frame see the
# identical split.
lp_grouped_split <- function(ids) {
  set.seed(LP_SEED)
  uid <- unique(ids)
  n_train <- floor(LP_TRAIN_FRAC * length(uid))
  train_ids <- sample(uid, size = n_train)
  ifelse(ids %in% train_ids, "train", "test")
}

# Target encoding: mean of y_train per level of course_train, looked up for
# every value in course_query (train and test alike). Levels absent from the
# training split fall back to the training split's overall mean - documented
# in the file header as the no-leakage guarantee (fit on train only).
lp_target_encode <- function(course_train, y_train, course_query) {
  te <- tapply(as.numeric(y_train), as.character(course_train), mean)
  gmean <- mean(as.numeric(y_train), na.rm = TRUE)
  out <- unname(te[as.character(course_query)])
  out[is.na(out)] <- gmean
  as.numeric(out)
}

# 1-SE pruning rule off an rpart cptable: find the minimum xerror row, take
# its xerror + xstd as the threshold, and prune to the SMALLEST tree (largest
# cp, i.e. the first row) whose xerror is still under that threshold.
lp_prune_1se <- function(fit) {
  cpt <- fit$cptable
  if (is.null(cpt) || nrow(cpt) < 1L) return(fit)
  min_row  <- which.min(cpt[, "xerror"])
  thresh   <- cpt[min_row, "xerror"] + cpt[min_row, "xstd"]
  best_row <- min(which(cpt[, "xerror"] <= thresh))
  rpart::prune(fit, cp = cpt[best_row, "CP"])
}

lp_score_row <- function(phat, y, n_train, n_test, note = "") {
  y <- as.integer(y)
  tibble::tibble(
    auc       = auc_score(phat, y),
    tjur      = mean(phat[y == 1L], na.rm = TRUE) - mean(phat[y == 0L], na.rm = TRUE),
    base_rate = mean(y, na.rm = TRUE),
    n_train   = n_train,
    n_test    = n_test,
    note      = note
  )
}

lp_na_row <- function(n_train, n_test, note) {
  tibble::tibble(auc = NA_real_, tjur = NA_real_, base_rate = NA_real_,
                 n_train = n_train, n_test = n_test, note = note)
}

# ---- the three model fitters, one frame's train/test at a time -------------

lp_fit_logit <- function(train, test, outcome, base_vars, extra_vars = character(0)) {
  rhs <- paste(c(base_vars, extra_vars), collapse = " + ")
  fml <- tryCatch(stats::as.formula(paste0(outcome, " ~ ", rhs)), error = function(e) NULL)
  if (is.null(fml)) return(lp_na_row(nrow(train), nrow(test), "bad formula"))

  fit <- tryCatch(
    suppressWarnings(stats::glm(fml, data = train, family = stats::binomial())),
    error = function(e) NULL
  )
  if (is.null(fit)) return(lp_na_row(nrow(train), nrow(test), "glm fit failed"))

  phat <- tryCatch(
    suppressWarnings(as.numeric(stats::predict(fit, newdata = test, type = "response"))),
    error = function(e) rep(NA_real_, nrow(test))
  )
  note <- if (any(is.na(phat)))
    "some test predictions NA (a factor level in test was absent from train)" else ""
  lp_score_row(phat, test[[outcome]], nrow(train), nrow(test), note)
}

# The tree is fit as a REGRESSION tree on the 0/1 outcome (method = "anova"),
# not a classification tree. rpart's classification default takes the
# OBSERVED class mix as its prior, and under imbalance (left_next runs well
# under 50% positive) that prior can make the tree refuse to split at any
# cp/minbucket even when a predictor carries real signal (confirmed on
# synthetic data during acceptance testing: a predictor correlated ~0.37
# with a ~9% positive outcome produced a single-node tree until the prior
# was overridden). The anova tree minimises squared error on the 0/1
# outcome (the Brier score), so its leaf means are event probabilities, its
# ranking feeds AUC directly, and the imbalance degeneracy does not arise.
lp_fit_tree <- function(train, test, outcome, tree_vars) {
  rhs <- paste(tree_vars, collapse = " + ")
  fml <- tryCatch(stats::as.formula(paste0(outcome, " ~ ", rhs)), error = function(e) NULL)
  if (is.null(fml)) return(lp_na_row(nrow(train), nrow(test), "bad formula"))

  fit <- tryCatch(
    rpart::rpart(fml, data = train, method = "anova",
                 control = rpart::rpart.control(minbucket = LP_TREE_MINBUCKET,
                                                cp = LP_TREE_CP_INIT, xval = LP_TREE_XVAL)),
    error = function(e) NULL
  )
  if (is.null(fit)) return(lp_na_row(nrow(train), nrow(test), "rpart fit failed"))
  fit <- tryCatch(lp_prune_1se(fit), error = function(e) fit)

  pred <- tryCatch(as.numeric(stats::predict(fit, newdata = test)), error = function(e) NULL)
  if (is.null(pred))
    return(lp_na_row(nrow(train), nrow(test), "rpart predict failed"))
  lp_score_row(pred, test[[outcome]], nrow(train), nrow(test), "")
}

lp_fit_forest <- function(train, test, outcome, forest_vars) {
  if (!requireNamespace("randomForest", quietly = TRUE))
    return(lp_na_row(nrow(train), nrow(test), "package not installed"))

  xtrain <- train[, forest_vars, drop = FALSE]
  xtest  <- test[,  forest_vars, drop = FALSE]
  ss <- min(nrow(train), LP_FOREST_SAMPSIZE_CAP)

  fit <- tryCatch(
    randomForest::randomForest(x = xtrain, y = train$.y_factor,
                               ntree = LP_FOREST_NTREE, sampsize = ss),
    error = function(e) NULL
  )
  if (is.null(fit)) return(lp_na_row(nrow(train), nrow(test), "randomForest fit failed"))

  pred <- tryCatch(stats::predict(fit, newdata = xtest, type = "prob"), error = function(e) NULL)
  if (is.null(pred) || !"1" %in% colnames(pred))
    return(lp_na_row(nrow(train), nrow(test), "randomForest predict failed"))
  lp_score_row(as.numeric(pred[, "1"]), test[[outcome]], nrow(train), nrow(test), "")
}

# Runs all three models on one frame's already-built sample `d` (must carry
# UniqueID, the outcome column, `course`, and every variable named in
# base_vars/tree_vars). Splits once, target-encodes once, fits all three.
lp_run_frame <- function(frame_label, d, outcome, base_vars, tree_vars,
                         extra_logit_vars = character(0)) {
  stopifnot(all(c("UniqueID", "course", outcome) %in% names(d)))

  d$.split    <- lp_grouped_split(d$UniqueID)
  d$.y_factor <- factor(d[[outcome]], levels = c(0, 1))

  train_course <- d$course[d$.split == "train"]
  train_y      <- d[[outcome]][d$.split == "train"]
  d$course_te  <- lp_target_encode(train_course, train_y, d$course)

  train <- d[d$.split == "train", , drop = FALSE]
  test  <- d[d$.split == "test",  , drop = FALSE]

  # ---- level alignment, once per frame, shared by all three models ---------
  # A factor level wholly absent from the training split (a tiny course, the
  # 3-student "other" family) makes predict.glm and predict.randomForest
  # error on the ENTIRE test set, not just the affected rows. Drop those test
  # rows here, once, so all three models score on the identical held-out rows
  # and none can fail wholesale on a level it never saw. The count goes in
  # the note column. droplevels(train) also stops glm building all-zero
  # (rank-deficient) columns for levels the training split does not contain.
  fac_vars <- intersect(c("course", "region", "cohort_f", "course_family"), names(d))
  train <- droplevels(train)
  keep  <- rep(TRUE, nrow(test))
  for (v in fac_vars)
    keep <- keep & (as.character(test[[v]]) %in% as.character(unique(train[[v]])))
  n_unseen <- sum(!keep)
  if (n_unseen > 0L) {
    message("leaver prediction (", frame_label, "): dropped ", n_unseen,
            " test rows whose factor levels are absent from the training split.")
    test <- test[keep, , drop = FALSE]
  }
  align_note <- if (n_unseen > 0L)
    sprintf("%d unseen-level test rows dropped", n_unseen) else ""

  logit  <- lp_fit_logit(train, test, outcome, base_vars, extra_logit_vars)
  tree   <- lp_fit_tree(train, test, outcome, tree_vars)
  forest <- lp_fit_forest(train, test, outcome, tree_vars)
  if (nzchar(align_note)) {
    logit$note  <- trimws(paste(logit$note,  align_note, sep = "; "), whitespace = "[; ]")
    tree$note   <- trimws(paste(tree$note,   align_note, sep = "; "), whitespace = "[; ]")
    forest$note <- trimws(paste(forest$note, align_note, sep = "; "), whitespace = "[; ]")
  }

  dplyr::bind_cols(
    tibble::tibble(frame = frame_label, model = c("logit_main", "tree", "forest")),
    dplyr::bind_rows(logit, tree, forest)
  )
}

# ===========================================================================
# frame builders - one sample per frame, built with the same code paths the
# corresponding Shapley decomposition uses (functions/analysis_importance.r).
# Each attaches its own course_family (crosswalk-or-regex, same as analysis_
# importance()'s entry frame) because tree/forest need it and none of these
# three frames' underlying samples carry it by default.
# ===========================================================================

# Shared course_family attach, duplicated (not called) from analysis_
# importance()'s entry-frame block deliberately: that wrapper's numbers are
# pinned and this file must not touch it. Operates on any data frame with a
# character `course` column and returns a factor(levels = PH_FAMILIES).
lp_attach_course_family <- function(df, label) {
  ph_files_present <- file.exists(file.path(REF_DIR, "placement_hours.csv")) &&
    file.exists(file.path(REF_DIR, "course_crosswalk.csv"))

  fam_out <- NULL
  if (ph_files_present) {
    fam_out <- tryCatch(attach_placement_hours(df, dir = REF_DIR), error = function(e) {
      message("leaver prediction (", label, "): attach_placement_hours() failed (",
              conditionMessage(e), "); falling back to course-string regex for course_family.")
      NULL
    })
  }

  if (!is.null(fam_out)) {
    fam <- as.character(fam_out$course_family)
    na_fam <- is.na(fam)
    if (any(na_fam)) fam[na_fam] <- importance_course_family_regex(df$course[na_fam])
  } else {
    message("leaver prediction (", label, "): reference/placement_hours.csv or ",
            "course_crosswalk.csv absent; course_family via regex fallback.")
    fam <- importance_course_family_regex(df$course)
  }
  factor(fam, levels = PH_FAMILIES)
}

lp_build_entry_ever <- function() {
  progress("leaver prediction: building entry_ever frame ...")

  ref    <- read_csv(file.path(REF_DIR, "provider_costofliving.csv"), show_col_types = FALSE, progress = FALSE)
  cpih   <- read_csv(file.path(REF_DIR, "cpih_index.csv"),           show_col_types = FALSE, progress = FALSE)
  awards <- read_csv(file.path(REF_DIR, "lsf_awards.csv"),           show_col_types = FALSE, progress = FALSE)

  samp <- rv_entry_sample(ref, awards, cpih, .label = "leaver prediction (entry_ever)") |>
    mutate(
      crit_course = as.integer(suppressWarnings(as.integer(funding_imp_crse)) >= 4L),
      crit_uni    = as.integer(suppressWarnings(as.integer(funding_imp_uni))  >= 4L),
      fund_availability  = to_01(fund_availability),
      grant_influence    = to_01(grant_influence),
      grant_helps_stay   = to_01(grant_helps_stay),
      left_before_finish = to_01(left_before_finish),
      course   = as.character(course),
      region   = as.character(region),
      cohort_f = factor(as.integer(entry_year)),
      rv       = as.numeric(scale(.data[[PRIMARY]]))
    )

  samp$course_family <- lp_attach_course_family(samp, "entry_ever")

  need_vars <- unique(c("UniqueID", "left_before_finish", "course", "course_family",
                        "region", "cohort_f", "rv", SURVEY_VARS))
  miss <- setdiff(need_vars, names(samp))
  if (length(miss))
    stop("analysis_leaver_prediction (entry_ever): sample is missing: ",
         paste(miss, collapse = ", "), call. = FALSE)

  ok <- stats::complete.cases(samp[need_vars])
  d  <- as.data.frame(samp[ok, need_vars, drop = FALSE])
  d$course <- factor(d$course)
  d$region <- factor(d$region)
  progress(sprintf("leaver prediction: entry_ever sample n=%s", format(nrow(d), big.mark = ",")))
  d
}

lp_build_y1_next <- function() {
  progress("leaver prediction: building y1_next frame ...")

  panel <- importance_build_wave_panel()
  first_year_row <- panel$first_year %in% TRUE | panel$year == panel$course_first_year_wave

  panel <- panel |> mutate(region = as.character(region), cohort_f = factor(as.integer(year)))

  need_vars <- unique(c("UniqueID", "left_next", "course", "course_family",
                        "region", "cohort_f", "rv", SURVEY_VARS))
  miss <- setdiff(need_vars, names(panel))
  if (length(miss))
    stop("analysis_leaver_prediction (y1_next): sample is missing: ",
         paste(miss, collapse = ", "), call. = FALSE)

  ok <- panel$at_risk %in% TRUE & first_year_row & stats::complete.cases(panel[need_vars])
  d  <- as.data.frame(panel[ok, need_vars, drop = FALSE])
  d$course <- factor(d$course)
  d$region <- factor(d$region)
  progress(sprintf("leaver prediction: y1_next sample n=%s", format(nrow(d), big.mark = ",")))
  d
}

lp_build_cont_next <- function() {
  progress("leaver prediction: building cont_next frame ...")

  panel <- importance_build_wave_panel()
  panel <- panel |> mutate(region = as.character(region), cohort_f = factor(as.integer(year)))

  need_vars <- unique(c("UniqueID", "left_next", "course", "course_family",
                        "region", "cohort_f", "rv", SURVEY_VARS, "confidence", "leave_course"))
  miss <- setdiff(need_vars, names(panel))
  if (length(miss))
    stop("analysis_leaver_prediction (cont_next): sample is missing: ",
         paste(miss, collapse = ", "), call. = FALSE)

  ok <- panel$at_risk %in% TRUE & stats::complete.cases(panel[need_vars])
  d  <- as.data.frame(panel[ok, need_vars, drop = FALSE])
  d$course <- factor(d$course)
  d$region <- factor(d$region)
  progress(sprintf("leaver prediction: cont_next sample n=%s", format(nrow(d), big.mark = ",")))
  d
}

# ===========================================================================
# analysis_leaver_prediction()  -  builds the three frames, fits the three
# models on each, writes tbl_leaver_prediction.csv, prints a verdict per
# frame. See file header for the full method and caveats.
# ===========================================================================
analysis_leaver_prediction <- function() {
  out <- tables_dir()

  d_entry <- lp_build_entry_ever()
  d_y1    <- lp_build_y1_next()
  d_cont  <- lp_build_cont_next()

  vars_basic <- c("region", "cohort_f", "rv", SURVEY_VARS)
  base_vars  <- c("course", vars_basic)
  tree_vars  <- c("course_family", "course_te", vars_basic)

  progress("leaver prediction: fitting entry_ever (logit / tree / forest) ...")
  r1 <- lp_run_frame("entry_ever", d_entry, "left_before_finish",
                     base_vars = base_vars, tree_vars = tree_vars)

  progress("leaver prediction: fitting y1_next (logit / tree / forest) ...")
  r2 <- lp_run_frame("y1_next", d_y1, "left_next",
                     base_vars = base_vars, tree_vars = tree_vars)

  progress("leaver prediction: fitting cont_next (logit / tree / forest) ...")
  r3 <- lp_run_frame("cont_next", d_cont, "left_next",
                     base_vars = base_vars, tree_vars = c(tree_vars, "confidence", "leave_course"),
                     extra_logit_vars = c("factor(confidence)", "leave_course"))

  res <- bind_rows(r1, r2, r3)
  res$auc       <- round(res$auc, 4)
  res$tjur      <- round(res$tjur, 4)
  res$base_rate <- round(res$base_rate, 4)

  write_csv(res, file.path(out, "tbl_leaver_prediction.csv"))
  progress(paste0("  wrote tbl_leaver_prediction.csv (", nrow(res), " rows)"))

  cat("\n=== Leaver prediction benchmark (test-set AUC by frame x model) ===\n")
  print(as.data.frame(res), row.names = FALSE)

  cat("\n=== Verdict: does a flexible model beat the logit? ===\n")
  for (fr in unique(res$frame)) {
    sub       <- res[res$frame == fr, ]
    auc_logit <- sub$auc[sub$model == "logit_main"]
    auc_flex  <- suppressWarnings(max(sub$auc[sub$model %in% c("tree", "forest")], na.rm = TRUE))
    if (length(auc_logit) == 1L && is.finite(auc_logit) && is.finite(auc_flex)) {
      cat(sprintf("  %-10s flexible models add %+.3f AUC over logit (logit %.3f, best flexible %.3f)\n",
                  fr, auc_flex - auc_logit, auc_logit, auc_flex))
    } else if (length(auc_logit) == 1L && is.finite(auc_logit)) {
      cat(sprintf("  %-10s logit AUC %.3f; no flexible-model AUC available\n", fr, auc_logit))
    } else {
      cat(sprintf("  %-10s no AUC available\n", fr))
    }
  }

  progress(paste0("leaver prediction: done -> ", out))
  cat("Wrote: tbl_leaver_prediction.csv\n")

  invisible(TRUE)
}
