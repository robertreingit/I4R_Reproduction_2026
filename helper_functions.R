# helper functions

# Table 1
## Generate target table

## Reported values (paper.txt, lines 481-555)

gen_table_1 <- function() {
  pre_raw <- c(
    ".77 ± 1.17*", "1.31±.48", "8.92 ± 1.85*", "35.88±.73",
    "1.92±.42*", "1.86±.59", "2.76±.51", "28.70 ± 2.30*",
    "29.68 ± 2.64*", "23.88 ± 6.55", "17.59 ± 3.42", "1.51 ± 3.19",
    ".53±.18", "1.30±.47", "1.54±.46", "−.54±.47",
    "−.07±.22*"
  )
  post_raw <- c(
    "5.85 ± 2.54", "1.54±.78", "5.31 ± 1.89", "36.22±.34",
    "2.59±.67", "1.79±.95", "3.26 ± 1.10", "27.80 ± 1.19",
    "28.51 ± 2.05", "24.39 ± 8.27", "18.49 ± 3.89", "11.41 ± 4.65",
    ".57±.22", "1.37±.49", "1.57±.46", "−.54±.43",
    ".11±.15"
  )
  p_raw <- c(
    ".006", ".337", ".002", ".066", ".025", ".764", ".113", ".038", ".029",
    ".224", ".274", ".244", ".497", ".161", ".229", ".968", ".019"
  )
  es_raw <- c(
    "4.08 [2.96; 5.19] – large", "0.45 [−0.35; 1.25] – small",
    "−1.83 [−2.64; −1.02] – large", "0.49 [0.08; 0.90] – small",
    "1.50 [0.46; 2.54] – large", "0.12 [−0.82; 0.58] – trivial",
    "0.93 [−0.04; 1.90] – large", "−0.36 [−0.64; −0.09] – small",
    "−0.42 [−0.72; −0.12] – small", "0.07 [−0.21; 0.36] – trivial",
    "0.24 [−0.14; 0.63] – small", "0.27 [−0.12; 0.65] – small",
    "0.17 [−0.27; 0.61] – small", "0.13 [−0.05; 0.30] – trivial",
    "0.06 [−0.03; 0.14] – trivial", "0.00 [−0.13; 0.14] – trivial",
    "0.78 [0.27; 1.30] – moderat"
  )

  # "m ± sd*" -> numeric mean, sd, and significance-star flag
  parse_msd <- function(x) {
    x <- str_replace_all(x, "−", "-")
    m <- str_match(x, "^\\s*(-?[0-9.]+)\\s*±\\s*([0-9.]+)\\s*(\\*?)")
    tibble(mean = as.numeric(m[, 2]), sd = as.numeric(m[, 3]), star = m[, 4] == "*")
  }

  es_m <- str_match(
    str_replace_all(es_raw, "−", "-"),
    "^(-?[0-9.]+) \\[(-?[0-9.]+); (-?[0-9.]+)\\] – (\\w+)"
  )

  reported <- tibble(
    row       = seq_along(pre_raw),
    pre_bar   = parse_msd(pre_raw)$mean,
    pre_sd    = parse_msd(pre_raw)$sd,
    #pre_star  = parse_msd(pre_raw)$star,
    post_bar  = parse_msd(post_raw)$mean,
    post_sd   = parse_msd(post_raw)$sd,
    p_value   = as.numeric(p_raw),
    es        = as.numeric(es_m[, 2]),
    es_lo     = as.numeric(es_m[, 3]),
    es_hi     = as.numeric(es_m[, 4]),
    es_rating = es_m[, 5]
  )
  reported
}

mean_sd <- function(x) c(mean=mean(x), sd=sd(x))

aov_graph <- function(path, sheet, con_pre_rng, con_post_rng, cool_pre_rng, cool_post_rng) {
  con_pre <- read_xls_1(path, sheet, con_pre_rng)
  con_post <- read_xls_1(path, sheet, con_post_rng)
  cool_pre <- read_xls_1(path, sheet, cool_pre_rng)
  cool_post <- read_xls_1(path, sheet, cool_post_rng)

  df <- tibble(
    id = factor(rep(1:15, 4)),
    time = factor(rep(c("pre", "post", "pre", "post"), each = 15), levels = c("pre", "post")),
    cond = factor(rep(c("control", "control", "cool", "cool"), each = 15)),
    y = c(con_pre, con_post, cool_pre, cool_post) 
  )

  df_av <- df |> group_by(cond, time) |> summarise(y = mean(y), .groups = "drop")
  ggplot(df, aes(cond, y)) + 
    geom_jitter(height=0, width=.1) + 
    geom_point(data = df_av, color='red', size=2) +
    facet_grid(~time)
}


expected_aov <- function(
  int_F = NA, int_p = NA, int_eps = NA, int_df1 = NA, int_df2 = NA,
  con_wtn_d = NA, con_wtn_p = NA, cool_wtn_d = NA, cool_wtn_p = NA,
  pre_p = NA, post_diff = NA, post_p = NA) {
  c(
    int_F, int_p, int_eps, int_df1, int_df2, con_wtn_d, con_wtn_p, cool_wtn_d, cool_wtn_p,
    pre_p, post_diff, post_p
  )
}


read_xls_2 <- function(pre_col, post_col) {
  read_xlsx(
    path = 'I4R_PKG_DIR/raw_perceptual_measures.xlsx',
    sheet = 'Experiment_1',
    range = paste0(pre_col, "3:", post_col, "15"),
    col_names = c('pre', 'post')
  )
}

read_xls_1 <- function(path, sheet, range) {
  read_xlsx(path, sheet, range, col_names = 'dummy') |> pull(dummy)
}

read_xls_11 <- function(path, sheet, range) {
  # range should be a vector with two strings
  pre <- read_xls_1(path, sheet, range[1])
  post <- read_xls_1(path, sheet, range[2])
  tibble(pre, post)
}

stats_tbl_1 <- function(df, parametric = T, log = FALSE){
  stats <- df|>
    summarize(
      pre_bar = mean(pre),
      pre_sd = sd(pre),
      post_bar = mean(post),
      post_sd = sd(post)
    ) |> round(2)

  if (parametric) {
    if (log) {
      test <- with(df, t.test(log(pre), log(post), paired = TRUE))
    }
    else {
      test <- with(df, t.test(pre, post, paired = TRUE))
    }
  } else {
    test <- with(df, wilcox.test(pre, post, paired = TRUE, conf.int = TRUE))
  }
  p_value <- test$p.value
  ci <- test$conf.int

  list(stats = stats, p_value = p_value, ci = ci)
}

compare_function <- function(res_lst, vals, tol = 0.001) {
  # given values may be numeric or strings such as "0.006"
  pre_bar <- vals[1]
  pre_sd <- vals[2]
  post_bar <- vals[3]
  post_sd <- vals[4]
  p_value <- vals[5]

  given <- list(
    pre_bar = pre_bar, pre_sd = pre_sd,
    post_bar = post_bar, post_sd = post_sd,
    p_value = p_value
  ) |> lapply(\(x) as.numeric(as.character(x)))

  computed <- c(
    as.list(res_lst$stats[1, c("pre_bar", "pre_sd", "post_bar", "post_sd")]),
    list(p_value = res_lst$p_value)
  )

  out <- tibble(
    measure  = names(given),
    given    = unlist(given),
    computed = unlist(computed[names(given)]),
    diff     = abs(computed - given),
    # small epsilon guards against floating-point error at the boundary
    match    = diff <= tol + 1e-12
  )

  # colored output: TRUE in green, FALSE in red
  fmt_num <- \(x) formatC(x, format = "f", digits = 3, width = 9)
  cat(
    cli::style_bold(sprintf("%-10s %9s %9s %9s  %s", "measure", "given",
                            "computed", "diff", "match")), "\n", sep = ""
  )
  for (i in seq_len(nrow(out))) {
    m <- if (out$match[i]) cli::col_green("TRUE") else cli::col_red("FALSE")
    cat(sprintf("%-10s %s %s %s  ", out$measure[i], fmt_num(out$given[i]),
                fmt_num(out$computed[i]), fmt_num(out$diff[i])),
        m, "\n", sep = "")
  }

  # confidence interval underneath the table
  ci <- res_lst$ci
  if (!is.null(ci)) {
    level <- attr(ci, "conf.level")
    lab <- if (is.null(level)) "CI" else sprintf("%g%% CI", 100 * level)
    cat(sprintf("%s: [%s, %s]\n", lab,
                formatC(ci[1], format = "f", digits = 3),
                formatC(ci[2], format = "f", digits = 3)))
  } else {
    cat("CI: not available\n")
  }
  cat("\n")

  invisible(out)
}

format_match <- function(name, given, expected, prec = 0.01) {
  # colored output: TRUE in green, FALSE in red
  if (is.na(given) || is.na(expected)) {
    return(invisible())
  }

  fmt_num <- \(x) formatC(x, format = "f", digits = 3, width = 9)
  diff_value <- abs(given - expected)
  if (diff_value < prec) match <- cli::col_green("TRUE")
    else match <- cli::col_red("FALSE")
  cat(sprintf("%-10s %9s %9s %9s\t",
    name,
    fmt_num(given),
    fmt_num(expected),
    fmt_num(diff_value)),
    match, 
    "\n", sep = "")
}

load_running <- function(path, sheet, range) {
  read_xlsx(path, sheet, range, 
    col_names = paste0("trial_", 1:10))
}

relia_docu <- function(range_control, range_cool, func, expected, parametric = TRUE) {
  control <- load_running(path, sheet, range_control)
  control_bar <- control |> apply(1, func)
  control_vals <- control_bar |> mean_sd()

  cool <- load_running(path, sheet, range_cool)
  cool_bar <- cool |>apply(1, func) 
  cool_vals <- cool_bar |> mean_sd() 

  if (parametric) mod <- t.test(control_bar, cool_bar, paired = T)
  else mod <- wilcox.test(control_bar, cool_bar, paired = TRUE, excact = FALSE)

  icc3 <- ICC(cbind(control_bar, cool_bar))$results$ICC[3]

  cat(
    cli::style_bold(sprintf("%-10s %9s %9s %9s\t%s", "measure", "given",
                            "computed", "diff", "match")), "\n", sep = ""
  )
  format_match("con mean", control_vals[1], expected[1])
  format_match("con sd", control_vals[2], expected[2])
  format_match("cool mean", cool_vals[1], expected[3])
  format_match("cool sd", cool_vals[2], expected[4])
  format_match("t-test t", abs(mod$statistic), expected[5])
  format_match("t-test p", abs(mod$p.value), expected[6], 0.001)
  format_match("ICC", icc3, expected[7])
}

dec <- function(x) {
  n <- length(x)
  (x[1] - x[n])/x[1] * 100
}

friedman_2_results <- function(m) {
  f <- friedman.test(m)
  c(
    chi2 = unname(f$statistic),
    df = unname(f$parameter),
    p = unname(f$p.value),
    W = unname(f$statistic) / (nrow(m) * (ncol(m) - 1))
  ) 
}

read_xls_2x <- function(path, sheet, range) {
  read_xlsx(
    path = path,
    sheet = sheet,
    range = range,
    col_names = c('pre', 'post')
  ) |> as.matrix()
}

friedman_docu <- function(range_control, range_cool, expected) {
  path <- "I4R_PKG_DIR/raw_perceptual_measures.xlsx"
  sheet <- "Experiment_2"
  con <- read_xls_2x(path, sheet, range_control)
  cool <- read_xls_2x(path, sheet, range_cool)
  res <- friedman_2_results(cbind(con, cool))

  cat(
    cli::style_bold(sprintf("%-10s %9s %9s %9s\t%s", "measure", "computed",
                            "given", "diff", "match")), "\n", sep = ""
  )
  format_match("chi", res[['chi2']], expected[1])
  format_match("p", res[['p']], expected[2], 0.001)
  format_match("W", res[['W']], expected[3])
  format_match("df", res[['df']], expected[4])
}

wilcox_docu <- function(path, sheet, range_a, range_b, expected) {
  a <- read_xls_1(path, sheet, range_a)
  b <- read_xls_1(path, sheet, range_b)
  n <- length(a)
  mod <- wilcox.test(a, b, paired = TRUE, exact = FALSE, correct = FALSE, conf.int = TRUE)
  z <- coin::wilcoxsign_test(a~ b, zero.method = 'Wilcoxon')@statistic@teststatistic
  d <- a - b
  
  res <- c(
    med = abs(median(d)),
    p = unname(mod$p.value),
    r = z/sqrt(n),
    IQR_low = unname(quantile(d, 0.25)),
    IQR_up = unname(quantile(d, 0.75)) 
  )

  cat(
    cli::style_bold(sprintf("%-10s %9s %9s %9s\t%s", "measure", "computed",
                            "given", "diff", "match")), "\n", sep = ""
  )
  format_match("med", median(d), expected[1])
  format_match("p", res[['p']], expected[2], 0.001)
  format_match("r", res[['r']], expected[3])
  format_match("IQR low", res[['IQR_low']], expected[4])
  format_match("IQR up", res['IQR_up'], expected[5])
}

AOV_test <- function(con_pre, con_post, cool_pre, cool_post) {
  # convention 
  df <- tibble(
    id = factor(rep(1:15, 4)),
    time = factor(rep(c("pre", "post", "pre", "post"), each = 15), levels = c("pre", "post")),
    cond = factor(rep(c("control", "control", "cool", "cool"), each = 15)),
    y = c(con_pre, con_post, cool_pre, cool_post) 
  )
  # ok <- tapply(!is.na(d$y), d$id, all); d <- d[d$id %in% names(ok)[ok], ]; d$id <- droplevels(d$id)
  mod <- afex::aov_4(y ~ time * cond + (time * cond | id), data = df)
  res <- summary(mod$Anova)$univariate.tests
  row <- function(e) {
    F <- res[e, "F value"]
    d1 <- res[e, "num Df"]
    d2 <- res[e, "den Df"]
    c(
      F = F,
      df1 = d1,
      df2 = d2,
      p = pf(F, d1, d2, lower.tail = FALSE),
      pes = F * d1 / (F * d1 + d2)
    )
  }
  within <- as.data.frame(summary(pairs(emmeans(mod, ~ time | cond), adjust = "bonferroni")))
  between <- as.data.frame(summary(pairs(emmeans(mod, ~ cond | time), adjust = "bonferroni")))
  list(
    n = nlevels(df$id),
    int = row("time:cond"),
    time = row("time"),
    cond = row("cond"),
    within = c(
      ctl_diff = -within$estimate[1],
      ctl_p = within$p.value[1],
      cool_diff = -within$estimate[2],
      cool_p = within$p.value[2]
    ),
    between = c(
      pre_p = between$p.value[1],
      post_diff = -between$estimate[2],
      post_p = between$p.value[2]
    )
  ) 
}

aov_docu <- function(path, sheet, con_pre_rng, con_post_rng, cool_pre_rng, cool_post_rng, expected) {
  con_pre <- read_xls_1(path, sheet, con_pre_rng)
  con_post <- read_xls_1(path, sheet, con_post_rng)
  cool_pre <- read_xls_1(path, sheet, cool_pre_rng)
  cool_post <- read_xls_1(path, sheet, cool_post_rng)
  res <- AOV_test(con_pre, con_post, cool_pre, cool_post)

  cat(
    cli::style_bold(sprintf("%-10s %9s %9s %9s\t%s", "measure", "computed",
                            "given", "diff", "match")), "\n", sep = ""
  )
  format_match("F", res[['int']][['F']], expected[1])
  format_match("p", res[['int']][['p']], expected[2], 0.001)
  format_match("eta", res[['int']][['pes']], expected[3])
  format_match("df1", res[['int']][['df1']], expected[4])
  format_match("df2", res[['int']][['df2']], expected[5])
  format_match("CON wthn d", res[['within']][['ctl_diff']], expected[6])
  format_match("CON wthn p", res[['within']][['ctl_p']], expected[7])
  format_match("COOL wthn d", res[['within']][['cool_diff']], expected[8])
  format_match("COOL wthn p", res[['within']][['cool_p']], expected[9])
  format_match("CON wthn d", res[['between']][['pre_p']], expected[10])
  format_match("CON wthn p", res[['between']][['post_diff']], expected[11])
  format_match("COOL wthn p", res[['between']][['post_p']], expected[12])
}

