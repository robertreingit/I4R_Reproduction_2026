# libs
require(tidyverse)
require(readxl)
require(testthat)

# load limb data
df_matlab_export <- read_csv('I4R_PKG_DIR/palucci_data.csv')
head(df_matlab_export)

# pre-process
# Problem: some variables have six, others seven, specifications parts
df_matlab_export <- df_matlab_export |> mutate(
  time = case_when(
    str_detect(var_name, 'pos_RSA') ~ 'inter',
    str_detect(var_name, 'pos_gelo|pos_control') ~ 'post',
    str_detect(var_name, 'pre') ~ 'pre',
    TRUE ~ 'undefined'
  )) |> 
  mutate(
    condition = case_when(
      str_detect(var_name, 'control') ~ 'CON',
      str_detect(var_name, 'gelo') ~ 'TRT',
      TRUE ~ 'undefined'
    )
  ) |> 
  mutate(
    id = str_extract(var_name, "part\\d+"),
    trial = paste0('t_', str_extract(var_name, "(?<=_t)\\d+"))
  )

## Check for identification problems
expect_false("undefined" %in% unique(df_matlab_export$time))
expect_false("undefined" %in% unique(df_matlab_export$condition))
expect_false("undefined" %in% unique(df_matlab_export$id))
expect_false("undefined" %in% unique(df_matlab_export$trial))
 
# helpers
get_mean <- function(var, df = df_matlab_export) df |> group_by(time, condition) |> summarize(m = mean({{ var }}), .groups='drop')
get_individual <- function(var, tm = 'pre', cdt = 'TRT', df = df_matlab_export) {
  df |> 
    filter(time == tm, condition == cdt) |>
    select(id, {{ var }}, trial) |> 
    pivot_wider(id_cols = 'id', names_from = 'trial', values_from = {{ var }}) |> 
    arrange(as.numeric(str_remove(id, 'part')))
}

arrange_part <- function(df = df_matlab_export) arrange(df, as.numeric(str_remove(id, 'part')))

get_id_mat <- function(var, tm = 'pre', cdt = 'TRT', df = df_matlab_export) {
  get_individual({{var}}, tm, cdt, df) |> select(-id) |> as.matrix()
}

# checking dataset completeness
df_matlab_export |> group_by(condition, id) |> summarize(n = n()) |>
  arrange_part() |> 
  pivot_wider(id_cols = 'condition', names_from = 'id', values_from = 'n')

# Checking datasets

df_matlab_export |> filter(id == 'part15') |> select(var_name, id, condition, time, trial, peak_knee)
df_matlab_export |> group_by(time, condition) |> summarize(m = mean(rom_knee))
df_matlab_export |> group_by(id) |> summarize(n = n())

get_mean(peak_knee)
get_mean(cm_to_knee)
get_mean(peak_hip_vel)

m1 <- get_id_mat(peak_hip_vel)
m2 <- read_xlsx('I4R_PKG_DIR/raw_lower_limb_factors.xlsx', 'Ndom_hip_peak_Vel', 'B28:D40', col_names=paste0('t',1:3)) |> 
  as.matrix()
m1 - m2

m1 <- get_id_mat(peak_hip_vel, tm = 'post')
m2 <- read_xlsx('I4R_PKG_DIR/raw_lower_limb_factors.xlsx', 'Ndom_hip_peak_Vel', 'G28:I42', col_names=paste0('t',1:3)) |>
  as.matrix()
abs(m1 - m2) |> round(4)

get_individual(peak_hip_vel, tm = 'post', cdt = 'CON')
get_individual(metatarsal, tm = 'post', cdt = 'TRT')
