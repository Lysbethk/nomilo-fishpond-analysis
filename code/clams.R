ksf_clam_growth_data <- dfs$ksf_clam_growth$sheet1

#### ksf_clam_growth_data

```{r}
#| eval: false
#| echo: false
new_var_names <- c("sort_date", "color", "clams_in_count", "clams_in_lbs", "clams_in_avg_per_lb",
                   "clams_out_count", "clams_out_lbs", "clams_out_avg_per_lb", "growth_in_lbs",
                   "growth_pct", "sr", "days_btwn_sort")

ksf_clam_growth_data_tidied <- ksf_clam_growth_data %>%
  slice(-1) %>%
  setNames(new_var_names) %>%
  pivot_longer(
    cols = c(clams_in_count, clams_in_lbs, clams_in_avg_per_lb,
             clams_out_count, clams_out_lbs, clams_out_avg_per_lb),
    names_to = c("stage", ".value"),
    names_prefix = "clams_", 
    names_sep = "_", 
    values_to = "value"
  ) %>%
  mutate(stage = if_else(str_detect(stage, "in"), "In", "Out")) %>%
  rename(avg_per_lbs = avg) %>% 
  mutate(across(c(color, stage), as.factor)) %>%
  mutate(across(c(count, lbs, avg_per_lbs, growth_in_lbs, growth_pct, sr), ~as.numeric(gsub("[^0-9.-]", "", .)))) %>% 
  arrange(sort_date, color, stage) %>% 
  dplyr::select(sort_date, days_btwn_sort, color, stage, count, lbs, avg_per_lbs, 
                growth_in_lbs, growth_pct, sr)
```

```{r}
#| label: tidy-ksf-clam-growth-data-dataset
#| code-fold: true
#| code-summary: <i>Steps to clean data</i>
ksf_clam_growth_data_tidied <- ksf_clam_growth_data

paged_table(ksf_clam_growth_data_tidied)
```

