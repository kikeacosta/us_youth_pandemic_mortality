# Reproduce every figure and table in the paper from the CDC WONDER exports in
# data_input/. Run from the project root (the .Rproj directory):
#
#   Rscript run_all.R
#
# Scripts 01-04 rebuild data_inter/; 05-08 draw figures/ and tables/;
# 09 prints the numbers quoted in the text to output/; 10 runs the sensitivity
# analyses (figures S1-S3 and output/sensitivity_summary.txt); 11 compares the CYP
# excess with the adult and all-age excess by state (Table S1); the alt_fig*
# scripts draw the alternative versions of figures 1 and 2 (figures/alternatives/).
# Each script runs in its own environment and only needs code/00_functions.R.

scripts <- c(
  "code/01_c19_deaths_by_age_state.R",
  "code/02_c19_rates_by_age_race.R",
  "code/03_all_cause_deaths_pop.R",
  "code/04_excess_baselines.R",
  "code/05_fig1_c19_excess_by_age.R",
  "code/06_fig2_fig3_tables_state_region.R",
  "code/07_fig4_rr_by_race.R",
  "code/08_fig5_change_by_age_race.R",
  "code/09_numbers_in_text.R",
  "code/10_sensitivity.R",
  "code/11_cyp_vs_all_ages_by_state.R",
  "code/alt_fig1_by_age.R",
  "code/alt_fig2_maps_by_state.R"
)

t0 <- Sys.time()
for (s in scripts) {
  message("\n---- ", s, " ----")
  source(s, local = new.env(), echo = FALSE)
}
message("\nDone in ", round(difftime(Sys.time(), t0, units = "mins"), 1), " min")
