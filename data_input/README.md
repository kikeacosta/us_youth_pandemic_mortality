# Input data: CDC WONDER exports

All files are tab-separated exports from [CDC WONDER](https://wonder.cdc.gov), downloaded between 4 June and 5 August 2024. Each file keeps WONDER's footer (`"---"` block) with the dataset, query parameters, query date, suggested citation and caveats; the scripts read the header rows by column position and drop the footer. `wonder_queries.csv` indexes every file (dataset, grouping, filters, query date).

Suggested citation (from the footers): *Centers for Disease Control and Prevention, National Center for Health Statistics. National Vital Statistics System, Provisional Mortality on CDC WONDER Online Database. Data are from the final Multiple Cause of Death Files, 2018–2022, and from provisional data for years 2023–2024.*

## Why so many files: recovering suppressed cells

WONDER suppresses any cell with fewer than 10 deaths. Age-specific COVID-19 deaths by state (or by race/ethnicity) are often below 10, so instead of querying closed age groups we query **open-ended** age groups, which are large enough not to be suppressed, and difference them:

```
deaths(0)      = deaths(all known ages) − deaths(1+)
deaths(1–4)    = deaths(all known ages) − deaths(0) − deaths(5+)
deaths(5–9)    = deaths(all known ages) − deaths(0–4) − deaths(10+)
...
```

Each `*_knw_*` file holds deaths with known age; `*_01plus_*`, `*_05plus_*`, … hold deaths at ages 1+, 5+, … (the age filter is listed in each footer). Deaths of unknown age (`"Not Stated"`) are excluded throughout.

## `c19/` — COVID-19 deaths (UCD ICD-10 U07.1), 2020–2023

Dataset: *Provisional Mortality Statistics, 2018 through Last Week*.

| Files | Grouping | Filter | Used by |
|---|---|---|---|
| `dts_c19_by_age_2020_2023.txt` | five-year age group × year | — | script 05 (Figure 1) |
| `dts_c19_ages_{knw,01plus,…,85plus}_state_2020_2023.txt` (13) | residence state × year | age ≥ cut | script 01 |
| `dts_c19_ages_{…}_all_2020_2023.txt` (13) | year | age ≥ cut | script 02 (all races) |
| `dts_c19_ages_{…}_hisp_2020_2023.txt` (13) | year | Hispanic or Latino, age ≥ cut | script 02 |
| `dts_c19_ages_{…}_nhsp_2020_2023.txt` (13) | year × Single Race 6 | Not Hispanic or Latino, age ≥ cut | script 02 |

2023 counts are **provisional** (deaths occurring through mid-July 2024 as of late July 2024).

## `all_cause/` — all-cause deaths, 2010–2023

Two datasets, overlapping in 2020 (the overlap is checked for agreement in script 03 and deduplicated):

- `*_2010_2020.txt` — *Multiple Cause of Death, 1999–2020* (final data)
- `*_2020_2023.txt` — *Provisional Mortality Statistics, 2018 through Last Week*

| Files | Grouping | Used by |
|---|---|---|
| `dts_ac_ages_all_sex_all_us_{2010_2020,2020_2023}.txt` | sex × five-year age group × year, US | script 03 (US by age) |
| `dts_ac_ages_{kwn,01plus,05plus,10plus,15plus,20plus,25plus}_sex_all_{period}.txt` (14) | state × sex × year, age ≥ cut | script 03 (states, ages 0–24) |
| `dts_ac_ages_all_sex_all_2020_2023.txt` | state × sex × year, all ages | not used (the all-age series uses the `kwn` files) |
| `dts_ac_ages_all_sex_all_2010_2020.txt` | state × sex × year — **ages 25+ despite the name** (see its footer); duplicate of the `25plus` file | not used |

## `population/`

| File | Dataset | Grouping | Used by |
|---|---|---|---|
| `pop_by_sex_age_2010_2023.txt` | State Population Projections 2004–2030 | age × sex × year, US | script 03 |
| `pop_state_sex_age_0_24_2010_2023.txt` | State Population Projections 2004–2030 | age × sex × state × year | script 03 |
| `pop_state_age_0_24_2020_2023.txt` | State Population Projections 2004–2030 | state × year, ages 0–24 | script 06 |
| `pop_all_races_by_age_2020_2022.txt` | Single-Race Population Estimates 2020–2022 | five-year age group × year | script 02 |
| `pop_hisp_by_age_2020_2022.txt` | Single-Race Population Estimates 2020–2022 | five-year age group × year, Hispanic | script 02 |
| `pop_single_race_no_hisp_by_age_2020_2022.txt` | Single-Race Population Estimates 2020–2022 | five-year age group × race × year, non-Hispanic | script 02 |

Race/ethnicity populations stop in 2022; script 02 carries the 2022 values forward to 2023.
