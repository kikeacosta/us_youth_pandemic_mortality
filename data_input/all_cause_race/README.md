# All-cause deaths by race/ethnicity, ages 0–24 (exploratory)

Inputs of script 12 (not part of `run_all.R`). National, by year × five-year age group × Hispanic origin (× race). Cells with fewer than 10 deaths are omitted by WONDER.

| File | Dataset | Race concept | Years | Source |
|---|---|---|---|---|
| `d76_ac_year_age_hisp_2010_2020.csv` | Multiple Cause of Death, 1999–2020 (D76) | bridged race; Hispanic origin only (Asian or Pacific Islander combined in the race file) | 2010–2020 | API, `wonder_api_queries.R`, 2 Nov 2026 |
| `d76_ac_year_age_nonhisp_race_2010_2020.csv` | idem, Not Hispanic only, by race | bridged race, four groups | 2010–2020 | idem |
| `d157_ac_year_age_hisp_2018_2023.csv` | Multiple Cause of Death, 2018–2023, Single Race (D157) | Hispanic origin only, no race grouping (so no hidden cells) | 2018–2023 | API, `wonder_api_queries.R`, 2 Nov 2026 |
| `dts_ac_hisp_race6_age_2018_2024.csv` | Underlying Cause of Death, 2018–2024, Single Race | Hispanic origin × Single Race 6, all ages, all causes; WONDER footer kept | 2018–2024 | web export, 2 Nov 2026 |

API files: columns `lab1` year, `lab2` age, `lab3` Hispanic origin, `lab4` race, deaths, population, crude rate per 100,000; population is "Not Applicable" for Hispanic origin "Not Stated".

In the web export the hidden cells (< 10 deaths) at ages 0–24 are Hispanic AIAN, Asian and NHOPI at ages 1–19 and non-Hispanic NHOPI at 5–14. The four non-Hispanic groups used (White, Black, Asian, AIAN) are complete. The Hispanic total is taken from the API query without race grouping, which is complete: summing the Hispanic rows of the export misses those small low-mortality groups and overstates Hispanic death rates at ages 1–14 by 2–5 %.

Bridged-race and single-race series are not directly comparable (multiracial decedents and residents are allocated to one race in the bridged series, and the population denominators differ). Script 12 fits the baseline on the bridged series (2010–2019) and calibrates it to the single-race series on the overlap years 2018–2019.
