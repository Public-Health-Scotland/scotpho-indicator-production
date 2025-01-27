aggregate_by_simd <- function(data, geo_col, simd_col) {

    data |>
    select(any_of(c("year", "numerator", "denominator", "age_grp", "sex_grp", {{geo_col}}, {{simd_col}}))) |>
        group_by(across(any_of(c("year", "age_grp", "sex_grp", {{geo_col}}, {{simd_col}})))) |>
        summarise_all(sum, na.rm = T) |>
        rename(code = {{geo_col}}, quintile = {{simd_col}}) |>
        ungroup() |>
        mutate(quint_type = simd_col)

}


