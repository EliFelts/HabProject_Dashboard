# helper function to match stream name to actual flowlines

match_project_stream <- function(project_row, flowlines) {
  huc8_val <- project_row$huc8[[1]]
  stream_name_val <- project_row$stream_name[[1]]
  project_id_val <- project_row$globalid[[1]]

  cand <- flowlines |>
    filter(huc8 == huc8_val)

  if (nrow(cand) == 0) {
    return(tibble(
      project_id = project_id_val,
      huc8 = huc8_val,
      entered_stream = project_row$stream_name[[1]],
      match_status = "no_flowlines_in_huc8",
      NAME = NA_character_,
      LLID = NA_character_,
      string_dist = NA_real_,
      match_rank = NA_integer_
    ))
  }

  exact <- cand |>
    filter(NAME == stream_name_val)

  if (nrow(exact) == 1) {
    return(exact |>
      st_drop_geometry() |>
      transmute(
        project_id = project_id_val,
        huc8 = huc8_val,
        entered_stream = project_row$stream_name[[1]],
        match_status = "exact",
        NAME,
        LLID,
        string_dist = NA_real_,
        match_rank = 1
      ))
  }

  if (nrow(exact) > 1) {
    return(exact |>
      st_drop_geometry() |>
      transmute(
        project_id = project_id_val,
        huc8 = huc8_val,
        entered_stream = project_row$stream_name[[1]],
        match_status = "multiple_exact",
        NAME,
        LLID,
        string_dist = NA_real_,
        match_rank = row_number()
      ))
  }

  fuzzy <- cand |>
    st_drop_geometry() |>
    mutate(
      string_dist = stringdist::stringdist(NAME, stream_name_val, method = "jw")
    ) |>
    arrange(string_dist) |>
    slice_head(n = 3) |>
    transmute(
      project_id = project_id_val,
      huc8 = huc8_val,
      entered_stream = project_row$stream_name[[1]],
      match_status = "fuzzy_candidate",
      NAME,
      LLID,
      string_dist,
      match_rank = row_number()
    )

  if (nrow(fuzzy) == 0) {
    return(tibble(
      project_id = project_id_val,
      huc8 = huc8_val,
      entered_stream = project_row$stream_name[[1]],
      match_status = "no_match",
      NAME = NA_character_,
      LLID = NA_character_,
      string_dist = NA_real_,
      match_rank = NA_integer_
    ))
  }

  fuzzy
}
