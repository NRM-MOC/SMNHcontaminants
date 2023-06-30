
#' Wide format for contaminants in marine biota database.
#'
#' @param ContaminantsDB data table of contaminants in marine biota presented in a long format.
#' @param group the contaminants substance groups. Choose from:  "all", "Metals","PFAS","PCBs", "Dioxins and furans" , "Pesticides","BFRs", "Solvents-phenols","PAH". "Organotin compounds" .
#' @param bio choose from: "Year", "Date", "Species", "English_name",  "Class","Number_individual","Sex", "Total_length","Age", "Weight","Shell_thickness" ,  "d13C", "d15N","C_dw_percentage", "N_dw_percentage","Station_name", "Latitude", "Longitude", "Helcom_basin", "Ices_basin","SMNH_basin".
#' @param Averages add the average of fat percentage and dry weight percentage.
#'
#' @return database in wide format with contaminants in separate columns.
#' @export
#'
#' @examples make_wide(ContaminantsDB, "Metals") |>  glimpse() |> View()
#' @examples make_wide(ContaminantsDB, "BFRs") |> write_csv2(file = "BFRs.csv")
#'
make_wide <- function(ContaminantsDB, group , bio = c("year", "date", "species", "species_EN",  "class",
                                                                "number_individuals","sex", "total_length","age", "weight",
                                                                "shell_thickness" ,  "d13C", "d15N","C_dw_percentage", "N_dw_percentage",
                                                                "station_name", "latitude", "longitude", "HELCOM_basin", "ICES_basin",
                                                                "SMNH_basin"), Averages = TRUE){
  cont <- ContaminantsDB |>
    filter((substance_group %in% group) | substance_group == "all") |>
    mutate(value = ifelse(is_censored == TRUE, paste0("<", value), as.character(value))) |>
    select(specimen_ID, contaminant, value) |>
    group_by(specimen_ID, contaminant) |>
    summarise(value = value[1], substance_group = "drop") |>
    pivot_wider(names_from = "contaminant", values_from = "value")

  bio <- ContaminantsDB |> select(specimen_ID, all_of(bio)) |>
    distinct()

  if (Averages == TRUE){
    prc <- ContaminantsDB |>
      group_by(specimen_ID) |>
      summarise(fat_percentage = ifelse(all(is.na(fat_percentage)), NA_real_, mean(fat_percentage, na.rm = TRUE)),
                dry_weight_percentage = ifelse(all(is.na(dry_weight_percentage)), NA, mean(dry_weight_percentage, na.rm = TRUE)))

  }
  else{
    prc <- data.frame(specimen_ID = unique(ContaminantsDB$specimen_ID))
  }

  bio |> left_join(cont) |>
    left_join(prc)
}

