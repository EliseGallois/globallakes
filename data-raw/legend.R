## code to prepare `legend` dataset goes here


#data_nutrients_3    <- load("lev3_vars.rda")
#data_nutrients_7    <- load("lev3_vars.rda")

data_nutrients_3 <- lev3_vars
data_nutrients_7 <- lev3_vars


variables <- c("Phosphorus_Rivers_Psurface_runoff_nat",
               "Phosphorus_Rivers_Psurface_runoff_agri",
               "Phosphorus_Rivers_Pweathering",
               "Phosphorus_Rivers_Pvegetation",
               "Phosphorus_Rivers_Paquaculture",
               "Phosphorus_Rivers_Psewage",
               "Nitrogen_Rivers_Nsurface_runoff_nat",
               "Nitrogen_Rivers_Nsurface_runoff_agri",
               "Nitrogen_Rivers_Ngroundwater_nat",
               "Nitrogen_Rivers_Ngroundwater_agri",
               "Nitrogen_Rivers_Nvegetation",
               "Nitrogen_Rivers_Ndeposition_water",
               "Nitrogen_Rivers_Naquaculture",
               "Nitrogen_Rivers_Nsewage")


legend <- list()


for (avariable in 1:length(variables)) {
  check <- variables[avariable]

  lev_3 <- data_nutrients_3[, grep(paste("_", check, "_", sep = ""), colnames(data_nutrients_3)), drop = FALSE]
  lev_7 <- data_nutrients_7[, grep(paste("_", check, "_", sep = ""), colnames(data_nutrients_7)), drop = FALSE]

  # Convert to numeric if possible
  lev_3 <- as.data.frame(lapply(lev_3, function(x) as.numeric(as.character(x))))
  lev_7 <- as.data.frame(lapply(lev_7, function(x) as.numeric(as.character(x))))

  # Handle empty or NULL cases
  if (ncol(lev_3) == 0 & ncol(lev_7) == 0) {
    warning(paste("No numeric columns found for:", check))
    legend[[check]] <- list(min = NA, max = NA)
    next
  }

  combined_data <- cbind(lev_3, lev_7)

  min_value <- ifelse(all(is.na(combined_data)), NA, min(combined_data, na.rm = TRUE))
  max_value <- ifelse(all(is.na(combined_data)), NA, max(combined_data, na.rm = TRUE))

  legend[[check]] <- list(min = min_value, max = max_value)
}




usethis::use_data(legend, overwrite = TRUE)
