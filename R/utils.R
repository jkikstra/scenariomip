#' Useful functions for the `decent` software package



# IAM utils --------------------------------------------------------------------

### IAM model/region -------------------------------------------------------------
#' Load into global environment the variable:
#' - regions.message (if MESSAGE.REGION.MAPPING exists in global environment)
#' - regions.image (if IMAGE.REGION.MAPPING exists in global environment)
#' - regions.remind (if REMIND.REGION.MAPPING exists in global environment)
#' - regions (if all of the following variables exist in global environment: MESSAGE.REGION.MAPPING, IMAGE.REGION.MAPPING, REMIND.REGION.MAPPING)
#'
#' @return
#' @export
#'
#' @examples
#' load_iam_region_mappings()
#' # subfolder `R/raw-data/iam_regions`
#' MESSAGE.REGION.MAPPING <<- "region_definitions_message.csv"
#' IMAGE.REGION.MAPPING <<- "region_definitions_image.csv"
#' REMIND.REGION.MAPPING <<- "region_definitions_remind.csv"
#' load_iam_region_mappings()
#' regions
load_iam_region_mappings <- function() {
  # TODO: rewrite to function with return-only, instead of global variables

  if (exists("MESSAGE.REGION.MAPPING")) {
    f.regions.message <- here("data-raw", "iam_regions", MESSAGE.REGION.MAPPING)
    regions.message <<- vroom(f.regions.message, show_col_types=FALSE) %>%
      select(iso, region.message)
  }
  if (exists("IMAGE.REGION.MAPPING")) {
    f.regions.image <- here("data-raw", "iam_regions", IMAGE.REGION.MAPPING)
    regions.image <<- vroom(f.regions.image, show_col_types=FALSE) %>% select(iso, region.image)
  }
  if (exists("REMIND.REGION.MAPPING")) {
    f.regions.remind <- here("data-raw", "iam_regions", REMIND.REGION.MAPPING)
    regions.remind <<- vroom(f.regions.remind, show_col_types=FALSE) %>% select(iso, region.remind)
  }
  if (exists("MESSAGE.REGION.MAPPING") & exists("IMAGE.REGION.MAPPING") & exists("REMIND.REGION.MAPPING")) {
    regions <<- regions.message %>%
      left_join(regions.image, by = "iso") %>%
      left_join(regions.remind, by = "iso")
  }
}

### IAMC data ------------------------------------------------------------------

##### Load data ----------------------------------------------------------------

#' Load an Excel file with specified column types for IAMC format
#'
#' `load_excel_iamc` reads data from an Excel file, ensuring that a specified
#' number of initial columns (typically for IAMC-compliant files) are treated
#' as character data, while all other columns are treated as numeric.
#' This function attempts to read the "data" sheet first, and falls back to the
#' first sheet if "data" is not available.
#'
#' @param file_path Character. The path to the Excel file to be loaded.
#' @param iamc_column_numbers Integer. The number of initial columns to treat
#'   as character (default is 5, for IAMC-compliant files).
#' @param ... Additional arguments passed to `readxl::read_excel` for flexibility,
#'   such as `range` to specify a cell range.
#'
#' @return A data frame with the first `iamc_column_numbers` columns as character
#'   and all remaining columns as numeric.
#'
#' @import readxl
#' @export
#'
#' @examples
#' # Load data from an Excel file, trying "data" sheet first
#' data <- load_excel_iamc("data.xlsx")
#'
#' # Load data with specific column numbers
#' data <- load_excel_iamc("data.xlsx", iamc_column_numbers = 3)

load_excel_iamc <- function(file_path, iamc_column_numbers = 5, sheet = NULL, ...) {

  if (is.null(sheet)){
    # Try reading the sheet "data" first
    data <- tryCatch({
      readxl::read_excel(file_path, sheet = "data", n_max = 1, ...)
    }, error = function(e) {
      NULL # Return NULL silently on failure
    })

    # Determine the sheet to use
    sheet_to_use <- if (!is.null(data)) "data" else 1
  } else {
    sheet_to_use <- sheet
  }


  # Load only the first row to determine the total number of columns
  n_cols <- ncol(readxl::read_excel(file_path, sheet = sheet_to_use, n_max = 1, ...))

  # Set column types: initial columns as character, the rest as numeric
  col_types <- c(rep("text", iamc_column_numbers),
                 rep("numeric", max(0, n_cols - iamc_column_numbers)))

  # Read the full data with specified column types
  data <- readxl::read_excel(file_path, sheet = sheet_to_use, col_types = col_types, ...)

  return(data)
}


#' Load a CSV file with specified column types for IAMC format
#'
#' `load_csv_iamc` reads data from a CSV file, treating a specified number of
#' initial columns (typically for IAMC-compliant files) as character data, while
#' the remaining columns are treated as numeric. The function supports two modes
#' for reading files: `standard` for using `readr::read_csv`, and `fast` for using
#' `vroom::vroom` to leverage faster file reading capabilities for larger datasets.
#'
#' @param file_path Character. The path to the CSV file to be loaded.
#' @param iamc_column_numbers Integer. The number of initial columns to treat
#'   as character (default is 5, for IAMC-compliant files).
#' @param mode Character. Specifies the file reading mode. Options are:
#'   - `"standard"`: Uses `readr::read_csv` for reading the file. This is the
#'     default option and is suitable for most use cases.
#'   - `"fast"`: Uses `vroom::vroom` for faster file reading, especially
#'     beneficial for large datasets.
#' @param ... Additional arguments passed to the file reading function (`readr::read_csv`
#'   or `vroom::vroom`) for flexibility, such as `col_names` to specify column names.
#'
#' @return A data frame with the first `iamc_column_numbers` columns as character
#'   and all remaining columns as numeric.
#'
#' @import readr
#' @importFrom vroom vroom
#' @export
#'
#' @examples
#' # Load data from a CSV file, treating first 5 columns as character
#' data <- load_csv_iamc("data.csv")
#'
#' # Load data with 3 character columns in fast mode
#' data <- load_csv_iamc("data.csv", iamc_column_numbers = 3, mode = "fast")

load_csv_iamc <- function(file_path, iamc_column_numbers = 5, mode="standard", ...) {

  if (
    mode == "standard"
  ){
    # Determine the total number of columns by reading only the header
    n_cols <- ncol(readr::read_csv(file_path, col_types = readr::cols(), n_max = 1, ...))

    # Set column types: initial columns as character, the rest as numeric
    col_types <- paste0(
      strrep("c", iamc_column_numbers),
      strrep("d", max(0, n_cols - iamc_column_numbers))
    )

    # Read the full data with specified column types
    data <- readr::read_csv(file_path, col_types = col_types, ...)

  } else if (
    mode == "fast"
  ) {
    # Determine the total number of columns by reading only the header
    n_cols <- ncol(vroom::vroom(file_path, col_types = readr::cols(), n_max = 1, ...))

    # Set column types: initial columns as character, the rest as numeric
    col_types <- paste0(
      strrep("c", iamc_column_numbers),
      strrep("d", max(0, n_cols - iamc_column_numbers))
    )

    # Read the full data with specified column types
    data <- vroom::vroom(file_path, col_types = col_types, ...)
  }


  return(data)
}


##### Filter data --------------------------------------------------------------


#' Filter rows based on whether a column starts with a certain string
#'
#' This function filters a data frame based on whether the values in a specified column
#' start with a given string. It supports both direct and inverse filtering.
#'
#' @param df A data frame to filter.
#' @param variable.string A string to match the beginning of column values.
#' @param inverse Logical. If `TRUE`, filters rows that do NOT start with the string. Default is `FALSE`.
#' @param column.name A character string specifying the column name to check (default is `"variable"`).
#'
#' @return A filtered data frame.
#' @export
#'
#' @examples
#' # Example data
#' df <- data.frame(variable = c("apple", "banana", "apricot", "berry"),
#'                  value = c(1, 2, 3, 4))
#'
#' # Filter rows where 'variable' starts with "ap"
#' filter_starts_with(df, "ap", inverse = FALSE, column.name = "variable")
#'
#' # Exclude rows where 'variable' starts with "ap"
#' filter_starts_with(df, "ap", inverse = TRUE, column.name = "variable")
filter_starts_with <- function(df, variable.string, inverse = FALSE, column.name = "variable") {
  # Check if the specified column exists in the data frame
  if (!column.name %in% colnames(df)) {
    stop(paste("Column", column.name, "not found in the data frame."))
  }

  # Generate the filtering condition
  match_condition <- substr(df[[column.name]], 1, nchar(variable.string)) == variable.string

  # Apply filtering based on `inverse`
  if (inverse) {
    return(df[!match_condition, ])
  } else {
    return(df[match_condition, ])
  }
}



#' Filter variables starting with a certain string
#' -> see filter_starts_with()
#'
filter_begins_with <- function(...){
  return(
    filter_starts_with(...)
  )
}

#' Filter variables ending with a certain string
#'
#'
#' @param df
#' @param variable.string
#'
#' @return df: filtered df
#' @export
#'
#' @examples
filter_ends_with <- function(df, variable.string){
  df %>%
    filter(
      substr(variable,nchar(variable)-nchar(variable.string)+1,nchar(variable)) == variable.string
    ) %>%
    return()
}

#' Filter variables that include a certain string
#'
#'
#' @param df
#' @param variable.string
#'
#' @return df: filtered df
#' @export
#'
#' @examples
filter_includes <- function(df, match.string, column = "variable", inverse = F){
  if (column == "variable"){
    return(
      filter_variable_includes(df, variable.string=match.string, inverse=inverse)
    )
  }
}

#' Filter variables that include a certain string
#'
#'
#' @param df
#' @param variable.string
#'
#' @return df: filtered df
#' @export
#'
#' @examples
filter_variable_includes <- function(df, variable.string, inverse = F){
  if (inverse == F){
    df %>%
      filter(
        grepl(x=variable,
              pattern=variable.string,
              fixed=T)
      ) %>%
      return()
  } else if (inverse == T){
    df %>%
      filter(
        !grepl(x=variable,
               pattern=variable.string,
               fixed=T)
      ) %>%
      return()
  }

}

#' Filter models that include a certain string
#'
#'
#' @param df
#' @param model.string
#'
#' @return df: filtered df
#' @export
#'
#' @examples
filter_model_includes <- function(df, model.string, inverse = F){
  if (inverse == F){
    df %>%
      filter(
        grepl(x=model,
              pattern=model.string,
              fixed=T)
      ) %>%
      return()
  } else if (inverse == T){
    df %>%
      filter(
        !grepl(x=model,
               pattern=model.string,
               fixed=T)
      ) %>%
      return()
  }

}

#' Filter regions that include a certain string
#'
#'
#' @param df
#' @param region.string
#'
#' @return df: filtered df
#' @export
#'
#' @examples
filter_region_includes <- function(df, region.string, inverse = F){
  if (inverse == F){
    df %>%
      filter(
        grepl(x=region,
              pattern=region.string,
              fixed=T)
      ) %>%
      return()
  } else if (inverse == T){
    df %>%
      filter(
        !grepl(x=region,
               pattern=region.string,
               fixed=T)
      ) %>%
      return()
  }

}

#' Filter scenarios that include a certain string
#'
#'
#' @param df
#' @param scenario.string
#'
#' @return df: filtered df
#' @export
#'
#' @examples
filter_scenario_includes <- function(df, scenario.string, inverse = F){
  if (inverse == F){
    df %>%
      filter(
        grepl(x=scenario,
              pattern=scenario.string,
              fixed=T)
      ) %>%
      return()
  } else if (inverse == T){
    df %>%
      filter(
        !grepl(x=scenario,
               pattern=scenario.string,
               fixed=T)
      ) %>%
      return()
  }

}

#' Filter variables with a specific number IAMC reporting variable levels,
#' which are separated by of "pipes", i.e. "|" characters.
#'
#' E.g., "Primary Energy|Coal|Liquids" is 3 levels, while
#' "Final Energy" is 1 level.
#'
#' N.B. Currently has issues with variables that have a "/" character.
#'
#' @param df
#' @param number.of.levels
#'
#' @return df: filtered df
#' @export
#'
#' @examples
filter_by_number_of_levels <- function(df, number.of.levels){
  df %>%
    mutate(number.of.pipes = str_count(variable, "\\|")) %>%
    filter(
      number.of.pipes == number.of.levels-1
    ) %>%
    return()
}

#' An imperfect filter function for IAMC data, allowing for wildcards (*).
#' Currently does not work for more than 2 wildcards.
#'
#' NB (to be fixed): this function does not work like pyam in that it does not respect only a wildcard in the front, for instance that "*Emissions|CO2" should end with "|CO2", rather it implicitly treats it as "*Emissions|CO2*"
#'
#' @param df
#' @param variable.string
#' @param lowercase.var.variable: variable or Variable
#'
#' @return df: filtered df
#' @export
#'
#' @examples
filter_wildcard_var <- function(df, variable.string,
                                lowercase.var.variable = F) {

  split.string <- str_split(string = variable.string, pattern = "\\*", n = Inf, simplify = FALSE)[[1]]
  n.split.string <- length(split.string)

  if (lowercase.var.variable) {
    # TODO: find a way to map this to go beyond 2 * characters
    if (n.split.string > 3) {
      message("Maximum wildcards that can be used in one string search is currently set to 2.")
    } else if (n.split.string == 2) {
      df <- df %>%
        filter(grepl(x = variable, pattern = split.string[1], fixed = T) & grepl(x = variable, pattern = split.string[2], fixed = T))
    } else if (n.split.string == 3) {
      df <- df %>%
        filter(grepl(x = variable, pattern = split.string[1], fixed = T) & grepl(x = variable, pattern = split.string[2], fixed = T) & grepl(x = variable, pattern = split.string[3], fixed = T))
    }
  } else {
    # TODO: find a way to map this to go beyond 2 * characters
    if (n.split.string > 3) {
      message("Maximum wildcards that can be used in one string search is currently set to 2.")
    } else if (n.split.string == 2) {
      df <- df %>%
        filter(grepl(x = Variable, pattern = split.string[1], fixed = T) & grepl(x = Variable, pattern = split.string[2], fixed = T))
    } else if (n.split.string == 3) {
      df <- df %>%
        filter(grepl(x = Variable, pattern = split.string[1], fixed = T) & grepl(x = Variable, pattern = split.string[2], fixed = T) & grepl(x = Variable, pattern = split.string[3], fixed = T))
    }
  }



  return(df)
}


remove_all_zero_values <- function(df){
  return(
    df %>%
      filter(value != 0)
  )
}


##### Adjust IAMC column names: lower/upper case -------------------------------

#' Capatilised first letter of IAMC columns to all lowercase
#'
#' @param df: with all upper case first letter
#'
#' @return df: with all lower case
#' @export
#'
#' @examples
upper_to_lower <- function(df){
  return(
    df %>% rename(
      model = Model,
      scenario = Scenario,
      region = Region,
      variable = Variable,
      unit = Unit
    )
  )
}

#' All lower case of IAMC columns to capitalised first letter
#'
#' @param df: with all lower case
#'
#' @return df: with all upper case first letter
#' @export
#'
#' @examples
lower_to_upper <- function(df){
  return(
    df %>% rename(
      Model = model,
      Scenario = scenario,
      Region = region,
      Variable = variable,
      Unit = unit
    )
  )
}

#' All only capitalised first letter IAMC columns to all fully capitalised
#'
#' @param df: with all upper case first letter
#'
#' @return df: with all capitalised
#' @export
#'
#' @examples
upper_to_allcaps <- function(df){
  return(
    df %>% rename(
      MODEL = Model,
      SCENARIO = Scenario,
      REGION = Region,
      VARIABLE = Variable,
      UNIT = Unit
    )
  )
}

#' All lower case IAMC columns to all fully capitalised
#'
#' @param df: with all lower case
#'
#' @return df: with all capitalised
#' @export
#'
#' @examples
lower_to_allcaps <- function(df){
  return(
    df %>% rename(
      MODEL = model,
      SCENARIO = scenario,
      REGION = region,
      VARIABLE = variable,
      UNIT = unit
    )
  )
}

#' All fully capitalised IAMC columns to all only capitalised first letter
#'
#' @param df: with all capitalised
#'
#' @return df: with all upper case first letter
#' @export
#'
#' @examples
allcaps_to_upper <- function(df){
  return(
    df %>% rename(
      Model = MODEL,
      Scenario = SCENARIO,
      Region = REGION,
      Variable = VARIABLE,
      Unit = UNIT
    )
  )
}

#' All fully capitalised IAMC columns to all lower case
#'
#' @param df: with all capitalised
#'
#' @return df: with all lower case
#' @export
#'
#' @examples
allcaps_to_lower <- function(df){
  return(
    df %>% rename(
      model = MODEL,
      scenario = SCENARIO,
      region = REGION,
      variable = VARIABLE,
      unit = UNIT
    )
  )
}

##### Transform data format ----------------------------------------------------


#' Transform IAMC data from wide to long format
#'
#' Function assumes all five basic IAMC columns are there, and then the year columns (and nothing more)
#'
#' @param df
#' @param upper.to.lower: default = F (assume lower case), if T: converts only from first letter capitalized IAMC column names
#'
#' @return df: IAMC data in long format, always with lower case column names
#' @export
#'
#' @examples
#' population.data <- vroom(here(get_data_location_raw(test=FALSE),"scenario_data","population",POPULATION.PROJECTION.FILE), show_col_types=FALSE) %>%
#'     iamc_wide_to_long(upper.to.lower = F)
#'
iamc_wide_to_long <- function(df, upper.to.lower = F) {

  if (upper.to.lower) {
    df <- df %>%
      upper_to_lower()
  }

  first.year <- colnames(df)[6] # assumes all five basic IAMC columns are there, and nothing more
  last.year <- colnames(df)[length(colnames(df))]

  df <- df %>%
    pivot_longer(
      cols = all_of(first.year):all_of(last.year),
      names_to = "year",
      values_to = "value"
    ) %>%
    drop_na(value) %>%
    mutate(year = as.numeric(year))

  return(df)
}

#' Transform IAMC data from long to wide format
#'
#' Function assumes:
#' - columns: year, value
#' - (any other columns possible, and uniquely define values; e.g. five basic IAMC columns)
#'
#' @param df
#'
#' @return df: {IAMC} data in wide format
#' @export
#'
#' @examples
#' read_excel(here("data-raw", "scenario_data", "original_regional", "IMAGE-original-files", "Consolidated_IMAGE_SHAPE_DLS_results.xlsx"),sheet = "data") %>%
#' iamc_wide_to_long() %>%
#' iamc_long_to_wide()
#'
iamc_long_to_wide <- function(df){
  df <- df %>%
    pivot_wider(
      values_from = value,
      names_from = year
    )

  return(df)
}

##### Bring in IAMC style ------------------------------------------------------
# Custom function to reorder columns
wide_sort_columns <- function(df) {
  # Columns to place first
  first_cols <- c("model", "scenario", "region", "variable", "unit")

  # Identify numeric columns (excluding first_cols)
  numeric_cols <- names(df) %>%
    setdiff(first_cols) %>%
    .[sapply(df[.], is.numeric)] %>%
    sort()

  # Identify remaining columns (non-numeric)
  other_cols <- names(df) %>%
    setdiff(c(first_cols, numeric_cols))

  # Reorder columns
  df %>%
    select(all_of(first_cols), all_of(numeric_cols), all_of(other_cols)) %>%
    return()
}


##### Return unique occurernces of IAMC columns --------------------------------

#' Return unique variables
#'
#' @param df
#'
#' @return sorted list of unique variables
#' @export
#'
#' @examples
variable_unique <- function(df){
  return(
    df %>% pull(variable) %>% unique() %>% sort()
  )
}

#' Return unique units
#'
#' @param df
#'
#' @return sorted list of unique units
#' @export
#'
#' @examples
unit_unique <- function(df){
  return(
    df %>% pull(unit) %>% unique() %>% sort()
  )
}

#' Return unique years
#'
#' @param df
#'
#' @return sorted list of unique years
#' @export
#'
#' @examples
year_unique <- function(df){
  return(
    df %>% pull(year) %>% unique() %>% sort()
  )
}

#' Return unique regions
#'
#' @param df
#'
#' @return sorted list of unique regions
#' @export
#'
#' @examples
region_unique <- function(df){
  return(
    df %>% pull(region) %>% unique() %>% sort()
  )
}

#' Return unique model-scenario combinations
#'
#' @param df
#'
#' @return sorted list of unique model-scenario combinations
#' @export
#'
#' @examples
ms_unique <- function(df){
  return(
    df %>% mutate(`model-scenario`=paste0(model,"-",scenario)) %>% pull(`model-scenario`) %>% unique() %>% sort()
  )
}

#' Return unique scenario names
#'
#' @param df
#'
#' @return sorted list of unique scenario names
#' @export
#'
#' @examples
scenario_unique <- function(df){
  return(
    df %>% pull(`scenario`) %>% unique() %>% sort()
  )
}

#' Return unique Variables
#'
#' @param df
#'
#' @return sorted list of unique Variables
#' @export
#'
#' @examples
Variable_unique <- function(df){
  return(
    df %>% pull(Variable) %>% unique() %>% sort()
  )
}

#' Return unique Units
#'
#' @param df
#'
#' @return sorted list of unique Units
#' @export
#'
#' @examples
Unit_unique <- function(df){
  return(
    df %>% pull(Unit) %>% unique() %>% sort()
  )
}

#' Return unique Years
#'
#' @param df
#'
#' @return sorted list of unique Years
#' @export
#'
#' @examples
Year_unique <- function(df){
  return(
    df %>% pull(Year) %>% unique() %>% sort()
  )
}

#' Return unique Regions
#'
#' @param df
#'
#' @return sorted list of unique Regions
#' @export
#'
#' @examples
Region_unique <- function(df){
  return(
    df %>% pull(Region) %>% unique() %>% sort()
  )
}

#' Return unique Model-Scenario combinations
#'
#' @param df
#'
#' @return sorted list of unique Model-Scenario combinations
#' @export
#'
#' @examples
MS_unique <- function(df){
  return(
    df %>% mutate(`Model-Scenario`=paste0(Model,"-",Scenario)) %>% pull(`Model-Scenario`) %>% unique() %>% sort()
  )
}

#' Return unique Scenario names
#'
#' @param df
#'
#' @return sorted list of unique Scenario names
#' @export
#'
#' @examples
Scenario_unique <- function(df){
  return(
    df %>% pull(`Scenario`) %>% unique() %>% sort()
  )
}

##### Simplify Data ------------------------------------------------------------
simplify_model_names <- function(df, keep.full.model.name=F){

  if (keep.full.model.name){
    df <- df %>% mutate(full.model.name = model)
  }

  df %>%
    mutate_cond(substr(model,1,nchar("AIM"))=="AIM", model="AIM") %>%
    mutate_cond(substr(model,1,nchar("COFFEE"))=="COFFEE", model="COFFEE") %>%
    mutate_cond(substr(model,1,nchar("GCAM"))=="GCAM", model="GCAM") %>%
    mutate_cond(substr(model,1,nchar("IMAGE"))=="IMAGE", model="IMAGE") %>%
    mutate_cond(substr(model,1,nchar("MESSAGE"))=="MESSAGE", model="MESSAGE") %>%
    mutate_cond(substr(model,1,nchar("REMIND"))=="REMIND", model="REMIND") %>%
    mutate_cond(substr(model,1,nchar("WITCH"))=="WITCH", model="WITCH") %>%
    return()
}

##### Add model-scenario column ------------------------------------------------
#' Add "model-scenario" column to an IAMC data frame
#'
#' Assumes that the data frame has columns "model" and "scenario"
#'
#' @param df
#'
#' @return df with extra column "model-scenario"
#' @export
#'
#' @examples
ms_add <- function(df){
  return(
    df %>% mutate(`model-scenario`=paste0(model,"-",scenario))
  )
}

#' Add "Model-Scenario" column to an IAMC data frame
#'
#' Assumes that the IAMC data frame has columns "Model" and "Scenario"
#'
#' @param df
#'
#' @return df with extra column "Model-Scenario"
#' @export
#'
#' @examples
MS_add <- function(df){
  return(
    df %>% mutate(`Model-Scenario`=paste0(Model,"-",Scenario))
  )
}
##### Climate assessment tools -------------------------------------------------
add_emissions_processing_col <- function(df) {

  # assumes all columns to be entirely lower cap

  df <- df %>%
    mutate(
      emissions.step =
        ifelse(
          grepl(
            x = variable, pattern = "Infilled", fixed = T
          ),
          "Infilled",
          ifelse(
            grepl(
              x = variable, pattern = "Harmonized", fixed = T
            ),
            "Harmonized",
            ifelse(
              grepl(
                x = variable, pattern = "Native-with-Infilled", fixed = T
              ),
              "Native-with-Infilled",
              "Native"
            )
          )
        )
    ) %>%
    mutate(
      variable =
        ifelse(
          grepl(
            x = variable, pattern = "Infilled", fixed = T
          ),
          str_remove(string = variable, pattern = "AR6 climate diagnostics\\|Infilled\\|Emissions\\|"),
          ifelse(
            grepl(
              x = variable, pattern = "Harmonized", fixed = T
            ),
            str_remove(string = variable, pattern = "AR6 climate diagnostics\\|Harmonized\\|Emissions\\|"),
            ifelse(
              grepl(
                x = variable, pattern = "Native-with-Infilled", fixed = T
              ),
              str_remove(string = variable, pattern = "AR6 climate diagnostics\\|Native-with-Infilled\\|Emissions\\|"),
              ifelse(
                grepl(
                  x = variable, pattern = "AR6 climate diagnostics\\|Emissions\\|", fixed = T
                ),
                str_remove(string = variable, pattern = "AR6 climate diagnostics\\|Emissions\\|"),
                str_remove(string = variable, pattern = "Emissions\\|")
              )
            )
          )
        )
    )

  # function could be made smarter with a more flexible filtering mechanism


  return(df)
}

##### ScenarioMIP-specific -----------------------------------------------------
add_scenariomip_targets_to_IAM_scenarios <- function(df){
  df %>% mutate(target = NA) %>%
    mutate_cond(grepl(x=scenario, pattern="High Emissions", fixed=T), target = "H") %>%
    mutate_cond(grepl(x=scenario, pattern="Medium Emissions", fixed=T), target = "M") %>%
    mutate_cond(grepl(x=scenario, pattern="Low Emissions", fixed=T), target = "L") %>% # needs to come before ML and VL, to correctly overwrite
    mutate_cond(grepl(x=scenario, pattern="Medium-Low Emissions", fixed=T), target = "ML") %>%
    mutate_cond(grepl(x=scenario, pattern="Very Low Emissions", fixed=T), target = "VLLO") %>% # not the case for REMIND
    mutate_cond(grepl(x=scenario, pattern="Low Overshoot", fixed=T), target = "VLHO") %>% # not the case for REMIND

    return()
}

add_ssp_basis_to_IAM_scenarios <- function(df){
  df %>% mutate(ssp=substr(scenario, start = 1, stop = 4))
}

add_sector_and_species_columns <- function(df){
  df %>%
    mutate(sector = str_replace(variable, "^Emissions\\|", "")) %>%
    mutate(species = str_extract(sector, "^[^|]+")) %>%
    mutate(sector = ifelse(
      species==sector,
      "Total",
      str_replace(sector, paste0("^",species,"\\|"), "")
    )) %>%
    return()

}

remove_scenarios_with_issues <- function(df){
  return(
    df %>%
      filter(!(scenario=="SSP2 - Medium Emissions_a" & model=="GCAM"), # reporting error; likely unit issue (at least in passenger transport pkm)
             !(full.model.name=="MESSAGEix-GLOBIOM 2.1-M-R12"), # only keep MESSAGE model that has "GAINS" in the name
             # !(scenario=="..." & model=="...")
             )
  )
}

##### Adjusting "value" --------------------------------------------------------

#' Normalise values in a long IAMC data frame to a starting year where the starting year value thus becomes 1
#'
#' @param df
#' @param starting.year
#'
#' @return
#' @export
#'
#' @examples
normalise_iamc_long <- function(df, starting.year) {

  # normalise the values in column "value" to the year "starting.year = 1"
  # - by (model, scenario, region, variable, unit)

  df.temp <- df %>%
    left_join(
      df %>% filter(year==starting.year) %>% rename(value.start=value) %>%
        select(model, scenario, region, variable, unit, value.start),
      by = c("model", "scenario", "region", "variable", "unit")
    ) %>%
    mutate(value=value/value.start)

  return(
    df.temp %>% select(-value.start)
  )

}

#' Divide all values by the value of the "Population" variable
#'
#' Assumes
#' - the IAMC data frame is in long format (with columns "model", "scenario", "region", "variable", "unit", "year", "value")
#' - the IAMC data frame has a variable called "Population" for all model-scenario combinations
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
to_per_capita <- function(df){
  pop <- df %>% filter(variable=="Population") %>% rename(pop=value) %>% select(model,scenario,region,year,pop)
  df <- df %>% filter(variable!="Population") %>%
    left_join(pop) %>%
    mutate(value=value/pop) %>%
    select(-pop)
  return(df)
}

#' Divide all values by the value of the GDP variable (e.g. "GDP (PPP)")
#'
#' Assumes
#' - the IAMC data frame is in long format (with columns "model", "scenario", "region", "variable", "unit", "year", "value")
#' - the IAMC data frame has the GDP variable variable for all model-scenario combinations
#'
#' @param df
#'
#' @return
#' @export
#'
#' @examples
to_per_gdp <- function(df, vgdp = "GDP (PPP)", return.gdp = FALSE){
  df.gdp <- df %>% filter(variable==vgdp)
  gdp <- df.gdp %>% rename(gdp=value) %>% select(model,scenario,region,year,gdp)
  df <- df %>% filter(variable!=vgdp) %>%
    left_join(gdp) %>%
    mutate(value=value/gdp) %>%
    select(-gdp)

  if(return.gdp){
    df <- df %>%
      bind_rows(
        df.gdp
      )
  }

  return(df)
}

#' Add columns for the percentiles of the values across groups of the (IAMC) data frame
#'
#' Options:
#' - percentiles: vector of percentiles between 0 and 1
#' - group.cols: vector of column names to group by (default is c("model", "scenario", "variable", "year"))
#'
#' @param df
#' @param percentiles
#' @param group.cols
#'
#' @return same df, but with extra columns, called e.g. p25 and p75 for percentiles = c(0.25, 0.75)
#' @export
#'
#' @examples
add_percentile_columns <- function(df, percentiles = c(0.25, 0.5, 0.75),
                                   group.cols = c("model", "scenario", "variable", "year"),
                                   only.keep.percentiles = FALSE,
                                   additional.keep.cols = NA) {
  # standard = across countries (iso is left out of group.cols)

  p_names <- map_chr(percentiles, ~ paste0("p", .x * 100))

  if(only.keep.percentiles){
    if(!is.na(additional.keep.cols)){
      keep.cols <- c(group.cols, additional.keep.cols, p_names)
    } else(
      keep.cols <- c(group.cols, p_names)
    )
  }

  p_funs <- map(percentiles, ~ partial(quantile, probs = .x, na.rm = TRUE)) %>%
    set_names(nm = p_names)

  group.cols <- enquo(group.cols) # need to quote

  df.percentiles <- df %>%
    group_by_at(vars(!!group.cols)) %>%
    summarize_at(vars(value), .funs = p_funs)

  if (only.keep.percentiles){
    return(df %>% left_join(df.percentiles) %>%
             distinct(across(all_of(keep.cols)))
    )
  } else {
    return(df %>% left_join(df.percentiles))
  }
}

#' Extrapolate with constant values.
#' In the case that later years do not have data values, but NAs, and you want to extrapolate with constant values, try this function.
#'
#' Currently not used, nor tested. It is here for future use, and may or may not work.
#' - Its use was considered and tried out in `calculator_needs-dle-efficiency.R`, for industry in the SHAPE SDP scenarios.
#'
#' @param df
#' @param variable.to.fill
#'
#' @return
#' @export
#'
#' @examples
constant_year_fill <- function(df,
                               variable.to.fill="value"){
  df <- df %>%
    group_by(model,scenario,region,variable) %>%
    arrange(model,scenario,region,variable,year) %>%
    fill((!!as.name(variable.to.fill)),
         .direction = "down" #c("down", "up", "downup", "updown")
    ) %>%
    ungroup()
  return(df)
}

# Function wishes:
# - "annualise" function, using something like tidyr::complete(year=2000:2100), and na.approx


##### Aggregation functions -----------------------------------------------------
# here, or across ISO

# Function wishes:
# - a proper function for region aggregation (e.g. see SHAPE manuscript figures for an example)
# region_aggregate_sum <- function(df,
#   region.col.name,
#   group.cols = c("model", "scenario", "variable", "year")
# ){
#   # only works for one variable at the moment (& column must be named `value`)
#
#   group.cols <- c(group.cols, region.col.name)
#   group.cols <- enquo(group.cols) # need to quote
#
#   grp.df <- df %>%
#     group_by(vars(!!group.cols)) %>%
#     summarise(
#       value = sum(value)
#     )
#
#   return(grp.df)
# }



##### Adjusting "variable" string ----------------------------------------------

#' In the IAMC variable column, use enters instead of pipes to separate levels
#'
#' @param df
#'
#' @return df with altered variable column strings
#' @export
#'
#' @examples
iamc_variable_enterise_levels <- function(df){

  df <- df %>%
    mutate(
      variable = str_replace(variable, "\\|", "\n")
    )

  return(df)
}

#' Keep only one level (between pipes) of the IAMC variable column
#'
#' Note: does not check against creating duplicate variable names.
#'
#' @param df
#' @param level
#'
#' @return df with altered variable column strings
#' @export
#'
#' @examples
iamc_variable_keep_one_level <- function(df, level){

  df <- df %>%
    mutate(str.split = strsplit(variable, "|", fixed = TRUE)) %>%
    mutate(variable = sapply(str.split,
                             function(x) if (length(x) >= abs(level)) ifelse(level>0,x[[level]],x[[length(x)+level+1]]) else NA)) %>%
    select(-str.split)

  return(df)
}

#' Keep two levels (between pipes) of the IAMC variable column
#'
#' Note: does not check against creating duplicate variable names.
#'
#' @param df
#' @param levels
#'
#' @return df with altered variable column strings
#' @export
#'
#' @examples
iamc_variable_keep_two_levels <- function(df, levels){

  l1 <- levels[[1]]
  l2 <- levels[[2]]

  df <- df %>%
    mutate(str.split = strsplit(variable, "|", fixed = TRUE)) %>%
    mutate(variable = sapply(str.split, function(x) if (length(x) >= abs(l1)) ifelse(l1>0,x[[l1]],x[[length(x)+l1+1]]) else x[[length(x)]])) %>%
    mutate(variable = sapply(str.split, function(x) if (length(x) >= abs(l2)) paste0(ifelse(l1>0,x[[l1]],x[[length(x)+l1+1]]),
                                                                                     "|",
                                                                                     ifelse(l2>0,x[[l2]],x[[length(x)+l2+1]])) else x[[length(x)]])) %>%
    select(-str.split)

  return(df)
}


#' Keep only one level (between pipes) of the IAMC region column
#'
#' Note: does not check against creating duplicate region names.
#'
#' @param df
#' @param level
#'
#' @return df with altered region column strings
#' @export
#'
#' @examples
iamc_region_keep_one_level <- function(df, level){

  df <- df %>%
    mutate(str.split = strsplit(region, "|", fixed = TRUE)) %>%
    mutate(region = sapply(str.split,
                             function(x) if (length(x) >= abs(level)) ifelse(level>0,x[[level]],x[[length(x)+level+1]]) else NA)) %>%
    select(-str.split)

  return(df)
}


# Load necessary libraries
library(dplyr)
library(stringr)

# Function: Remove a specific first-level match from the variable column
# @param df A data frame containing a column named `variable`.
# @param match.string A string to be matched and removed if it appears at the start of the `variable` column, followed by "|".
# @return A modified data frame with the specified first-level match removed from the `variable` column.
remove_variable_firstlevel_match <- function(df, match.string) {
  # Input validation
  if (!"variable" %in% colnames(df)) {
    stop("The data frame must contain a column named 'variable'.")
  }
  if (!is.character(match.string) || length(match.string) != 1) {
    stop("`match.string` must be a single character string.")
  }

  # Replace the matching pattern at the start of the string
  df %>%
    mutate(
      variable = str_replace(variable, paste0("^", match.string, "\\|"), "")
    )
}

# Function: Remove a specific last-level match from the variable column
# @param df A data frame containing a column named `variable`.
# @param match.string A string to be matched and removed if it appears at the end of the `variable` column, preceded by "|".
# @return A modified data frame with the specified last-level match removed from the `variable` column.
remove_variable_lastlevel_match <- function(df, match.string) {
  # Input validation
  if (!"variable" %in% colnames(df)) {
    stop("The data frame must contain a column named 'variable'.")
  }
  if (!is.character(match.string) || length(match.string) != 1) {
    stop("`match.string` must be a single character string.")
  }

  # Replace the matching pattern at the end of the string
  df %>%
    mutate(
      variable = str_replace(variable, paste0("\\|", match.string, "$"), "")
    )
}

# Function: Remove a specific first-level match from the region column
# @param df A data frame containing a column named `region`.
# @param match.string A string to be matched and removed if it appears at the start of the `region` column, followed by "|".
# @return A modified data frame with the specified first-level match removed from the `region` column.
remove_region_firstlevel_match <- function(df, match.string) {
  # Input validation
  if (!"region" %in% colnames(df)) {
    stop("The data frame must contain a column named 'region'.")
  }
  if (!is.character(match.string) || length(match.string) != 1) {
    stop("`match.string` must be a single character string.")
  }

  # Replace the matching pattern at the start of the string
  df %>%
    mutate(
      region = str_replace(region, paste0("^", match.string, "\\|"), "")
    )
}






# Code development utils -------------------------------------------------------

### Testing utils ----------------------------------------------------------------

#' Remove all files produced by running the tests from the test data output directory
#'
#' @return
#' @export
#'
#' @examples
remove_test_data_output <- function() {
  output.files.from.running.tests <- list.files(here(get_data_location(test=TRUE)))
  output.files.from.running.tests <- output.files.from.running.tests[! output.files.from.running.tests == "README.txt"]
  output.files.from.running.tests <- file.path(here(get_data_location(test=TRUE)),
                                               output.files.from.running.tests)

  file.remove(output.files.from.running.tests)

  print(paste0("Removed ", length(output.files.from.running.tests), " files from ", here(get_data_location(test=TRUE))))
}

### Coding style utils ---------------------------------------------------------

#' Clean up the code in this directory, following tidyverse style
#'
#' NOTE: can change the code quite a lot, but most often only enters and spaces
#'
#' @return
#' @export
#'
#' @examples
clean_code_style <- function() {
  library(styler)
  styler::style_dir()
}



# Generic utils ----------------------------------------------------------------

### Country classifications ----------------------------------------------------

#' Load categories that countries fall into.
#' This country classification can be useful for region aggregation of country-level results.
#'
#' Groupings available (grouping.to.load) are:
#' "region_ar6_6_ipcc_fgd": IPCC R6 (AR6 version)
#' "region_ar6_10_ipcc_fgd": IPCC R10 (AR6 version)
#' "region_ar6_22_ipcc_fgd": IPCC R22 (AR6 version)
#' "M49_Hi_M49_Regions": UN Statistics Division M49 High level of aggregation
#' "M49_Med__M49_Regions": UN Statistics Division M49 Medium level of aggregation
#' "M49_lo_M49_Regions": UN Statistics Division M49 Low level of aggregation
#' "Developing_2021_M49_other": UN Statistics Division M49 Developing/Developed countries classification
#' "SIDS_M49_other": UN Statistics Division M49 Small Island Developing States classification
#' "LLDC_M49_other": UN Statistics Division M49 Landlocked Developing Countries classification
#' "LDC_M49_other": UN Statistics Division M49 Least Developed Countries classification
#' "Annex_I_unfccc": UNFCCC Annex I countries classification
#' "Annex _II_unfccc": UNFCCC Annex II countries classification
#' "WMO": World Meteorological Organization classification
#' "EU": European Union classification (post-brexit; EU-27)
#' "OECD": Organization for Economic Co-operation and Development classification
#' "Income _status_WB": World Bank Income Status classification
#' "Former Soviet Union": Former Soviet Union countries (15 countries)
#' "iamc_r5": Integrated Assessment Modeling Consortium (IAMC) R5 regions
#' "iamc_r10": Integrated Assessment Modeling Consortium (IAMC) R10 regions
#' "iamc_r11": Integrated Assessment Modeling Consortium (IAMC) R11 regions
#' "iamc_r12": Integrated Assessment Modeling Consortium (IAMC) R12 regions
#'
#' @param grouping.to.load
#' @param keep.long.names: F (default), T: include longer names besides iso3c in the output
#'
#' @return
#' @export
#'
#' @examples
load_official_country_grouping <- function(grouping.to.load,
                                           keep.long.names = F) {
  #' Load grouping of ISO (with longer names, which are removed by default)

  if (is.na(grouping.to.load) & !keep.long.names) {
    stop("Please specify a grouping to load.")
  } else if (is.na(grouping.to.load) & keep.long.names) {
    grp <- vroom(
        here("data-raw", "countrygroupings_2024_01.csv"), show_col_types=FALSE
      ) %>%
      rename(iso = ISO) %>%
      select(
        iso,
        name
      )
  } else {
    grp <- vroom(
      here("data-raw", "countrygroupings_2024_01.csv"), show_col_types=FALSE
    ) %>%
      rename(iso = ISO) %>%
      left_join(
        vroom(
          here("data-raw", "iso3c_region_mapping.csv"), show_col_types=FALSE
        ) %>%
          rename(iso = iso3c) %>%
          select(
            iso,
            iamc_r5, iamc_r10, iamc_r11, iamc_r12
          ),
        by = "iso"
      ) %>%
      select(
        iso,
        name,
        !!as.symbol(grouping.to.load)
      )
  }


  if (!keep.long.names) {
    grp <- grp %>% select(-name)
  }


  return(grp)
}


#' Add long country names alongside an iso3c column
#'
#' Takes a dataframe with an `iso` column and adds a `name` column to it with full names for the country.
#'
#' @return a dataframe with iso3c and long names
#' @export
#'
#' @examples df %>% left_join(load_long_names_of_iso3c())
load_long_names_of_iso3c <- function(){
  return(
    load_official_country_grouping(grouping.to.load = NA, keep.long.names = T)
  )
}


### Visualisation utils --------------------------------------------------------

##### Plotting style -----------------------------------------------------------
#' Apply Jarmo's custom plotting style
#'
#' @param
#'
#' @return
#' @export
#'
#' @examples
#' # example ggplot
#' ggplot(s %>% filter(variable=="Primary Energy|Gas")),
#'     aes(x=year,y=value,colour=variable,linetype=scenario,group=interaction(model,scenario,region,variable))) +
#'     facet_wrap(~region, scales = "free_y") +
#'     geom_line() +
#'     theme_jsk() +
#'     ylab("Primary Energy Gas")
theme_jsk <- function() {

  theme_classic() + theme_hc() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.title.x = element_blank()
    )

}

#' Apply background colour to historical values (in a timeseries plot)
#'
#' @param
#'
#' @return
#' @export
#'
#' @examples
#' # example ggplot
#' ggplot(s %>% filter(variable=="Primary Energy|Gas")),
#'     aes(x=year,y=value,colour=variable,linetype=scenario,group=interaction(model,scenario,region,variable))) +
#'     facet_wrap(~region, scales = "free_y") +
#'     mark_history() +
#'     geom_line()
mark_history <- function(sy=STARTYEAR){
  annotate("rect", xmin=-Inf, xmax=sy, ymin=-Inf, ymax=Inf, alpha=0.2, fill="lightgrey", colour=NA)
}


##### Adjusting plots ----------------------------------------------------------
#' Desaturate colors by specified proportion
#'
#' @param cols
#' @param sat
#'
#' @return
#' @export
#'
#' @examples
#' # example using colours from the brewer.pal function of RColorBrewer
#' c.pal <- desat(brewer.pal(n = 12, name = "Set3"), 1.5)
desat <- function(cols, sat = 0.5) {
  X <- diag(c(1, sat, 1)) %*% rgb2hsv(col2rgb(cols))
  hsv(X[1, ], X[2, ], X[3, ])
}


##### Saving plots -------------------------------------------------------------
#' Save ggplot objects as both PNG and PDF (or one of the two)
#'
#' @param p: ggplot object
#' @param f: filename
#' @param h: height
#' @param w: width
#' @param format: "png-pdf" (default) or "png" or "pdf"
#' @param unit: "mm" (default) or "cm" or "in" or "px"
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
#' p <- ggplot(data=iris, aes(x=Sepal.Length, y=Sepal.Width)) + geom_point()
#' save_ggplot(p=p, f="testplotname", h=150, w=150, format="png-pdf", unit="mm")
save_ggplot = function(p,f,h=150,w=150,format="png-pdf",unit="mm",
                       ...){
  if(format=="png-pdf"){
    ggsave(
      plot = p,
      file = paste0(f,".png"),
      height = h,
      width = w,
      unit = unit,
      ...
    )
    ggsave(
      plot = p,
      file = paste0(f,".pdf"), device = cairo_pdf,
      height = h,
      width = w,
      unit = unit,
      ...
    )
  } else if (format=="png") {
    ggsave(
      plot = p,
      file = paste0(f,".png"),
      height = h,
      width = w,
      unit = unit,
      ...
    )
  } else if (format=="pdf") {
    ggsave(
      plot = p,
      file = paste0(f,".pdf"), device = cairo_pdf,
      height = h,
      width = w,
      unit = unit,
      ...
    )
  }
}

##### Standard plots -----------------------------------------------------------
plot_standard_line_one_region <- function(df, colour="scenario"){
  if (
    colour=="scenario"
  ){
    p <- ggplot(df,
                aes(x=year,y=value,
                    colour=scenario,
                    group=interaction(model,scenario,variable,region))) +
      facet_wrap(~variable) +
      geom_line() +
      theme_jsk()
    return(p)
  } else if (
    colour=="variable"
  ){
    p <- ggplot(df,
                aes(x=year,y=value,
                    colour=variable,
                    group=interaction(model,scenario,variable,region))) +
      facet_wrap(~scenario) +
      geom_line() +
      theme_jsk()
    return(p)
  } else (
    stop("This colouring option has not been implemented in this function (`plot_standard_line`).")
  )

}

### String utils ---------------------------------------------------------------
#' Clean a String by Removing Non-Alphanumeric Characters
#'
#' This function removes all non-alphanumeric characters (anything that is
#' not a letter or a number) from the input string. Spaces, punctuation,
#' and special characters are removed, leaving only letters and numbers.
#'
#' @param x A character string to be cleaned.
#'
#' @return A character string containing only letters and numbers.
#'
#' @examples
#' clean_string("Hello World | R123!")  # Returns "HelloWorldR123"
#' clean_string("Data-Science_2024!")  # Returns "DataScience2024"
#'
#' @export
clean_string <- function(x) {
  gsub("[^a-zA-Z0-9]", "", x)
}


### Data manipulation ----------------------------------------------------------
# The opposite of %in%, to be used in a dplyr::filter() call
`%nin%` <- Negate(`%in%`)

#' Conditional manipulation, of subsets of data with dplyr
#'
#' @param .data
#' @param condition
#' @param ...
#' @param envir
#'
#' @return same dataframe format but with the mutations as specified
#' @export
#'
#' @examples
#' DF.tei %>% mutate_cond(elec == "non.elec", e.int = 0)
#' df %>% mutate_cond(variable == "Final Energy|Industry", variable = "Industry")
mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}

#' Interpolate NA values by year
#'
#' Currently not used, nor tested. It is here for future use, and may or may not work.
#'
#' NB: could be made specific for IAMC data. At the moment it is not, although it assumes the column called "value" to be the data column to be interpolated.
#'
#' @param df
#' @param mg
#' @param r
#'
#' @return
#' @export
#'
#' @examples
interpolate_NA_annual <- function(df, mg = Inf, r = 2) {
  return(
    df %>%
      mutate(
        value = na.approx(value, maxgap = mg, rule = r)
      )
  )
}



### R/RStudio behaviour --------------------------------------------------------
#' Silences R output of a function x
#'
#' @param x
#'
#' @return
#' @export
#'
#' @examples
#' quiet(load_dimensions())
quiet <- function(x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}


### Formatting other data sources ----------------------------------------------

##### World Bank data ----------------------------------------------------------

#' Parse World Bank wide data (already loaded), to long format, aligning closer to long format IAMC style
#'
#' @param df
#'
#' @return long dataframe, with the collumns iso, indicator, year, value
#' @export
#'
#' @examples
wb_parse <- function(df){

  return(
    df %>%
      select(`Country Code`, `Indicator Name`, where(is.numeric)) %>%
      rename(iso = `Country Code`, indicator = `Indicator Name`) %>%
      pivot_longer(names_to = "year", values_to = "value", cols = -c("iso", "indicator")) %>%
      mutate(year = as.numeric(year)) %>%
      arrange(iso,indicator,year) %>%
      drop_na()
  )

}
