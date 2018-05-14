#------------------------------------------------------------------------------*
# Get raw data from VICo database
#------------------------------------------------------------------------------*



#------------------------------------------------------------------------------*
# Prepare environment ----
#------------------------------------------------------------------------------*

# Load used packages
library(package = "tidyverse")


# Connect to database
data_base <- DBI::dbConnect(
  odbc::odbc(),
  # Use locally defined server access
  "PEIEServer",
  # Use locally stored credentials
  uid = scan("/data/odeleon/user", what = "character"),
  pwd = scan("/data/odeleon/password", what = "character")
)




#------------------------------------------------------------------------------*
# Get data ----
#------------------------------------------------------------------------------*

# List possible tables withihn the Clinicos schema
vico_tables <- DBI::dbGetQuery(
  data_base,
  paste(
    "SELECT TABLE_NAME",
    "FROM INFORMATION_SCHEMA.COLUMNS",
    "WHERE TABLE_SCHEMA = 'Clinicos'"
  )
) %>%
  filter(
    grepl(
      paste(
        "(^[CHP][0-9][^_ ]*$)",
        "(^Sujeto_[^_ ]+$)",
        "(^DiarreaResultados$|^RespiratorioResultados$)",
        sep = "|"
      ),
      TABLE_NAME
    )
  ) %>%
  count(TABLE_NAME) %>%
  pull(TABLE_NAME) %>%
  print()



# Download every table
vico_data <- vico_tables %>%
  set_names(.) %>%
  sapply(
    function(table){
      # Show current table in console
      cat(
        "\nDownloading: ", table, "...(", as.character(Sys.time()),")\n",
        sep = ""
      )
      
      # Get names for all the columns
      variables <- DBI::dbGetQuery(
        data_base,
        paste(
          "SELECT COLUMN_NAME",
          "FROM INFORMATION_SCHEMA.COLUMNS",
          "WHERE TABLE_SCHEMA = 'Clinicos' AND",
          paste0("TABLE_NAME = '", table, "';")
        )
      ) %>%
        pull(COLUMN_NAME)
      
      # Download all data
      data <- DBI::dbGetQuery(
        data_base,
        paste0("SELECT * FROM Clinicos.", table)
      ) %>%
        # And set correct names (long names were truncated)
        set_names(variables) %>%
        as_tibble()
      
      return(data)
    },
    USE.NAMES = TRUE
  )




#------------------------------------------------------------------------------*
# Export raw data ----
#------------------------------------------------------------------------------*

# Write out each table
vico_data %>%
  # SubjectID blob as character
  map(
    ~ .x %>%
      mutate(
        SubjectID = sapply(
          SubjectID, function(x) paste(as.character(x), collapse = "")
        )
      )
  ) %>%
  # Export each table
  list(., names(.)) %>%
  pmap(
    ~write_csv(
      x = .x,
      path = paste0("output/", .y, ".csv")
    )
  )


# End of script
