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

# Get Clinicos schema
vico_schema <- DBI::dbGetQuery(
  data_base,
  paste(
    "SELECT TABLE_NAME, COLUMN_NAME",
    "FROM INFORMATION_SCHEMA.COLUMNS",
    "WHERE TABLE_SCHEMA = 'Clinicos'"
  )
)


# List possible tables withihn the Clinicos schema
vico_tables <- vico_schema %>%
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


# Get all needed variables
vico_variables <- vico_schema %>%
  select(table = TABLE_NAME, variable = COLUMN_NAME) %>%
  filter(
    # Keep all VICo tables
    table %in% vico_tables,
    # Remove troublesome variables
    variable != "H1Q0021"
  ) %>%
  mutate(
    # Correctly encode variable names
    variable = iconv(variable, to = "ISO-8859-1//TRANSLIT")
  )

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
# Check for private data ----
#------------------------------------------------------------------------------*

# Get table of private variables that should not be exported
private <- read_csv("data/private.csv")


# Remove private variables
vico_wo_private_data <- map(vico_data, ~select(.x, -one_of(private$private)))


# Check that no private variables remain in the data
vico_wo_private_data %>%
  map(select, one_of(private$private)) %>%
  walk(print, n = 5)



#------------------------------------------------------------------------------*
# Export raw data ----
#------------------------------------------------------------------------------*

# Write out each table
vico_wo_private_data %>%
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
