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
    "SELECT TABLE_SCHEMA, TABLE_NAME, COLUMN_NAME",
    "FROM INFORMATION_SCHEMA.COLUMNS",
    "WHERE TABLE_SCHEMA IN ('Clinicos', 'Lab')"
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
  select(schema = TABLE_SCHEMA, table = TABLE_NAME, variable = COLUMN_NAME) %>%
  arrange(schema, table, variable) %>%
  filter(
    # Keep all VICo tables
    table %in% vico_tables,
    # Remove troublesome variables
    variable != "H1Q0021",
    variable != "observaciones" | !grepl("Resultados", table)
  ) %>%
  mutate(
    # Correctly encode variable names
    variable = iconv(variable, to = "ISO-8859-1//TRANSLIT")
  )


# Prepare query for each table
vico_data <- vico_variables %>%
  nest(variable, .key = "variables") %>%
  mutate(
    data = pmap(
      .,
      function(schema, table, variables){
        # Show current table in console
        cat(
          "\nDownloading: ", table, "...(", as.character(Sys.time()),")\n",
          sep = ""
        )
        
        # Get names for all the columns
        variables <- unlist(variables)
        
        # Build query
        query <- paste(
          "SELECT", paste(variables, collapse = ", "),
          paste0("FROM ", schema, ".", table)
        )
        
        # Download all data
        data <- DBI::dbGetQuery(
          data_base,
          query
        ) %>%
          # And set correct names (long names were truncated)
          set_names(
            # Only use ASCII characters in variable names
            iconv(variables, from = "ISO-8859-1", to = "ASCII//TRANSLIT")
          ) %>%
          as_tibble()
        
        return(data)
      }
    )
  ) %>%
  mutate(
    # SubjectID blob as character
    data = map(
      data,
      ~ .x %>%
        mutate(
          SubjectID = sapply(
            SubjectID, function(x) paste(as.character(x), collapse = "")
          )
        )
    )
  )




#------------------------------------------------------------------------------*
# Check for private data ----
#------------------------------------------------------------------------------*

# Get table of private variables that should not be exported
private <- read_csv("data/private.csv")


# Remove private variables
vico_data <- mutate(
  vico_data,
  wo_private = map(data, select, -one_of(private$private))
)


# Check that no private variables remain in the data
vico_data %>%
  pull(wo_private) %>%
  map(select, one_of(private$private)) %>%
  walk(print, n = 5)




#------------------------------------------------------------------------------*
# Export raw data ----
#------------------------------------------------------------------------------*

# Delete all previous data
file.remove(
  list.files(path = "output/tables", pattern = ".csv$", full.names = TRUE)
)


# Write out each table
vico_data %>%
  # Export each table
  select(wo_private, table) %>%
  pwalk(
    ~write_csv(
      x = .x,
      path = paste0("output/tables/", .y, ".csv")
    )
  )


# Pack in tar and compress
tar(
  tarfile = "output/vico_tables.tar.gz",
  files = list.files(path = "output/tables", pattern = ".csv"),
  compression = "gzip",
  tar = "tar",
  extra_flags = "--directory output/tables"
)


# Encrypt
system(
  paste(
    "gpg --yes --encrypt ",
    "--output output/vico_tables.tar.gz.gpg",
    "--recipient fredy@munoz.im",
    "--recipient odeleon@ces.uvg.edu.gt",
    "output/vico_tables.tar.gz"
  )
)

# Delete unencrypted archive
file.remove("output/vico_tables.tar.gz")


# End of script
