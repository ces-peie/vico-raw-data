#------------------------------------------------------------------------------*
# Download most recently published dataset
#------------------------------------------------------------------------------*


# Load utility functions
source("scripts/utils-github.R")


# Name encrypted file
encrypted_file <- "output/vico_tables.tar.gz.gpg"



# Get most recent asset
get_recent_asset(
  user = "ces-peie", repo = "vico-raw-data", filename = encrypted_file
)


# Decrypt downloaded file
system(
  paste(
    "gpg2 --yes",
    "--output", gsub(".gpg", "", encrypted_file),
    "--decrypt", encrypted_file
  )
)

# Remobe encrypted file
file.remove(encrypted_file)


# End of script
