#------------------------------------------------------------------------------*
# Publish datasets
#------------------------------------------------------------------------------*
# Publish dataset when new data is available
#------------------------------------------------------------------------------*

#------------------------------------------------------------------------------*
# Prepare environment ----
#------------------------------------------------------------------------------*

# Load used packages
library(package = "magrittr")

# Load helper functions
source(file = "scripts/utils-github.R")

# Name encrypted file
encrypted_file <- "output/vico_tables.tar.gz.gpg"




#------------------------------------------------------------------------------*
# Check data availability ----
#------------------------------------------------------------------------------*

# If the encrypted archive is not available
if(!file.exists(encrypted_file)){
  # Get data from server
  source("scripts/get-raw-data.R")
}




#------------------------------------------------------------------------------*
# Check if data should be uploaded ----
#------------------------------------------------------------------------------*

# Get latest dataset
get_recent_asset(
  user = "ces-peie", repo = "vico-raw-data", filename = "output/temp.pgp"
)

# Compare with local dataset
if(tools::md5sum("output/temp.pgp") != tools::md5sum(encrypted_file)){
  # And release new dataset if there are differences
  
  # Authenticate
  gtoken <- get_gh_token()
  
  # Create release
  release <- post_gh_release(
    user = "ces-peie", repo = "vico-raw-data",
    tag_name = Sys.time(),
    name = Sys.time(),
    body = "Updated encrypted dataset.",
    auth_token = gtoken
  )
  
  # Upload encrypted file
  response <- post_gh_asset(
    user = "ces-peie", repo = "vico-raw-data", release_id = release$id,
    name = file, path = "output", file = gsub("output/", "", encrypted_file),
    auth_token = gtoken
  )
}

# Remove encrypted archives
file.remove(c("output/temp.pgp", "output/vico_tables.tar.gz.gpg"))
