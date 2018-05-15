#------------------------------------------------------------------------------*
# Interact with GitHub REST API
#------------------------------------------------------------------------------*
# These functions provide simple access to the GitHub API to publish archived
# tables and download the most recent published archive
#------------------------------------------------------------------------------*


#------------------------------------------------------------------------------*
# Functions to get releases data ----
#------------------------------------------------------------------------------*

# Get all repo releases
get_gh_releases <- function(user, repo){
  # Get releases dataframe
  releases <- jsonlite::fromJSON(
    httr::content(
      httr::GET(
        paste("https://api.github.com/repos", user, repo, "releases", sep = "/")
      ),
      as = "text"
    )
  )
  
  # Set published date as date
  releases$published_at <- as.POSIXct(releases$published_at)
  
  # Return releases
  return(releases)
}




# Get the latest release
get_gh_latest_release <- function(user, repo){
  # Get releses
  releases <- get_gh_releases(user, repo)
  
  # Select latest
  release <- subset(releases, subset = published_at == max(published_at))
  
  # Return latest release
  return(release)
}




# Get the asset URI
get_asset_uri <- function(release){
  # Get all assets URIs
  assets <- purrr::map_chr(release$assets, "browser_download_url")
  
  # Return all assets URIs
  return(assets)
}




#------------------------------------------------------------------------------*
# Functions to publish releases data ----
#------------------------------------------------------------------------------*

# Authenticate
get_gh_token <- function(
  token_path = Sys.getenv("gs"),
  key = NULL,
  secret = NULL
){
  if(token_path != "" & !is.null(token_path) & file.exists(token_path)){
    # Use available token
    github_token <- readRDS(file = Sys.getenv("gs"))
  } else {
    # Negotiate new token
    if(
      token_path != "" & !is.null(token_path) & !is.null(key) & !is.null(secret)
    ){
      myapp <- httr::oauth_app("github", key = key, secret = secret)
      
      # Requesting repo scope to have push credentials
      github_token <- httr::oauth2.0_token(
        httr::oauth_endpoints("github"), myapp, scope = "repo"
      )
      
      saveRDS(github_token, file = token_path)
    } else {
      stop(
        "You need to provide a valid token path, ",
        "and a key and secret to negotiate a new token if one is not available."
      )
    }
  }
  
  gtoken <- httr::config(token = github_token)
  
  # Return the authentication token
  return(gtoken)
}




# Create a new release
post_gh_release <- function(
  user, repo,
  tag_name, name, body,
  auth_token,
  target_commitish = "master", draft = FALSE, prerelease = FALSE,
  verbose = FALSE
){
  # Create the release
  response <- httr::POST(
    paste("https://api.github.com/repos", user, repo, "releases", sep = "/"),
    body = list(
      tag_name = tag_name,
      target_commitish = target_commitish,
      name = name,
      body = body,
      draft = draft,
      prerelease = prerelease
    ),
    encode = "json",
    auth_token,
    if(verbose) httr::verbose() else NULL
  )
  
  # Show response
  print(response)
  
  # Get response as list
  response <- jsonlite::fromJSON(
    httr::content(
      response,
      as = "text"
    )
  )
  
  # Return response
  return(response)
}




# Publish an asset to a release
post_gh_asset <- function(
  user, repo, release_id,
  name, path, file,
  auth_token, encode = "raw",
  verbose = FALSE
){
  # prepare data
  file_path <- paste(path, file, sep = "/")
  payload <- curl::form_data(
    value = readBin(con = file_path, what = "raw", n = file.size(file_path)),
    type = "application/pgp-encrypted"
  )
  payload <- readBin(con = file_path, what = "raw", n = file.size(file_path))
  
  # Post the asset
  response <- httr::POST(
    paste(
      "https://uploads.github.com/repos", user, repo,
      "releases", release_id,
      paste(
        paste0("assets?name=", file),
        sep = "&?"
      ),
      sep = "/"
    ),
    body = payload,
    encode = encode,
    auth_token,
    httr::add_headers(
      "Content-Type" = "application/pgp-encrypted"
    ),
    if(verbose) httr::verbose() else NULL
  )
  
  # Show response
  print(response)
  
  # Get response as list
  response <- jsonlite::fromJSON(
    httr::content(
      response,
      as = "text"
    )
  )
  
  # Return response
  return(response)
}

# End of script
