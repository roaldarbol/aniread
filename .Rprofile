# Source user .Rprofile first (for GitHub Actions extra-repositories)
user_rprofile <- path.expand("~/.Rprofile")
if (file.exists(user_rprofile)) {
  source(user_rprofile)
}

# Set repositories before renv activates
local({
  repos <- getOption("repos")
  repos["roaldarbol"] <- "https://roaldarbol.r-universe.dev"
  if (is.null(repos["CRAN"]) || repos["CRAN"] == "@CRAN@") {
    repos["CRAN"] <- "https://cloud.r-project.org"
  }
  options(repos = repos)
})

source("renv/activate.R")
