# Restore packages in renv.lock file
renv::restore()

# Install packages for development
## General use
renv::install("devtools@2.4.3")
renv::install("attachment")

## VS Code specific
renv::install("languageserver")
renv::install("httpgd")
