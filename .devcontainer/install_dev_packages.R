# Restore packages in renv.lock file
renv::restore()

# Install packages for development
## General use
renv::install("devtools@2.4.5")
renv::install("tinytex@0.56")
renv::install("attachment@0.4.5")

## Deployment
renv::install("rsconnect@1.3.4")

## VS Code specific
renv::install("languageserver@0.3.16")
renv::install("httpgd@2.0.4")
