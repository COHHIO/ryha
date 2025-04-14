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

# Set up R Language Server
## Edit R_LIBS_USER environmental variable
system(sprintf("sed -i 's|R_LIBS_USER=.*|R_LIBS_USER=%s|' /usr/local/lib/R/etc/Renviron", renv::paths$library()))

## Message for the user
message("
================================================================================
✅ R_LIBS_USER has been updated.

⚠️  Action Required:

   Reload VS Code to complete R Language Server setup.

     Instructions:

   - Open the Command Palette (Ctrl + Shift + P or Cmd + Shift + P)

   - Run: **Developer: Reload Window**
================================================================================
")
