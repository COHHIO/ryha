# Use pak as renv backend to automatically handle system dependencies.
# Set repos to CRAN so pak uses the same source as renv.lock (pak defaults to
# RSPM, which diverges from the CRAN-sourced versions recorded in renv.lock).
options(
    renv.config.pak.enabled = TRUE,
    repos = c(
        CRAN = "https://packagemanager.posit.co/cran/latest",
        RSPM = "https://packagemanager.posit.co/cran/latest"
    )
)

# Restore packages in renv.lock file
renv::restore()

# Install dev packages without upgrading already-installed app packages
pak::pak(
    c(
        ## General Use
        "devtools",
        "tinytex",
        "attachment",

        ## Deployment
        "rsconnect",

        ## VS Code Specific
        "languageserver",
        "unigd",
        "httpgd"
    ),
    upgrade = FALSE
)

## Message for the user
message(
    "
========================================================================
   ✅ Dev setup completed.

   ⚠️ Action Required:

   Reload VS Code to complete R Language Server setup.

     Instructions:

   - Open the Command Palette (Ctrl + Shift + P or Cmd + Shift + P)

   - Run: **Developer: Reload Window**
=======================================================================
"
)
