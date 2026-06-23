# Get arf and REditorSupport extension to play nicely;
# This way we can see (in the REditorSupport extension window in VSCode) the
# R objects created in the arf terminal
local({
    vscr_init <- file.path(
        Sys.getenv(if (.Platform$OS.type == "windows") "USERPROFILE" else "HOME"),
        ".vscode-R",
        "init.R"
    )
    if (file.exists(vscr_init)) {
        source(vscr_init)
    }
    # `arf` sources .Rprofile without completing R's normal startup sequence, so
    # `.First.sys()` (which attaches default packages: stats, utils, grDevices,
    # etc.) is never called automatically. `vscr_init` above replaces
    # `globalenv()$.First.sys` with its own wrapper that calls the real
    # `base::.First.sys` first, then wires up the session watcher — so calling it
    # here once triggers both: package attachment and session watcher init.
    .First.sys()
})

# Route file.edit()/edit() through VSCode so usethis/pkgdown don't fall back to
# `vi`. The `arf` terminal doesn't inherit VSCode's injected PATH, so resolve the
# `code` binary explicitly (falling back to the VSCode server's remote CLI, whose
# path contains a commit hash) rather than relying on `code` being on PATH.
local({
    code_bin <- Sys.which("code")
    if (!nzchar(code_bin)) {
        candidates <- Sys.glob(c(
            "/vscode/vscode-server/bin/*/bin/remote-cli/code",
            file.path(Sys.getenv("HOME"), ".vscode-server/bin/*/bin/remote-cli/code")
        ))
        if (length(candidates)) code_bin <- candidates[[1]]
    }
    if (nzchar(code_bin)) {
        options(editor = paste(shQuote(code_bin), "--wait"))
    }
})
