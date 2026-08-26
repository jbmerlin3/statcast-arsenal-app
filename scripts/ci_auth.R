# ci_auth.R
#
# Register the shinyapps.io account from environment variables, for the GitHub
# Actions run. A human on a laptop does this once with rsconnect::setAccountInfo()
# and never thinks about it again; a runner is a fresh machine every time.
#
# The values come from repository secrets and are never written to disk in the
# repo, never echoed, and never passed on a command line where they would show
# up in a process list. rsconnect writes them into its own config dir on the
# runner, which is destroyed when the job ends.
#
# Set these three as repository secrets:
#   SHINYAPPS_NAME    the account name, e.g. jonathanmerlin
#   SHINYAPPS_TOKEN   from https://www.shinyapps.io/admin/#/tokens
#   SHINYAPPS_SECRET  the matching secret
#
# Fails loudly on a missing value rather than letting the deploy get most of the
# way through and then fail with something that reads like a network problem.

name   <- Sys.getenv("SHINYAPPS_NAME")
token  <- Sys.getenv("SHINYAPPS_TOKEN")
secret <- Sys.getenv("SHINYAPPS_SECRET")

missing <- c("SHINYAPPS_NAME", "SHINYAPPS_TOKEN", "SHINYAPPS_SECRET")[
  c(name, token, secret) == ""]
if (length(missing)) {
  stop("Missing repository secret(s): ", paste(missing, collapse = ", "),
       ". Set them with: gh secret set <NAME>", call. = FALSE)
}

rsconnect::setAccountInfo(name = name, token = token, secret = secret)

# Confirm without printing anything sensitive. accounts() returns names and
# servers only, no token material.
acc <- rsconnect::accounts()
if (!NROW(acc)) stop("setAccountInfo ran but no account is registered", call. = FALSE)
message("rsconnect authenticated as ", paste(acc$name, collapse = ", "),
        " on ", paste(acc$server, collapse = ", "))
