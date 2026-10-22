# ci_auth.R
# Register the shinyapps.io account from repository secrets in GitHub Actions:
#   SHINYAPPS_NAME, SHINYAPPS_TOKEN, SHINYAPPS_SECRET
# Nothing sensitive is written or printed.

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

# accounts() returns names and servers only
acc <- rsconnect::accounts()
if (!NROW(acc)) stop("setAccountInfo ran but no account is registered", call. = FALSE)
message("rsconnect authenticated as ", paste(acc$name, collapse = ", "),
        " on ", paste(acc$server, collapse = ", "))
