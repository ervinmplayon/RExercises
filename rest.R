install.packages(
    c(
        "httr",
        "httr2",
        "glue"
    ),
    repos = "http://cran.us.r-project.org",
    INSTALL_opts = "--no-lock"
)
library("httr")
library("httr2")
library(glue)

# ! this needs to be a secret
server <- "lol.com/v2/rest"

msg <- "checking myself before I wreck myself"
users <- 10
app_installs <- 6
language <- "Czech"

payload <- glue("'{'
    message: {msg}
    new_users: {users}
    new_app_installs: {app_installs}
    language: {language}
''}'")

print(payload)
# ! httr2 method
