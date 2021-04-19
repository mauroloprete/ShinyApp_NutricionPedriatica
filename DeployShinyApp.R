if(!require(rsconnect)){install.packages("rsconnect")}

rsconnect::setAccountInfo(
    name = 'mauro-loprete',
    token = "Secret",
    secret = 'Secret'
)

deployApp()