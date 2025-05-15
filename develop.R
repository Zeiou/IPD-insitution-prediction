#install.packages("rsconnect")

library(rsconnect)
rsconnect::setAccountInfo(name='xxxx',
                          token='xxx5',
                          secret='xxxxx')

# Deploy app from your R project folder
rsconnect::deployApp("C:/RiskofInstitutionalisation")

rsconnect::showLogs(streaming = TRUE)

#shinyApp(ui = ui, server = server) only for local
