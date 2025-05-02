#install.packages("rsconnect")

library(rsconnect)
rsconnect::setAccountInfo(name='yan61726',
                          token='32ACCB2536BE4ED96E13DC03DE199F65',
                          secret='EpqhV2tIw7xb2ZtBhGV1Z7DKUcHvjLRHapTiO7kj')

# Deploy app from your R project folder
rsconnect::deployApp("C:/RiskofInstitutionalisation")

rsconnect::showLogs(streaming = TRUE)

#shinyApp(ui = ui, server = server) only for local
