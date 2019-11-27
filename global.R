library(shinydashboard)
library(data.table)
library(shinyWidgets)
library(xgboost)
library(plotly)
library(DT)
library(lime)       # ML local interpretation
# remotes::install_github("AppliedDataSciencePartners/xgboostExplainer")
# library(xgboostExplainer)
load("data/varexp.RData")
geo_eta = fread("data/datasets/annexes/geo_correspondance.csv",select=c("nom.service","cod_ror_eg"))
geo_eta = unique(geo_eta)

load("data/prepared/test_tout.RData")
test_tout = test
load("data/prepared/test_hospi.RData")
test_hospi= test
load("data/prepared/test_rad.RData")
test_rad = test
rm(test)

load("data/vars_gtrends.RData")

# m_train = as.matrix(test_tout[,vars_explicatives,with=F])
# dtrain <- xgb.DMatrix(m_train, label = test_tout$nb)

erreur_hospi = test_hospi[,.(erreur_quadratique=sqrt(mean((nb-pred)^2)),erreur_abs = mean(abs(nb-pred)),flux_moyen = mean(nb)),by=.(categorie_age,cod_ror_eg)]
erreur_rad = test_rad[,.(erreur_quadratique=sqrt(mean((nb-pred)^2)),erreur_abs = mean(abs(nb-pred)),flux_moyen = mean(nb)),by=.(categorie_age,cod_ror_eg)]




bst_tout = xgb.load("data/prepared/model_tout.xgb")
bst_hospi = xgb.load("data/prepared/model_hospi.xgb")
bst_rad = xgb.load("data/prepared/model_rad.xgb")

geo_eta = merge(geo_eta,unique(test_tout[,.(cod_ror_eg)],by="cod_ror_eg"))

# bst = xgb.load("data/prepared/bst.xgb")
load("data/prepared/moyennes_de_saison.RData")




