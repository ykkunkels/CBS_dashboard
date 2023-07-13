
################################
### TEST Shiny CBS Dashboard ###
### #!Debug                  ###
### YKK - 13-07-2023         ###
###~*~*~*~*~*~*~*~*~*~*~*~*~*###


#! Connect to SQL Server
library(DBI)
verbinding_iSR <- dbConnect(odbc(), Driver = "SQL SERVER", Server = "SQL_HSR_ANA_PRD\\i01,50001", 
                            Database = "HSR_ANA_PRD")

#! Get JPS info from SQL server ----
# tbl_SR_JPSReferentie
tbl_SR_JPSReferentie <- dbGetQuery(verbinding_iSR, paste0("SELECT * FROM tbl_SR_JPSReferentie"))


# tbl_SR_JPS_wtps
tbl_SR_JPS_wtps <- dbGetQuery(verbinding_iSR, paste0("SELECT * FROM tbl_SR_JPS"))


# tbl_SR_SectorClassificatie
tbl_SR_SectorClassificatie <- dbGetQuery(verbinding_iSR, paste0("SELECT * FROM tbl_SR_SectorClassificatie"))

# vw_SR_VolgordeJPS
vw_SR_VolgordeJPS <- dbGetQuery(verbinding_iSR, paste0("SELECT * FROM vw_SR_VolgordeJPS"))


# Difference between tables
temp_index <- !(tbl_SR_JPS_wtps$VolgordeJPS == tbl_SR_JPS_wtps_R$VolgordeJPS)

tbl_SR_JPS_wtps$VolgordeJPS[temp_index]
tbl_SR_JPS_wtps_R$VolgordeJPS[temp_index]


temp_out <- data.frame("from_SQL_logfile" = tbl_SR_JPS_wtps$VolgordeJPS[temp_index], "from_SQL_view" = tbl_SR_JPS_wtps_R$VolgordeJPS[temp_index])

write.csv(temp_out, "Verschil in VolgordeJPS variabele.csv")

if(any(FALSE %in% (tbl_SR_JPS_wtps$VolgordeJPS == tbl_SR_JPS_wtps_R$VolgordeJPS))){
  message("Warning: Inconsistency in 'VolgordeJPS' variable")
}
