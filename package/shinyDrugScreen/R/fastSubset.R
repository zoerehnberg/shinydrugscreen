fastSubset_gdsc1 <- function(CL = NULL, drug = NULL, combo = NULL){
  if(is.null(CL) & is.null(drug)) tmp <- shinyDrugScreen::index_all$combo_gdsc1[[which(shinyDrugScreen::ID_all$comboID_gdsc1 == combo)]]
  else if(is.null(CL)) tmp <- shinyDrugScreen::index_all$drug_gdsc1[[which(shinyDrugScreen::ID_all$drugID_gdsc1 == drug)]]
  else if(is.null(drug)) tmp <- shinyDrugScreen::index_all$cl_gdsc1[[which(shinyDrugScreen::ID_all$clID_gdsc1 == CL)]]
  else tmp <- intersect(shinyDrugScreen::index_all$drug_gdsc1[[which(shinyDrugScreen::ID_all$drugID_gdsc1 == drug)]],
                        shinyDrugScreen::index_all$cl_gdsc1[[which(shinyDrugScreen::ID_all$clID_gdsc1 == CL)]])
  return(shinyDrugScreen::shiny_gdsc1[tmp,])
}

fastSubset_ccle <- function(CL = NULL, drug = NULL, combo = NULL){
  if(is.null(CL) & is.null(drug)) tmp <- shinyDrugScreen::index_all$combo_ccle[[which(shinyDrugScreen::ID_all$comboID_ccle == combo)]]
  else if(is.null(CL)) tmp <- shinyDrugScreen::index_all$drug_ccle[[which(shinyDrugScreen::ID_all$drugID_ccle == drug)]]
  else if(is.null(drug)) tmp <- shinyDrugScreen::index_all$cl_ccle[[which(shinyDrugScreen::ID_all$clID_ccle == CL)]]
  else tmp <- intersect(shinyDrugScreen::index_all$drug_ccle[[which(shinyDrugScreen::ID_all$drugID_ccle == drug)]],
                        shinyDrugScreen::index_all$cl_ccle[[which(shinyDrugScreen::ID_all$clID_ccle == CL)]])
  return(shinyDrugScreen::shiny_ccle[tmp,])
}

fastSubset_gdsc2 <- function(CL = NULL, drug = NULL, combo = NULL){
  if(is.null(CL) & is.null(drug)) tmp <- shinyDrugScreen::index_all$combo_gdsc2[[which(shinyDrugScreen::ID_all$comboID_gdsc2 == combo)]]
  else if(is.null(CL)) tmp <- shinyDrugScreen::index_all$drug_gdsc2[[which(shinyDrugScreen::ID_all$drugID_gdsc2 == drug)]]
  else if(is.null(drug)) tmp <- shinyDrugScreen::index_all$cl_gdsc2[[which(shinyDrugScreen::ID_all$clID_gdsc2 == CL)]]
  else tmp <- intersect(shinyDrugScreen::index_all$drug_gdsc2[[which(shinyDrugScreen::ID_all$drugID_gdsc2 == drug)]],
                        shinyDrugScreen::index_all$cl_gdsc2[[which(shinyDrugScreen::ID_all$clID_gdsc2 == CL)]])
  return(shinyDrugScreen::shiny_gdsc2[tmp,])
}