fastSubset_gdsc1 <- function(CL = NULL, drug = NULL, combo = NULL){
  if(is.null(CL) & is.null(drug)) tmp <- index_all$combo_gdsc1[[which(ID_all$comboID_gdsc1 == combo)]]
  else if(is.null(CL)) tmp <- index_all$drug_gdsc1[[which(ID_all$drugID_gdsc1 == drug)]]
  else if(is.null(drug)) tmp <- index_all$cl_gdsc1[[which(ID_all$clID_gdsc1 == CL)]]
  else tmp <- intersect(index_all$drug_gdsc1[[which(ID_all$drugID_gdsc1 == drug)]],
                        index_all$cl_gdsc1[[which(ID_all$clID_gdsc1 == CL)]])
  return(shiny_gdsc1[tmp,])
}

fastSubset_ccle <- function(CL = NULL, drug = NULL, combo = NULL){
  if(is.null(CL) & is.null(drug)) tmp <- index_all$combo_ccle[[which(ID_all$comboID_ccle == combo)]]
  else if(is.null(CL)) tmp <- index_all$drug_ccle[[which(ID_all$drugID_ccle == drug)]]
  else if(is.null(drug)) tmp <- index_all$cl_ccle[[which(ID_all$clID_ccle == CL)]]
  else tmp <- intersect(index_all$drug_ccle[[which(ID_all$drugID_ccle == drug)]],
                        index_all$cl_ccle[[which(ID_all$clID_ccle == CL)]])
  return(shiny_ccle[tmp,])
}

fastSubset_gdsc2 <- function(CL = NULL, drug = NULL, combo = NULL){
  if(is.null(CL) & is.null(drug)) tmp <- index_all$combo_gdsc2[[which(ID_all$comboID_gdsc2 == combo)]]
  else if(is.null(CL)) tmp <- index_all$drug_gdsc2[[which(ID_all$drugID_gdsc2 == drug)]]
  else if(is.null(drug)) tmp <- index_all$cl_gdsc2[[which(ID_all$clID_gdsc2 == CL)]]
  else tmp <- intersect(index_all$drug_gdsc2[[which(ID_all$drugID_gdsc2 == drug)]],
                        index_all$cl_gdsc2[[which(ID_all$clID_gdsc2 == CL)]])
  return(shiny_gdsc2[tmp,])
}