# plot a GDSC plate layout
plot.form_gdsc <- function(dat = shiny_gdsc1, plate, combo){
  dat <- subset(dat, SCAN_ID == plate, select = c("POSITION","TAG","COMBO_ID"))
  if(nrow(subset(dat, COMBO_ID == combo)) == 9){
    myColors <- c("grey0","grey10","grey20","grey30","grey40", "grey50",
                  "grey60", "grey70","grey80", "#E69F00", "#56B4E9", "#009E73",
                  "white","#D55E00")
    names(myColors) <- c("Dose 1","Dose 2","Dose 3","Dose 4","Dose 5","Dose 6",
                         "Dose 7","Dose 8","Dose 9","Other","Untreated",
                         "Blank","Missing","Unused")
  }
  else{
    myColors <- c("grey0","grey20","grey40","grey60","grey80", "#E69F00",
                  "#56B4E9", "#009E73", "white","#D55E00")
    names(myColors) <- c("Dose 1","Dose 2","Dose 3","Dose 4","Dose 5","Other",
                         "Untreated","Blank","Missing","Unused")
  }
  
  to.drug <- function(x){
    if(x %in% c("k", "k1", "k2", "ss")) x = "Other"
    else if(x == "B") x = "Blank"
    else if(x == "NC-0") x = "Untreated"
    else if(x == "UN-USED") x = "Unused"
    else x = paste0("Dose ",strsplit(strsplit(x, split = "-")[[1]][2], split = "")[[1]][2])
    return(x)
  }
  dat$TAG <- sapply(dat$TAG, to.drug, USE.NAMES = F)
  
  if(max(dat$POSITION > 100)){
    use.me <- data.frame(ROW = rep(1:16, each = 24), COL = rep(1:24, 16),
                         TAG = "Missing", COMBO = NA)
  }
  else{
    use.me <- data.frame(ROW = rep(1:8, each = 12), COL = rep(1:12, 8),
                         TAG = "Missing", COMBO = NA)
  }
  
  use.me$TAG[dat$POSITION] <- dat$TAG
  use.me$COMBO[dat$POSITION] <- dat$COMBO_ID
  use.box <- subset(use.me, COMBO == combo)
  
  return(ggplot(data = use.me, aes(x = COL, y = ROW)) +
           geom_rect(data = use.box,
                     aes(xmin = min(COL) - 0.5, xmax = max(COL) + 0.5,
                         ymin = min(ROW) - 0.5, ymax = max(ROW) + 0.5),
                     fill = "palegreen", color = "green", size = 1.5) +
           geom_point(aes(color = TAG), size = 7) +
           scale_color_manual(values = myColors) +
           labs(x = "", y = "") +
           scale_x_continuous(breaks = c(1:24)) +
           scale_y_reverse(breaks = c(1:16)) +
           theme(legend.title = element_blank(),
                 legend.text = element_text(size = 16),
                 axis.text = element_text(size = 13),
                 title = element_text(size = 18)) +
           guides(color = guide_legend(override.aes = list(size = 5))))
}

# plot a CCLE plate layout
plot.form_ccle <- function(dat = shiny_ccle, plate, drug){
  # subset to columns of interest
  dat <- subset(dat, ASSAY_PLATE_NAME == plate,
                select = c("COLUMN_ID","ROW_ID","WELL_TYPE","CONCENTRATION","COMPOUND"))
  dat$USE <- ifelse(dat$WELL_TYPE == "SA", dat$CONCENTRATION, dat$WELL_TYPE)
  
  # get well types
  plot.dat <- data.frame(ROW = rep(1:32,each = 48), COL = rep(1:48,32),
                         TAG = "Missing")
  for(i in 1:nrow(dat)){
    plot.dat[plot.dat$ROW == dat[i,2] & plot.dat$COL == dat[i,1],3] <- dat[i,6]
  }
  plot.dat$TAG <- as.character(plot.dat$TAG)
  
  # translate tag names to well types
  to.drug <- function(x){
    if(x == "Missing") x <- "Missing"
    else if(x == "8") x <- "Dose 1"
    else if(x == "2.53164601") x <- "Dose 2"
    else if(x == "0.801153719") x <- "Dose 3"
    else if(x == "0.253529608") x <- "Dose 4"
    else if(x == "0.0802308992") x <- "Dose 5"
    else if(x == "0.0253895205") x <- "Dose 6"
    else if(x == "0.00803465955") x <- "Dose 7"
    else if(x == "0.00254261401") x <- "Dose 8"
    else if(x == "AC") x <- "Blank"
    else if(x == "NC") x <- "Untreated"
    return(x)
  }
  plot.dat$TAG <- factor(sapply(plot.dat$TAG, to.drug, USE.NAMES = F))
  
  myColors <- c("grey0","grey10","grey20","grey30","grey40", "grey50",
                "grey60", "grey70","#56B4E9","#009E73", "white")#"#F0E442")
  names(myColors) <- c("Dose 1","Dose 2","Dose 3","Dose 4","Dose 5","Dose 6",
                       "Dose 7","Dose 8","Untreated","Blank","Missing")
  
  use.box <- subset(dat, COMPOUND == drug)
  
  return(ggplot() +
           geom_rect(data = use.box,
                     aes(xmin = min(COLUMN_ID) - 0.5, xmax = max(COLUMN_ID) + 0.5,
                         ymin = min(ROW_ID) - 0.5, ymax = max(ROW_ID) + 0.5),
                     fill = "palegreen", color = "green", size = 0.75) +
           geom_point(data = plot.dat, aes(x = COL, y = ROW, color = TAG), size = 2) +
           scale_color_manual(values = myColors) +
           labs(x = "", y = "") +
           scale_x_continuous(breaks = 1:48,
                              labels = interleave(seq(1, 48, 2), "")) +
           scale_y_reverse(breaks = 1:32,
                           labels = interleave(seq(1, 32, 2), "")) +
           theme(legend.title = element_blank(),
                 legend.text = element_text(size = 16),
                 axis.text = element_text(size = 13)) +
           guides(color = guide_legend(override.aes = list(size = 5))))
}

# plot a GDSC2 plate layout
plot.form_gdsc2 <- function(dat = shiny_gdsc2, plate, combo){
  dat <- subset(dat, SCAN_ID == plate, select = c("COMBO_ID","TAG","POSITION"))
  dat <- dat[order(dat$TAG),]
  toDrug <- function(x){
    if(!(x %in% c("UN-USED","NC-1","NC-0","DMSO","B",
                  "R1-D1-S","R1-D2-S","R1-D3-S","R1-D4-S","R1-D5-S",
                  "R1-D6-S","R1-D7-S","R2-D1-S","R2-D2-S","R2-D3-S",
                  "R2-D4-S","R2-D5-S","R2-D6-S","R2-D7-S",
                  "PC-1","PC-2","PC-3","PC-4","PC-5",
                  "PC1-D1-S","PC1-D2-S","PC1-D3-S","PC1-D4-S","PC1-D5-S",
                  "PC2-D1-S","PC2-D2-S","PC2-D3-S","PC2-D4-S","PC2-D5-S"))){
      return(paste0("Dose ",strsplit(strsplit(x, split = "-")[[1]][2], split = "")[[1]][2]))
    }
    else if(x %in% c("R1-D1-S","R1-D2-S","R1-D3-S","R1-D4-S","R1-D5-S",
                     "R1-D6-S","R1-D7-S","R2-D1-S","R2-D2-S","R2-D3-S",
                     "R2-D4-S","R2-D5-S","R2-D6-S","R2-D7-S")){
      #return(paste0("Reference ",strsplit(x, split = "-")[[1]][2]))
      return("Reference Drugs")
    }
    else if(x %in% c("PC-1","PC-2","PC-3","PC-4","PC-5",
                     "PC1-D1-S","PC1-D2-S","PC1-D3-S","PC1-D4-S","PC1-D5-S",
                     "PC2-D1-S","PC2-D2-S","PC2-D3-S","PC2-D4-S","PC2-D5-S")){
      return("Positive Control")
    }
    #else if(x %in% c("PC-1","PC-2","PC-3","PC-4","PC-5")){
    #  return("PC-noT")
    #}
    #else if(x %in% c("PC1-D1-S","PC1-D2-S","PC1-D3-S","PC1-D4-S","PC1-D5-S",
    #                 "PC2-D1-S","PC2-D2-S","PC2-D3-S","PC2-D4-S","PC2-D5-S")){
    #  return("PC-T")
    #}
    #else if(x %in% c("PC2-D1-S","PC2-D2-S","PC2-D3-S","PC2-D4-S","PC2-D5-S")){
    #  return("PC2-T")
    #}
    else if(x == "B") return("Blank")
    else if(x == "NC-1") return("Untreated + DMSO")
    else if(x == "NC-0") return("Untreated")
    else if(x == "UN-USED") return("Unused")
    else return(x)
  }
  dat$USE_TAG <- sapply(dat$TAG, toDrug)
  
  use.me <- data.frame(ROW = rep(1:32, each = 48), COL = rep(1:48, 32),
                       TAG = "Missing", COMBO = NA)
  use.me$TAG[dat$POSITION] <- dat$USE_TAG
  use.me$COMBO[dat$POSITION] <- dat$COMBO_ID
  
  myColors <- c("grey0","grey10","grey20","grey30",
                "grey40", "grey50","grey60",
                "#009E73", "#56B4E9", "mediumblue",
                "#F0E442","#D55E00", "#CC79A7","deeppink","white")
  names(myColors) <- c("Dose 1","Dose 2","Dose 3","Dose 4","Dose 5","Dose 6",
                       "Dose 7",
                       "Blank","Untreated","Untreated + DMSO","DMSO",
                       "Unused","Positive Control", "Reference Drugs","Missing")
  #                       "Reference D1","Reference D2","Reference D3","Reference D4",
  #                       "Reference D5","Reference D6","Reference D7")
  
  use.box <- subset(use.me, COMBO == combo)
  
  return(ggplot() +
           geom_rect(data = use.box,
                     aes(xmin = min(COL) - 0.5, xmax = max(COL) + 0.5,
                         ymin = min(ROW) - 0.5, ymax = max(ROW) + 0.5),
                     fill = "palegreen", color = "green", size = 0.75) +
           geom_point(data = use.me, aes(x = COL, y = ROW, color = TAG), size = 2) +
           scale_color_manual(values = myColors) +
           labs(x = "", y = "") +
           scale_x_continuous(breaks = 1:48,
                              labels = interleave(seq(1, 48, 2), "")) +
           scale_y_reverse(breaks = 1:32,
                           labels = interleave(seq(1, 32, 2), "")) +
           theme(legend.title = element_blank(),
                 legend.text = element_text(size = 16),
                 axis.text = element_text(size = 13),
                 title = element_text(size = 18)) +
           guides(color = guide_legend(override.aes = list(size = 5))))
}
