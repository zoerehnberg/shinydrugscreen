# plot a GDSC heatmap
plot.heat_gdsc <- function(data = shinyDrugScreen::shiny_gdsc1, scan){
  # get data for the plate of interest
  use.dat <- dplyr::filter(data, SCAN_ID == scan)[,c("POSITION","INTENSITY",
                                                     "UNTREAT")]
  # get row/column info
  if(max(use.dat$POSITION) < 100){
    nwells <- 96
    COL <- rep(1:12,8)
    ROW <- rep(1:8, each = 12)
  }
  else{
    nwells <- 384
    COL <- rep(1:24,16)
    ROW <- rep(1:16, each = 24)
  }
  
  # create vector of intensities -- fill in missing entries with NA
  ord.int <- rep(NA, nwells)
  ord.int[use.dat$POSITION] <- 2^use.dat$INTENSITY
  
  plot.dat <- data.frame(ord.int, ROW, COL)
  max_int <- max(plot.dat$ord.int, na.rm = T)
  min_int <- min(plot.dat$ord.int, na.rm = T)
  
  # choose color cutoffs
  medUC <- 2^use.dat$UNTREAT[1]
  fill_values <- c(-1,(c(0.25*medUC, 0.5*medUC, 0.75*medUC, medUC,
                         1.25*medUC, 1.5*medUC, 1.75*medUC) - min_int)/
                     (max_int - min_int),2)
  
  fill_colors <- RColorBrewer::brewer.pal(9, "PuOr")
  tmp <- (fill_values <= 1 & fill_values >= 0)
  tmp_low <- which(tmp[1:4] == FALSE)
  tmp_high <- which(tmp[6:9] == FALSE)
  tmp[c(max(tmp_low), min(tmp_high + 5))] <- TRUE
  fill_colors <- fill_colors[tmp]
  fill_values <- c(0, fill_values[fill_values <= 1 & fill_values >= 0], 1)
  
  return(ggplot(data = plot.dat, aes(x = COL, y = ROW)) +
           geom_tile(aes(fill = ord.int), color = "white") +
           scale_fill_gradientn(values = fill_values, colours = fill_colors,
                                guide = "colorbar", labels = comma) +
           scale_x_continuous(breaks = unique(plot.dat$COL),
                              labels = interleave(seq(1, max(plot.dat$COL), 2), "")) +
           scale_y_reverse(breaks = unique(plot.dat$ROW),
                           labels = interleave(seq(1, max(plot.dat$ROW), 2), "")) +
           labs(x = "", y = "") +
           guides(fill = guide_colorbar(title = "Intensity", ticks = FALSE)) +
           theme(panel.background = element_rect(fill = "black",color = "black"),
                 panel.grid = element_blank(),
                 title = element_text(size = 16),
                 axis.text = element_text(size = 12),
                 legend.title = element_text(size = 14),
                 legend.text = element_text(size = 14)))
}

# plot a CCLE heatmap
plot.heat_ccle <- function(data = shinyDrugScreen::shiny_ccle, scan){
  # get data for the plate of interest
  use.dat <- dplyr::filter(data, ASSAY_PLATE_NAME == scan)[,c("COLUMN_ID","ROW_ID",
                                                              "VALUE","UNTREAT")]
  # get the data to plot
  plot.dat <- data.frame(COLUMN_ID = rep(1:48, 32),
                         ROW_ID = rep(1:32, each = 48))
  plot.dat <- merge(x = plot.dat, y = use.dat, by = c("COLUMN_ID", "ROW_ID"),
                    all.x = TRUE)
  plot.dat$VALUE <- 2^plot.dat$VALUE
  
  max_int <- max(plot.dat$VALUE, na.rm = T)
  min_int <- min(plot.dat$VALUE, na.rm = T)
  
  # choose color cutoffs
  medUC <- 2^use.dat$UNTREAT[1]
  fill_values <- c(-1,(c(0.25*medUC, 0.5*medUC, 0.75*medUC, medUC,
                         1.25*medUC, 1.5*medUC, 1.75*medUC) - min_int)/
                     (max_int - min_int),2)
  fill_colors <- RColorBrewer::brewer.pal(9, "PuOr")
  tmp <- (fill_values <= 1 & fill_values >= 0)
  tmp_low <- which(tmp[1:4] == FALSE)
  tmp_high <- which(tmp[6:9] == FALSE)
  tmp[c(max(tmp_low), min(tmp_high + 5))] <- TRUE
  fill_colors <- fill_colors[tmp]
  fill_values <- c(0, fill_values[fill_values <= 1 & fill_values >= 0], 1)
  
  return(ggplot(data = plot.dat, aes(x = COLUMN_ID, y = ROW_ID)) +
           geom_tile(aes(fill = VALUE), color = "white") +
           scale_fill_gradientn(values = fill_values, colours = fill_colors,
                                guide = "colorbar", labels = comma) +
           scale_x_continuous(breaks = 1:48,
                              labels = interleave(seq(1, 48, 2), "")) +
           scale_y_reverse(breaks = 1:32,
                           labels = interleave(seq(1, 32, 2), "")) +
           labs(x = "", y = "") +
           guides(fill = guide_colorbar(title = "Intensity", ticks = FALSE)) +
           theme(panel.background = element_rect(fill = "black", color = "black"),
                 panel.grid = element_blank(),
                 title = element_text(size = 16),
                 axis.text = element_text(size = 10),
                 legend.title = element_text(size = 14),
                 legend.text = element_text(size = 14)))
}

# plot a GDSC2 heatmap
plot.heat_gdsc2 <- function(data = shinyDrugScreen::shiny_gdsc2, scan){
  use.dat <- dplyr::filter(data, SCAN_ID == scan)[,c("POSITION","INTENSITY",
                                                     "UNTREAT")]
  # get row/column info
  nwells <- 1536
  COL <- rep(1:48,32)
  ROW <- rep(1:32, each = 48)
  
  # create vector of intensities -- fill in missing entries with NA
  ord.int <- rep(NA, nwells)
  ord.int[use.dat$POSITION] <- 2^use.dat$INTENSITY
  
  plot.dat <- data.frame(ord.int, ROW, COL)
  max_int <- max(plot.dat$ord.int, na.rm = T)
  min_int <- min(plot.dat$ord.int, na.rm = T)
  
  # choose color cutoffs
  medUC <- 2^use.dat$UNTREAT[1]
  fill_values <- c(-1,(c(0.25*medUC, 0.5*medUC, 0.75*medUC, medUC,
                         1.25*medUC, 1.5*medUC, 1.75*medUC) - min_int)/
                     (max_int - min_int),2)
  fill_colors <- RColorBrewer::brewer.pal(9, "PuOr")
  tmp <- (fill_values <= 1 & fill_values >= 0)
  tmp_low <- which(tmp[1:4] == FALSE)
  tmp_high <- which(tmp[6:9] == FALSE)
  tmp[c(max(tmp_low), min(tmp_high + 5))] <- TRUE
  fill_colors <- fill_colors[tmp]
  fill_values <- c(0, fill_values[fill_values <= 1 & fill_values >= 0], 1)
  
  return(ggplot(data = plot.dat, aes(x = COL, y = ROW)) +
           geom_tile(aes(fill = ord.int), color = "white") +
           scale_fill_gradientn(values = fill_values, colours = fill_colors,
                                guide = "colorbar", labels = comma) +
           scale_x_continuous(breaks = unique(plot.dat$COL),
                              labels = interleave(seq(1, max(plot.dat$COL), 2), "")) +
           scale_y_reverse(breaks = unique(plot.dat$ROW),
                           labels = interleave(seq(1, max(plot.dat$ROW), 2), "")) +
           labs(x = "", y = "", title = paste0("Plate ", scan)) +
           guides(fill = guide_colorbar(title = "Intensity", ticks = FALSE)) +
           theme(panel.background = element_rect(fill = "black",color = "black"),
                 panel.grid = element_blank(),
                 title = element_text(size = 16),
                 axis.text = element_text(size = 12),
                 legend.title = element_text(size = 14),
                 legend.text = element_text(size = 14)))
}
