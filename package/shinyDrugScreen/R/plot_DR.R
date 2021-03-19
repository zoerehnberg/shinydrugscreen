# plot dose-response observations for one GDSC combo
plot.DR_gdsc <- function(dat = shiny_gdsc1, combo){
  dat <- fastSubset_gdsc1(combo = combo)
  medUC <- 2^unique(dat$UNTREAT)
  
  plot.dat <- dat[,c("CONC","INTENSITY")]
  plot.dat$CONC <- log2(plot.dat$CONC)
  plot.dat$INTENSITY <- 2^plot.dat$INTENSITY
  names(plot.dat) <- c("x","y")
  
  # get limits for plots
  m <- max(c(plot.dat$y, medUC))
  upperlim <- m + m*0.3
  
  # make the plot
  return(ggplot(data = plot.dat) +
           geom_point(aes(x = x, y = y), shape = 4, size = 2, stroke = 1.5) +
           geom_hline(yintercept = medUC, color = "red", linetype = "dashed",
                      size = 1.5) +
           scale_y_continuous(limits = c(0,upperlim), labels = comma) +
           labs(x = expression("dose (log"["2 "]*mu*"M)"),
                y = "intensity") +
           theme(plot.title = element_text(size = 18),
                 plot.subtitle = element_text(size = 16),
                 axis.text = element_text(size = 13),
                 axis.title = element_text(size = 16)))
}

# plot dose-response observations for all GDSC reps
plotReps.DR_gdsc <- function(dat = shiny_gdsc1, combo){
  if(is.null(combo)) return(NULL)
  dat <- subset(dat, COMBO_ID %in% combo,
                select = c("COMBO_ID","CONC","INTENSITY","UNTREAT"))
  dat <- dat[order(dat$CONC),]
  dat$CONC <- log2(dat$CONC)
  dat$RV <- 2^(dat$INTENSITY - dat$UNTREAT)
  dat$Replicate <- factor(sapply(dat$COMBO_ID, function(j) which(j == combo)))
  
  m <- max(c(dat$RV,1))
  upperlim <- m + m*0.3
  
  return(ggplot(data = dat, aes(x = CONC, y = RV, color = Replicate,
                                shape = Replicate)) +
           geom_point(size = 2, stroke = 1.5) +
           geom_path() +
           geom_hline(yintercept = 1, color = "red", linetype = "dashed",
                      size = 1.5) +
           scale_y_continuous(limits = c(0, upperlim)) +
           labs(title = " ", x = expression("dose (log"["2 "]*mu*"M)"),
                y = "relative viability") +
           theme(plot.title = element_text(size = 18),
                 plot.subtitle = element_text(size = 16),
                 axis.text = element_text(size = 13),
                 axis.title = element_text(size = 16)))
}

# plot dose-response observations for one CCLE combo
plot.DR_ccle <- function(dat = shiny_ccle, combo){
  dat <- fastSubset_ccle(combo = combo)
  medUC <- 2^unique(dat$UNTREAT)
  
  plot.dat <- dat[,c("CONCENTRATION","VALUE")]
  plot.dat$CONCENTRATION <- log2(plot.dat$CONCENTRATION)
  names(plot.dat) <- c("x","y")
  
  plot.dat$y <- 2^(plot.dat$y)
  yAxis <- "intensity"
  
  # get limits for plots
  m <- max(c(plot.dat$y, medUC))
  upperlim <- m + m*0.3
  
  # make the plots
  return(ggplot(data = plot.dat) +
           geom_point(aes(x = x, y = y), shape = 4, size = 2, stroke = 1.5) +
           geom_hline(yintercept = medUC, color = "red", linetype = "dashed",
                      size = 1.5) +
           scale_y_continuous(limits = c(0, upperlim)) +
           labs(title = " ",
                x = expression("dose (log"["2 "]*mu*"M)"),
                y = yAxis) +
           theme(plot.title = element_text(size = 18),
                 plot.subtitle = element_text(size = 16),
                 axis.text = element_text(size = 13),
                 axis.title = element_text(size = 16)))
}

# plot dose-response observations for all CCLE reps
plotReps.DR_ccle <- function(dat = shiny_ccle, combo){
  if(is.null(combo)) return(NULL)
  dat <- subset(dat, COMBO_ID %in% combo,
                select = c("COMBO_ID","CONCENTRATION","VALUE","UNTREAT"))
  dat <- dat[order(dat$CONCENTRATION),]
  dat$CONCENTRATION <- log2(dat$CONCENTRATION)
  dat$RV <- 2^(dat$VALUE - dat$UNTREAT)
  dat$Replicate <- factor(sapply(dat$COMBO_ID, function(j) which(j == combo)))
  
  m <- max(c(dat$RV,1))
  upperlim <- m + m*0.3
  
  return(ggplot(data = dat, aes(x = CONCENTRATION, y = RV, color = Replicate,
                                shape = Replicate)) +
           geom_point(size = 2, stroke = 1.5) +
           geom_path() +
           geom_hline(yintercept = 1, color = "red", linetype = "dashed",
                      size = 1.5) +
           scale_y_continuous(limits = c(0, upperlim)) +
           labs(title = " ", x = expression("dose (log"["2 "]*mu*"M)"),
                y = "relative viability") +
           theme(plot.title = element_text(size = 18),
                 plot.subtitle = element_text(size = 16),
                 axis.text = element_text(size = 13),
                 axis.title = element_text(size = 16)))
}

# plot dose-response observations for all GDSC2 reps
plot.DR_gdsc2 <- function(dat = shiny_gdsc2, combo){
  dat <- subset(dat, COMBO_ID == combo)
  medUC <- 2^unique(dat$UNTREAT)
  
  plot.dat <- dat[,c("CONC","INTENSITY")]
  plot.dat$CONC <- log2(plot.dat$CONC)
  plot.dat$INTENSITY <- 2^plot.dat$INTENSITY
  names(plot.dat) <- c("x","y")
  
  # get limits for plots
  m <- max(c(plot.dat$y, medUC))
  upperlim <- m + m*0.3
  
  # make the plot
  return(ggplot(data = plot.dat) +
           geom_point(aes(x = x, y = y), shape = 4, size = 2, stroke = 1.5) +
           geom_hline(yintercept = medUC, color = "red", linetype = "dashed",
                      size = 1.5) +
           scale_y_continuous(limits = c(0,upperlim), labels = comma) +
           labs(x = expression("dose (log"["2 "]*mu*"M)"), y = "intensity") +
           theme(plot.title = element_text(size = 18),
                 plot.subtitle = element_text(size = 16),
                 axis.text = element_text(size = 13),
                 axis.title = element_text(size = 16)))
}

# plot dose-response observations for all GDSC2 reps
plotReps.DR_gdsc2 <- function(dat = shiny_gdsc2, combo){
  if(is.null(combo)) return(NULL)
  dat <- subset(dat, COMBO_ID %in% combo,
                select = c("COMBO_ID","CONC","INTENSITY","UNTREAT"))
  dat <- dat[order(dat$CONC),]
  dat$CONC <- log2(dat$CONC)
  dat$RV <- 2^(dat$INTENSITY - dat$UNTREAT)
  dat$Replicate <- factor(sapply(dat$COMBO_ID, function(j) which(j == combo)))
  
  m <- max(c(dat$RV,1))
  upperlim <- m + m*0.3
  
  return(ggplot(data = dat, aes(x = CONC, y = RV, color = Replicate,
                                shape = Replicate)) +
           geom_point(size = 2, stroke = 1.5) +
           geom_path() +
           geom_hline(yintercept = 1, color = "red", linetype = "dashed",
                      size = 1.5) +
           scale_y_continuous(limits = c(0, upperlim)) +
           labs(title = " ", x = expression("dose (log"["2 "]*mu*"M)"),
                y = "relative viability") +
           theme(plot.title = element_text(size = 18),
                 plot.subtitle = element_text(size = 16),
                 axis.text = element_text(size = 13),
                 axis.title = element_text(size = 16)))
}
