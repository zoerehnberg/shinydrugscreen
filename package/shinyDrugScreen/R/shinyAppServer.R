server <- function(input, output) {
  # initialize display
  rv <- reactiveValues(site = "Screening Site: Broad Institute",
                       CLname = "1321N1",
                       Dname = "CGP055750-NX-1",
                       scan = "BH017499",
                       combo = 23566,
                       CLtoD = NULL, DtoCL = NULL, rep_gdsc = NULL,
                       CLtoD_ccle = NULL, DtoCL_ccle = NULL, rep_ccle = c(23566,23568,40481,40483),
                       CLtoD_gdsc2 = NULL, DtoCL_gdsc2 = NULL, rep_gdsc2 = NULL,
                       show_reps = c(23566,23568,40481,40483),
                       showRep = 1:4,
                       dset = "c")
  
  ###### SIDEBAR PANEL ######
  
  # CELL LINE --> DRUG
  # ##############################################
  # GDSC
  # create cell line drop-down menu
  output$searchCL <- renderUI({
    if(input$button == "Cell Line ID"){
      rv$CLtoD <- NULL
      rv$DtoCL <- NULL
      rv$rep_gdsc <- NULL
      selectInput(inputId = "clID",
                  label = "Select or enter a cell line ID:",
                  choices = sort(shinyDrugScreen::ID_all$clID_gdsc1))
    }
  })
  
  # output the condition for displaying full cell line list
  output$condCL <- reactive(input$button == "Cell Line ID")
  outputOptions(output, "condCL", suspendWhenHidden = FALSE)
  
  # when "Find Drug IDs" is clicked
  observeEvent(input$clgo, {
    rv$CLtoD <- sort(na.omit(unique(fastSubset_gdsc1(CL = input$clID)$DRUG_ID)))
    rv$DtoCL <- NULL
    rv$rep_gdsc <- NULL
  })
  output$CLtoD <- renderUI(selectInput(inputId = "CLtoDID",
                                       label = "Select or enter a drug ID:",
                                       choices = rv$CLtoD))
  output$condCLtoD <- reactive(length(rv$CLtoD) > 0)
  outputOptions(output, "condCLtoD", suspendWhenHidden = FALSE)
  
  # when "Select Combination" is clicked
  observeEvent(input$combo1go, {
    tmp <- fastSubset_gdsc1(CL = input$clID, drug = input$CLtoDID)
    rv$rep_gdsc <- sort(unique(tmp$COMBO_ID))
    rv$showRep <- 1:length(rv$rep_gdsc)
  })
  output$rep_gdsc <- renderUI(selectInput(inputId = "rep_gdsc",
                                          label = "Select a replicate:",
                                          choices = rv$showRep))
  output$condRep_gdsc <- reactive(length(rv$rep_gdsc) > 0)
  outputOptions(output, "condRep_gdsc", suspendWhenHidden = FALSE)
  
  # when "Select Repicate" is clicked
  observeEvent(input$repgo, {
    rv$combo <- rv$rep_gdsc[as.numeric(input$rep_gdsc)]
    tmp <- fastSubset_gdsc1(combo = rv$combo)[1,]
    if(tmp$SITE == "WTSI") rv$site = "Screening Site: Wellcome Trust - Sanger Institute"
    else rv$site = "Screening Site: Massachusetts General Hospital"
    rv$CLname <- tmp$COSMIC_ID
    rv$Dname <- tmp$DRUG_ID
    rv$scan <- tmp$SCAN_ID
    rv$show_reps <- rv$rep_gdsc
    rv$dset = "g1"
  })
  
  # CCLE
  # create cell line drop-down menu
  output$searchCL_ccle <- renderUI({
    if(input$button_ccle == "Cell Line ID"){
      rv$CLtoD_ccle <- NULL
      rv$DtoCL_ccle <- NULL
      rv$rep_ccle <- NULL
      selectInput(inputId = "clID_ccle",
                  label = "Select or enter a cell line ID:",
                  choices = sort(shinyDrugScreen::ID_all$clID_ccle))
    }
  })
  
  # output the condition for displaying full cell line list
  output$condCL_ccle <- reactive(input$button_ccle == "Cell Line ID")
  outputOptions(output, "condCL_ccle", suspendWhenHidden = FALSE)
  
  # when "Find Drug IDs" is clicked
  observeEvent(input$clgo_ccle, {
    rv$CLtoD_ccle <- sort(unique(subset(fastSubset_ccle(CL = input$clID_ccle),
                                        WELL_TYPE == "SA")$COMPOUND))
    rv$DtoCL_ccle <- NULL
    rv$rep_ccle <- NULL
  })
  output$CLtoD_ccle <- renderUI(selectInput(inputId = "CLtoDID_ccle",
                                            label = "Select or enter a drug ID:",
                                            choices = rv$CLtoD_ccle))
  output$condCLtoD_ccle <- reactive(length(rv$CLtoD_ccle) > 0)
  outputOptions(output, "condCLtoD_ccle", suspendWhenHidden = FALSE)
  
  # when "Select Combination" is clicked
  observeEvent(input$combo1go_ccle, {
    tmp <- fastSubset_ccle(CL = input$clID_ccle, drug = input$CLtoDID_ccle)
    rv$rep_ccle <- sort(unique(tmp$COMBO_ID))
    rv$showRep <- 1:length(rv$rep_ccle)
  })
  output$rep_ccle <- renderUI(selectInput(inputId = "rep_ccle",
                                          label = "Select a replicate:",
                                          choices = rv$showRep))
  output$condRep_ccle <- reactive(length(rv$rep_ccle) > 0)
  outputOptions(output, "condRep_ccle", suspendWhenHidden = FALSE)
  
  # when "Select Replicate" is clicked
  observeEvent(input$repgo_ccle, {
    rv$combo <- rv$rep_ccle[as.numeric(input$rep_ccle)]
    tmp <- fastSubset_ccle(combo = rv$combo)[1,]
    rv$site <- "Screening Site: Broad Institute"
    rv$CLname <- tmp$CELL_LINE_NAME
    rv$Dname <- tmp$COMPOUND
    rv$scan <- tmp$ASSAY_PLATE_NAME
    rv$show_reps <- rv$rep_ccle
    rv$dset = "c"
  })
  
  # GDSC2
  # create cell line drop-down menu
  output$searchCL_gdsc2 <- renderUI({
    if(input$button_gdsc2 == "Cell Line ID"){
      rv$CLtoD_gdsc2 <- NULL
      rv$DtoCL_gdsc2 <- NULL
      rv$rep_gdsc2 <- NULL
      selectInput(inputId = "clID_gdsc2",
                  label = "Select or enter a cell line ID:",
                  choices = sort(shinyDrugScreen::ID_all$clID_gdsc2))
    }
  })
  
  # output the condition for displaying full cell line list
  output$condCL_gdsc2 <- reactive(input$button_gdsc2 == "Cell Line ID")
  outputOptions(output, "condCL_gdsc2", suspendWhenHidden = FALSE)
  
  # when "Find Drug IDs" is clicked
  observeEvent(input$clgo_gdsc2, {
    rv$CLtoD_gdsc2 <- sort(unique(subset(fastSubset_gdsc2(CL = input$clID_gdsc2),
                                         !is.na(COMBO_ID))$DRUG_ID))
    rv$DtoCL_gdsc2 <- NULL
    rv$rep_gdsc2 <- NULL
  })
  output$CLtoD_gdsc2 <- renderUI(selectInput(inputId = "CLtoDID_gdsc2",
                                             label = "Select or enter a drug ID:",
                                             choices = rv$CLtoD_gdsc2))
  output$condCLtoD_gdsc2 <- reactive(length(rv$CLtoD_gdsc2) > 0)
  outputOptions(output, "condCLtoD_gdsc2", suspendWhenHidden = FALSE)
  
  # when "Select Combination" is clicked
  observeEvent(input$combo1go_gdsc2, {
    tmp <- fastSubset_gdsc2(CL = input$clID_gdsc2, drug = input$CLtoDID_gdsc2)
    rv$rep_gdsc2 <- sort(unique(tmp$COMBO_ID))
    rv$showRep <- 1:length(rv$rep_gdsc2)
  })
  output$rep_gdsc2 <- renderUI(selectInput(inputId = "rep_gdsc2",
                                           label = "Select a replicate:",
                                           choices = rv$showRep))
  output$condRep_gdsc2 <- reactive(length(rv$rep_gdsc2) > 0)
  outputOptions(output, "condRep_gdsc2", suspendWhenHidden = FALSE)
  
  # when "Select Replicate" is clicked
  observeEvent(input$repgo_gdsc2, {
    rv$combo <- rv$rep_gdsc2[as.numeric(input$rep_gdsc2)]
    tmp <- fastSubset_gdsc2(combo = rv$combo)[1,]
    rv$site <- "Screening Site: Wellcome Trust Sanger Institute"
    rv$CLname <- tmp$COSMIC_ID
    rv$Dname <- tmp$DRUG_ID
    rv$scan <- tmp$SCAN_ID
    rv$show_reps <- rv$rep_gdsc2
    rv$dset = "g2"
  })
  
  # DRUG --> CELL LINE
  # ##############################################
  # GDSC
  # create drug drop-down menu
  output$searchD <- renderUI({
    if(input$button == "Drug ID"){
      rv$CLtoD <- NULL
      rv$DtoCL <- NULL
      rv$rep_gdsc <- NULL
      selectInput(inputId = "drugID",
                  label = "Select or enter a drug ID:",
                  choices = sort(shinyDrugScreen::ID_all$drugID_gdsc1))
    }
  })
  
  # output the condition for displaying full drug list
  output$condD <- reactive(input$button == "Drug ID")
  outputOptions(output, "condD", suspendWhenHidden = FALSE)
  
  # when "Find Cell Line IDs" is clicked
  observeEvent(input$dgo, {
    rv$DtoCL <- sort(unique(fastSubset_gdsc1(drug = input$drugID)$COSMIC_ID))
    rv$CLtoD <- NULL
    rv$rep_gdsc <- NULL
  })
  output$DtoCL <- renderUI(selectInput(inputId = "DtoCL",
                                       label = "Select or enter a cell line ID:",
                                       choices = rv$DtoCL))
  output$condDtoCL <- reactive(length(rv$DtoCL) > 0)
  outputOptions(output, "condDtoCL", suspendWhenHidden = FALSE)
  
  # when "Select Combination" is clicked
  observeEvent(input$combo2go, {
    tmp <- fastSubset_gdsc1(CL = input$DtoCL, drug = input$drugID)
    rv$rep_gdsc <- sort(unique(tmp$COMBO_ID))
    rv$showRep <- 1:length(rv$rep_gdsc)
  })
  output$rep_gdsc <- renderUI(selectInput(inputId = "rep_gdsc",
                                          label = "Select a replicate:",
                                          choices = rv$showRep))
  output$condRep_gdsc <- reactive(length(rv$rep_gdsc) > 0)
  outputOptions(output, "condRep_gdsc", suspendWhenHidden = FALSE)
  
  # when "Select Replicate" is clicked
  observeEvent(input$repgo, {
    # get the site, drugset, CL and drugs
    rv$combo <- rv$rep_gdsc[as.numeric(input$rep_gdsc)]
    tmp <- fastSubset_gdsc1(combo = rv$combo)[1,]
    if(tmp$SITE == "WTSI") rv$site = "Screening Site: Wellcome Trust - Sanger Institute"
    else rv$site = "Screening Site: Massachusetts General Hospital"
    rv$CLname <- tmp$COSMIC_ID
    rv$Dname <- tmp$DRUG_ID
    rv$scan <- tmp$SCAN_ID
    rv$show_reps <- rv$rep_gdsc
    rv$dset = "g1"
  })
  
  # CCLE
  # create drug drop-down menu
  output$searchD_ccle <- renderUI({
    if(input$button_ccle == "Drug ID"){
      rv$CLtoD_ccle <- NULL
      rv$DtoCL_ccle <- NULL
      rv$rep_ccle <- NULL
      selectInput(inputId = "drugID_ccle",
                  label = "Select or enter a drug ID:",
                  choices = sort(shinyDrugScreen::ID_all$drugID_ccle))
    }
  })
  
  # output the condition for displaying full drug list
  output$condD_ccle <- reactive(input$button_ccle == "Drug ID")
  outputOptions(output, "condD_ccle", suspendWhenHidden = FALSE)
  
  # when "Find Cell Line IDs" is clicked
  observeEvent(input$dgo_ccle, {
    rv$DtoCL_ccle <- sort(unique(fastSubset_ccle(drug = input$drugID_ccle)$CELL_LINE_NAME))
    rv$CLtoD_ccle <- NULL
    rv$rep_ccle <- NULL
  })
  output$DtoCL_ccle <- renderUI(selectInput(inputId = "DtoCL_ccle",
                                            label = "Select or enter a cell line ID:",
                                            choices = rv$DtoCL_ccle))
  output$condDtoCL_ccle <- reactive(length(rv$DtoCL_ccle) > 0)
  outputOptions(output, "condDtoCL_ccle", suspendWhenHidden = FALSE)
  
  # when "Select Combination" is clicked
  observeEvent(input$combo2go_ccle, {
    # get the site, drugset, CL and drugs
    tmp <- fastSubset_ccle(CL = input$DtoCL_ccle, drug = input$drugID_ccle)
    rv$rep_ccle <- sort(unique(tmp$COMBO_ID))
    rv$showRep <- 1:length(rv$rep_ccle)
  })
  output$rep_ccle <- renderUI(selectInput(inputId = "rep_ccle",
                                          label = "Select a replicate:",
                                          choices = rv$showRep))
  output$condRep_ccle <- reactive(length(rv$rep_ccle) > 0)
  outputOptions(output, "condRep_ccle", suspendWhenHidden = FALSE)
  
  # when "Select Replicate" is clicked
  observeEvent(input$repgo_ccle, {
    rv$combo <- rv$rep_ccle[as.numeric(input$rep_ccle)]
    tmp <- fastSubset_ccle(combo = rv$combo)[1,]
    rv$site <- "Screening Site: Broad Institute"
    rv$CLname <- tmp$CELL_LINE_NAME
    rv$Dname <- tmp$COMPOUND
    rv$scan <- tmp$ASSAY_PLATE_NAME
    rv$show_reps <- rv$rep_ccle
    rv$dset = "c"
  })
  
  # GDSC2
  # create drug drop-down menu
  output$searchD_gdsc2 <- renderUI({
    if(input$button_gdsc2 == "Drug ID"){
      rv$CLtoD_gdsc2 <- NULL
      rv$DtoCL_gdsc2 <- NULL
      rv$rep_gdsc2 <- NULL
      selectInput(inputId = "drugID_gdsc2",
                  label = "Select or enter a drug ID:",
                  choices = sort(shinyDrugScreen::ID_all$drugID_gdsc2))
    }
  })
  
  # output the condition for displaying full drug list
  output$condD_gdsc2 <- reactive(input$button_gdsc2 == "Drug ID")
  outputOptions(output, "condD_gdsc2", suspendWhenHidden = FALSE)
  
  # when "Find Cell Line IDs" is clicked
  observeEvent(input$dgo_gdsc2, {
    rv$DtoCL_gdsc2 <- sort(unique(fastSubset_gdsc2(drug = input$drugID_gdsc2)$COSMIC_ID))
    rv$CLtoD_gdsc2 <- NULL
    rv$rep_gdsc2 <- NULL
  })
  output$DtoCL_gdsc2 <- renderUI(selectInput(inputId = "DtoCL_gdsc2",
                                             label = "Select or enter a cell line ID:",
                                             choices = rv$DtoCL_gdsc2))
  output$condDtoCL_gdsc2 <- reactive(length(rv$DtoCL_gdsc2) > 0)
  outputOptions(output, "condDtoCL_gdsc2", suspendWhenHidden = FALSE)
  
  # when "Select Combination" is clicked
  observeEvent(input$combo2go_gdsc2, {
    # get the site, drugset, CL and drugs
    tmp <- fastSubset_gdsc2(CL = input$DtoCL_gdsc2, drug = input$drugID_gdsc2)
    rv$rep_gdsc2 <- sort(unique(tmp$COMBO_ID))
    rv$showRep <- 1:length(rv$rep_gdsc2)
  })
  output$rep_gdsc2 <- renderUI(selectInput(inputId = "rep_gdsc2",
                                           label = "Select a replicate:",
                                           choices = rv$showRep))
  output$condRep_gdsc2 <- reactive(length(rv$rep_gdsc2) > 0)
  outputOptions(output, "condRep_gdsc2", suspendWhenHidden = FALSE)
  
  # when "Select Replicate" is clicked
  observeEvent(input$repgo_gdsc2, {
    rv$combo <- rv$rep_gdsc2[as.numeric(input$rep_gdsc2)]
    tmp <- fastSubset_gdsc2(combo = rv$combo)[1,]
    rv$site <- "Screening Site: Wellcome Trust Sanger Institute"
    rv$CLname <- tmp$COSMIC_ID
    rv$Dname <- tmp$DRUG_ID
    rv$scan <- tmp$SCAN_ID
    rv$show_reps <- rv$rep_gdsc2
    rv$dset = "g2"
  })
  
  ###### MAIN PANEL ######
  
  # print out the site, the cell line ID, and the drug IDs
  output$sitename <- renderText(rv$site)
  output$CLname <- renderText(paste0("Cell Line: ", rv$CLname))
  output$CLname2 <- renderText(paste0("Cell Line ID: ", rv$CLname))
  output$scanID <- renderText(paste0("Scan ID: ", rv$scan))
  
  output$drugTable <- renderDataTable({
    if(rv$dset == "g1"){
      tmp <- subset(shinyDrugScreen::format_gdsc1, SCAN_ID == rv$scan)
      tmp <- tmp[,c(2,5,3,4)]
      names(tmp) <- c("Drug", "Row", "Start Column", "End Column")
      tmp <- tmp[order(tmp$Row),]
      tmp
    }
    else if(rv$dset == "c"){
      tmp <- subset(shinyDrugScreen::shiny_ccle, ASSAY_PLATE_NAME == rv$scan & WELL_TYPE == "SA" &
                      CONCENTRATION == 8, select = c(3,4,6))
      tmp$END <- tmp$ROW_ID + 14
      names(tmp) <- c("Drug","Column","Start Row","End Row")
      tmp <- tmp[order(tmp$Column),]
      tmp
    }
    else if(rv$dset == "g2"){
      tmp <- subset(shinyDrugScreen::format_gdsc2, SCAN_ID == rv$scan)
      if(tmp$MAX_ROW[1] == tmp$MIN_ROW[1]){
        tmp <- tmp[,c(2,4,7,5)]
        names(tmp) <- c("Drug", "Row", "Start Column", "End Column")
        tmp <- tmp[order(tmp$Row),]
      }
      else{
        tmp <- tmp[,c(2,5,6,4)]
        names(tmp) <- c("Drug","Column","Start Row","End Row")
        tmp <- tmp[order(tmp$Column),]
      }
      tmp
    }
  },
  rownames = FALSE, options = list(info = FALSE, lengthChange = FALSE))
  
  # create the heatmap, format plot, and DR plot
  output$heat <- renderPlot({
    if(rv$dset == "g1") plot.heat_gdsc(scan = rv$scan)
    else if(rv$dset == "c") plot.heat_ccle(scan = rv$scan)
    else if(rv$dset == "g2") plot.heat_gdsc2(scan = rv$scan)
  })
  output$format <- renderPlot({
    if(rv$dset == "g1") plot.form_gdsc(plate = rv$scan, combo = rv$combo)
    else if(rv$dset == "c") plot.form_ccle(plate = rv$scan, drug = rv$Dname)
    else if(rv$dset == "g2") plot.form_gdsc2(plate = rv$scan, combo = rv$combo)
  })
  output$DR <- renderPlot({
    if(rv$dset == "g1") plot.DR_gdsc(combo = rv$combo)
    else if(rv$dset == "c") plot.DR_ccle(combo = rv$combo)
    else if(rv$dset == "g2") plot.DR_gdsc2(combo = rv$combo)
  })
  
  output$allRep <- renderPlot({
    if(rv$dset == "g1") plotReps.DR_gdsc(combo = rv$show_reps)
    else if(rv$dset == "c") plotReps.DR_ccle(combo = rv$show_reps)
    else if(rv$dset == "g2") plotReps.DR_gdsc2(combo = rv$show_reps)
  })
  output$condPlotRep <- reactive(length(rv$show_reps) > 0)
  outputOptions(output, "condPlotRep", suspendWhenHidden = FALSE)
  
  # create the data tables
  output$clInfo <- renderDataTable({
    shinyDrugScreen::map_CL[,c("COSMIC ID","CCLE Name","GDSC Name","Site GDSC",
                               "Histology GDSC")]
  },
  rownames = FALSE, options = list(info = FALSE, lengthChange = FALSE))
  output$drugInfo <- renderDataTable({
    shinyDrugScreen::map_drug[,c("CCLE ID","CCLE Name 1","CCLE Name 2",
                                 "GDSC ID","GDSC Name","Target GDSC",
                                 "Target Pathway GDSC")]
  },
  rownames = FALSE, options = list(info = FALSE, lengthChange = FALSE))
}