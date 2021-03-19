ui <- fluidPage(theme = shinytheme("yeti"),
                withMathJax(),
                # app title
                titlePanel("Raw Drug Screening Data"),
                
                sidebarLayout(
                  sidebarPanel(width = 3,
                               # choose input type:
                               h4("Select data source: "),
                               wellPanel(
                                 tabsetPanel(
                                   # CCLE
                                   tabPanel(title = "CCLE",
                                            radioButtons(inputId = "button_ccle",
                                                         label = "Select initial input method:",
                                                         choices = c("Cell Line ID","Drug ID"),
                                                         inline = TRUE),
                                            # create drop-down of all CL IDs or all drug IDs:
                                            conditionalPanel(condition = "output.condCL_ccle",
                                                             uiOutput("searchCL_ccle"),
                                                             actionButton(inputId = "clgo_ccle",
                                                                          label = "Find Drug IDs")),
                                            conditionalPanel(condition = "output.condD_ccle",
                                                             uiOutput("searchD_ccle"),
                                                             actionButton(inputId = "dgo_ccle",
                                                                          label = "Find Cell Line IDs")),
                                            # create drop-down of corresponding drug IDs or CL IDs
                                            conditionalPanel(condition = "output.condCLtoD_ccle",
                                                             uiOutput("CLtoD_ccle"),
                                                             actionButton(inputId = "combo1go_ccle",
                                                                          label = "Select Combination")),
                                            conditionalPanel(condition = "output.condDtoCL_ccle",
                                                             uiOutput("DtoCL_ccle"),
                                                             actionButton(inputId = "combo2go_ccle",
                                                                          label = "Select Combination")),
                                            # create drop-down of all replicates
                                            conditionalPanel(condition = "output.condRep_ccle",
                                                             uiOutput("rep_ccle"),
                                                             actionButton(inputId = "repgo_ccle",
                                                                          label = "Select Replicate"))),
                                   # GDSC1
                                   tabPanel(title = "GDSC 1",
                                            radioButtons(inputId = "button",
                                                         label = "Select initial input method:",
                                                         choices = c("Cell Line ID","Drug ID"),
                                                         inline = TRUE),
                                            # create drop-down of all CL IDs or all drug IDs:
                                            conditionalPanel(condition = "output.condCL",
                                                             uiOutput("searchCL"),
                                                             actionButton(inputId = "clgo",
                                                                          label = "Find Drug IDs")),
                                            conditionalPanel(condition = "output.condD",
                                                             uiOutput("searchD"),
                                                             actionButton(inputId = "dgo",
                                                                          label = "Find Cell Line IDs")),
                                            # create drop-down of corresponding drug IDs or CL IDs
                                            conditionalPanel(condition = "output.condCLtoD",
                                                             uiOutput("CLtoD"),
                                                             actionButton(inputId = "combo1go",
                                                                          label = "Select Combination")),
                                            conditionalPanel(condition = "output.condDtoCL",
                                                             uiOutput("DtoCL"),
                                                             actionButton(inputId = "combo2go",
                                                                          label = "Select Combination")),
                                            # create drop-down of all replicates
                                            conditionalPanel(condition = "output.condRep_gdsc",
                                                             uiOutput("rep_gdsc"),
                                                             actionButton(inputId = "repgo",
                                                                          label = "Select Replicate"))),
                                   # GDSC2
                                   tabPanel(title = "GDSC 2",
                                            radioButtons(inputId = "button_gdsc2",
                                                         label = "Select initial input method:",
                                                         choices = c("Cell Line ID","Drug ID"),
                                                         inline = TRUE),
                                            # create drop-down of all CL IDs or all drug IDs:
                                            conditionalPanel(condition = "output.condCL_gdsc2",
                                                             uiOutput("searchCL_gdsc2"),
                                                             actionButton(inputId = "clgo_gdsc2",
                                                                          label = "Find Drug IDs")),
                                            conditionalPanel(condition = "output.condD_gdsc2",
                                                             uiOutput("searchD_gdsc2"),
                                                             actionButton(inputId = "dgo_gdsc2",
                                                                          label = "Find Cell Line IDs")),
                                            # create drop-down of corresponding drug IDs or CL IDs
                                            conditionalPanel(condition = "output.condCLtoD_gdsc2",
                                                             uiOutput("CLtoD_gdsc2"),
                                                             actionButton(inputId = "combo1go_gdsc2",
                                                                          label = "Select Combination")),
                                            conditionalPanel(condition = "output.condDtoCL_gdsc2",
                                                             uiOutput("DtoCL_gdsc2"),
                                                             actionButton(inputId = "combo2go_gdsc2",
                                                                          label = "Select Combination")),
                                            # create drop-down of all replicates
                                            conditionalPanel(condition = "output.condRep_gdsc2",
                                                             uiOutput("rep_gdsc2"),
                                                             actionButton(inputId = "repgo_gdsc2",
                                                                          label = "Select Replicate")))
                                 ))),
                  # OUTPUTS:
                  mainPanel(width = 9,
                            tabsetPanel(
                              tabPanel(title = "Visualization",
                                       # plot DR observations and heatmap:
                                       fluidRow(column(6, h3("Dose-Response Observations"), plotOutput("DR")),
                                                column(6, h3("Heatmap"), plotOutput("heat"))),
                                       # plot plate format:
                                       h3("Plate Format"),
                                       fluidRow(column(10, offset = 1,plotOutput("format"))),
                                       # give plate information:
                                       wellPanel(h3("Plate Information"),
                                                 fluidRow(column(10, h5(strong(textOutput("scanID"))),
                                                                 h5(strong(textOutput("CLname2"))),
                                                                 h5(strong(textOutput("sitename"))),
                                                                 h5(strong("Drugs:")),
                                                                 dataTableOutput("drugTable")))),
                                       # create DR plot of all replicates
                                       conditionalPanel(condition = "output.condPlotRep",
                                                        h3("All Replicates"),
                                                        fluidRow(column(10, plotOutput("allRep"))))),
                              tabPanel(title = "Cell Line Information",
                                       dataTableOutput("clInfo")),
                              tabPanel(title = "Drug Information",
                                       dataTableOutput("drugInfo"))
                            )))
)