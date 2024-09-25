library(shiny)
shinyUI(fluidPage(
  
  # Application title
  titlePanel("La Boize"),
  hr(),
  sidebarLayout(
    sidebarPanel(
      wellPanel( 
        conditionalPanel(condition ="input.confirmdatabutton==0" ,
          radioButtons("analysis","",c("new analysis","previous analysis"),inline=T),
          conditionalPanel( condition="input.analysis=='previous analysis' ",     
            fileInput("modelfile",label=h4("previous analysis"),accept=".RData")
          ), 
           conditionalPanel(condition="input.analysis=='new analysis' ",
            fluidRow(
              column(12,br(),radioButtons("filetype", "Extention of the file",c("csv" = "csv", "xlsx" = "xlsx"),inline = TRUE))
            ),
            fluidRow(
              column(12,conditionalPanel(condition ="input.help",
                helpText("Learning file is obligatory to continue")
              ),
                fileInput("learningfile", 
                          label = h4("learning File"),
                          accept =  c("text/csv",
                                      "application/vnd.ms-excel",
                                      "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
                                      ".xls",".xlsx"))
              )
              ,
              column(12,
                fileInput("validationfile", label = h4("validation File "),accept =  c("text/csv","application/vnd.ms-excel","application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",".xls",".xlsx")) 
              )
            ),
            fluidRow(
              conditionalPanel(condition ="input.filetype=='csv' ",column(6,textInput('dec', 'character for decimal point',value = "." ))),
              column(6,textInput("NAstring", label = "characters for missing values",value = "NA"))
            ),   
            fluidRow(
              conditionalPanel(condition ="input.filetype=='csv' ",
                radioButtons('sep', 'Separator',c(Comma=',',Semicolon=';',Tab='\t'),inline = TRUE )),
              conditionalPanel(condition ="input.filetype=='xlsx' ",
                column(6,numericInput("skipn",label = "number of lines to skip",value = 0)),
                column(6,numericInput("sheetn",label = "sheet",value = 1)))
            ),hr(),
              checkboxInput("transpose","Transpose the table",FALSE),
              checkboxInput("zeroegalNA","consider 0 as NA",FALSE)
          )
            ,
          actionButton("confirmdatabutton","Confirm data", 
                        style = "background-color: #63BFBF;
                                  color: white;
                                  border-color: #63BFBF;"),
          conditionalPanel(condition ="input.help",
             helpText("Data has to be confirm to continue"))
        ),
        conditionalPanel(condition ="input.confirmdatabutton!=0",
          h4("Learning data"),
          textOutput("namefilelearn",inline=T), tags$head(tags$style("#namefilelearn{color: grey;font-size: 15px;font-style: italic;}")),
          br(),
          textOutput("dim1learn",inline=T), "lines (individuals)",
          br(),
          textOutput("dim2learn",inline=T), "columns (variables)",
          br()
        ),
      conditionalPanel(condition ="input.confirmdatabutton!=0 & output.fileUploadedval",
        h4("Validation data"),
        textOutput("namefileval",inline=T), tags$head(tags$style("#namefileval{color: grey;font-size: 15px;font-style: italic;}")),
        br(),
        textOutput("dim1val",inline=T), "lines (individuals)",br(),
        textOutput("dim2val",inline=T), "columns (variables)",
        br()
      ), 
      conditionalPanel(condition ="input.confirmdatabutton!=0",
        hr(),
        fluidRow(
          column(3,checkboxInput("invers", "inverse" , value = FALSE)),
          column(8,
            p(textOutput("positif",inline=T),HTML( '&#x21D2;'), "case ",br(),
            textOutput("negatif",inline=T),HTML( '&#x21D2;'), "control",align="center")
          )
        ),
        hr(),
        radioButtons("paramdownplot","Download images as",choices=list("png"="png","jpg"="jpg","pdf"="pdf"),selected="png"),
        radioButtons("paramdowntable","Download datasets as",choices=list("csv"="csv","xlsx"="xlsx"),selected="csv"),
        hr(),
        downloadButton("savestate","Save settings RData",class = "dlButton"),
        hr(),
        downloadButton("savestatetable","Save settings table and main results",class = "dlButton")
        
      ),
        hr(),
        checkboxInput("help","show help",FALSE)
    ),width=3      
  ) ,
  
    mainPanel(
      conditionalPanel(condition ="!output.fileUploaded & !output.modelUploaded",
        h3("The purpose of this application is to provide a user-friendly tool to build a prediction model from omics datas.",
           align="center"),
  h4("Check the box 'show help' for any further informations."),br(),br(),br(),
       
       
        fluidRow(column(6,imageOutput("image1")),column(2,imageOutput("image2"))),
       br(),
      h4("This application is developped in the 12th team of I2MC for internal used.",align="center")

      ),           
      conditionalPanel(condition ="output.fileUploaded || output.modelUploaded",
        tabsetPanel(id = "data",              
          tabPanel("Learning Data",
            br(),
            conditionalPanel(condition ="input.help",
              fluidRow(
                column(5,br(),helpText("To verify if the import parameters are correct : the first column has to be the names of the individual, 
                                       the second the groups. Others are the datas."), 
                               helpText("Non attributes values have to appears empty, "),
                            helpText()
                ),
                column(7,imageOutput("image3",width = "100%")))
            ),
              dataTableOutput("JDDlearn")%>% withSpinner(color="#0dc5c1",type = 1),
              p(downloadButton("downloaddataJDDlearn","Download dataset"),align="center")
          ),
          tabPanel("Validation Data",
            conditionalPanel(condition ="output.fileUploadedval",
              br(),
              dataTableOutput("JDDval")%>% withSpinner(color="#0dc5c1",type = 1),
              p(downloadButton("downloaddataJDDval","Download dataset"),align="center")
            )
          ),

          tabPanel("Select Data", 
            conditionalPanel(condition ="input.help",
              helpText(" Select variables to extract variables from the learning dataset according to the number or the structure of Non-Attribute values (missing values)")
            ),  
            fluidRow(
              column(7, numericInput("prctvalues","Percentage minimum of values" , 0, min = 0, max = 100, step = 5),
                conditionalPanel(condition ="input.help",
                  helpText("")),br(),
                checkboxInput("NAstructure", "Select variables with a NA's structure " , value = FALSE),
                conditionalPanel(condition ="input.help",
                    helpText("The structure test is a proportion test of the Non Attributes values between the 2 groups."))
              ),
              column(5,radioButtons("selectmethod","Methods of selection ",c("selection on all samples"="nogroup","each group has more than x% of values "="bothgroups","at least one group has more than x% of more"="onegroup")),
                  conditionalPanel(condition ="input.help",helpText("3 ways of selection : select variables which got at least x% of values in all samples, "),
                    helpText("                select variables which which have more than x% in the two groups"),
                    helpText("                select variables which have at leat one group whith more than x% of values"))
              )
              
            ),p(downloadButton('downloaddataselect', 'Download data selected'),align="center"),
            hr(),
                  
            fluidRow(
              column(7,textOutput("nvarselect",inline=T), "selected variables" ,
                plotOutput("heatmapNA",width = "100%",height = 500)%>% withSpinner(color="#0dc5c1",type = 1) ,
                p(downloadButton("downloadplotheatmapNA","Download plot"),downloadButton('downloaddataheatmapNA', 'Download raw data'),align="center")
              ),
              column(5,br(),
                conditionalPanel(condition ="input.help",helpText("The 3 curves present the number of variables selected according to the three possible options and the % of Na's selected"))
                ,
                plotOutput("plotNA",width = "100%",height = 500)%>% withSpinner(color="#0dc5c1",type = 1),
                p(downloadButton("downloadplotNA","Download plot"),downloadButton('downloaddataplotNA', 'Download raw data'),align="center")
              )
            ),
            hr(),
            fluidRow(
              column(6,  
                conditionalPanel(condition ="input.NAstructure==true",
                  conditionalPanel(condition ="input.help",helpText("Consider the NA in the group with less values as real 0 (replace by 0) the NA in the group with more values are raplace by the solution chosen later")),
                  numericInput("thresholdNAstructure","pvalue for the structure test" , 0.05, min = 0, max = 1, step = 0.005))
              ),
            conditionalPanel(condition ="input.NAstructure==true",
              column(6,radioButtons("structdata", "search structure in : ",c("all dataset" = "alldata","selected dataset" = "selecteddata"))))
            ), 
            hr(),
            fluidRow(
              conditionalPanel(condition ="input.NAstructure==true", 
                column(9,textOutput("nstructuredfeatures",inline=T),"structured features",
                  plotOutput("heatmapNAstructure" ,width = "95%",height = 600)%>% withSpinner(color="#0dc5c1",type = 1),
                  p(downloadButton("downloadstructur","Download plot"),downloadButton('downloaddatastructur', 'Download raw data'),align="center")),
                column(3,br(),br(),
                  numericInput("maxvaluesgroupmin","The group with the minimum number of values has at most x% of values",value = 25,min = 0,max = 100,step = 5),
                  numericInput("minvaluesgroupmax","The group with the maximum number of values has at least y% of values",value = 75,min = 0,max = 100,step = 5))
              )
            )
          ),
          tabPanel("Transform Data",
                   conditionalPanel(condition ="input.help",
                                    helpText("")),
                   fluidRow(
                     column(5,radioButtons("rempNA", "Replacing NA (Not Attributes) by:",
                                           c("zero" = "z","mean of the cohort" = "moy",
                                             "mean by group"="moygr","PCA estimation" = "pca","Random forest estimation /!\\" = "missforest")),
                            helpText("/!\\ process can be long"),
                            
                            conditionalPanel(condition ="input.help",
                                             helpText("Random Forest can "))),
                     column(3,
                        checkboxInput("log","transform data in log",FALSE),
                        checkboxInput("standardization","standardization dataset",FALSE),
                        conditionalPanel(condition ="input.help",helpText("dividing the columns quadratic mean")),
                        checkboxInput("arcsin","arcsine transformation",FALSE),
                        conditionalPanel(condition ="input.help",helpText("each column is rescaled between 1 and 0, and arcsin transformation is applying"))
                    ),
                    column(4,
                    conditionalPanel(condition="input.log",
                                     radioButtons("logtype",label = NULL,c("ln"="logn","log 10"="log10","log2"="log2"),inline = TRUE)))
                  ),p(downloadButton('downloaddatatransform', 'Download transform data '),align="center"),
                   hr(),
                   
                   fluidRow(
                     column(5,plotOutput("plotheatmaptransformdata" ,width = "100%",height = 500)%>% withSpinner(color="#0dc5c1",type = 1),
                            p(downloadButton("downloadplotheatmap","Download plot"),
                              downloadButton('downloaddataheatmap', 'Download raw data'),align="center")),  
                     
                     column(7,conditionalPanel(condition ="input.help",
                                               helpText("The mds (MultiDimensionnal Scaling) calcul the distances between the individuals (rows) and represented it on a plan as well as possible."),
                                               helpText("The aim of this graphic is to vizualized if the selection and transform parameters separate well the 2 groups.")),
                            plotOutput("plotmds",height=500,width = "100%")%>% withSpinner(color="#0dc5c1",type = 1),
                      p(downloadButton("downloadplotmds","Download plot"),
                        downloadButton('downloaddatamds', 'Download raw data'),align="center"))),
                  plotOutput("plothist",height=500,width = "100%")%>% withSpinner(color="#0dc5c1",type = 1),
                  p(downloadButton("downloadplothist","Download plot"),
                    downloadButton('downloaddatahist', 'Download raw data'),align="center")
          ),
          tabPanel("Statistics",
            conditionalPanel(condition ="input.help",
                helpText("")),
            fluidRow(
              column(6,
                radioButtons("test", "Tests",c( "No test"="notest","Wilcoxon Test" = "Wtest","Student Test" = "Ttest")),
                conditionalPanel(condition ="input.help",
                  helpText("The test will select the differently expressed variables. The willcoxon test (Mann-Whitney-Willcoxon test) is a non parametric test. The student test is parametrics (the group has to be normally distribute and  the variance equal (or effective superior to 30))")),
                  checkboxInput("SFtest","Shapiro and Fisher Tests",F),
                conditionalPanel(condition ="input.help",helpText("The shapiro test is a test of normallity. The F test is a test of equality of variance."))
              ),
              column(6,br(),
                numericInput("thresholdFC","choise of the Fold change threshold" , 0, min =0, max = 5, step = 0.5),
                conditionalPanel(condition ="input.help",helpText("Fold Change is a mean of a groups divided by the mean of the other group. Mesure of the difference between the means of the 2 groups")),
                numericInput("thresholdpv","choise of the p-value threshold %" , 0.05, min =0, max = 1, step = 0.01),
                checkboxInput("adjustpv", "adjust p-value " , value = FALSE),
                conditionalPanel(condition ="input.help", helpText("Benjamini & Hochberg correction"))
              )
            ),br(),
            p(downloadButton('downloaddatastatistics', 'Download statistics'),downloadButton('downloadddatadiff', 'Download differently expressed variables'),align="center"),
            hr(),
            conditionalPanel(condition= "input.test== 'Wtest' || input.test== 'Ttest'",
              fluidRow(
                column(6,
                  textOutput("nvarselect2",inline=T), "selected variables",
                  plotOutput("volcanoplot" ,width = 500,height = 500)%>% withSpinner(color="#0dc5c1",type = 1),
                  p(downloadButton("downloadvolcanoplot","Download plot"),downloadButton('downloaddatavolcanoplot', 'Download raw data'),align="center")
                ),
                column(6,
                  textOutput("nbdiff",inline=T), "differently expressed",
                  plotOutput("barplottest" ,width = 400,height = 500)%>% withSpinner(color="#0dc5c1",type = 1),   
                  p(downloadButton("downloadbarplottest","Download plot"),downloadButton('downloaddatabarplottest', 'Download raw data'),align="center")
                )
              )
            )
            ,
            conditionalPanel(condition ="input.SFtest==true  ",
              column(6,conditionalPanel(condition ="input.help",
                helpText("Barplot presents the Results of shapiro and Fisher test")),
                plotOutput("plottestSF")%>% withSpinner(color="#0dc5c1",type = 1),
              p(downloadButton("downloadplottestSF","Download plot"),downloadButton('downloaddatatestSF', 'Download raw data'),align="center"))
              
            )
          )
          ,
          tabPanel("Model",
            fluidRow(
              column(4,
                radioButtons("model", "Type of model to adjust", 
                             c("No model" = "nomodel","Random Forest"="randomforest","Support Vector Machine" = "svm"))),
              column(6,
                numericInput("thresholdmodel","Threshold model" ,0, min = -1, max = 1, step = 0.05),
                conditionalPanel(condition ="input.help", helpText("The threshold of the score is used for the validation")),
                fluidRow(
                  column(7,checkboxInput("fs","features selection by cross validation /!\\ ",F))
                  # ,
                  # column(5,radioButtons("fstype",label=NULL,choice=c("Learning"="learn","Validation" = "val"),inline=T))
                  ),
                helpText("/!\\ process can be long")
              )
            ),
            conditionalPanel(condition ="output.fileUploadedval & input.model!='nomodel'  ",
              checkboxInput("adjustval","Adjust model on validation data",F)
            )
            ,
            hr(),
            conditionalPanel(condition ="input.model!='nomodel'  ",
              fluidRow(
                column(4,
                  textOutput('nbselectmodel',inline=T),'selected variables', 
                           
                  h3("model learning")
                ),
                column(4,br(),downloadButton('downloaddatalearning', 'Download learning data')),
                column(4,radioButtons("plotscoremodel", "",c( "boxplot"="boxplot","points" = "points")))
                ),
              fluidRow(
                column(6,
                  plotOutput("plotmodeldecouvroc")%>% withSpinner(color="#0dc5c1",type = 1),
                  p(downloadButton("downloadplotdecouvroc","Download plot"),
                    downloadButton('downloaddatadecouvroc', 'Download raw data'),align="center")
                ),
                column(4,
                  plotOutput("plotmodeldecouvbp")%>% withSpinner(color="#0dc5c1",type = 1),
                  p(downloadButton("downloadplotmodeldecouvbp","Download plot"),
                    downloadButton('downloaddatamodeldecouvbp', 'Download raw data'),align="center")
                ),
                column(2,
                  conditionalPanel(condition="input.plotscoremodel=='points'",checkboxInput("shownames1","show indivuals names",value=FALSE)),
                br(),
                tableOutput("tabmodeldecouv"),
                "Sensibility = ",textOutput("sensibilitydecouv",inline=T), 
                br(),
                "Specificity = ",textOutput("specificitydecouv",inline=T),
                br(),hr(),br(),
                tableOutput("youndendecouv")
                
              )
            ),
            hr(),
            conditionalPanel(condition ="input.adjustval==true  ",
              fluidRow(div(
                column(6,h3("model validation")), 
                column(6,br(),downloadButton('downloaddatavalidation', 'Download validation data')))
              ), 
              fluidRow(
                column(6,plotOutput("plotmodelvalroc")%>% withSpinner(color="#0dc5c1",type = 1),
                p(downloadButton("downloadplotvalroc","Download plot"),
                  downloadButton('downloaddatavalroc', 'Download raw data'),align="center")
                ),
                column(4,plotOutput("plotmodelvalbp")%>% withSpinner(color="#0dc5c1",type = 1),
                p(downloadButton("downloadplotmodelvalbp","Download plot"),
                  downloadButton('downloaddatamodelvalbp', 'Download raw data'),align="center")
                ),
                column(2,
                  #conditionalPanel(condition="input.plotscoremodel=='points'",checkboxInput("shownames2","show indivuals names",value=FALSE)),
                  tableOutput("tabmodelval"),
                  "Sensibility = ",textOutput("sensibilityval",inline=T), 
                  br(),
                  "Specificity = ",textOutput("specificityval",inline=T),
                  br(),hr(),br(),
                  tableOutput("youndenval")
                  
                )
              )
            )
          )),
          tabPanel("Details of the model", 
            h3("Summary of the model"),
            verbatimTextOutput("summarymodel"),
            plotOutput("plotimportance"),
            p(downloadButton("downloadplotimportance","Download plot"),
              downloadButton('downloaddataplotimportance', 'Download raw data'),align="center")
          ),
          tabPanel("Test parameters",
            fluidRow(
              column(6,
                h4("Selection Parameters"),
                sliderInput("prctvaluestest", "Percent of values accepted",min = 0, max = 100, value = c(50,90),width="60%"),
                checkboxGroupInput("selectmethodtest","Methods of selection ",c("selection on all samples"="nogroup","each group has more than x% of values "="bothgroups",
                                                                                "at least one group has more than x% of more"="onegroup"),selected ="bothgroups" )
              ),
              column(6,
                checkboxGroupInput("NAstructuretest", "Select variables with a NA's structure " , choices = list("TRUE /!\\"=TRUE,"FALSE"=FALSE),selected ="FALSE"),
                helpText("/!\\ process can be long"),
                conditionalPanel(condition ="output.testNAstructure ",
                  fluidRow(
                    column(6,
                      numericInput("thresholdNAstructuretest","pvalue for the structure test" , 0.05, min = 0, max = 1, step = 0.005),
                      radioButtons("structdatatest", "search structure in",c("all dataset" = "alldata","selected dataset" = "selecteddata"))
                    ),
                    column(6,
                      numericInput("maxvaluesgroupmintest","The group with the minimum number of values has at most x% of values",value = 25,min = 0,max = 100,step = 5),
                      numericInput("minvaluesgroupmaxtest","The group with the maximum number of values has at least y% of values",value = 75,min = 0,max = 100,step = 5)
                    )
                  )
                )
              )
            ),
            #textOutput("testNAstructure"),
            #hr(),
            fluidRow(
              column(6,h3("Transform Parameters")),
              column(6,h3("Statistics Parameters"))
            ),
            fluidRow(
              column(3,
                checkboxGroupInput("rempNAtest", "Replacing NA (Not Attributes) by",
                               c("zero" = "z","mean of the cohort" = "moy",
                                 "mean by group"="moygr","PCA estimation" = "pca","Random forest estimation /!\\" = "missforest"),selected = "moygr")
              ),
              column(3,
                #br(),br(),
                checkboxGroupInput("logtest","transform data in log",choices = list("TRUE"=TRUE,"FALSE"=FALSE),inline = TRUE,selected = "FALSE"),
                radioButtons("logtypetest",label = NULL,c("ln"="logn","log 10"="log10","log2"="log2"),inline = TRUE),
                checkboxGroupInput("standardizationtest","standardization dataset",choices = list("TRUE"=TRUE,"FALSE"=FALSE),inline = TRUE,selected = "FALSE"),
                checkboxGroupInput("arcsintest","arcsine transformation",choices = list("TRUE"=TRUE,"FALSE"=FALSE),inline = TRUE,selected ="FALSE")
              ),
            #),
            #hr(),
            #fluidRow(
              column(3,
                checkboxGroupInput("testtest", "Tests",c( "No test"="notest","Wilcoxon Test" = "Wtest","Student Test" = "Ttest"),selected = "Wtest"),
                checkboxGroupInput("adjustpvtest", "adjust p-value " , choices = list("TRUE"=TRUE,"FALSE"=FALSE),inline = TRUE,selected = "FALSE")
              ),
              column(3,
                numericInput("thresholdFCtest","choise of the Fold change threshold" , 0, min =0, max = 5, step = 0.5),
                numericInput("thresholdpvtest","choise of the p-value threshold %" , 0.05, min =0, max = 1, step = 0.01)
              )
            ),
            #hr(),
            h3("Model Parameters"),
            fluidRow(
              column(3,
                checkboxGroupInput("modeltest", "Type of model to adjust", c("No model" = "nomodel","Random Forest"="randomforest","Support Vector Machine" = "svm"),selected = "svm")
              ),
              column(4,
                #numericInput("thresholdmodeltest","threshold model" ,0, min = -1, max = 1, step = 0.05),
                checkboxGroupInput("fstest","features selection by cross validation",choices = list("TRUE /!\\"=TRUE,"FALSE"=FALSE),inline = TRUE,selected ="FALSE"),
                helpText("/!\\ process can be long")
              ),
              column(5,
                p(actionButton("tunetest",h4("Test all models"),width=200),align="center")
              )
            ),
            dataTableOutput("tabtestparameters")%>% withSpinner(color="#0dc5c1",type = 1),
            p(downloadButton("downloadtabtestparameters","Download dataset"),align="center")
          )
        )
      )
    )
  )
))
