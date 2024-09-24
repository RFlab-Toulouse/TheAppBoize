options(shiny.maxRequestSize=60*1024^2) 
source("global.R")
#options(xtable.include.colnames=T)
#options(xtable.include.rownames=T)

shinyServer(function(input, output,session) {
  
  output$modelUploaded <- reactive({
    return(!is.null(input$modelfile))
  })
  outputOptions(output, 'modelUploaded', suspendWhenHidden=FALSE)
  
  output$fileUploaded <- reactive({
    return(!is.null(input$learningfile))
  })
  outputOptions(output, 'fileUploaded', suspendWhenHidden=FALSE)
  
  output$image1<-renderImage({return (list(src="pictures/Logo I2MC.jpg", 
                                           contentType="image/jpeg",
                                           width=300,
                                           height=200,
                                           alt="I2MC logo"))},deleteFile = F)
  output$image2<-renderImage({return (list(src="pictures/rflabxx.png", 
                                           contentType="image/png",
                                           width=600,
                                           height=200,
                                           alt="RFlab logo"))},deleteFile = F)
  output$image3<-renderImage({return (list(src="pictures/structurdata2.jpg", 
                                           contentType="image/jpeg",
                                           width=600,
                                           height=300,
                                           alt="structure data"))},deleteFile = F)

  output$fileUploadedval <- reactive({
    return( !is.null(DATA()$VALIDATION))
  })
  outputOptions(output, 'fileUploadedval', suspendWhenHidden=FALSE)
  
  output$modelUploadedval <- reactive({
    return(!is.null(DATA()$VALIDATION))
  })
  outputOptions(output, 'modelUploadedval', suspendWhenHidden=FALSE)
  
#Save state#############  
  state <- reactiveValues()
  observe({
    importparameters<<-list("learningfile"=input$learningfile,"validationfile"=input$validationfile,"modelfile"=input$modelfile,"extension" = input$filetype,
                            "NAstring"=input$NAstring,"sheetn"=input$sheetn,"skipn"=input$skipn,"dec"=input$dec,"sep"=input$sep,
                            "transpose"=input$transpose,"zeroegalNA"=input$zeroegalNA,confirmdatabutton=input$confirmdatabutton)
    
    selectdataparameters<<-list("prctvalues"=input$prctvalues,"selectmethod"=input$selectmethod,"NAstructure"=input$NAstructure,"structdata"=input$structdata,
                                "thresholdNAstructure"=input$thresholdNAstructure,"maxvaluesgroupmin"=input$maxvaluesgroupmin,"minvaluesgroupmax"=input$minvaluesgroupmax)
    
    transformdataparameters<<-list("log"=input$log,"logtype"=input$logtype,"standardization"=input$standardization,"arcsin"=input$arcsin,"rempNA"=input$rempNA)
    
    
    testparameters<<-list("SFtest"=input$SFtest,"test"=input$test,"adjustpv"=input$adjustpv,"thresholdpv"=input$thresholdpv,"thresholdFC"=input$thresholdFC)
    
    modelparameters<<-list("modeltype"=input$model,"invers"=input$invers,"thresholdmodel"=input$thresholdmodel,
                           "fs"=input$fs,"adjustval"=input$adjustval)
    parameters<-list("importparameters"=importparameters,"selectdataparameters"=selectdataparameters,
                     "transformdataparameters"=transformdataparameters,"testparameters"=testparameters,"modelparameters"=modelparameters)
    data<-DATA()
    selectdata<-SELECTDATA()
    transformdata<-TRANSFORMDATA()
    test<-TEST()
    model<-MODEL()
    settingstable<-statetable()
    isolate(state<<-list("parameters"=parameters,"data"=data,"selectdata"=selectdata,"transformdata"=transformdata,"test"=test,"model"=model,"settingstable"=settingstable)) 
  })
  
  output$savestate <- downloadHandler(
    filename <- function(){
      paste("model.RData")
    },
    content = function(file) { 
      save(state, file = file)
    }
  )
  observe({
    if(input$confirmdatabutton!=0 & !is.null(input$modelfile)){
      print("update")
      dataaaa<<-DATA()
      updateNumericInput(session, "prctvalues", value = DATA()$previousparameters$selectdataparameters$prctvalues)
      updateRadioButtons(session,"selectmethod",selected =  DATA()$previousparameters$selectdataparameters$selectmethod)
      updateCheckboxInput(session ,"NAstructure",value=DATA()$previousparameters$selectdataparameters$NAstructure)
      updateRadioButtons(session,"structdata",selected=DATA()$previousparametersselectdataparameters$parameters$structdata)
      updateNumericInput(session, "maxvaluesgroupmin", value = DATA()$previousparametersselectdataparameters$parameters$maxvaluesgroupmin)
      updateNumericInput(session, "minvaluesgroupmax", value = DATA()$previousparametersselectdataparameters$parameters$minvaluesgroupmax)
      updateNumericInput(session, "thresholdNAstructure", value = DATA()$previousparameters$selectdataparameters$thresholdNAstructure)
      
      updateRadioButtons(session,"rempNA",selected=DATA()$previousparameters$transformdataparameters$rempNA)
      updateCheckboxInput(session ,"log",value=DATA()$previousparameters$transformdataparameters$log)
      updateRadioButtons(session ,"logtype",selected=DATA()$previousparameters$transformdataparameters$logtype)
      updateCheckboxInput(session ,"standardization",value=DATA()$previousparameters$transformdataparameters$standardization)
      updateCheckboxInput(session ,"arcsin",value=DATA()$previousparameters$transformdataparameters$arcsin)
      
      updateRadioButtons(session,"test",selected=DATA()$previousparameters$testparameters$test)
      updateNumericInput(session, "thresholdFC", value = DATA()$previousparameterstestparameters$parameters$thresholdFC)
      updateNumericInput(session, "thresholdpv", value = DATA()$previousparameterstestparameters$parameters$thresholdpv)
      updateCheckboxInput(session ,"adjustpval",value=DATA()$previousparameterstestparameters$parameters$adjustpval)
      updateCheckboxInput(session ,"SFtest",value=DATA()$previousparameterstestparameters$parameters$SFtest)
      
      updateRadioButtons(session,"model",selected=DATA()$previousparameters$modelparameters$modeltype)
      updateNumericInput(session, "thresholdmodel", value = DATA()$previousparameters$modelparameters$thresholdmodel)
      updateCheckboxInput(session ,"fs",value=DATA()$previousparameters$modelparameters$fs)

      updateCheckboxInput(session ,"adjustval",value=DATA()$previousparameters$modelparameters$adjustval)
      updateCheckboxInput(session ,"invers",value=DATA()$previousparameters$modelparameters$invers)
      
    }
  })
  
  statetable<-reactive({
    table<-matrix(data = "",nrow = 20,ncol=11)
    if((input$confirmdatabutton!=0 & !is.null(input$modelfile))){
      learningfile<-DATA()$previousparameters$importparameters$learningfile
    }
    else{learningfile<-input$learningfile}

    table[1,1:9]<-c("#","Extensionfile","decimal character","separator character","NA string","sheet number","skip lines","consider NA as 0","transpose")
    table[2,1:9]<-c("import parameters",learningfile$type,input$dec,input$sep,input$NAstring,
                         input$sheetn,input$skipn,input$zeroegalNA,input$transpose)

    table[3,]<-c("#","name learning file", "number of rows", "number of columns", paste("number of ",levels(DATA()$LEARNING[,1])[1]),
             paste("number of ",levels(DATA()$LEARNING[,1])[2]),"name validation file", "number of rows", "number of columns", paste("number of ",levels(DATA()$VALIDATION[,1])[1]),
             paste("number of ",levels(DATA()$VALIDATION[,1])[2]))
    table[4,]<-c("main results",learningfile$name,dim(DATA()$LEARNING)[1],dim(DATA()$LEARNING)[2],nll(sum(DATA()$LEARNING[,1]==levels(DATA()$LEARNING[,1])[1])),
                 nll(sum(DATA()$LEARNING[,1]==levels(DATA()$LEARNING[,1])[2])),nll(input$validationfile$name),nll(dim(DATA()$VALIDATION)[1]),
                 nll(dim(DATA()$VALIDATION)[2]),nll(sum(DATA()$VALIDATION[,1]==levels(DATA()$VALIDATION[,1])[1])),
                 nll(sum(DATA()$VALIDATION[,1]==levels(DATA()$VALIDATION[,1])[2])))
    table[5,1:8]<-c("#","percentage of values minimum","method of selection","select features structured","search structur in",
                     "threshold p-value of proportion test", "maximum % values of the min group","minimum % values of the max group")
    table[6,1:8]<-c("select parameters",selectdataparameters[[1]],selectdataparameters[[2]],selectdataparameters[[3]],
                    selectdataparameters[[4]],selectdataparameters[[5]],selectdataparameters[[6]],selectdataparameters[[7]])
    table[7,1:3]<-c("#","number of feature selected","number of feature structured")
    table[8,1:3]<-c("main results",dim(SELECTDATA()$LEARNINGSELECT)[2]-1,nll(dim(SELECTDATA()$STRUCTUREDFEATURES)[2]))
    table[9,1:5]<-c("#","remplace NA by","transformation log","strandardisation","arcsin transformation")
    if(transformdataparameters[[1]]=="FALSE"){logprint<-"FALSE"}
    else{logprint<-transformdataparameters[[2]]}
    table[10,1:5]<-c("transform parameters",transformdataparameters[[5]],logprint,transformdataparameters[[3]],transformdataparameters[[4]])
    table[11,1]<-c("#")
    table[12,1]<-c("main results")
    table[13,1:5]<-c("#","test","use Bonferroni adjustment","threshold of significativity","Fold change threshold")
    table[14,1:5]<-c("test parameters",input$test,input$adjustpv,input$thresholdpv,input$thresholdFC)
    table[15,1:2]<-c("#","number of differently expressed features")
    table[16,1:2]<-c("main results",dim(TEST()$LEARNINGDIFF)[2]-1)

    if(input$model!="nomodel"){
      table[17,1:6]<-c("#","model type","cut-off of the model","feature selection","apply model on validation","invers groups")
      table[18,1:6]<-c("model parameters",input$model,input$thresholdmodel,input$fs,input$adjustval,input$invers)
      table[19,1:8]<-c("#","number of features","AUC learning","sensibility learning","specificity learning","AUC validation","sensibility validation","specificity validation")
#       line20<<-c("main results",dim(MODEL()$DATALEARNINGMODEL$learningmodel)[2]-1,
#                  as.numeric(auc(roc(MODEL()$DATALEARNINGMODEL$reslearningmodel$classlearning,MODEL()$DATALEARNINGMODEL$reslearningmodel$scorelearning))),
#                  sensibility(MODEL()$DATALEARNINGMODEL$reslearningmodel$predictclasslearning,MODEL()$DATALEARNINGMODEL$reslearningmodel$classlearning),
#                  specificity(MODEL()$DATALEARNINGMODEL$reslearningmodel$predictclasslearning,MODEL()$DATALEARNINGMODEL$reslearningmodel$classlearning),
#                  as.numeric(auc(roc(MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$classval,MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$scoreval))),
#                  sensibility(MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$classval,MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$predictclassval),
#                  specificity(MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$classval,MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$predictclassval)
#       )
      table[20,1:5]<-c("main results",dim(MODEL()$DATALEARNINGMODEL$learningmodel)[2]-1,
                  round(as.numeric(auc(roc(MODEL()$DATALEARNINGMODEL$reslearningmodel$classlearning,MODEL()$DATALEARNINGMODEL$reslearningmodel$scorelearning))),digits = 3),
                  sensibility(MODEL()$DATALEARNINGMODEL$reslearningmodel$predictclasslearning,MODEL()$DATALEARNINGMODEL$reslearningmodel$classlearning),
                  specificity(MODEL()$DATALEARNINGMODEL$reslearningmodel$predictclasslearning,MODEL()$DATALEARNINGMODEL$reslearningmodel$classlearning)
                  )
      if(input$adjustval){
      table[20,6:8]<-c(round(as.numeric(auc(roc(MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$classval,MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$scoreval))),digits = 3),
                  sensibility(MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$predictclassval,MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$classval),
                  specificity(MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$predictclassval,MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$classval)
                  )
      }
    }
    return(table)
    
  }) 
  
  output$savestatetable<- downloadHandler(
    filename = function() { paste('settingstable', '.',input$paramdowntable, sep='') },
    content = function(file) {
      downloaddataset(   statetable(), file,cnames=F,rnames=F) })
  
############
  output$namefilelearn<-renderText({
    namelearn<-input$learningfile$name
  })
  output$dim1learn<-renderText({
    di1<-dim(x = DATA()$LEARNING)[1]  
  })
  output$dim2learn<-renderText({
    di2<-dim(x = DATA()$LEARNING)[2]-1  
  })
  output$namefileval<-renderText({
    nameval<-input$validationfile$name
  })  
  output$dim1val<-renderText({
    di1<-dim(x = DATA()$VALIDATION)[1]  
  })
  output$dim2val<-renderText({
    di2<-dim(x = DATA()$VALIDATION)[2]  
  })  

  #si erreur envoyÃÂÃÂ© pb import
  DATA<-reactive({
     importparameters<<-list("learningfile"=input$learningfile,"validationfile"=input$validationfile,"modelfile"=input$modelfile,"extension" = input$filetype,
                            "NAstring"=input$NAstring,"sheetn"=input$sheetn,"skipn"=input$skipn,"dec"=input$dec,"sep"=input$sep,
                            "transpose"=input$transpose,"zeroegalNA"=input$zeroegalNA,confirmdatabutton=input$confirmdatabutton,invers=input$invers)

     out<-tryCatch(importfunction(importparameters),error=function(e) e )
#      if(any(class(out)=="error"))print("error")
#      else{resimport<-out}
     validate(need(any(class(out)!="error"),"error import"))
     resimport<<-out
      #resimport<-importfunction(importparameters)
    list(LEARNING=resimport$learning, 
         VALIDATION=resimport$validation,
        previousparameters=resimport$previousparameters  
#          LEVELS=resimport$lev
         )
  })
  
  output$JDDlearn=renderDataTable({
    learning<-DATA()$LEARNING
    validate(need(!is.null(learning),"problem import"))
    colmin<-min(ncol(learning),100)
    rowmin<-min(nrow(learning),100)
    cbind(Names=rownames(learning[1:rowmin,1:colmin]),learning[1:rowmin,1:colmin])},
    options = list(    "orderClasses" = F,
                       "responsive" = F,
                       "pageLength" = 10))
  
  output$downloaddataJDDlearn <- downloadHandler(
    filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
    content = function(file) {
      downloaddataset(   DATA()$LEARNING, file) })
  
  
  output$JDDval=renderDataTable({
    validation<-DATA()$VALIDATION
    validate(need(!is.null(validation),"problem import"))
    colmin<-min(ncol(validation),100)
    rowmin<-min(nrow(validation),100)
    cbind(Names=rownames(validation[1:rowmin,1:colmin]),validation[1:rowmin,1:colmin])},
    options = list(    "orderClasses" = F,
                       "responsive" = F,
                       "pageLength" = 10)) 
  
  output$downloaddataJDDval <- downloadHandler(
    filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
    content = function(file) {
      downloaddataset(   DATA()$VALIDATION, file) })


#################
SELECTDATA<-reactive({
  selectdataparameters<<-list("prctvalues"=input$prctvalues,"selectmethod"=input$selectmethod,"NAstructure"=input$NAstructure,"structdata"=input$structdata,
                              "thresholdNAstructure"=input$thresholdNAstructure,"maxvaluesgroupmin"=input$maxvaluesgroupmin,"minvaluesgroupmax"=input$minvaluesgroupmax)
  validate(need(selectdataparameters$prctvalues>=0 &selectdataparameters$prctvalues<=100,"%  NA has to be between 0 and 100"))
  validate(need(input$minvaluesgroupmax>=0 &input$minvaluesgroupmax<=100 & input$maxvaluesgroupmin>=0 &input$maxvaluesgroupmin<=100,"% threshold has to be between 0 and 100"),
           need(input$thresholdNAstructure>0,input$thresholdNAstructure<1,"threshold of the pvalue has to be between 0 and 1"))
  learning<<-DATA()$LEARNING
  validate(need(input$confirmdatabutton!=0,"Importation of datas has to be confirmed"))
  
  validate(need(length(levels(learning[,1]))==2,"number of groups is not equal to 2"))
  resselectdata<<-selectdatafunction(learning = learning,selectdataparameters = selectdataparameters)
  list(LEARNINGSELECT=resselectdata$learningselect,STRUCTUREDFEATURES=resselectdata$structuredfeatures,DATASTRUCTUREDFEATURES=resselectdata$datastructuredfeatures,selectdataparameters)
})
#####
#Selection Output
#####
  output$downloaddataselect<- downloadHandler(
    filename = function() { paste('Dataselect', '.',input$paramdowntable, sep='') },
    content = function(file) {
      downloaddataset(SELECTDATA()$LEARNINGSELECT, file)
    }
  )
  
output$nvarselect=renderText({
    di1<-dim(x = SELECTDATA()$LEARNINGSELECT)[2]-1  
  })
  
output$heatmapNA<-renderPlot({
  learningselect<-SELECTDATA()$LEARNINGSELECT
  heatmapNA(toto =learningselect)
})
output$downloadplotheatmapNA = downloadHandler(
  filename = function() { 
    paste('graph','.',input$paramdownplot, sep='') 
  },
  content = function(file) {
    ggsave(file, plot =    heatmapNA(toto =SELECTDATA()$LEARNINGSELECT), 
           device = input$paramdownplot)
  },
  contentType=NA)

output$downloaddataheatmapNA <- downloadHandler(
  filename = function() { paste('dataset distribution of NA', '.',input$paramdowntable, sep='') },
  content = function(file) {
    downloaddataset(as.data.frame(heatmapNA(toto =SELECTDATA()$LEARNINGSELECT,graph = F)), file)
  }
)

# observe({
#   req(heatmapNA(toto =SELECTDATA()$LEARNINGSELECT,graph = F))
#   print(class(heatmapNA(toto =SELECTDATA()$LEARNINGSELECT,graph = F)))
# })

output$plotNA<-renderPlot({
  learningselect<-SELECTDATA()$LEARNINGSELECT
  learning<-DATA()$LEARNING
  distributionvalues(toto = learning,prctvaluesselect =input$prctvalues/100,nvar = ncol(learningselect) ,ggplot =  T)  
})


output$downloadplotNA = downloadHandler(
  filename = function() { 
    paste('graph','.',input$paramdownplot, sep='') 
  },
  content = function(file) {
    ggsave(file, plot =         distributionvalues(toto = DATA()$LEARNING,prctvaluesselect =input$prctvalues/100,nvar = ncol(SELECTDATA()$LEARNINGSELECT) ,ggplot =  T), 
           device = input$paramdownplot)},contentType=NA)

output$downloaddataplotNA <- downloadHandler( 
  filename = function() {
    paste('dataset', '.',input$paramdowntable, sep='') 
    },
  content = function(file) {
    downloaddataset(distributionvalues(toto = DATA()$LEARNING,prctvaluesselect =input$prctvalues/100,nvar = ncol(SELECTDATA()$LEARNINGSELECT) ,ggplot =  T,graph = F)  , file)
  }
)

output$nstructuredfeatures<-renderText({
  ncol(SELECTDATA()$STRUCTUREDFEATURES)
})
output$heatmapNAstructure<-renderPlot({
  group<<-DATA()$LEARNING[,1]
  structuredfeatures<<-SELECTDATA()$STRUCTUREDFEATURES
  heatmapNA(toto=cbind(group,structuredfeatures))            
  #else{errorplot(text = " No NA's structure")}
})
  
output$downloadstructur = downloadHandler(
  filename = function() { 
    paste('graph','.',input$paramdownplot, sep='') 
  },
  content = function(file) {
    ggsave(file, plot = heatmapNA(cbind(DATA()$LEARNING[,1],SELECTDATA()$STRUCTUREDFEATURES)), 
           device = input$paramdownplot)
  },
  contentType=NA)

output$downloaddatastructur <- downloadHandler( 
  filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
  content = function(file) {
    downloaddataset(SELECTDATA()$STRUCTUREDFEATURES, file)
  }
) 
  
#####  
TRANSFORMDATA<-reactive({
  learningselect<<-SELECTDATA()$LEARNINGSELECT
  structuredfeatures<<-SELECTDATA()$STRUCTUREDFEATURES
  datastructuresfeatures<<-SELECTDATA()$DATASTRUCTUREDFEATURES
  transformdataparameters<<-list("log"=input$log,"logtype"=input$logtype,"standardization"=input$standardization,"arcsin"=input$arcsin,"rempNA"=input$rempNA)
  validate(need(ncol(learningselect)>0,"No select dataset"))
  if(transformdataparameters$rempNA%in%c("pca","missforest")){
    validate(need(min(apply(X = learningselect,MARGIN = 2,FUN = function(x){sum(!is.na(x))}))>1,"not enough data for pca estimation"))
  } 
  learningtransform<-transformdatafunction(learningselect = learningselect,structuredfeatures = structuredfeatures,
                                      datastructuresfeatures =   datastructuresfeatures,transformdataparameters = transformdataparameters)

  list(LEARNINGTRANSFORM=learningtransform,transformdataparameters=transformdataparameters)
})

##
output$downloaddatatransform<- downloadHandler(
  filename = function() { paste('Transformdata', '.',input$paramdowntable, sep='') },
  content = function(file) {
    downloaddataset(TRANSFORMDATA()$LEARNINGTRANSFORM, file)
  }
)

output$plotheatmaptransformdata<-renderPlot({
  learningtransform<-TRANSFORMDATA()$LEARNINGTRANSFORM
  heatmapplot(toto =learningtransform,ggplot = T,scale=F)
})

output$downloadplotheatmap = downloadHandler(
  filename = function() { 
    paste0('graph','.',input$paramdownplot, sep='') 
  },
  content = function(file) {
    ggsave(file, plot =    heatmapplot(toto =TRANSFORMDATA()$LEARNINGTRANSFORM,ggplot = T,scale=F), 
           device = input$paramdownplot)},
  contentType=NA)

output$downloaddataheatmap <- downloadHandler(
  filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
  content = function(file) {
    downloaddataset(as.data.frame(heatmapplot(toto =TRANSFORMDATA()$LEARNINGTRANSFORM,ggplot = T,scale=F,graph=F)), file)
  })

output$plotmds<-renderPlot({
  learningtransform<-TRANSFORMDATA()$LEARNINGTRANSFORM
  mdsplot(toto = learningtransform,ggplot=T)
})
output$downloadplotmds = downloadHandler(
  filename = function() { 
    paste('graph','.',input$paramdownplot, sep='')},
  content = function(file) {
    ggsave(file, plot =        mdsplot(toto = TRANSFORMDATA()$LEARNINGTRANSFORM,ggplot=T),  device = input$paramdownplot)},
  contentType=NA)

output$downloaddatamds <- downloadHandler(
  filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
  content = function(file) {
    downloaddataset(    mdsplot(toto = TRANSFORMDATA()$LEARNINGTRANSFORM,ggplot=T,graph=F), file)
  })


output$plothist<-renderPlot({
  learningtransform<-TRANSFORMDATA()$LEARNINGTRANSFORM
  histplot(toto=learningtransform)
})
output$downloadplothist = downloadHandler(
  filename = function() { 
    paste('graph','.',input$paramdownplot, sep='')},
  content = function(file) {
    ggsave(file, plot =         histplot(toto=TRANSFORMDATA()$LEARNINGTRANSFORM),  device = input$paramdownplot)},
  contentType=NA)

output$downloaddatahist <- downloadHandler(
  filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
  content = function(file) {
    downloaddataset(histplot(toto=TRANSFORMDATA()$LEARNINGTRANSFORM,graph=F), file)
  })

#########
TEST<-reactive({
  testparameters<<-list("SFtest"=input$SFtest,"test"=input$test,"adjustpval"=input$adjustpv,"thresholdpv"=input$thresholdpv,
                        "thresholdFC"=input$thresholdFC,"invers"=input$invers)
  learningtransform<<-TRANSFORMDATA()$LEARNINGTRANSFORM
  restest<<-testfunction(tabtransform = learningtransform,testparameters = testparameters )
  validate(need(testparameters$thresholdFC>=0,"threshold Foldchange has to be positive"))
  validate(need(testparameters$thresholdpv>=0 &testparameters$thresholdpv<=1,"p-value has to be between 0 and 1"))
  
  list(LEARNINGDIFF=restest$tabdiff,DATATEST=restest$datatest,HYPOTHESISTEST=restest$hypothesistest,#GROUP=restest$group,
       USEDDATA=restest$useddata,testparameters=restest$testparameters)

})
##
output$downloadddatadiff<- downloadHandler(
  filename = function() { paste('Datadiff', '.',input$paramdowntable, sep='') },
  content = function(file) {
    downloaddataset(TEST()$LEARNINGDIFF, file)
  }
)
output$downloaddatastatistics<- downloadHandler(
  filename = function() { paste('Datastatistics', '.',input$paramdowntable, sep='') },
  content = function(file) {
    downloaddataset(TEST()$DATATEST, file)
  }
)
output$positif<-renderText({
  res<-levels(DATA()$LEARNING[,1])[1]
})
output$negatif<-renderText({
  res<-levels(DATA()$LEARNING[,1])[2]
})
output$volcanoplot <- renderPlot({
  datatest<<-TEST()$DATATEST
  useddata<<-TEST()$USEDDATA
  colnames(useddata)[3]<-colnames(datatest)[5]
  volcanoplot(logFC =useddata[,3],pval = useddata$pval,thresholdFC = input$thresholdFC,thresholdpv = (input$thresholdpv ),completedata=useddata[,1:3] )
})
output$downloadvolcanoplot = downloadHandler(
  filename = function() {paste('graph','.',input$paramdownplot, sep='')},
  content = function(file) {
    ggsave(file, plot = volcanoplot(logFC =TEST()$USEDDATA$logFC,pval = TEST()$USEDDATA$pval,thresholdFC = input$thresholdFC,
                                    thresholdpv = input$thresholdpv ,completedata=TEST()$DATATEST ) ,  device = input$paramdownplot)},
  contentType=NA)
output$downloaddatavolcanoplot<- downloadHandler(
  filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
  content = function(file) {
    downloaddataset(volcanoplot(logFC =TEST()$USEDDATA$logFC,pval = TEST()$USEDDATA$pval,thresholdFC = input$thresholdFC,
                                    thresholdpv = (input$thresholdpv ),completedata=TEST()$DATATEST,graph=F ), file) })
output$nvarselect2<-renderText({
  di1<-dim(x = SELECTDATA()$LEARNINGSELECT)[2]-1  
})  
output$nbdiff<-renderText({
  nbdiff = positive(ncol(TEST()$LEARNINGDIFF)-1)
})


output$barplottest <- renderPlot({
  learningdiff<<-TEST()$LEARNINGDIFF
  useddata<<-TEST()$USEDDATA
  if(nrow(learningdiff)!=0){barplottest(feature=useddata$names,logFC=useddata$logFC,levels=levels(learningdiff[,1]),pval=useddata$pval,mean1=useddata$mean1,mean2=useddata$mean2,thresholdpv=input$thresholdpv,
                                             thresholdFC=input$thresholdFC,graph=T,maintitle="Mean by group for differentially expressed variables")
}
  else{errorplot(text = " No differently expressed ")}
  
})
output$downloadbarplottest = downloadHandler(
  filename = function() {paste('graph','.',input$paramdownplot, sep='')},
  content = function(file) {
    ggsave(file, plot = barplottest(feature=TEST()$USEDDATA$names,logFC=TEST()$USEDDATA$logFC,levels=levels(TEST()$LEARNINGDIFF[,1]),pval=TEST()$USEDDATA$pval,mean1=TEST()$USEDDATA$mean1,mean2=TEST()$USEDDATA$mean2,thresholdpv=input$thresholdpv,
                                    thresholdFC=input$thresholdFC,graph=T,maintitle="Mean by group for differentially expressed variables"),  device = input$paramdownplot)},
  contentType=NA)
output$downloaddatabarplottest <- downloadHandler(
  filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
  content = function(file) {
    downloaddataset(barplottest(feature=TEST()$USEDDATA$names,logFC=TEST()$USEDDATA$logFC,levels=levels(TEST()$LEARNINGDIFF[,1]),pval=TEST()$USEDDATA$pval,mean1=TEST()$USEDDATA$mean1,mean2=TEST()$USEDDATA$mean2,thresholdpv=input$thresholdpv,
                                thresholdFC=input$thresholdFC,maintitle="Mean by group for differentially expressed variables",graph=F), file) })

# output$dataconditiontest=renderDataTable({
#   hypothesistest<-TEST()$hypothesistest},options = list("orderClasses" = F,
#                                                         "responsive" = F,
#                                                         "pageLength" = 10))
output$plottestSF=renderPlot({
  hypothesistest<-TEST()$HYPOTHESISTEST   
  barplottestSF(hypothesistest)
})
output$downloadplottestSF = downloadHandler(
  filename = function() {paste('graph','.',input$paramdownplot, sep='')},
  content = function(file) {
    ggsave(file, plot =   barplottestSF(TEST()$HYPOTHESISTEST  ),  device = input$paramdownplot)},
  contentType=NA)
output$downloaddatatestSF <- downloadHandler(
  filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
  content = function(file) {
    downloaddataset(  barplottestSF(TEST()$HYPOTHESISTEST ,graph=F), file) })



######
MODEL<-reactive({
  if(input$test=="notest"){learningmodel<<-TRANSFORMDATA()$LEARNINGTRANSFORM}
  else{learningmodel<<-TEST()$LEARNINGDIFF}
  validation<<-DATA()$VALIDATION
  datastructuresfeatures<<-SELECTDATA()$DATASTRUCTUREDFEATURES
  transformdataparameters<<-TRANSFORMDATA()$transformdataparameters
  learningselect<-SELECTDATA()$LEARNINGSELECT
  modelparameters<<-list("modeltype"=input$model,"invers"=F,"thresholdmodel"=input$thresholdmodel,
                         "fs"=input$fs,"adjustval"=input$adjustval)
  print(ncol(learningmodel))
  validate(need(ncol(learningmodel)>1,"Not enough features"))


  resmodel<<-modelfunction(learningmodel = learningmodel,validation = validation,modelparameters = modelparameters,
                           transformdataparameters = transformdataparameters,datastructuresfeatures =  datastructuresfeatures,
                           learningselect = learningselect)
  
 list("DATALEARNINGMODEL"=resmodel$datalearningmodel,"MODEL"=resmodel$model,"DATAVALIDATIONMODEL"=resmodel$datavalidationmodel,
      "GROUPS"=resmodel$groups,"modelparameters"=resmodel$modelparameters)
  
  })

observe({
  if (input$model=="svm") { updateNumericInput(session, "thresholdmodel", value = 0)} 
  else if (input$model=="randomforest"){  updateNumericInput(session, "thresholdmodel", value = 0.5)}
})

####
output$downloaddatalearning <- downloadHandler(
  filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
  content = function(file) {
    downloaddataset(   MODEL()$DATALEARNINGMODEL$learningmodel, file) })


output$plotmodeldecouvroc <- renderPlot({
  datalearningmodel<<-MODEL()$DATALEARNINGMODEL
  ROCcurve(validation = datalearningmodel$reslearningmodel$classlearning,decisionvalues =  datalearningmodel$reslearningmodel$scorelearning)
})
output$youndendecouv<-renderTable({
  datalearningmodel<<-MODEL()$DATALEARNINGMODEL
  resyounden<-younden(datalearningmodel$reslearningmodel$classlearning, datalearningmodel$reslearningmodel$scorelearning)
  resyounden<-data.frame(resyounden)
  colnames(resyounden)<-c("")
  rownames(resyounden)<-c("younden","sensibility younden","specificity younden","threshold younden")
  
  resyounden
},include.rownames=TRUE)
 
output$downloadplotdecouvroc = downloadHandler(
  filename = function() {paste('graph','.',input$paramdownplot, sep='')},
  content = function(file) {
    ggsave(file, plot =  ROCcurve(validation = datalearningmodel$reslearningmodel$classlearning,
                                  decisionvalues =  datalearningmodel$reslearningmodel$scorelearning),  device = input$paramdownplot)},
  contentType=NA)

output$downloaddatadecouvroc <- downloadHandler(
  filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
  content = function(file) {
    downloaddataset(ROCcurve(validation = datalearningmodel$reslearningmodel$classlearning,decisionvalues =  datalearningmodel$reslearningmodel$scorelearning,graph=F), file) })

output$plotmodeldecouvbp <- renderPlot({
  datalearningmodel<<-MODEL()$DATALEARNINGMODEL
  scoremodelplot(class =datalearningmodel$reslearningmodel$classlearning ,score =datalearningmodel$reslearningmodel$scorelearning,names=rownames(datalearningmodel$reslearningmodel),
                 threshold =input$thresholdmodel ,type =input$plotscoremodel,graph = T,printnames=input$shownames1)
})
output$downloadplotmodeldecouvbp = downloadHandler(
  filename = function() {paste('graph','.',input$paramdownplot, sep='')},
  content = function(file) {
    ggsave(file, plot =scoremodelplot(class =datalearningmodel$reslearningmodel$classlearning ,score =datalearningmodel$reslearningmodel$scorelearning,names=rownames(datalearningmodel$reslearningmodel),
                                      threshold =input$thresholdmodel ,type =input$plotscoremodel,graph = T),  device = input$paramdownplot)},
  contentType=NA)

output$downloaddatamodeldecouvbp <- downloadHandler(
  filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
  content = function(file) {
    downloaddataset(   scoremodelplot(class =datalearningmodel$reslearningmodel$classlearning ,score =datalearningmodel$reslearningmodel$scorelearning,names=rownames(datalearningmodel$reslearningmodel),
                                      threshold =input$thresholdmodel ,type =input$plotscoremodel,graph = F), file) })
output$nbselectmodel<-renderText({
  datalearningmodel<-MODEL()$DATALEARNINGMODEL
  ncol(datalearningmodel$learningmodel)-1
})

output$tabmodeldecouv<-renderTable({
  datalearningmodel<-MODEL()$DATALEARNINGMODEL
  as.data.frame.matrix(table(datalearningmodel$reslearningmodel$predictclasslearning,datalearningmodel$reslearningmodel$classlearning ))
},include.rownames=TRUE)

output$sensibilitydecouv<-renderText({
  datalearningmodel<-MODEL()$DATALEARNINGMODEL
  sensibility(predict = datalearningmodel$reslearningmodel$predictclasslearning,class = datalearningmodel$reslearningmodel$classlearning)
})

output$specificitydecouv<-renderText({
  datalearningmodel<-MODEL()$DATALEARNINGMODEL
  specificity(predict = datalearningmodel$reslearningmodel$predictclasslearning,class = datalearningmodel$reslearningmodel$classlearning )
})


output$downloaddatavalidation <- downloadHandler(
  filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
  content = function(file) {
    downloaddataset( data.frame("Class"=MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$classval,MODEL()$DATAVALIDATIONMODEL$validationmodel,check.names = F), file) })


output$plotmodelvalroc <- renderPlot({
  datavalidationmodel<-MODEL()$DATAVALIDATIONMODEL
  ROCcurve(validation =  datavalidationmodel$resvalidationmodel$classval,decisionvalues =  datavalidationmodel$resvalidationmodel$scoreval)
})

output$downloadplotvalroc = downloadHandler(
  filename = function() {paste('graph','.',input$paramdownplot, sep='')},
  content = function(file) {
    ggsave(file, plot =ROCcurve(validation =  MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$classval,decisionvalues =  MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$scoreval),  device = input$paramdownplot)},
  contentType=NA)

output$downloaddatavalroc <- downloadHandler(
  filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
  content = function(file) {
    downloaddataset(   ROCcurve(validation =  MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$classval,decisionvalues =  MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$scoreval,graph=F ), file) 
    })

output$plotmodelvalbp <- renderPlot({
  datavalidationmodel<-MODEL()$DATAVALIDATIONMODEL
  scoremodelplot(class = datavalidationmodel$resvalidationmodel$classval ,score =datavalidationmodel$resvalidationmodel$scoreval,names=rownames(datavalidationmodel$resvalidationmodel),
                 threshold =input$thresholdmodel ,type =input$plotscoremodel,graph = T,printnames=input$shownames1)
})

output$downloadplotmodelvalbp = downloadHandler(
  filename = function() {paste('graph','.',input$paramdownplot, sep='')},
  content = function(file) {
    ggsave(file, plot =scoremodelplot(class = MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$classval ,score =MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$scoreval,names=rownames(MODEL()$DATAVALIDATIONMODEL$resvalidationmodel),
                                      threshold =input$thresholdmodel ,type =input$plotscoremodel,graph = T),  device = input$paramdownplot)},
  contentType=NA)

output$downloaddatamodelvalbp <- downloadHandler(
  filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
  content = function(file) {
    downloaddataset(   scoremodelplot(class = MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$classval ,score =MODEL()$DATAVALIDATIONMODEL$resvalidationmodel$scoreval,names=rownames(MODEL()$DATAVALIDATIONMODEL$resvalidationmodel),
                                      threshold =input$thresholdmodel ,type =input$plotscoremodel,graph = F), file) })

output$youndenval<-renderTable({
  datavalidationmodel<<-MODEL()$DATAVALIDATIONMODEL
  resyounden<-younden(datavalidationmodel$resvalidationmodel$classval,datavalidationmodel$resvalidationmodel$scoreval )
  resyounden<-data.frame(resyounden)
  colnames(resyounden)<-c("")
  rownames(resyounden)<-c("younden","sensibility younden","specificity younden","threshold younden")
  resyounden
},include.rownames=TRUE)

output$tabmodelval<-renderTable({ 
  datavalidationmodel<-MODEL()$DATAVALIDATIONMODEL
  as.data.frame.matrix(table(datavalidationmodel$resvalidationmodel$predictclassval, datavalidationmodel$resvalidationmodel$classval))
},include.rownames=TRUE)
output$sensibilityval<-renderText({
  datavalidationmodel<-MODEL()$DATAVALIDATIONMODEL
  sensibility(predict = datavalidationmodel$resvalidationmodel$predictclassval,class = datavalidationmodel$resvalidationmodel$classval)
})
output$specificityval<-renderText({
  datavalidationmodel<-MODEL()$DATAVALIDATIONMODEL
  specificity(predict = datavalidationmodel$resvalidationmodel$predictclassval,class =  datavalidationmodel$resvalidationmodel$classval)
})
####Detail of the model
output$summarymodel<-renderPrint({
  model<-print(MODEL()$MODEL)
})
output$plotimportance<-renderPlot({
  model<<-MODEL()$MODEL
  learningmodel<<-MODEL()$DATALEARNINGMODEL$learningmodel
  modeltype<<-input$model
  importanceplot(model = model,learningmodel = learningmodel,modeltype =modeltype,graph=T )
})
output$downloadplotimportance = downloadHandler(
  filename = function() {paste('graph','.',input$paramdownplot, sep='')},
  content = function(file) {
    ggsave(file, plot =  importanceplot(model = MODEL()$MODEL,learningmodel = MODEL()$DATALEARNINGMODEL$learningmodel,modeltype =input$model,graph=T ),  device = input$paramdownplot)},
  contentType=NA)

output$downloaddataplotimportance <- downloadHandler(
  filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
  content = function(file) {
    downloaddataset(     importanceplot(model = MODEL()$MODEL,learningmodel = MODEL()$DATALEARNINGMODEL$learningmodel,modeltype =input$model,graph=F ), file) })

####Test prameters
output$testNAstructure<- reactive({
  if("TRUE"%in%input$NAstructuretest ){test<-as.logical(TRUE)}
  else{test<-as.logical(FALSE)}
  return(test)
})
outputOptions(output, 'testNAstructure', suspendWhenHidden=FALSE)

TESTPARAMETERS <- eventReactive(input$tunetest, { 
  prctvaluestest<-seq(input$prctvaluestest[1],input$prctvaluestest[2],by = 5)
  listparameters<<-list("prctvalues"=prctvaluestest,"selectmethod"=input$selectmethodtest,"NAstructure"=as.logical(input$NAstructuretest),
                        "thresholdNAstructure"=input$thresholdNAstructuretest,"structdata"=input$structdatatest,"maxvaluesgroupmin"=input$maxvaluesgroupmintest,
                        "minvaluesgroupmax"=input$minvaluesgroupmaxtest,"rempNA"=input$rempNAtest,"log"=as.logical(input$logtest),"logtype"=input$logtypetest,
                        "standardization"=as.logical(input$standardizationtest),"arcsin"=as.logical(input$arcsintest),"test"=input$testtest,"adjustpv"=as.logical(input$adjustpvtest),
                        "thresholdpv"=input$thresholdpvtest,"thresholdFC"=input$thresholdFCtest,"model"=input$modeltest,"thresholdmodel"=0,"fs"=as.logical(input$fstest))
    length(listparameters$prctvalues)
    validate(need( sum(do.call(rbind, lapply(listparameters, FUN=function(x){length(x)==0})))==0,"One of the parameters is empty"))
    tabparameters<<-constructparameters(listparameters)
    tabparameters$thresholdmodel[which(tabparameters$model=="randomforest")]<-0.5
    validation<<-DATA()$VALIDATION
    learning<<-DATA()$LEARNING
    tabparametersresults<<-testparametersfunction(learning,validation,tabparameters)
    #clean useless columns
    if(length(which(apply(X = tabparametersresults,MARGIN=2,function(x){sum(is.na(x))})==nrow(tabparametersresults)))!=0){
      tabparametersresults<-tabparametersresults[,-which(apply(X = tabparametersresults,MARGIN=2,function(x){sum(is.na(x))})==nrow(tabparametersresults))]}
    return(tabparametersresults)

#     if(sum(listparameters$NAstructure)==0){tabparametersresults<-
#       tabparametersresults[,-c("thresholdNAstructure","structdata")]
#     }
    
                       
  })
# output$testtabparameters<- reactive({
#   if(!tabparameters ){test<-as.logical(FALSE)}
#   else{test<-as.logical(TRUE)}
#   return(test)
# })
# outputOptions(output, 'testNAstructure', suspendWhenHidden=FALSE)

output$tabtestparameters<-renderDataTable({
  resparameters<<-TESTPARAMETERS()
  cbind(Names=rownames(resparameters),resparameters)},
  options = list(    "orderClasses" = F,
                     "responsive" = F,
                     "pageLength" = 100
            #          ,rowCallback = I('
            # function(nRow, aData, iDisplayIndex, iDisplayIndexFull) {$("td:eq(1)", nRow).css("color", "red");}'
                                                        # )
            )
            )

output$downloadtabtestparameters <- downloadHandler(
  filename = function() { paste('dataset', '.',input$paramdowntable, sep='') },
  content = function(file) {
    downloaddataset(   TESTPARAMETERS(), file) })

}) 


 
