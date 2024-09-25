options(xtable.include.colnames=T)
options(xtable.include.rownames=T)
#Packages
#rm(list=ls())
usePackage <- function(p) 
{
  if (!is.element(p, installed.packages()[,1]))
    install.packages(p, dep = TRUE)
  require(p, character.only = TRUE)
}
usePackage("zoo")
usePackage("missMDA")#imputepca
usePackage("ggplot2")#Graphs
usePackage("stats")
usePackage("e1071")#svm
usePackage("pROC")#roccurve
usePackage("devtools")
usePackage("readxl")
usePackage("shiny")
# if (!is.element("factoextra", installed.packages()[,1]))
#   install_github("kassambara/factoextra")
#usePackage("factoextra")#PCA graphs
usePackage("reshape2")#melt function
usePackage("xlsx")#import fichier xls#Fonctions
usePackage("randomForest")
usePackage("missForest")
usePackage("Hmisc")
usePackage("corrplot")
usePackage("penalizedSVM")
usePackage("DT")
usePackage("shinycssloaders")
usePackage("writexl")

##########################
importfile<-function (datapath,extension,NAstring="NA",sheet=1,skiplines=0,dec=".",sep=","){
  # datapath: path of the file
  #extention: extention of the file : csv, xls, ou xlsx
  if(extension=="csv"){
    toto <<- read.csv2(datapath,header = F,sep =sep,dec=dec,na.strings = NAstring,stringsAsFactors = F,row.names=NULL,check.names = F )
  }
  if(extension=="xlsx"){
    options(warn=-1)
    filerm<<-file.rename(datapath,paste(datapath, ".xlsx", sep=""))
    options(warn=0)
    toto <<- read_excel(paste(datapath, ".xlsx", sep=""),na=NAstring,col_names = F,skip = skiplines,sheet = sheet) %>% as.data.frame()
    #toto <<- read_xlsx(paste(datapath, ".xlsx", sep=""),na=NAstring,col_names = F,skip = skiplines,sheet = sheet)
    #toto <-read.xlsx2(file = datapath,sheetIndex = sheet)
    #toto <-read_excel(datapath,na=NAstring,col_names = F,skip = skiplines,sheet = sheet)
  }
  #remove empty column
  if(length(which(apply(X = toto,MARGIN=2,function(x){sum(is.na(x))})==nrow(toto)))!=0){
    toto<-toto[,-which(apply(X = toto,MARGIN=2,function(x){sum(is.na(x))})==nrow(toto))]}
  #remove empty row
  if(length(which(apply(X = toto,MARGIN=1,function(x){sum(is.na(x))})==ncol(toto)))!=0){
    toto<-toto[-which(apply(X = toto,MARGIN=1,function(x){sum(is.na(x))})==ncol(toto)),]}
  print(class(toto))
  
  rnames<-as.character(as.matrix(toto[,1]))
  cnames<-as.character(as.matrix(toto[1,]))
  toto<-toto[,-1]
  toto<-toto[-1,]
  row.names(toto)<-rnames[-1]
  colnames(toto)<-cnames[-1]

  toto<-as.data.frame(toto)
  rownames(toto)<-rnames[-1]
  colnames(toto)<-cnames[-1]
  return(toto)
}

# downloaddataset <- function(x,file,cnames=T,rnames=T){
#   ext<-strsplit(x = file,split = "[.]")[[1]][2]
#   if(ext=="csv"){
#     if(sum(cnames,rnames)==2){
#       write.csv(x,file)
#       }
#     else{
#       write.table(x,file,col.names = cnames,row.names = rnames,sep=";",dec=".")
#       }
#   }
#   if(ext=="xlsx"){
#     write.xlsx(x,file,col.names = cnames,row.names =rnames )
#   }
#   
# }

# df <- reactive({
#   req(input$learningfile)
#   file <- input$learningfile
#   ext <- tools::file_ext(file$datapath)
#   
#   req(file)
#   validate(need(ext == "xlsx", "Veuillez télécharger un fichier CSV"))
#   
#   df = read_excel(file$datapath)
#   print(head(df))
#   return( df)
# })


downloaddataset <- function(x,file,cnames=T,rnames=T){
  ext = tools::file_ext(file)
  if(ext=="csv"){
    if(sum(cnames,rnames)==2){
      write.csv(x,file)
    }
    else{
      write.table(x,file,col.names = cnames,row.names = rnames,sep=";",dec=".")
    }
  }
  if(ext=="xlsx"){
    #write.xlsx(x,file,col.names = cnames,row.names =rnames )
    writexl::write_xlsx(x,file, col_names = cnames)
  }
  
}

downloadplot <- function(file){
  ext<-strsplit(x = file,split = "[.]")[[1]][2]
  
  if(ext=="png"){
    png(file)
  }
  if(ext=="jpg"){
    jpeg(file)
  }  
  if(ext=="pdf"){
    pdf(file) 
  }     
}
# renamvar<-function(names){
#   #rename the duplicate name by adding ".1, .2 ....
#   #toto is a vector of the col names of the tab
#   names[is.na(names)]<-"NA"
#   for(i in 1:length(names)){
#     ind <- which(names%in%names[i])
#     if(length(ind)>1){
#       nb<-c(1:length(ind))
#       newnames<-paste(names[ind],".",nb,sep="")
#       
#       names[ind]<-newnames
#     }
#   }
#   return(names)
# }
gg_color_hue <- function(n) {
  hues = seq(15, 375, length=n+1)
  hcl(h=hues, l=65, c=100)[1:n]
}
transformdata<-function(toto,transpose,zeroegalNA){
#   if(length(which(apply(X = toto,MARGIN=1,function(x){sum(is.na(x))})==ncol(toto)))!=0){
#     toto<-toto[-which(apply(X = toto,MARGIN=1,function(x){sum(is.na(x))})==ncol(toto)),]}
#   #remove empty rows
#   if(length(which(apply(X = toto,MARGIN=2,function(x){sum(is.na(x))})==nrow(toto)))!=0){
#     toto<-toto[,-which(apply(X = toto,MARGIN=2,function(x){sum(is.na(x))})==nrow(toto))]}
#   #remove empty columns
  
  # transpose du data frame
  if(transpose){
    toto<-t(toto)
    }
  
  if(zeroegalNA){
    toto[which(toto==0,arr.ind = T)]<-NA
    }
  
toto<-as.data.frame(toto[,c(colnames(toto)[1],sort(colnames(toto)[-1]))])
}
confirmdata<-function(toto){
  toto<-as.data.frame(toto)
  toto[,1]<-as.factor(as.character(toto[,1]))
  for (i in 2:ncol(toto)){
    toto[,i]<-as.numeric(as.character(toto[,i]))
  }
  return(toto)
}


importfunction<-function(importparameters){
  previousparameters<-NULL
  validation<-NULL
  learning<-NULL
  
  if(is.null(importparameters$learningfile)&is.null(importparameters$modelfile)){return()}
  
  if(!is.null(importparameters$modelfile) ){
    load(file = importparameters$modelfile$datapath)
    previous<-state
    learning<-previous$data$LEARNING
    validation<-previous$data$VALIDATION
    #lev<-previous$data$LEVELS
    previousparameters<-previous$parameters
  }

  if(!is.null(importparameters$learningfile)  ){
    #if(importparameters$confirmdatabutton==0){
      datapath<- importparameters$learningfile$datapath
      #datapath <- input$learningfile$datapath
      #print(datapath)
      #print(paste(datapath, ".xlsx", sep=""))
      #out<<-tryCatch(
      learning<-importfile(datapath = datapath,extension = importparameters$extension,NAstring=importparameters$NAstring,
                           sheet=importparameters$sheetn,skiplines=importparameters$skipn,dec=importparameters$dec,sep=importparameters$sep)
      #              ,error=function(e) e )
      #            if(any(class(out)=="error")){tablearn<-data.frame()}
      #            else{tablearn<<-out}
      #            validate(need(ncol(tablearn)>1 & nrow(tablearn)>1,"problem import"))
      
      learning<-transformdata(toto = learning,transpose=importparameters$transpose,zeroegalNA=importparameters$zeroegalNA)
      
    #}
    if(importparameters$confirmdatabutton!=0){
      learning<-confirmdata(toto = learning)
      if(importparameters$invers){learning[,1]<-factor(learning[,1],levels = rev(levels(learning[,1])))}
      
      #learning<-learning[-which(apply(X = learning,MARGIN=1,function(x){sum(is.na(x))})==ncol(learning)),]
      
#       lev<-levels(x = tablearn[,1])
#       print(lev)
#       names(lev)<-c("positif","negatif")
    }
    # else{lev<-NULL}
  }

  
  if(!is.null(importparameters$validationfile)  ){
    
    # if(importparameters$confirmdatabutton==0){
      datapathV<- importparameters$validationfile$datapath
      # out<<-tryCatch(
      validation<-importfile(datapath = datapathV,extension = importparameters$extension,
                 NAstring=importparameters$NAstring,sheet=importparameters$sheetn,skiplines=importparameters$skipn,dec=importparameters$dec,sep=importparameters$sep)
      #             ,error=function(e) e)
      #             if(any(class(out)=="error")){tabval<-NULL}
      #            else{tabval<<-out}
      #            validate(need(ncol(tabval)>1 & nrow(tabval)>1,"problem import"))
        validation<-transformdata(toto = validation,transpose=importparameters$transpose,zeroegalNA=importparameters$zeroegalNA)
      
      
    # }
    if(importparameters$confirmdatabutton!=0){
      validation<-confirmdata(toto = validation)
      if(importparameters$invers){validation[,1]<-factor(validation[,1],levels = rev(levels(validation[,1])))}
      
      #validation<-validation[-which(apply(X = validation,MARGIN=1,function(x){sum(is.na(x))})==ncol(validation)),]
        
    }
    
  }

  res<-list("learning"=learning,"validation"=validation,previousparameters=previousparameters)#,"lev"=lev)
  return(res)
}


# selectvar<-function(resPCA,toto){
#   #select variables which are correlate to the axes correlate to the cotegorial variable of the first column
#   restri<-dimdesc(resPCA,axes = c(1:(min(ncol(toto),10)-1)) )
#   varquali<-vector()
#   score<-0
#   #restri is a dimdesc data
#   for (i in 1:length(restri)){
#     if ( !is.null(restri[[i]]$quali ) ) {
#       score<-score+restri[[i]]$quali[[1]]
#       varquali<-c(varquali,row.names(restri[[i]]$quanti))
#     }
#   }
#   #score<-1- ( ( (1+score)*(nrow(toto)-1) )/(nrow(toto)-ncol(toto)-1) )
#   return(list("varquali"=varquali,"score"=score))
# }

# selectdata<-function(toto){
#   #remove variable  with less than 2 value and replace 0 by NA
#   n<-ncol(toto)
#   toto[which(toto==0 ,arr.ind = T )]<-NA
#   vec<-rep(T,length=n)
#   for(i in 2:n){
#     vec[i]<-( (length(unique(toto[,i]))>2) )
#   }
#   #rm var with less than 3 values (0 or NA , and 2 other (important for the rempNA PCA))
#   toto<-toto[,as.logical(vec)]
#   return(toto)
# }

selectdatafunction<-function(learning,selectdataparameters){
  learningselect<-selectprctvalues(toto = learning,prctvalues = selectdataparameters$prctvalues,selectmethod =selectdataparameters$selectmethod)
  if(selectdataparameters$NAstructure==T){
    if(selectdataparameters$structdata=="selecteddata"){learning<-learningselect}
    restestNAstructure<-testNAstructure(toto = learning,threshold = selectdataparameters$thresholdNAstructure,maxvaluesgroupmin=selectdataparameters$maxvaluesgroupmin,
                                        minvaluesgroupmax=selectdataparameters$minvaluesgroupmax)
    if(!is.null(restestNAstructure)){
      learningselect<-cbind(learningselect[,!colnames(learningselect)%in%restestNAstructure$restestNAstructure$names],restestNAstructure$varNAstructure)}
  }
  else{restestNAstructure<-NULL}
  
  return(list(learningselect=learningselect,structuredfeatures=restestNAstructure$varNAstructure,datastructuredfeatures=restestNAstructure$restestNAstructure))
}

testObject <- function(object){
  #test if the object is in the global environnement
  exists(as.character(substitute(object)))
}

selectprctvalues<-function(toto,prctvalues=100,selectmethod="nogroup"){ 
  n<-ncol(toto)
  if (selectmethod=="nogroup"){
    NAvec<-vector(length =max(n,0) )
    for(i in 1:n){
      NAvec[i]<-  (sum(!is.na(toto[,i]))/nrow(toto)  ) 
    }
    vec<-(NAvec>=(prctvalues/100))
    
  } 
  
  if(selectmethod!="nogroup"){
    nbcat<-length(levels(toto[,1]))
    tabgroup<-matrix(nrow = nbcat, ncol=n )
    for(i in 1:nbcat){
      tab<-toto[which(toto[,1]==levels(toto[,1])[i]),]
      for(j in 1:(n) ){
        tabgroup[i,j]<-(sum(!is.na(tab[,j]))/nrow(tab))  
      }  
    }
    if(selectmethod=="onegroup"){
      vec<-apply(X = tabgroup,MARGIN = 2,FUN = function(x){(max (x) >= (prctvalues/100)) }) 
    }
    if(selectmethod=="bothgroups"){
      vec<-apply(X = tabgroup,MARGIN = 2,FUN = function(x){(min (x) >= (prctvalues/100)) }) 
    }
  }
  totoselect<-toto[,as.logical(vec)]
}

heatmapNA<-function(toto,maintitle="Distribution of NA",graph=T){
 
    if(ncol(toto)==1){errorplot(text = " No structured variables")}
    else{
      names<- paste(toto[,1],1:length(toto[,1]))
      tab<-as.data.frame(toto[,-1])
      tab[which(!is.na(tab) ,arr.ind = T )]<-"Value"
      tab[which(is.na(tab) ,arr.ind = T )]<-"NA"
      #tab<-cbind(paste(toto[,1],1:length(toto[,1])),tab)
      tab<-apply(tab,2,as.factor)
      rownames(tab)<-names
      if(!graph){ return(cbind(rownames(toto),tab))}
      if(graph){
      tabm <- melt(tab)
      #tabm<-tabm[-c(1:nrow(toto)),]
      colnames(tabm)<-c("individuals","variables","value")
      tabm$variables<-as.character(tabm$variables)
      tabm$individuals<-as.character(tabm$individuals)
      if(ncol(toto)>60){
        ggplot(tabm, aes(variables, individuals)) + geom_tile(aes(fill = value)) + scale_fill_manual(values=c("lightgrey","steelblue"),name="")+ 
          ggtitle(maintitle) + theme(plot.title = element_text(size=15),axis.text.x=element_blank())
      }
      else{
        ggplot(tabm, aes(variables, individuals)) + geom_tile(aes(fill = value), colour = "white") + scale_fill_manual(values=c("lightgrey","steelblue"))+ 
          ggtitle(maintitle) + theme(plot.title = element_text(size=15),axis.text.x=element_blank())
      }
    }
  }
}

distributionvalues<-function(toto,prctvaluesselect,nvar,maintitle="Number of variables according to\nthe % of values's selected",graph=T,ggplot=T){
  percentagevalues<-seq(0,1,by = 0.01)
  prctall<-apply(X = toto,MARGIN = 2,FUN = function(x){sum(!is.na(x))})/nrow(toto)
  prctvalueswhithoutgroup<-sapply(X = percentagevalues,FUN = function(x,prct=prctall){sum(x<=prct)})
  prctlev1<-apply(X = toto[which(toto[,1]==levels(toto[,1])[1]),],MARGIN = 2,FUN = function(x){sum(!is.na(x))})/nrow(toto[which(toto[,1]==levels(toto[,1])[1]),])
  prctlev2<-apply(X = toto[which(toto[,1]==levels(toto[,1])[2]),],MARGIN = 2,FUN = function(x){sum(!is.na(x))})/nrow(toto[which(toto[,1]==levels(toto[,1])[2]),])
  
  nvareachgroups<-sapply(X = percentagevalues,FUN = function(x,prct1=prctlev1,prct2=prctlev2){sum(x<=apply(rbind(prct1,prct2),2,min))})  
  nvaronegroup<-sapply(X = percentagevalues,FUN = function(x,prct1=prctlev1,prct2=prctlev2){sum(x<=apply(rbind(prct1,prct2),2,max))})  
  
  distribvalues<-data.frame("percentagevalues"=percentagevalues,"all samples"=prctvalueswhithoutgroup,"each groups"= nvareachgroups,"at least one group"=nvaronegroup)
  if(!graph)(return(distribvalues))
  col<-gg_color_hue(ncol(distribvalues)-1)
  if(!ggplot){
    matplot(x=distribvalues$percentagevalues,distribvalues[,-1],type=c("l","l"),lty = c(1,1,1),
            col=c("red","green","blue"), xlab="percentage of values selected",ylab="Number of variables",main=maintitle)
    legend("bottomright",colnames(distribvalues[,-1]),col=c("red","green","blue"),lty=1)
    abline(v = prctvaluesselect,lty=3,col="grey")
    abline(h = nvar,lty=3,col="grey")
  }
  if (ggplot){
    distribvalueslong<- melt(distribvalues,id.vars = "percentagevalues",variable.name = "select_method",value.name = "number_of_variables")  # convert to long format
    p<-ggplot(data=distribvalueslong,
              aes(x=percentagevalues, y=number_of_variables, colour=select_method)) +geom_line()+
      ggtitle(maintitle)
    p+theme(plot.title=element_text( size=15),legend.text=element_text(size=10),legend.title=element_text(color = 0),legend.position=c(0.20,0.15))+
      geom_vline(xintercept=prctvaluesselect,linetype=3)+
      geom_hline(yintercept=nvar,linetype=3)
  }
}

proptestNA<-function(toto){
  group<-toto[,1]
  toto[,1]<-as.character(toto[,1])
  toto[which(!is.na(toto),arr.ind=T)]<-"value"
  toto[which(is.na(toto),arr.ind=T)]<-"NA"
  pval<-vector("numeric",length = ncol(toto))
  lessgroup<-vector("character",length = ncol(toto))
  prctmore<-vector("numeric",length = ncol(toto))
  prctless<-vector("numeric",length = ncol(toto))
  for (i in 1:ncol(toto)){
    conting<-table(group,factor(toto[,i],levels=c("value","NA")))
    options(warn=-1)
    res<-prop.test(conting)
    options(warn=0)
    pval[i]<-res$p.value
    prctmore[i]<-max(res$estimate)
    prctless[i]<-min(res$estimate)
    if(res$estimate[1]==res$estimate[2]){ lessgroup[i]<-"NA"}
    else{lessgroup[i]<-rownames(conting)[which(res$estimate==min(res$estimate))]}
  }
  pval[is.na(pval)]<-1
  return(data.frame("pval"=pval,"lessgroup"=lessgroup,"prctless"=prctless,"prctmore"=prctmore,"names"=colnames(toto)))
}

testNAstructure<-function(toto,threshold=0.05,maxvaluesgroupmin=100,minvaluesgroupmax=0){
  class<-toto[,1]
  resproptest<-proptestNA(toto=toto)
  vecond<-c(resproptest$pval<=threshold & resproptest$prctless<=(maxvaluesgroupmin/100) & resproptest$prctmore>=(minvaluesgroupmax/100))
  if(sum(vecond)>0){
    resp<-resproptest[vecond,]
    totopropselect<-data.frame(toto[,vecond])
    colnames(totopropselect)<-resp$names
    totopropselect<-as.data.frame(totopropselect[, order(resp[,2])])
    colnames(totopropselect)<-resp$names[order(resp[,2])]
  }
  else{return(NULL)}

  return(list("varNAstructure"=totopropselect,"restestNAstructure"=resp))
}

transformdatafunction<-function(learningselect,structuredfeatures,datastructuresfeatures,transformdataparameters){
  learningtransform<-learningselect
  if(!is.null(structuredfeatures)){
    for(i in 1:ncol(structuredfeatures)){
      learningtransform[which(is.na(structuredfeatures[,i])&learningselect[,1]==as.character(datastructuresfeatures[i,"lessgroup"])),as.character(datastructuresfeatures[i,"names"])]<-0
    }
  }
  if(transformdataparameters$log){ 
    learningtransform[,-1]<-transformationlog(x = learningtransform[,-1]+1,logtype=transformdataparameters$logtype)}
  if(transformdataparameters$arcsin){
    learningtransform[,-1]<-apply(X = learningtransform[,-1],MARGIN = 2,FUN = function(x){(x-min(x,na.rm = T))/(max(x,na.rm = T)-min(x,na.rm = T))})
    learningtransform[,-1]<-asin(sqrt(learningtransform[,-1]))
  }
  if(transformdataparameters$standardization){
    learningtransformsd<<-learningtransform
    sdlearningtransform<-apply(X = learningtransform[-1],MARGIN = 2,FUN = sd,na.rm=T)
    #print('sdlearningtransform')
    #print(sdlearningtransform)
    learningtransform[,-1]<-scale(learningtransform[,-1],center = F,scale=sdlearningtransform)
    #learningtransform[,-1]<-scale(learningtransform[,-1], center = F, scale = TRUE)
  }
  learningtransform<-replaceNA(toto=learningtransform,rempNA=transformdataparameters$rempNA,pos=T,NAstructure = F)
  
  return(learningtransform)
}

transformationlog<-function(x,logtype){
  if(logtype=="log10"){x<-log10(x)}
  if(logtype=="log2"){x<-log2(x)}
  if(logtype=="logn"){x<-log(x)}
  return(x)
}

histplot<-function(toto,graph=T){

    data<-data.frame("values"=as.vector(as.matrix(toto[,-1])))
    if(graph==F){ return(datahistogram(data = data,nbclass = 20))}
    if(graph==T){
    ggplot(data=data,aes(x=values) )+ 
      geom_histogram(col="lightgrey",fill="steelblue",bins=20)+ggtitle("Distribution of values")+
      theme(plot.title = element_text(size=15))+
       annotate("text",x=Inf,y=Inf,label=paste(nrow(data),"values"),size=6,vjust=2,hjust=1.5)
  }
}
datahistogram<-function(data,nbclass){
  dh<-hist(data[,1],nclass=nbclass,plot=F)
  minclass<-dh$breaks[-(length(dh$breaks))]
  maxclass<-dh$breaks[2:(length(dh$breaks))]
  count<-dh$counts
  res<-data.frame("count"=count,"minclass"=minclass,"maxclass"=maxclass)
}

replaceNA<-function(toto,rempNA="z",pos=F,NAstructure=F,thresholdstruct=0.05,maxvaluesgroupmin=100,minvaluesgroupmax=0){ 
  #rempNA: remplace Non ATtributes values by zero("z"), the mean of the colum (moy), 
  # the mean in each group define by the factor of the first column(moygr), itarative pca (pca), or keep th NA
  if(NAstructure){
    totoNAstruct<-replaceproptestNA(toto = toto,threshold = thresholdstruct ,rempNA =rempNA,maxvaluesgroupmin,minvaluesgroupmax)
    toto[,colnames(totoNAstruct)]<-totoNAstruct
  }
  
  if (rempNA == "none" | sum(is.na(toto))==0 ) {return(toto)}
  cnames<-colnames(toto)
  class<-(toto[,1])
  cat<-levels(class)
  toto<-as.data.frame(toto[,-1],optional = T)
  #toto<-apply(toto,MARGIN = 2,function(x)as.numeric(x))
  n<-ncol(toto) 
  #par default je remplace les NA par 0
  if (rempNA == "z") {
    toto[which(is.na(toto),arr.ind = T)]<-0
  }
  if (rempNA== "moy") {
    toto<-na.aggregate(toto)}
  if(rempNA=="moygr"){
    
    for (i in 1:length(cat)){
      tab<-toto[which(class==cat[i]),]
      tab<-na.aggregate(tab)
      toto[which(class==cat[i]),]<-tab
    }
    toto[which(is.na(toto) ,arr.ind = T )]<-0
  }
  if (rempNA == "pca"){
    
    #prise en compte des liaisons entre variable et de la ressemblance entre individus    
    #nb<-estim_ncpPCA(toto[,(nbqualisup+1):n],ncp.min = 0,ncp.max = 10,method.cv = "Kfold")    #take a lot time
    nindiv<-nrow(toto)
    prctnacol<-apply(X = toto,MARGIN = 2,FUN=function(x){ if(sum(!is.na(x))<=0){x<-rep(0,length=nindiv)}
      else{x}})
    toto<-imputePCA(prctnacol,ncp = min(n-1,5),method.cv="Kfold")$completeObs
    if(pos){toto[which(toto<0,arr.ind = T)]<-0}
    toto<-as.data.frame(toto)
    
  }
  if(rempNA=="missforest"){
    toto<-missForest(toto,maxiter = 5)$ximp
    if(pos){toto[which(toto<0,arr.ind = T)]<-0}
  }
  
  toto<-cbind(class,toto)
  toto[which(is.na(toto),arr.ind = T)]<-0
  
  colnames(toto)<-cnames
  
  return(toto)
}
mdsplot<-function(toto,ggplot=T,maintitle="MDS representation of the individuals",graph=T){
  class<-toto[,1]
  toto<-toto[-1]
  d <- dist(toto) # euclidean distances between the rows
  fit <- cmdscale(d,eig=TRUE, k=2) # k is the number of dim
  x <- fit$points[,1]
  y <- fit$points[,2] 
  coord<-(data.frame("class"=class,x,y))
  if(!graph){return(coord)}
  if(!ggplot){
    colr<-c("red","blue")
    
    plot(x, y, xlab="", ylab="",pch=20,main=maintitle, type="p",col=c(rep(colr[1],times=15),rep(colr[2],times=34) ))
    text(x, y, labels = row.names(toto), cex=.7,col=c(rep(colr[1],times=15),rep(colr[2],times=34) ))
    legend("topleft",legend=levels(class),text.col = colr)
  }
  #MDS ggplot
  if(ggplot){
    p <- ggplot(coord, aes(x, y,label=rownames(toto)))
    p + geom_text(aes(colour = class))+ggtitle(maintitle)+theme(plot.title=element_text( size=15))
  }
}

heatmapplot<-function(toto,ggplot=T,maintitle="Heatmap of the transform data ",scale=F,graph=T){
  row.names(toto)<-paste(toto[,1],1:length(toto[,1]))
  toto<-as.matrix(toto[,-1])
  if(!graph){return(toto)}
  #colnames(toto)<-seq(1:ncol(toto))
  if(scale)toto<-scale(toto, center = F, scale = TRUE)
  if(!ggplot){
      heatmap.2(toto,Rowv = NA,Colv=F,trace="none",dendrogram = "none",key=T,margins=c(2,4),keysize=1.30,main=maintitle)
    }
  if(ggplot){
    titi<-melt(toto,value.name = "Intensity")
    colnames(titi)<-c("Individuals","Variables","Intensity")
    titi[,2]<-as.character(titi[,2])
    ggplot(titi, aes( Variables, Individuals,fill = Intensity),colour=NA) + geom_raster()+ggtitle(maintitle)+theme(plot.title=element_text( size=15))
  }
}

#############
testfunction<-function(tabtransform,testparameters){
  #condition tests
  if (testparameters$SFtest){
    datatesthypothesis<-SFtest(tabtransform,shaptest=T,Ftest=T,threshold=0.05)
  }
  else{datatesthypothesis<-data.frame()}
  #diff test
  if(testparameters$test=="notest"){
    tabdiff<-tabtransform
    datatest<-NULL
    testparameters<-NULL
    useddata<-NULL}
  else{
    datatest<-diffexptest(toto = tabtransform,test = testparameters$test )
    #differential expressed
    logFC<-datatest[,5]
    if(testparameters$adjustpval){pval<-datatest[,3]}
    if(!testparameters$adjustpval){pval<-datatest[,2]}
    datatestdiff<-datatest[which( (pval<testparameters$thresholdpv)&abs(logFC)>testparameters$thresholdFC ),]
    if(dim(datatestdiff)[1]==0){
      print("no differentially expressed variables")
      tabdiff<<-data.frame()
    }
    else{
      indvar<-(colnames(tabtransform)%in%datatestdiff$name)
      indvar[1]<-T #keep the categorial variable
      tabdiff<<-tabtransform[,indvar]
    }
  useddata<-data.frame("names"=datatest[,1],"pval"=pval,"logFC"=datatest[,5],"mean1"=datatest[,9],"mean2"=datatest[,10])
  }
  return(list("tabdiff"=tabdiff,"datatest"=datatest,"hypothesistest"=datatesthypothesis,"useddata"=useddata,"testparameters"=testparameters))
}
  

diffexptest<-function(toto,test="Wtest"){ 
  #fonction test if the variables (in column) of toto (dataframe) are differently 
  #expressed according to the first variable (first column) (two groups : OP Tem)
  #test= Ttes: porsuit a sTudent test for each column (parmetric test), the sample have to be normal and with the same variance
  #Wtest : willcoxon test (nonparametric), assume that dispersion a on the same scale
  group<-toto[,1]
  toto<-toto[,-1]
  pval<-vector()
  adjustpval<-vector()
  mlev1<-vector()
  namelev1<-levels(group)[1]
  mlev2<-vector()
  namelev2<-levels(group)[2]
  FC1o2<-vector()
  FC2o1<-vector()
  auc<-vector()
  resyounden<-matrix(ncol = 4,nrow = ncol(toto))
  for (i in 1:max(1,ncol(toto)) ){
    lev1<-toto[which(group==namelev1),i]
    lev2<-toto[which(group==namelev2),i]
    mlev1[i]<-mean(lev1,na.rm = T)+0.0001
    mlev2[i]<-mean(lev2,na.rm = T)+0.0001
    
    FC1o2[i]<-mlev1[i]/mlev2[i]
    FC2o1[i]<-mlev2[i]/mlev1[i]
    auc[i]<-auc(roc(group,toto[,i],quiet=TRUE))
    resyounden[i,]<-younden(response = group,predictor = toto[,i])
    if( test=="Ttest"){pval[i]<-t.test(x = lev1,y = lev2)$p.value}
    else if( test=="Wtest"){pval[i]<-wilcox.test(lev1 ,lev2,exact = F)$p.value } 
  } 
  pval[which(is.na(pval))]<-1
  adjustpval<-p.adjust(pval, method = "BH")
  logFC1o2<-log2(abs(FC1o2))
  logFC2o1<-log2(abs(FC2o1))
  
  
  listgen<-data.frame(colnames(toto),pval,adjustpval,auc,FC1o2,logFC1o2,FC2o1,logFC2o1,mlev1,mlev2,resyounden) 
  colnames(listgen)<-c("name",paste("pval",test,sep = ""),paste("BHadjustpval",test,sep = ""),"AUC",paste("FoldChange ",namelev1,"/",namelev2,sep = ""),paste("logFoldChange ",namelev1,"/",namelev2,sep = ""),
                       paste("FoldChange ",namelev2,"/",namelev1,sep = ""),paste("logFoldChange ",namelev2,"/",namelev1,sep = ""),paste("mean",namelev1,sep = ""),paste("mean",namelev2,sep = ""),
                       "younden criterion","sensibility younden","specificity younden","threshold younden") 
  return(listgen)
}

younden<-function(response,predictor){
  res<-roc(response,predictor,quiet=T)
  youndenscore<-res$sensitivities+res$specificities-1
  best<-which(youndenscore==max(youndenscore))[1] # Only the first best is kept
  youndenbest<-youndenscore[best]
  sensiyounden<-res$specificities[best]
  speciyounden<-res$sensitivities[best]
  thresholdyounden<-res$thresholds[best]
  return(c(youndenbest,sensiyounden,speciyounden,thresholdyounden))
}


volcanoplot<-function(logFC,pval,thresholdFC=0,thresholdpv=0.05,graph=T,maintitle="Volcano plot",completedata){
  ##Highlight genes that have an absolute fold change > 2 and a p-value < Bonferroni cut-off
  
  threshold <- (as.numeric(abs(logFC) > thresholdFC &pval< thresholdpv ) +1)*2
  listgen<-data.frame("logFC"=logFC,"pval"=pval,"threshold"=threshold)
  if(!graph){return(completedata)}
  ##Construct the plot object
  g = ggplot(data=listgen, aes(x=logFC, y=-log10(pval))) +
    geom_point(alpha=0.4, size=1.75, colour=threshold) +
    theme(legend.position = "none") +
    #xlim(c(-(max(listgen$logFC)+0.2), max(listgen$logFC)+0.2)) + ylim(c(0, max(-log10(listgen$pval))+0.2)) +
    xlab("log2 fold change") + ylab("-log10 p-value")+
    ggtitle(maintitle)+theme(plot.title=element_text( size=15))+
    annotate("text",x=Inf,y=Inf,label=paste(substring(colnames(completedata)[3],first=4)),size=6,vjust=2,hjust=1.5)
  
  g
} 

barplottest<-function(feature,logFC,levels,pval,mean1,mean2,thresholdpv=0.05,thresholdFC=1,graph=T,maintitle="Mean by group for differentially expressed variables"){
  feature<-rep(feature,each=2)
  group<-rep(c(levels[1],levels[2]),times=(length(feature)/2))
  group<-factor(group,levels =c(levels[1],levels[2]))
  pval2<-rep((pval< thresholdpv),each=2)
  logFC2<-rep((abs(logFC)> thresholdFC),each=2) 
  mean<-vector() 
  mean[seq(from=1,to=length(feature),by = 2)]<-mean1
  mean[seq(from=2,to=length(feature),by = 2)]<-mean2
  data<-data.frame(feature,group,pval,logFC,mean,logFC2,pval2)
  data<-data[order(data$pval),]
  if(!graph){
    data<-data[order(data[,1]),]
    return(data[which((data$pval2==TRUE)& (data$logFC2==TRUE)),c(1,2,5)])}
  else{
    ggplot(data[which( ( data$pval2) & (data$logFC2) ),], aes(feature, mean,fill=group))+geom_bar(stat="identity", position="dodge")+ 
      ggtitle(maintitle)+theme(plot.title=element_text( size=15))
  }
}
errorplot<-function(text=paste("error /n","text error")){
  plot(c(0, 1), c(0, 1), ann = F, bty = 'n', type = 'n', xaxt = 'n', yaxt = 'n')
  text(x = 0.5, y = 0.5, text,cex = 1.6, col = "black")}

barplottestSF<-function(toto,graph=T){
  #toto: dataframe res from conditiontest function
  if(!graph){return(toto)}
  rescond<-vector()
  for (i in (1:nrow(toto))){
    if(toto$samplenorm[i]=="norm" & toto$varequal[i]!="varequal"){rescond[i]<-"norm"}
    else if(toto$samplenorm[i]=="norm" & toto$varequal[i]=="varequal"){rescond[i]<-"both"}
    else if( toto$samplenorm[i]!="norm" &toto$varequal[i]=="varequal"){rescond[i]<-"varequal"}
    else{rescond[i]<-"none"}
    
  }
  data<-as.factor(rescond)
  p<-qplot(factor(data), geom="bar", fill=factor(data))
  p+ggtitle("Repartition of the variables according to the test results")+
    theme(plot.title=element_text(size=15))
}

SFtest<-function(toto,shaptest=T,Ftest=T,threshold=0.05){
  x<-toto[,1]
  toto<-toto[,-1]
  pvalF<-vector()
  pvalnormlev1<-vector()
  pvalnormlev2<-vector()
  vlev1<-vector()
  vlev2<-vector()
  samplenorm<-vector()
  varequal<-vector()
  conditiontest<-data.frame("name"=colnames(toto))
  for (i in 1:ncol(toto) ){
    lev1<-toto[which(x==levels(x)[1]),i]
    lev2<-toto[which(x==levels(x)[2]),i]
    if(shaptest){
      #pvalnormTem[i]<-shapiro.test(Tem)$p.value
      
      out<- tryCatch(shapiro.test(lev1)$p.value, error = function(e) e)
      if(any(class(out)=="error"))pvalnormlev1[i]<-1
      else{pvalnormlev1[i]<-out}
      
      out<- tryCatch(shapiro.test(lev2)$p.value, error = function(e) e)
      if(any(class(out)=="error"))pvalnormlev2[i]<-1
      else{pvalnormlev2[i]<-out}
      
      if((pvalnormlev2[i]>=threshold) & (pvalnormlev1[i]>=threshold)){samplenorm[i]<-"norm"}
      else{samplenorm[i]<-"notnorm"}
    }
    if(Ftest){
      #to perform a fisher test the value have to be normal
      pvalF[i]<-var.test(lev1,lev2)$p.value
      if(is.na(pvalF[i]))pvalF[i]<-1
      vlev1[i]<-var(lev1)
      vlev2[i]<-var(lev2)
      if(pvalF[i]>=threshold){varequal[i]<-"varequal"}
      else{varequal[i]<-"varnotequal"}
    }
  }
  if(shaptest){ conditiontest<-data.frame(conditiontest,pvalnormlev1,pvalnormlev2,"samplenorm"=samplenorm)
                colnames(conditiontest)<-c("names",paste("pvalshapiro",levels(x)[1],sep=""),paste("pvalshapiro",levels(x)[2],sep = ""),"samplenorm")
  }
  if(Ftest){conditiontest<-data.frame(conditiontest,"pvalF"=pvalF,"variancelev1"=vlev1,"variancelev2"=vlev2,"varequal"=varequal)}
  return(conditiontest) 
}

####

modelfunction<-function(learningmodel,validation=NULL,modelparameters,transformdataparameters,datastructuresfeatures=NULL,learningselect){
  if(modelparameters$modeltype!="nomodel"){
    colnames(learningmodel)[1]<-"group"
    
    if(modelparameters$invers){
      learningmodel[,1]<-factor(learningmodel[,1],levels = rev(levels(learningmodel[,1])),ordered = TRUE)
    }
    lev<-levels(x = learningmodel[,1])
    names(lev)<-c("positif","negatif")
    
    #Build model
    if (modelparameters$modeltype=="randomforest"){
  #model <- randomForest(learningmodel[,-1],learningmodel[,1],ntree=500,
   #                           importance=T,keep.forest=T)
      learningmodel<-as.data.frame(learningmodel[sort(rownames(learningmodel)),])
      
      x<-as.data.frame(learningmodel[,-1])
      colnames(x)<-colnames(learningmodel)[-1]
      x<-as.data.frame(x[,sort(colnames(x))])
      set.seed(20011203)
      model<- randomForest(x = x,y = learningmodel[,1],ntree = 1000,importance = T)
      
      #model<-tuneRF(x = x,y = learningmodel[,1],doBest=T,importance=T,plot=F,trace=F,ntreeTry = 500)
      if(modelparameters$fs){
        featureselect<-selectedfeature(model=model,modeltype = "randomforest",tab=learningmodel,
                                       criterionimportance = "fscore",criterionmodel = "auc")
        model<-featureselect$model
        learningmodel<-featureselect$dataset
      }
      
      scorelearning =data.frame(model$votes[,lev["positif"]])
      colnames(scorelearning)<-paste(lev[1],"/",lev[2],sep="")
      predictclasslearning<-factor(levels = lev) 
      predictclasslearning[which(scorelearning>=modelparameters$thresholdmodel)]<-lev["positif"]
      predictclasslearning[which(scorelearning<modelparameters$thresholdmodel)]<-lev["negatif"]
      predictclasslearning<-as.factor(predictclasslearning)
      #predictclasslearning==model$predicted
    }   
    
    if(modelparameters$modeltype=="svm"){
      model <- best.tune(svm,group ~ ., data = learningmodel,cross=min(dim(learningmodel)[1]-2,10) )   
      
      if(modelparameters$fs){
      
        featureselect<-selectedfeature(model=model,modeltype = "svm",tab=learningmodel,
                                       criterionimportance = "fscore",criterionmodel = "auc")
        model<-featureselect$model
        learningmodel<-featureselect$dataset
      }
      scorelearning <-model$decision.values
      if(sum(lev==(strsplit(colnames(scorelearning),split = "/")[[1]]))==0){
        scorelearning<-scorelearning*(-1)
        colnames(scorelearning)<-paste(lev[1],"/",lev[2],sep="")
      }
      predictclasslearning<-factor(levels = lev) 
      predictclasslearning[which(scorelearning>=modelparameters$thresholdmodel)]<-lev["positif"]
      predictclasslearning[which(scorelearning<modelparameters$thresholdmodel)]<-lev["negatif"]
      predictclasslearning<-as.factor(predictclasslearning)
    }
    
    #levels(predictclassval)<-paste("test",levels(predictclasslearning),sep="")
    levels(predictclasslearning)<-paste("test",lev,sep="")
    classlearning<-learningmodel[,1]
    
    reslearningmodel<-data.frame(classlearning,scorelearning,predictclasslearning)
    colnames(reslearningmodel) <-c("classlearning","scorelearning","predictclasslearning") 
    datalearningmodel<-list("learningmodel"=learningmodel,"reslearningmodel"=reslearningmodel)
    
    if (modelparameters$adjustval){
      #Validation
      colnames(validation)[1]<-"group"
      validationdiff<-validation[,which(colnames(validation)%in%colnames(learningmodel))]
      learningselect2<-learningselect
      if(transformdataparameters$log) { 
        validationdiff[,-1]<-transformationlog(x = validationdiff[,-1]+1,logtype =transformdataparameters$logtype )
        learningselect2[,-1]<-transformationlog(x = learningselect2[,-1]+1,logtype=transformdataparameters$logtype)}
      if(transformdataparameters$arcsin){
        maxlearn<-apply(X = learningselect[,-1],MARGIN = 2,FUN = max,na.rm=T)
        minlearn<-apply(X = learningselect[,-1],MARGIN = 2,FUN = min,na.rm=T)
        for (i in 2:dim(validationdiff)[2]){
        validationdiff[,i]<-(validationdiff[,i]-minlearn[i-1])/(maxlearn[i-1]-minlearn[i-1])
        #validationdiff[,-1]<-apply(X = as.data.frame(validationdiff[,-1]),MARGIN = 2,FUN = function(x){{(x-min(x,na.rm = T))/(max(x,na.rm = T)-min(x,na.rm = T))}})
        validationdiff[which(validationdiff[,i]>1),i]<-1
        validationdiff[which(validationdiff[,i]<0),i]<-0
        validationdiff[,i]<-asin(sqrt(validationdiff[,i]))
        }     
        learningselect2[,-1]<-apply(X = learningselect2[,-1],MARGIN = 2,FUN = function(x){{(x-min(x,na.rm = T))/(max(x,na.rm = T)-min(x,na.rm = T))}})
        learningselect2[,-1]<-asin(sqrt(learningselect2[,-1]))
      }
      if(transformdataparameters$standardization){
        learningselectval<<-learningselect2
        sdselect<-apply(learningselect2[,which(colnames(learningselect2)%in%colnames(validationdiff))], 2, sd,na.rm=T)
        print('sdselect')
        print(sdselect)
        validationdiff[,-1]<-scale(validationdiff[,-1],center=F,scale=sdselect[-1])
      }

      #NAstructure if NA ->0
      if(!is.null(datastructuresfeatures)){
        validationdiff[which(is.na(validationdiff),arr.ind = T)[which(which(is.na(validationdiff),arr.ind = T)[,2]%in%which(colnames(validationdiff)%in%datastructuresfeatures$names)),]]<-0
      }
      #
      validationmodel<<- replaceNAvalidation(as.data.frame(validationdiff[,-1]),toto=as.data.frame(learningmodel[,-1]),rempNA=transformdataparameters$rempNA)
      colnames(validationmodel)<-colnames(validationdiff)[-1]
      rownames(validationmodel)<-rownames(validationdiff)
      
      #prediction a partir du model
      if(modelparameters$modeltype=="randomforest"){
        scoreval <-predict(object=model,type="prob",newdata = validationmodel)[,lev["positif"]]
        predictclassval<-vector(length = length(scoreval) ) 
        predictclassval[which(scoreval>=modelparameters$thresholdmodel)]<-lev["positif"]
        predictclassval[which(scoreval<modelparameters$thresholdmodel)]<-lev["negatif"]
        predictclassval<-as.factor(predictclassval)
        
      }
      
      if(modelparameters$modeltype=="svm"){
        scoreval =attr(predict(model,newdata =  validationmodel,decision.values=T),"decision.values")
        if(sum(lev==(strsplit(colnames(scoreval),split = "/")[[1]]))==0){scoreval<-scoreval*(-1)}
        predictclassval<-vector(length = length(scoreval) ) 
        predictclassval[which(scoreval>=modelparameters$thresholdmodel)]<-lev["positif"]
        predictclassval[which(scoreval<modelparameters$thresholdmodel)]<-lev["negatif"]
        predictclassval<-as.factor(predictclassval)
      }
      
      if(sum(lev==(levels(predictclassval)))==0){
        predictclassval<-factor(predictclassval,levels = rev(levels(predictclassval)),ordered = TRUE)
      }
      classval<- validation[,1]
      if(sum(lev==(levels(classval)))==0){
        classval<-factor(classval,levels = rev(levels(classval)),ordered = TRUE)
      }
      
      #levels(predictclassval)<-paste("test",levels(predictclassval),sep="")
      levels(predictclassval)<-paste("test",lev,sep="")
      resvalidationmodel<-data.frame(classval,scoreval,predictclassval)
      colnames(resvalidationmodel) <-c("classval","scoreval","predictclassval") 
      auc<-auc(roc(as.vector(classval), as.vector(scoreval),quiet=T))
      datavalidationmodel<-list("validationdiff"=validationdiff,"validationmodel"=validationmodel,"resvalidationmodel"=resvalidationmodel,"auc"=auc)
      
    }
    else{datavalidationmodel<-list()}
    res<-list("datalearningmodel"=datalearningmodel,"model"=model,"datavalidationmodel"=datavalidationmodel,"groups"=lev,"parameters"=modelparameters)
  }
}


replaceNAvalidation<-function(validationdiff,toto,rempNA){
  validationdiffssNA<-validationdiff
  for(i in 1:nrow(validationdiff)){
    validationdiffssNA[i,]<-replaceNAoneline(lineNA = validationdiff[i,],toto = toto,rempNA =rempNA)
  }
  return(validationdiffssNA)
}

replaceNAoneline<-function(lineNA,toto,rempNA){
  alldata<-rbind(lineNA,toto)
  if(rempNA=="moygr"){ 
    #print("impossible de remplacer les NA par la moyenne par group pour la validation")
    linessNA<-replaceNA(toto = cbind(rep(0,nrow(alldata)),alldata),rempNA ="moy")[1,-1]        }
  
  else{linessNA<-replaceNA(toto = cbind(rep(0,nrow(alldata)),alldata),rempNA =rempNA)[1,-1]}
  
  return(linessNA)
}




ROCcurve<-function(validation,decisionvalues,maintitle="Roc curve",graph=T,ggplot=T){
  validation<-factor(validation,levels = rev(levels(validation)),ordered = TRUE)
  
  #argument : validation, vector of appartenance,
  #            decisionvalues, vector of scores
  #fulldata<-rocdata(grp = validation, pred = as.vector(decisionvalues))
  data<-roc(validation,decisionvalues)
  if(!graph){return(data.frame("sensitivity"=data$sensitivities,"specificity"=data$specificities,"thresholds"=data$thresholds))}
  if(!ggplot){plot(data)}
  if(ggplot){
    y<-rev(data$sensitivities)
    x<-rev(data$specificities)
    roc<-data.frame(x,y)
    auc<-as.numeric(auc(data))
    
    col<-gg_color_hue(3)
    roccol<-col[1]
    bin = 0.01
    diag = data.frame(x = seq(0, 1, by = bin), y = rev(seq(0, 1, by = bin)))
    p <- ggplot(data = roc, aes(x = x, y = y)) + 
      geom_point(color = roccol) +
      geom_line(color = roccol) + 
      geom_line(data = diag, aes(x = x, y = y), color =col[3])
    sp = 19
    f <- p + geom_point(data = diag, aes(x = x, y = y), color = "lightgrey",
                        shape = sp) + theme(axis.text = element_text(size = 16),
                                            title = element_text(size = 15)) + labs(x = "Sensitivity",
                                                                                    y = "Specificity", title = maintitle) +
      annotate("text",x=0.2,y=0.1,label=paste("AUC = ",as.character(round(auc,digits = 3))),size=7,colour= roccol)+
      scale_x_reverse()
    
    f
  }
}

scoremodelplot<-function(class,score,names,threshold,type,graph,printnames){
  class<-factor(class,levels =rev(levels(class)))

  if(type=="boxplot"){
    boxplotggplot(class =class,score =score,names=names,threshold=threshold,
                  graph = graph)
  }
  else if(type=="points"){
    plot_pred_type_distribution(class = class, score = score,names=names,threshold=threshold,graph=graph,printnames=printnames  )
  } 
}

boxplotggplot<-function(class,score,names,threshold,maintitle="Score representation ",graph=T){
  data<-data.frame("names"=names,"class"= class,"score"=as.vector(score))
  if(!graph){return(data)}
  p<-ggplot(data, aes(x=class, y=score)) +
    scale_fill_manual( values = c("#00BFC4","#F8766D") )+
    geom_boxplot(aes(fill=class))+
    geom_hline(yintercept=threshold, color='red', alpha=0.6)+
    ggtitle(maintitle)+theme(plot.title=element_text( size=15))
  
  p
}

plot_pred_type_distribution <- function(class,score,names, threshold,maintitle="Score representation",printnames=F,graph=T) {
  #in this function the levels of the class is inverted in order to have the control group on the left side of the graph
  df<-data.frame(names,class,score)
  colnames(df)<-c("names","class","score")
  v <-rep(NA, nrow(df))
  v <- ifelse(df$score >= threshold & df$class == levels(class)[2], "TruePositiv", v)
  v <- ifelse(df$score >= threshold & df$class == levels(class)[1], "FalsePositiv", v)
  v <- ifelse(df$score < threshold & df$class ==  levels(class)[2], "FalseNegativ", v)
  v <- ifelse(df$score < threshold & df$class == levels(class)[1], "TrueNegativ", v)
  
  df$predtype <-factor(v,levels = c("FalseNegativ","FalsePositiv","TrueNegativ","TruePositiv"),ordered = T)
  if(!graph){return(df)}
  set.seed(20011203)
  if(printnames){
    g<-ggplot(data=df, aes(x=class, y=score)) + 
      #geom_violin(fill=rgb(1,1,1,alpha=0.6), color=NA) + 
      geom_text(label=names,colour=palet(df$predtype,multiple = TRUE))+
      geom_jitter(aes(color=predtype), alpha=0.6) +
      geom_hline(yintercept=threshold, color="red", alpha=0.6) +
      scale_color_manual(values=palet(predtype = df$predtype),name="") +
      ggtitle(maintitle)+theme(plot.title=element_text( size=15), legend.position ="bottom")
  }
  else{
    g<-ggplot(data=df, aes(x=class, y=score)) + 
      #geom_violin(fill=rgb(1,1,1,alpha=0.6), color=NA) + 
      geom_jitter(aes(color=predtype), alpha=0.6) +
      geom_hline(yintercept=threshold, color="red", alpha=0.6) +
      scale_color_manual(values=palet(predtype = df$predtype),name="") +
      ggtitle(maintitle)+theme(plot.title=element_text( size=15), legend.position ="bottom")
  }
  g
  
}

palet<-function(predtype,multiple=FALSE){
  if(multiple){col<-as.character(predtype)}
  else{col<-sort(unique(as.character(predtype)))}
  col[which(col=="FalseNegativ")]<-"#C77CFF"
  col[which(col=="FalsePositiv")]<-"#00BA38"
  col[which(col=="TrueNegativ")]<-"#00BFC4"
  col[which(col=="TruePositiv")]<-"#F8766D"
  return(col)
}


selectedfeature<-function(model,modeltype,tab,validation,criterionimportance,criterionmodel,fstype="learn"){
  rmvar<-testmodel(model=model,modeltype = modeltype,tab=tab,validation=validation,
                   criterionimportance = criterionimportance,criterionmodel = criterionmodel,fstype=fstype)
  i=0
  tabdiff2<-tab
  while(rmvar!=0){
    i<-i+1
    print(paste(i,"eliminates features"))
    tabdiff2<-tabdiff2[,-rmvar]
    if(modeltype=="svm"){model<- best.tune(svm,train.y=tabdiff2[,1] ,train.x=tabdiff2[,-1],cross=min(dim(tabdiff2)[1]-2,10))}
    if (modeltype=="randomforest"){      
      tabdiff2<-as.data.frame(tabdiff2[,c(colnames(tabdiff2)[1],sort(colnames(tabdiff2[,-1])))])
      tabdiff2<-as.data.frame(tabdiff2[sort(rownames(tabdiff2)),])
      
      set.seed(20011203)
      model <- randomForest(tabdiff2[,-1],tabdiff2[,1],ntree=1000,importance=T,keep.forest=T)}
      rmvar<-testmodel(model=model,modeltype = modeltype,tab=tabdiff2,validation=validation,
                     criterionimportance = criterionimportance,criterionmodel = criterionmodel,fstype=fstype)
  }
  res<-list("dataset"=tabdiff2,"model"=model)
  return(res)
}


testmodel<-function(model,modeltype,tab,validation,criterionimportance,criterionmodel,fstype){
  #retourn la variable a enlever
  importancevar<-importancemodelsvm(model = model,modeltype=modeltype,tabdiff=tab,criterion = criterionimportance)
  lessimportantevar<-which(importancevar==min(importancevar,na.rm =T) )
  test<-vector()
  if(modeltype=="svm"){
    if(criterionmodel=="BER"){bermod<-BER(class = tab[,1],classpredict = model$fitted)}
    if(criterionmodel=="auc"){
      if (fstype=='learn'){aucmod<-auc(roc(tab[,1], as.vector(model$decision.values),quiet=T))}
      if (fstype=='val'){
        print("")
        #predict sur la validation
        #mais pour ca validation doit etre = a validationmodel, avec toute les transformation
        }}
    for(i in 1:length(lessimportantevar)){
      tabdiff2<-tab[,-lessimportantevar[i]]
      resmodeldiff<- best.tune(svm,train.y=tabdiff2[,1] ,train.x=tabdiff2[,-1],cross=min(dim(tabdiff2)[1]-2,10))
      if(criterionmodel=="accuracy"){test[i]<-resmodeldiff$tot.accuracy-model$tot.accuracy}
      if(criterionmodel=="BER"){
        #print(paste("Ber test :",BER(class = tabdiff2[,1],classpredict = resmodeldiff$fitted) ))
        test[i]<-bermod-BER(class = tabdiff2[,1],classpredict = resmodeldiff$fitted)}
      if(criterionmodel=="auc"){
        test[i]<-auc(roc(tabdiff2[,1], as.vector(resmodeldiff$decision.values),quiet=T))-aucmod}
    }}
  if(modeltype=="randomforest"){
    if(criterionmodel=="BER"){bermod<-BER(class = tab[,1],classpredict = model$predicted)}
    if(criterionmodel=="auc"){aucmod<-auc(roc(tab[,1], as.vector(model$votes[,1]),quiet=T))}
    for(i in 1:length(lessimportantevar)){
      tabdiff2<-tab[,-lessimportantevar[i]]
      tabdiff2<-as.data.frame(tabdiff2[,c(colnames(tabdiff2)[1],sort(colnames(tabdiff2[,-1])))])
      tabdiff2<-as.data.frame(tabdiff2[sort(rownames(tabdiff2)),])
      
      set.seed(20011203)
      resmodeldiff <-randomForest(tabdiff2[,-1],tabdiff2[,1],ntree=1000,importance=T,keep.forest=T,trace=T)
      if(criterionmodel=="accuracy"){test[i]<-mean(resmodeldiff$confusion[,3])-mean(model$confusion[,3])}
      if(criterionmodel=="BER"){
        test[i]<-bermod-BER(class = tabdiff2[,1],classpredict = resmodeldiff$predicted)}
      if(criterionmodel=="auc"){
        test[i]<-auc(roc(tabdiff2[,1], as.vector(resmodeldiff$votes[,1]),quiet=T))-aucmod}
    }
  }
  #print(paste("test :",max(test)))
  if(max(test)>=0){num<-lessimportantevar[which(test==max(test))[1]]}
  else(num<-0)
  #print(paste( "num", num))
  return(num)
} 

importancemodelsvm<-function(model,modeltype,tabdiff,criterion){
  #function calculate the importance of each variable of the model
  #first column of tabdiff is the group
  importancevar<-vector()
  if(criterion=="accuracy"){
    if(modeltype=="svm"){
      for (i in 2:ncol(tabdiff)){
        vec<-vector()
        tabdiffmodif<-tabdiff
        for( j in 1:20){
          tabdiffmodif[,i]<-tabdiffmodif[sample(1:nrow(tabdiff)),i]
          #tabdiffmodif<-tabdiffmodif[,-i]
          
          resmodeldiff<-svm(y =tabdiffmodif[,1],x=tabdiffmodif[,-1],cross=10,type ="C-classification", kernel="radial",cost=model$cost,gamma=model$gamma)
          vec[j]<-abs(resmodeldiff$tot.accuracy-model$tot.accuracy)
        }
        importancevar[i]<-mean(vec)}
      
    }
    if(modeltype=="randomforest"){
      
      tabdiff<-as.data.frame(tabdiff[,c(colnames(tabdiff)[1],sort(colnames(tabdiff[,-1])))])
      tabdiff<-as.data.frame(tabdiff2[sort(rownames(tabdiff)),])
      
      set.seed(20011203)
      model <- randomForest(tabdiff[,-1],tabdiff[,1],ntree=1000,importance=T,keep.forest=T)
      importancevar<-model$importance[,4]
      importancevar<-c(NA,importancevar)
    }
  }
  if(criterion=="fscore"){
    importancevar<-Fscore(tab = as.data.frame(tabdiff[,-1]),class=tabdiff[,1])
  }
  return(importancevar)
}
Fscore<-function(tab,class){
  tabpos<-as.data.frame(tab[which(class==levels(class)[1]),])
  npos<-nrow(tabpos)
  tabneg<-as.data.frame(tab[which(class==levels(class)[2]),])
  nneg<-nrow(tabneg)
  fscore<-vector()
  for(i in 1:ncol(tab)){
    moypos<-mean(tabpos[,i])
    moyneg<-mean(tabneg[,i])
    moy<-mean(tab[,i])
    numerateur<-(moypos-moy)^2+(moyneg-moy)^2
    denominateur<-(sum((tabpos[,i]-moypos)^2)*(1/(npos-1)))+(sum((tabneg[,i]-moyneg)^2)*(1/(nneg-1)))
    fscore[i]<-numerateur/denominateur
  }
  return(c(NA,fscore))
}

BER<-function(class,classpredict){
  pos<-which(class==levels(class)[1])
  neg<-which(class==levels(class)[2])
  (1/2)*( sum(class[pos]!=classpredict[pos])/length(pos)+ sum(class[neg]!=classpredict[neg])/length(neg)  )
}

nll<-function(element){
  if(is.null(element)){return("")}
  else{return(element)}
}

sensibility<-function(predict,class){
data<-table(predict,class)
sensi<-round(data[1,1]/(data[1,1]+data[2,1]),digits = 3)
return(sensi)
}
specificity<-function(predict,class){
  data<-table(predict,class )
  round(data[2,2]/(data[1,2]+data[2,2]),digit=3)
}

constructparameters<-function(listparameters){
  resparameters<-data.frame(listparameters[[1]])
  namescol<-names(listparameters)
  
  for(i in 2:length(listparameters)){
    tt<-rep(listparameters[[i]],each=nrow(resparameters))
    res<-resparameters
    if(length(listparameters[[i]])>1){
      for (j in 1:(length(listparameters[[i]])-1)){
        res<-rbind(res,resparameters)
      }
    }
    resparameters<-cbind(res,tt)
  }
  colnames(resparameters)<-namescol
  return(resparameters)
}

testparametersfunction<-function(learning,validation,tabparameters){
  results<-matrix(data = NA,nrow =nrow(tabparameters), ncol=9 )
  colnames(results)<-c("auc validation","sensibility validation","specificityvalidation","auc learning","sensibility learning","specificity learning","number of features in model","number of differented features","number of features selected")
  print(paste(nrow(tabparameters),"parameters "))
  for (i in 1:nrow(tabparameters)){
    print(i)
    parameters<-tabparameters[i,]
    if(!parameters$NAstructure){tabparameters[i,c("thresholdNAstructure","structdata","maxvaluesgroupmin","minvaluesgroupmax")]<-rep(x = NA,4)    }
    #selectdataparameterst<-parameters[1:7]
    selectdataparameters<<-list("prctvalues"=parameters$prctvalues,"selectmethod"=parameters$selectmethod,"NAstructure"=parameters$NAstructure,"structdata"=parameters$structdata,
                                "thresholdNAstructure"=parameters$thresholdNAstructure,"maxvaluesgroupmin"=parameters$maxvaluesgroupmin,"minvaluesgroupmax"=parameters$minvaluesgroupmax)
    resselectdata<<-selectdatafunction(learning = learning,selectdataparameters = selectdataparameters)
    
    #transformdataparameters<<-parameters[8:11]
    if(!parameters$log){tabparameters[i,"logtype"]<-NA}
    transformdataparameters<<-list("log"=parameters$log,"logtype"=parameters$logtype,"standardization"=parameters$standardization,"arcsin"=parameters$arcsin,"rempNA"=parameters$rempNA)
    
    learningtransform<-transformdatafunction(learningselect = resselectdata$learningselect,structuredfeatures = resselectdata$structuredfeatures,
                                             datastructuresfeatures =   resselectdata$datastructuresfeatures,transformdataparameters = transformdataparameters)
    
    testparameters<<-list("SFtest"=FALSE,"test"=parameters$test,"adjustpval"=as.logical(parameters$adjustpv),"thresholdpv"=parameters$thresholdpv,"thresholdFC"=parameters$thresholdFC)
    restest<<-testfunction(tabtransform = learningtransform,testparameters = testparameters)
    
    if(parameters$test=="notest"){
      learningmodel<-learningtransform
      tabparameters[i,c("adjustpv","thresholdpv","thresholdFC")]<-rep(x = NA,3)
    }
    else{learningmodel<-restest$tabdiff}
    
    if(ncol(learningmodel)!=0){
    modelparameters<<-list("modeltype"=parameters$model,"invers"=FALSE,"thresholdmodel"=parameters$thresholdmodel,"fs"=as.logical(parameters$fs),"adjustval"=!is.null(validation))
    validate(need(ncol(learning)!=0,"No select dataset"))
    

    #resmodel<<-modelfunction(learningmodel = learningmodel,validation = validation,modelparameters = modelparameters,
    #                         transformdataparameters = transformdataparameters,datastructuresfeatures =  datastructuresfeatures)
    out<- tryCatch(modelfunction(learningmodel = learningmodel,validation = validation,modelparameters = modelparameters,
                                 transformdataparameters = transformdataparameters,datastructuresfeatures =  datastructuresfeatures,
                                 learningselect = resselectdata$learningselect), error = function(e) e)
    if(any(class(out)=="error"))parameters$model<-"nomodel"
    else{resmodel<-out}
    }
    else{parameters$model<-"nomodel"}
    #numberfeaturesselected
    results[i,9]<-positive(dim(resselectdata$learningselect)[2]-1)
    #numberfeaturesdiff
    if(parameters$test!="notest"){
      results[i,8]<-positive(dim(restest$tabdiff)[2]-1)
    }
    #numberfeaturesmodel
    if(parameters$model!="nomodel"){
      results[i,7]<-dim(resmodel$datalearningmodel$learningmodel)[2]-1
      #auclearning
      results[i,4]<-round(as.numeric(auc(roc(resmodel$datalearningmodel$reslearningmodel$classlearning,resmodel$datalearningmodel$reslearningmodel$scorelearning,quiet=T))),digits = 3)
      #sensibilitylearning
      results[i,5]<-sensibility(resmodel$datalearningmodel$reslearningmodel$predictclasslearning,resmodel$datalearningmodel$reslearningmodel$classlearning)
      #specificitylearning
      results[i,6]<-specificity(resmodel$datalearningmodel$reslearningmodel$predictclasslearning,resmodel$datalearningmodel$reslearningmodel$classlearning)
      if(!is.null(validation)){
      #aucvalidation
      results[i,1]<-round(as.numeric(auc(roc(resmodel$datavalidationmodel$resvalidationmodel$classval,resmodel$datavalidationmodel$resvalidationmodel$scoreval,quiet=T))),digits = 3)
      #sensibilityvalidation
      results[i,2]<-sensibility(resmodel$datavalidationmodel$resvalidationmodel$predictclassval,resmodel$datavalidationmodel$resvalidationmodel$classval)
      #specificityvalidation
      results[i,3]<-specificity(resmodel$datavalidationmodel$resvalidationmodel$predictclassval,resmodel$datavalidationmodel$resvalidationmodel$classval)
    }
    }
  }
  return(cbind(results,tabparameters))
}

##
importanceplot<-function(model,learningmodel,modeltype,graph=T){
  validate(need(!is.null(model),"No model"))
  validate(need(ncol(learningmodel)>2,"only one feature"))
  if(modeltype=="randomforest"){
    var_importance<- data.frame(variables=rownames(model$importance),
                                importance=as.vector(model$importance[,4]))

    varo<-var_importance[order(var_importance$importance,decreasing = T),1]
    var_importance$variables<-as.character(var_importance$variables)
    var_importance$variables<-factor(x =var_importance$variables,levels =varo  )
    
    p <- ggplot(var_importance, aes(x=variables, weight=importance,fill=variables))
    g<-p + geom_bar()+coord_flip()+ylab("Variable Importance (Mean Decrease in Gini Index)")+
      theme(legend.position="none",plot.title=element_text( size=15))+ggtitle("Importance of variables in the model")+scale_fill_grey()
  }
  if(modeltype=="svm"){
    importancevar<-importancemodelsvm(model = model,modeltype="svm",tabdiff=learningmodel,criterion = "fscore")
    
    var_importance<-as.data.frame(cbind(colnames(learningmodel),importancevar)[-1,])
    var_importance[,1]<-as.character(var_importance[,1])
    var_importance[,2]<-as.numeric(as.character(var_importance[,2]))
    colnames(var_importance)<-c("variables","importance")
    varo<-var_importance[order(var_importance$importance,decreasing = T),1]
    var_importance$variables<-as.character(var_importance$variables)
    var_importance$variables<-factor(x =var_importance$variables,levels =varo  )
    
    p <- ggplot(var_importance, aes(x=variables, weight=importance,fill=variables))
    g<-p + geom_bar()+coord_flip()+ylab("Variable Importance (fscore)")+theme(legend.position="none",plot.title=element_text( size=15))+ggtitle("Importance of variables in the model")+scale_fill_grey()
  }
  if(!graph){return(var_importance)}
  if(graph){
    g
  }
}


positive<-function(x){
  if(x<0){x<-0}
  else{x}
  return(x)
}

