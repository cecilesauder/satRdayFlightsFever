library(shiny)
library(fda.usc)
library(plyr)
library(Funclustering)

load("/media/KINGSTON/TabloAout2013.RData")

tab=join(TAB[,c(1:21,110:135)],tabLait, by="ID")
tabH=subset(tab,Race=="HF")
tabN=subset(tab,Race=="NO")

# Define server logic required to summarize and view the selected dataset
shinyServer(function(input, output) {
  
  # Return the requested dataset
  datasetInput <- reactive({
    switch(input$dataset,
           "Toutes" = tab,
           "Holstein" = tabH,
           "Normande" = tabN)
  })

  
  # Return the requested variable
  datasetVariableInput <- reactive({
    tab=datasetInput()
    tabPL=tab[,53:76]
    tabTB=tab[,141:164]
    tabTP=tab[,185:208]
    tabPV=tab[,229:252]
    tabNEC=tab[,22:28]
    switch(input$variable,
           "Production laitière"=tabPL, 
           "Taux Butyreux"=tabTB, 
           "Taux protéique"=tabTP, 
           "Poids vif"=tabPV,
           "Note d'état corporel"=tabNEC)
           })   
  
  
  # Expression that generates a plot of the distribution. The expression
  # is wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically 
  #     re-executed when inputs change
  #  2) Its output type is a plot 
  #
  output$fdaPlot <- renderPlot({
    
    # faire l'objet fda avec la base and plot it
    
    tabV=datasetVariableInput()
    sem=1:24
    argNEC=seq(-4,20, by=4)
    if(dim(tabV)[2]==24){arg<-sem}
    if(dim(tabV)[2]==7){arg<-argNEC }
    mini=min(arg)
    maxi=max(arg)

    basisobj = create.bspline.basis(c(mini,maxi), 
                                    nbasis=input$nbsplines,norder=6)
    lambda=input$lambda
    fdParobj = fdPar(fdobj=basisobj, Lfdobj=4, lambda=lambda)
    plfd=smooth.basis(arg,t(tabV),fdParobj=fdParobj)$fd
    par(mfrow=c(1,3))
    plot(plfd)
    x=tabV
    xhat=t(eval.fd(arg,plfd))
    n=ncol(x)
    tabAbs2= abs(xhat-x)^2
    mse=1/n*sum(apply(tabAbs2,1,sum))
    title(paste("MSE = ",round(mse,1)), cex.main = 2,   font.main= 4, col.main= "blue")
    plot(deriv.fd(plfd, 1))
    title(paste("lambda = ",input$lambda), cex.main = 2,   font.main= 4, col.main= "blue")
    plot(deriv.fd(plfd, 2))
    title(paste("nombre de splines = ",input$nbsplines), cex.main = 2,   font.main= 4, col.main= "blue")

  })
  
  output$fdaPlot2 <- renderPlot({
    
    # faire l'objet fda avec la base and plot it
    tab=datasetInput()
    tabV=datasetVariableInput()
    numID=input$id
    sem=1:24
    argNEC=seq(-4,20, by=4)
    if(dim(tabV)[2]==24){arg<-sem}
    if(dim(tabV)[2]==7){arg<-argNEC}
    mini=min(arg)
    maxi=max(arg)
    
    basisobj = create.bspline.basis(c(mini,maxi), nbasis=input$nbsplines,norder=6)
    lambda=input$lambda
    fdParobj = fdPar(fdobj=basisobj, Lfdobj=4, lambda=lambda)
    plfd=smooth.basis(arg,t(tabV[numID,]),fdParobj=fdParobj)$fd
    par(mfrow=c(1,3))
    plot(plfd, lwd=2)
    points(x=arg, y=tabV[numID,])  
    titre=paste("numID :",numID,  
                tab[numID,1],tab[numID,4],"\n","lot :",
                tab[numID,8]," rg lactation",
                tab[numID,52], sep=" ")
    title(titre, cex.main = 2,   font.main= 4, col.main= "blue")
    plot(deriv.fd(plfd, 1), lwd=2)
    plot(deriv.fd(plfd, 2), lwd=2)
    
#     #classification
#     #A FAIRE : proposer le nb de groupes à l'utilisateur
#     nbgroup=7
#     res=funclust(plfd, K=3)
#     clPL=res$cls
#     par(mfrow=c(1,3))
#     plot(plfd, lwd=2, col=clPL)
#     points(x=arg, y=tabV[numID,])  
#     titre=paste("numID :",numID,  
#                 tab[numID,1],tab[numID,4],"\n","lot :",
#                 tab[numID,8]," rg lactation",
#                 tab[numID,52], sep=" ")
#     title(titre, cex.main = 2,   font.main= 4, col.main= "blue")
#     plot(deriv.fd(plfd, 1), lwd=2,col=clPL)
#     plot(deriv.fd(plfd, 2), lwd=2,col=clPL)
    
  })
  

})