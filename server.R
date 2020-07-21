# Kan Luo    ST558 Project3   7/18/2020
library(shiny)
library(shinydashboard)
library(plotly)
library(dplyr)
library(ggplot2)
library(knitr)
library(plyr)
library(DT)
library(readxl)
library(httr)

#Readin data from my Github url address
url1<-'https://github.com/luokan1227/Project3/raw/master/antibody.xlsx'
GET(url1, write_disk(tf <- tempfile(fileext = ".xlsx")))
antibody <- read_excel(tf)

#Readin data from local, adjust path if needed
#antibody <- read_excel("C:/Users/Luo/Documents/tmp/Project3/antibody.xlsx")

shinyServer(function(input, output, session)  {
    
    #Setup url link
    url <- a("RV144", href="https://www.hivresearch.org/rv144-trial")
    output$tab <- renderUI({
        tagList("RV144 Trial:", url)
    })
    
    ########### Code for Exploration Tab ##################
    
    #Support label for Isotype plot
    supp.reac <- c("HIV Env negative", "HIV Env positive")
    names(supp.reac) <- c(0,1)
    
    #Get reactive data
    getData <- reactive({
        newData <- antibody %>% filter(PTID == input$ptid)
    })
    
    getData2 <- reactive({
        newData2 <- antibody %>% filter(PTID == input$ptid2)
    })
    
    #Create the numeric summary table
    output$abnumber <- renderTable({
        newData <- getData()
        if(input$sample){  #check if the sample box selected or not
            if(input$env){  #if sample box selected, check if env box selected or not
                table(newData$PTID, newData$Sample, newData$Reactivity) 
                
            } else {  #env box not selected
                table(newData$PTID, newData$Sample)
            }
        } else {  #sample box not selected
            table(newData$PTID)
        }
    })
    
    #Create Isotype plot summary
    output$isotype <- renderPlotly({
        newData <- getData()
        if(input$sample){  #check if the sample box selected or not
            if(input$env){  #if sample box selected, check if env box selected or not
                #Create new data set for plotly plot
                newData0 <- newData %>% filter(Reactivity == 0)
                newData1 <- newData %>% filter(Reactivity == 1)
                #Create env negative and positive plot separately
                fig0 <- plot_ly(newData0, x = ~Isotype, color = ~Sample) %>% layout(xaxis = list(title = "Ig Isotype"), yaxis = list(title = "Count"))
                fig1 <- plot_ly(newData1, x = ~Isotype, color = ~Sample) %>% layout(title = "Ig Isotype of Env Negative (left) and Positive (right) antibodies", xaxis = list(title = "Ig Isotype"), yaxis = list(title = "Count"))
                #Combine the two plots, hide the legend of fig0
                subplot(style(fig0, showlegend = F), fig1)
            } else {  #env box not selected
                plot_ly(newData, x=~Isotype, color = ~Sample) %>% layout(title = "Ig Isotype by sample tissue", xaxis = list(title = "Ig Isotype"), yaxis = list(title = "Count"))
            }
        } else {  #sample box not selected
            plot_ly(newData, x = ~Isotype) %>% layout(title = "Ig Isotype  of all isolated antibodies", xaxis = list(title = "Ig Isotype"), yaxis = list(title = "Count"))
        }
    })
    
    #Create HCDR3 length vs heavy chain mutation rate plot summary
    output$hcdr3mut <- renderPlotly({
        newData <- getData()
        if(input$sample){  #check if the sample box selected or not
            if(input$env){  #if sample box selected, check if env box selected or not
                #Create new data set for plotly plot
                newData0 <- newData %>% filter(Reactivity == 0)
                newData1 <- newData %>% filter(Reactivity == 1)
                #Create env negative and positive plot separately
                fig3 <- plot_ly(newData0, x = ~HCDR3, y = ~Hmuated, type = "scatter", color = ~Sample) %>% layout(xaxis=list(title="HCDR3 Length"), yaxis=list(title="Heavy chain mutation rate"))
                fig4 <- plot_ly(newData1, x = ~HCDR3, y = ~Hmuated, type = "scatter", color = ~Sample) %>% layout(title = "HCDR3 vs H_mutation rate of Env Negative (left) and Positive (right) antibodies", xaxis=list(title="HCDR3 length"), yaxis=list(title="Heavy chain mutation rate"))
                #Combine the two plots, hide the legend of fig3
                subplot(style(fig3, showlegend = F), fig4)
            } else {  #env box not selected
                plot_ly(newData, x = ~HCDR3, y = ~Hmuated, type = "scatter", color = ~Sample) %>% layout(title="HCDR3 vs H_mutation rate of antibodies by sample tissue", xaxis=list(title="HCDR3 length"), yaxis=list(title="Heavy chain mutation rate"))
            }
        } else {  #sample box not selected
            plot_ly(newData, x = ~HCDR3, y = ~Hmuated, type = "scatter") %>% layout(title="HCDR3 vs H_mutation rate of all isolated antibodies", xaxis=list(title="HCDR3 length"), yaxis=list(title="Heavy chain mutation rate"))
        }
    })
    
    
    #UserenderUI/uiOutput to dynamic change subtitle
    output$subt1 <- renderUI({paste0(input$ptid, " antibody number")})
    output$subt2 <- renderUI({paste0(input$ptid, " Ig Isotype")})
    output$subt3 <- renderUI({paste0(input$ptid, " HCDR3 length vs H mutation rate")})
    
    #Download data option for the Exploration Tab
    #Table of selected data set
    output$table <- renderTable({
        getData()
    })
    output$downloadData <- downloadHandler(
        filename = function(){paste(input$ptid, "antibody", ".csv")},
        content = function(file){write.csv(getData(), file, row.names = FALSE)}
    )
########################Code for PCA Tab #################################
    #Screeplot for 1st sub-tab
    output$pov <- renderPlot({
        newData2 <- getData2()
        anti1 <- newData2 %>% select(HV, HCDR3, Hmuated, Lmutated)
        ScreePlot <- prcomp(anti1, center = TRUE, scale = TRUE)
        screeplot(ScreePlot, type="lines")
    })
    
    #download scree plot
    output$povdownload <- downloadHandler(
        filename = function(){paste(input$ptid2, "Scree Plot.png")},
        content = function(file){
            png(file)
            newData2 <- getData2()
            anti1 <- newData2 %>% select(HV, HCDR3, Hmuated, Lmutated)
            ScreePlot <- prcomp(anti1, center = TRUE, scale = TRUE)
            screeplot(ScreePlot, type="lines")
            dev.off()
        }
    )
    
    #Table for 1st  sub-tab
    output$povtable <- renderTable({
        newData2 <- getData2()
        anti1 <- newData2 %>% select(HV, HCDR3, Hmuated, Lmutated)
        ScreePlot <- prcomp(anti1, center = TRUE, scale = TRUE)
        ScreePlot[[2]]
    })
    #Text describtion for 1st  sub-tab
    output$povexp <- renderText({
        paste("Above picture represented the percentage of variance of the data set that can be explained by each components.  Eigenvectors represent the weight for each variables importance in the principal components.")
    })
    
    #Download data option for PCA tab
    output$table <- renderTable({
        getData2()
    })
    output$downloadData2 <- downloadHandler(
        filename = function(){paste(input$ptid2, "antibody", ".csv")},
        content = function(file){write.csv(getData(), file, row.names = FALSE)}
    )
    
    #UserenderUI/uiOutput to dynamic change subtitle
    output$subt4 <- renderUI({paste0(input$ptid2, " proportion of variable explained")})
    output$subt5 <- renderUI({paste0(input$ptid2, " Bioplot")})
    
    #Biplot
    output$bip <- renderPlot({
        newData2 <- getData2()
        anti1 <- newData2 %>% select(HV, HCDR3, Hmuated, Lmutated)
        PC1 <- prcomp(anti1, center = TRUE, scale = TRUE)
        #Use if condition to out put user selected algorithm of biplot
        if(input$hv && input$hcdr3){
            biplot(PC1, xlabs=rep(".", nrow(newData2)), choices=c(1,2), cex=1.2)} else if
        (input$hv && input$hmuated) {
            biplot(PC1, xlabs=rep(".", nrow(newData2)), choices=c(1,3), cex=1.2)} else if
        (input$hcdr3 && input$hmuated){
            biplot(PC1, xlabs=rep(".", nrow(newData2)), choices=c(2,3), cex=1.2)} else if
        (input$hv && input$lmutated){
            biplot(PC1, xlabs=rep(".", nrow(newData2)), choices=c(1,4), cex=1.2)} else if
        (input$hcdr3 && input$lmutated){
            biplot(PC1, xlabs=rep(".", nrow(newData2)), choices=c(2,4), cex=1.2)} else if
        (input$hmuated && input$lmutated){
            biplot(PC1, xlabs=rep(".", nrow(newData2)), choices=c(3,4), cex=1.2)
        } #Close all if conditions
    })
    
    #Download biplot
    output$bipdownload <- downloadHandler(
        filename = function(){paste(input$ptid2, "BIP.png")},
        content = function(file){
            png(file)
            newData2 <- getData2()
            anti1 <- newData2 %>% select(HV, HCDR3, Hmuated, Lmutated)
            PC1 <- prcomp(anti1, center = TRUE, scale = TRUE)
            #Use if condition to out put user selected algorithm of biplot
            if(input$hv && input$hcdr3){
                biplot(PC1, xlabs=rep(".", nrow(newData2)), choices=c(1,2), cex=1.2)} else if
            (input$hv && input$hmuated) {
                biplot(PC1, xlabs=rep(".", nrow(newData2)), choices=c(1,3), cex=1.2)} else if
            (input$hcdr3 && input$hmuated){
                biplot(PC1, xlabs=rep(".", nrow(newData2)), choices=c(2,3), cex=1.2)} else if
            (input$hv && input$lmutated){
                biplot(PC1, xlabs=rep(".", nrow(newData2)), choices=c(1,4), cex=1.2)} else if
            (input$hcdr3 && input$lmutated){
                biplot(PC1, xlabs=rep(".", nrow(newData2)), choices=c(2,4), cex=1.2)} else if
            (input$hmuated && input$lmutated){
                biplot(PC1, xlabs=rep(".", nrow(newData2)), choices=c(3,4), cex=1.2)
            } 
            dev.off()
        }
    )
    
    
    
    
    
    

    
###############Code for Model Tab##################
    #Create click plot
    output$mutclick <- renderPlot({
        plot(antibody$Hmuated, antibody$Lmutated, xlab = "H_Muatation", ylab = "L_mutation")
    })
    #Show Heavy chain mutation rate and light chain mutation rate been selected
    output$info <- renderText({
        paste0("H_Mutation=", input$plot_click$x, "\nL_Mutation=", input$plot_click$y)
    })
    
    #setup model data
    modeldata <- antibody %>% select(HV, HCDR3, Hmuated, Lmutated, Clone, Reactivity)
    
    #Clone model
    clonemodel<- glm(Clone ~ HV + HCDR3 + Hmuated + Lmutated, data=modeldata, family = "binomial")
    
    #Env reactivity model
    envreactmodel<- glm(Reactivity ~ HV + HCDR3 + Hmuated + Lmutated, data=modeldata, family = "binomial")
    
    #Export Model info output
    output$coef <- renderTable({
        if(input$choosemodel == "Clone Model"){
            data.frame(as.list(coef(clonemodel)))
        } else {data.frame(as.list(coef(envreactmodel)))}
    })
    
    #Export the predict results
    output$predictresult <- renderText({
        #if choose the clone model
        if(input$choosemodel == "Clone Model"){
            #predict clone model by input
            predictresult <- predict(clonemodel, newdata = data.frame(HV = input$heavyv, HCDR3 = input$modelhcdr3, Hmuated = input$plot_click$x, Lmutated = input$plot_click$y), type = "response", se.fit = TRUE)
            #output predict results by words
            if(predictresult[1] < 0.5) {
                response = "Non-Clone Lineage Related"} else{
                    response = "Clone Lineage Related"
                }
            paste(response)
            
        } else if(input$choosemodel == "Env Reactivity Model"){
            #predict env reactivity model by inputs
            predictresult2 <- predict(envreactmodel, newdata = data.frame(HV = input$heavyv, HCDR3 = input$modelhcdr3, Hmuated = input$plot_click$x, Lmutated = input$plot_click$y), type = "response", se.fit = TRUE)
            #Output predict results by words
            #In our data, most of the antibodies were isolated through antigen specific sort, tend to be envelope positive, the data set is kind of biased. So I adjust to <0.8 to make sure there is a fair ratio of envelope negative prediction come out for better show of function.
            if(predictresult2[1] < 0.8) {
                response2 = "HIV Envelope Negative"} else{
                    response2 = "HIV Envelope Positive"
                }
            paste(response2)
        }
    })
    
    
##################Code for Data Tab##################################
    
    #Download data option for the Data tab
    #Table of All data set
    getData3 <- reactive({newData3 <- antibody})
    #All antibody table that can scroll through
    output$table3 <- DT::renderDataTable({
        DT::datatable(getData3())
    })
    #For download
    output$downloadData3 <- downloadHandler(
        filename = function(){paste("All", "antibody", ".csv")},
        content = function(file){write.csv(getData3(), file, row.names = FALSE)}
    )
    
})
