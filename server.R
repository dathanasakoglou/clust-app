library(shiny)
library(datasets)
library(rpart)
library(party)
library(fpc)
library(googleVis)
library(data.table)
library(rmarkdown)
library(knitr)
library(ggplot2)
library(highcharter)
library(tidyverse)


#Colors
palette(c("#E73032", "#377EB8", "#4DAF4A", "#984EA3",
          "#FF7F00", "#FFDD33", "#A65628", "#F781BF", "#999999"))


#Server
shinyServer(function(input, output, session) {
    
    datasetInput <- reactive({
        switch(input$dataset,
               "all iris data" = iris,
               "setosa" = subset(iris, iris$Species == "setosa"),
               "versicolor" = subset(iris, iris$Species == "versicolor"),
               "virginica" = subset(iris, iris$Species == "virginica"))
    })
    
    colX <- reactive({
        switch(input$Xvar,
               "Sepal.Length" = iris$Sepal.Length,
               "Sepal.Width" = iris$Sepal.Width,
               "Petal.Length" = iris$Petal.Length,
               "Petal.Width" = iris$Petal.Width)
    })
    
    colY <- reactive({
        switch(input$Yvar,
               "Sepal.Length" = iris$Sepal.Length,
               "Sepal.Width" = iris$Sepal.Width,
               "Petal.Length" = iris$Petal.Length,
               "Petal.Width" = iris$Petal.Width)
    })
    
    clusters <- reactive({
        kmeans(iris[,1:4], input$clusters)
    })
    
    myColors <- reactive({
        switch(input$dataset,
               "all iris data" = c(palette()[1],palette()[2],palette()[3]),
               "setosa" = palette()[1],
               "versicolor" = palette()[2],
               "virginica" = palette()[3])
    })

    
#---Value boxes---#
    output$rate <- renderValueBox({
        valueBox(
            value = 50,
            subtitle = "setosa",
            icon = icon("area-chart"),
            color = "red",
            href="https://en.wikipedia.org/wiki/Iris_setosa"
        )
    })
    
    output$count <- renderValueBox({
        valueBox(
            value = 50,
            subtitle = "Versicolor",
            icon = icon("area-chart"),
            color = "blue",
            href = "https://en.wikipedia.org/wiki/Iris_versicolor"
        )
    })
    
    output$users <- renderValueBox({
        valueBox(
            value = 50,
            subtitle = "Virginica",
            icon = icon("area-chart"),
            color = "green",
            href = "https://en.wikipedia.org/wiki/Iris_virginica"
        )
    })

    
#---Dashboard---#
    
    #Scatterplot
    output$overview <- renderPlot({
        df_iris <- datasetInput()
        
        plot(df_iris[,c(input$Xvar,input$Yvar)], xlab = input$Xvar, ylab = input$Yvar,
             main=toupper(ifelse(input$dataset == "all iris data", "iris", input$dataset)), pch=16, cex = 2,
             col = ifelse(df_iris$Species == "setosa", palette()[1], 
                          ifelse(df_iris$Species == "versicolor", palette()[2], palette()[3])) )
        
        legend("bottomright", legend = unique(df_iris[,5]), 
               col = myColors(), title = expression(bold("Iris.Species")),
               pch = 16, bty = "n", pt.cex = 2, 
               cex = 0.8, text.col = "black", horiz = FALSE, inset = c(0.05, 0.05))
    })
    
    #Boxplot
    output$boxPlot <- renderPlot({
        df_iris <- datasetInput()
        
        if (input$dataset == "all iris data") {
            boxplot(df_iris[,c(input$Yvar)] ~ df_iris[,5], xlab = "Species", ylab = input$Yvar, main = "IRIS", 
                    border = "black", col = myColors())
        }
        else {
            boxplot(df_iris[,c(input$Yvar)], xlab = "Species", ylab = input$Yvar, main = toupper(input$dataset),
                    border = "black", col = myColors())
        }
    })
    
    #PieChart
    output$pie <- renderGvis({
        types <- c("setosa", "versicolor", "virginica")
        freq <- c(50, 50, 50)
        df_iris2 <- data.frame(types, freq)
        gvisPieChart(df_iris2, chartid = "pie",
                     options = list(colors="['#E73032', '#377EB8', '#4DAF4A']"))
    })
    
    #Violin Plot
    output$violin <- renderPlot({
        iris %>%
            ggplot()+
            geom_violin(aes(x = Species, y = Petal.Length, fill = iris$Species))+
            theme_classic()
    })
    
    #Bar Plot
    output$bar <- renderHighchart({
        hchart(iris$Species, colorByPoint = TRUE, name = "Species") %>%
            hc_add_theme(hc_theme_google())
    })
    
    #Line Chart
    output$line <- renderPlot({
        ggplot(cbind(iris %>%
                        gather(feature_name, feature_value,
                        one_of(c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"))), id=1:nrow(iris))) +
                        geom_line(aes(x=feature_name, y=feature_value, group=id, colour=Species)) +
                        theme_classic()
    })
    
    #Density Plot
    output$dens <- renderPlot({
      ggplot(iris) +
        geom_density(aes(x=Petal.Length, colour=Species)) +
        theme_classic()
    })
    

#---About---#
    
    #Text
    output$text <- renderText("The Iris flower data set or Fishers Iris data set is a multivariate data set introduced by the British
                               statistician and biologist Ronald Fisher in his 1936 paper. The use of multiple measurements in taxonomic
                               problems as an example of linear discriminant analysis. It is sometimes called Andersons Iris data set
                               because Edgar Anderson collected the data to quantify the morphologic variation of Iris flowers of three
                               related species. Two of the three species were collected in the Gaspé Peninsula all from the same pasture,
                               and picked on the same day and measured at the same time by the same person with the same apparatus.
                               The data set consists of 50 samples from each of three species of Iris (Iris setosa, Iris virginica and Iris
                               versicolor). Four features were measured from each sample: the length and the width of the sepals and
                               petals, in centimetres. Based on the combination of these four features, Fisher developed a linear
                               discriminant model to distinguish the species from each other. Based on Fisher's linear discriminant model,
                               this data set became a typical test case for many statistical classification techniques in machine learning
                               such as support vector machines. The use of this data set in cluster analysis however is not common,
                               since the data set only contains two clusters with rather obvious separation. One of the clusters contains
                               Iris setosa, while the other cluster contains both Iris virginica and Iris versicolor and is not separable
                               without the species information Fisher used. This makes the data set a good example to explain the
                               difference between supervised and unsupervised techniques in data mining: Fisher's linear discriminant
                               model can only be obtained when the object species are known: class labels and clusters are not
                               necessarily the same."
    )
    #Image
    getPage<-function() {
      return(includeHTML("www/report.html"))
    }
    output$inc<-renderUI({getPage()
    })
    
#---Charts---#
    
    #Sankey     
    df <- data.frame(From=c(rep("A",3), rep("B", 3)),
                     To=c(rep(c("X", "Y", "Z"),2)),
                     Weight=c(5,7,6,2,9,4))
    
    output$sankey <- renderGvis({
        gvisSankey(df, from="From", to="To", weight="Weight", chartid="1")
        
    })
    
    #Bubble
    output$bubble <- renderGvis({
        gvisBubbleChart(Fruits, idvar="Fruit", 
                        xvar="Sales", yvar="Expenses",
                        colorvar="Year", sizevar="Profit", chartid = "2",
                        options=list(title = "An example of Bubble Chart",
                                     hAxis='{title: "Sales", minValue:75, maxValue:125}',
                                     vAxis='{title: "Expenses"}'))
    })
    
    #Scatter
    output$scatter <- renderGvis({
        gvisScatterChart(iris[, -5], chartid = "3", 
                         options=list(
                            legend="right",
                            lineWidth=0, pointSize=1,
                            colors="['#E73032', '#377EB8', '#4DAF4A']",
                            title="Example of Scatterplot using the Iris dataset", vAxis="{title:'length'}",
                            hAxis="{title:'width'}"))
    })

#---Dataset---#
    
    #Table
    output$view <- renderDataTable({
        df_iris <- datasetInput()
    })  
    
    #Summary
    output$summary <- renderPrint({
        dataset <- datasetInput()
        summary(dataset)
    })
    

#---Clusters---#
    
    #K-Means
    output$NbClust <- renderText({ 
        paste("K-means clustering performed with ", input$clusters," clusters.")
    })
    output$kmeansPlot <- renderPlot({
        plot(iris[,c(input$Xvar,input$Yvar)],
             col = clusters()$cluster,
             pch = 20, cex = 2)
        points(clusters()$centers, pch = 4, cex = 4, lwd = 4)
    })
    
    #DBSCAN
    output$dbscan_Param <- renderText({ 
        paste("DBSCAN clustering performed with eps = ", input$eps," and minPts = ", input$minPoints,".")
    })
    output$dbscanPlot <- renderPlot({
        cluster <- dbscan(iris[,-5], eps = input$eps, MinPts = input$minPoints)
        plot(cluster, iris[,c(input$Xvar, input$Yvar)])
    })
    
    #Decision Tree
    output$treePlot <- renderPlot({
        ctree <- ctree(Species ~ ., data = iris)
        plot(ctree, type="simple")
    })
    

#---Download Buttons---#
    
    #Dataset iris.csv
    output$downloadData <- downloadHandler(
        filename = function() {
            paste('data-Iris-', Sys.Date(), '.csv', sep='')
        },
        content = function(con) {
            write.csv(iris, con)
        }
    )
    
    #Report
    output$downloadReport <- downloadHandler(
        filename = function() {
            paste('my-report-', Sys.Date(), sep = '', switch(
                input$format, PDF = '.pdf', HTML = '.html', Word = '.docx'
            ))
        },
        content = function(file) {
            src <- normalizePath('report.Rmd')
            
            # temporarily switch to the temp dir, in case you do not have write
            # permission to the current working directory
            owd <- setwd(tempdir())
            on.exit(setwd(owd))
            file.copy(src, 'report.Rmd', overwrite = TRUE)
            
            out <- rmarkdown::render('report.Rmd',
                                     params = list(clusters = input$clusters,
                                                   x = input$Xvar, y = input$Yvar,
                                                   data = input$dataset),
                                     switch(input$format,
                                            PDF = pdf_document(), 
                                            HTML = html_document(), 
                                            Word = word_document()
                                     ))
            file.rename(out, file)
        }
    )
})