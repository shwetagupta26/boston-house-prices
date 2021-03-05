#
# This is the server logic of a Shiny web application for dataset- hosue price. 
#You can run the application by clicking 'Run App' above.
#
#

# Defining server logic
shinyServer(function(input, output) {
    
    passData <- reactive({
        
        #filtering the data on the basis of year selected
        print(input$dateRange[1])
        housingfilter <- housing[housing[,"datesold"] %in% seq.Date(input$dateRange[1]
                                                              , input$dateRange[2]
                                                              , by = "days")
                           , ]
        
        # below code filter the data based on the filter type selectd from the dashboard
        if(input$filtertype == "salecondition"){
            housingfilter <- housingfilter[housingfilter[,"salecondition"] %in% unlist(input$salecondition),]
        }
        
        if(input$filtertype == "lotarea"){
            
            housingfilter <- housingfilter[housingfilter[,"lotarea"] %in% min(housingfilter$lotarea) : as.numeric(input$lotarea),]
            
        }
        
        if(input$filtertype == "bedroom"){
            
            housingfilter <- housingfilter[housingfilter[,"bedroomabvgr"] %in% min(housingfilter$bedroomabvgr) : as.numeric(input$bedroom),]
            
        }
        
        if(input$filtertype == "neighborhood"){
            housingfilter <- housingfilter
            if(is.null(input$neighborhood)==F){
                housingfilter <- housingfilter[housingfilter[,"neighborhood"]%in% unlist(input$neighborhood),]
            }
        }
        if(input$filtertype == "housestyle"){
            housingfilter <- housingfilter
            housingfilter <- housingfilter[housingfilter[,"housestyle"]%in% unlist(input$housestyle),]
           
            
        }
        
        housingfilter
    })
    
    # plotting the price distrbtuion
    output$pricehist <- renderPlot({
        graphData <-  housing[housing[,"datesold"] %in% seq.Date(input$dateRange[1]
                                                                 , input$dateRange[2]
                                                                 , by = "days")
                              , ]
        theGraph <- ggplot(graphData, aes(x = saleprice)) +  
            geom_histogram(aes(y = ..density..), colour ="black", fill = "white") +
            geom_density(alpha = .2, fill = "#FF6666")
        
        theGraph <- theGraph+xlab("Sale Price")
        print(theGraph)
        
    })
    
    output$priceoutlier <- renderPlot({
        graphData <-  housing[housing[,"datesold"] %in% seq.Date(input$dateRange[1]
                                                                 , input$dateRange[2]
                                                                 , by = "days")
                              , ]
        theGraph <- ggplot(graphData, aes(x = datesold,y=saleprice,group = 1)) +
            geom_boxplot(outlier.colour = "red",   outlier.shape = 1,
                         outlier.size = 4)
        theGraph <- theGraph+xlab("Year Sold")+ylab("Sale Price")
        
        print(theGraph)
        
    })
    
    output$logtransform <- renderPlot({
        graphData <-  housing[housing[,"datesold"] %in% seq.Date(input$dateRange[1]
                                                                 , input$dateRange[2]
                                                                 , by = "days")
                              , ]
        theGraph <- ggplot(graphData, aes(x = log(saleprice))) +  
            geom_histogram(aes(y = ..density..), colour ="black", fill = "white") +
            geom_density(alpha = .2, fill = "#FF6666")
        
        theGraph <- theGraph+xlab("log(Sale Price)")
        
        if(input$logtransform){
            
            print(theGraph)
        }
        
    })
    
    
    # Plotting the average saleprice per year sold as per the filter type selected
    output$PricePlot <- renderPlot({
        
        graphData <-  aggregate(data = passData(),
                                saleprice~datesold
                                , FUN=mean)
        theGraph <- ggplot(graphData,
                           aes(x=datesold,
                               y=log(saleprice),group =datesold,colour = datesold))+
            geom_line()
        
        # aggregating the data based on the values selected for the different variables 
        if(input$filtertype == "lotarea"){
            
            graphData <-  aggregate(data = passData(),
                                    saleprice~lotarea+datesold
                                    , FUN=mean)
            theGraph <- ggplot(graphData,
                               aes(x=datesold,
                                   y=log(saleprice),group =lotarea,colour = lotarea))+
                geom_point() 
            
        }
        
        if(input$filtertype == "salecondition"){
            
            graphData <-  aggregate(data = passData(),
                                    saleprice~salecondition+datesold
                                    , FUN=mean)
            theGraph <- ggplot(graphData,
                               aes(x=datesold,
                                   y=log(saleprice),group =salecondition,colour = salecondition))+
                geom_line()
            
        }
        
        if(input$filtertype == "bedroom"){
            
            graphData <-  aggregate(data = passData(),
                                    saleprice~bedroomabvgr+datesold
                                    , FUN=mean)
            theGraph <- ggplot(graphData,
                               aes(x=datesold,
                                   y=log(saleprice),group =bedroomabvgr,colour = bedroomabvgr))+
                geom_point()
            
        }
        
        if(input$filtertype == "neighborhood"){
            graphData <-  aggregate(data = passData(),
                                    saleprice~neighborhood+datesold
                                    , FUN=mean)
            theGraph <- ggplot(graphData,
                               aes(x=datesold,
                                   y=(saleprice),group =neighborhood,colour = neighborhood,size=saleprice))+
                geom_point()
            
        }
        
        if(input$filtertype == "housestyle"){
            graphData <-  aggregate(data = passData(),
                                    saleprice~housestyle+datesold
                                    , FUN=mean)
            theGraph <- ggplot(graphData,
                               aes(x=datesold,
                                   y=log(saleprice),group =housestyle,size=saleprice))+
                geom_line()
            
        }
        
        if(input$filtertype == "overallcondqual"){
            
            theGraph <-ggplot(passData(),
                              aes(x=datesold,
                                  y=log(saleprice)))+
                geom_point(aes(color = overallcond, size =overallqual))
            
        }
        
        if(input$smoother){
            
            theGraph <- theGraph + geom_smooth()
            
        }
        theGraph +
            ylab("Year(Sold)")
        print(theGraph)
        
    })
    
    # summarising the maximum sale price, and the year sold in tabular format as per the filter type selected
    output$outTable <- renderTable({ 
        textdata <- passData()
        if(input$filtertype == "salecondition"){
            outdat <-
                sqldf(
                    "select max(saleprice) 'Maximum Price',salecondition 'Condition of sale',yrsold 'Year Sold'
                from textdata
                group by salecondition
                order by 3 desc"
                )
        }
        if(input$filtertype == "lotarea"){
            outdat <-
                sqldf(
                    "select max(saleprice) 'Maximum Price',lotarea 'Lot size',yrsold 'Year Sold'
                from textdata
                group by lotarea
                order by 3 desc"
                )
        }
        if(input$filtertype == "bedroom"){
            outdat <-
                sqldf(
                    "select max(saleprice) 'Maximum Price',bedroomabvgr 'Number of Bedrooms',yrsold 'Year Sold'
                from textdata
                group by bedroomabvgr
                order by 3 desc"
                )
        }
        if(input$filtertype == "neighborhood"){
            outdat <-
                sqldf(
                    "select max(saleprice) 'Maximum Price',neighborhood 'Neighborhood',yrsold 'Year Sold'
                from textdata
                group by neighborhood
                order by 3 desc"
                )
        }
        if(input$filtertype == "overallcondqual"){
            outdat <-
                sqldf(
                    "select max(saleprice) 'Maximum Price'
                    ,overallcond 'Overall Condtion'
                    ,overallqual 'Overall Quality Rating'
                    ,yrsold 'Year Sold'
                from textdata
                group by overallcond,overallqual
                order by 3 desc"
                )
        }
        if(input$filtertype == "housestyle"){
            outdat <-
                sqldf(
                    "select max(saleprice) 'Maximum Price'
                    ,housestyle 'House Style'
                    ,yrsold 'Year Sold'
                from textdata
                group by housestyle
                order by 3 desc"
                )
        }
        outdat
        
        
    })
})

