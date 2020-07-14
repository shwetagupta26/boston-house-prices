#
# This is the user-interface definition of a Shiny web application for dataset- hosue price. You can
# run the application by clicking 'Run App' above.
#


library(shiny)

# Define UI for application
shinyUI(fluidPage(

    # Application title
    titlePanel("Real Estate Price"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(
            dateRangeInput(inputId = "dateRange",
                           label = "Year Sold",
                           start = min(housing$datesold),
                           end = max(housing$datesold),
                           min =  min(housing$datesold),
                           max = max(housing$datesold)
            ),
            hr(),
            checkboxInput(inputId = "logtransform",
                          label = "Sale Price is skewed,perform logarithmic transformation?",
                          value = TRUE),
            hr(),
            conditionalPanel( condition = ("output.PricePlot"),
            radioButtons(inputId = "filtertype",
                         label = "Select variable to analyse the variation with price",
                         choices = list(
                                        "Condition of sale" = "salecondition",
                                        "Lot size" = "lotarea",
                                        "Number of bedrooms" = "bedroom",
                                        "Neighborhood" = "neighborhood",
                                        "Overall condition and quality rating" ="overallcondqual",
                                        "House Style"="housestyle"
                                        )
                        )),
            # using conditonalpanel in ordee to be displayed only if its corrosponding category is selected.
                         conditionalPanel( condition = ("input.filtertype=='salecondition' && output.PricePlot"),
                                           checkboxGroupInput(inputId = "salecondition",
                                                              label = "Choose salecondition (defaults to Abnormal Sale)?",
                                                              choices = list("Abnormal Sale -  trade, foreclosure, short sale"="Abnorml"
                                                                             ,"Adjoining Land Purchase"="AdjLand"
                                                                             ,"Allocation - two linked properties with separate deeds, typically condo with a garage unit"="Alloca"
                                                                             ,"Sale between family members"="Family"
                                                                             ,"Normal Sale"="Normal"
                                                                             ,"Home was not completed when last assessed (associated with New Homes)"="Partial"
                                                              ),
                                                              selected = unlist(list("Abnormal Sale -  trade, foreclosure, short sale"="Abnorml"
                                                              ))
                                           )),
            conditionalPanel( condition = ("input.filtertype=='lotarea'&& output.PricePlot"),
                              sliderInput(inputId = "lotarea",
                                          label = "Lot size",
                                          min = min(housing$lotarea),
                                          max = max(housing$lotarea),
                                          value = max(housing$lotarea),
                                          step = 1),),
            conditionalPanel( condition = ("input.filtertype=='bedroom'&& output.PricePlot"),
                              sliderInput(inputId = "bedroom",
                                          label = "No of Bedroom",
                                          min = min(housing$bedroomabvgr),
                                          max = max(housing$bedroomabvgr),
                                          value = max(housing$bedroomabvgr),
                                          step = 1),),
            
            conditionalPanel( condition = ("input.filtertype=='neighborhood'&& output.PricePlot"),
                              selectInput(inputId = "neighborhood"
                                          , label = "Choose Neighborhood"
                                          , choices = housing[,"neighborhood"]
                                          ,multiple = TRUE
                              ),),
            conditionalPanel( condition = ("input.filtertype=='housestyle'&& output.PricePlot"),
                              selectInput(inputId = "housestyle"
                                          , label = "Choose Style of dwelling"
                                          , choices = sort(housing[,"housestyle"])
                                          , selected = "1story"
                              ),),
            hr(),

            checkboxInput(inputId = "smoother",
                          label = "Add smoother?",
                          value = FALSE),
            hr()
         
        ),
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel( 
                tabPanel("Price Distirbution",fluidRow(
                    column(6, plotOutput("pricehist")),
                    column(6, plotOutput("priceoutlier")),
                    column(9, plotOutput("logtransform"))),
                    ), 
                tabPanel("Yearly Average Price", plotOutput("PricePlot")),
                tabPanel("Summary",tableOutput("outTable"))
            )
        )
    )
))
