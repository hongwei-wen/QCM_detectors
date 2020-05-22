#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(readr)
library(stringr)
first_category_name = list.files("/Users/wenhongwei/Desktop/textbook/QCM_Sensor_Alcohol_Dataset") 
dir = paste("/Users/wenhongwei/Desktop/textbook/QCM_Sensor_Alcohol_Dataset/",first_category_name,sep="")
for(i in 1:length(dir)){
    index = str_sub(first_category_name[i],1,-5)
    data_index = data.frame(read_csv(dir[i]))
    #print(names(data_index))
    names(data_index) = c("ch1p0.8", "ch2p0.8", "ch1p0.7","ch2p0.7","ch1p0.6","ch2p0.6","ch1p0.5","ch2p0.5","ch1p0.4","ch2p0.4","Oct1","Pro1","But2","pro2","iso1")
    data_index = mutate(data_index,labels = Oct1*1+Pro1*2+But2*3+pro2*4+iso1*5) %>% select(-(Oct1:iso1))
    assign(index, data_index)
    #print(head(get(index)))
}
QCM_all = rbind(QCM3,QCM6,QCM7,QCM10,QCM12)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Measurement of QCM detectors"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("type",
                        "Type of alcohol:",
                        min = 1,
                        max = 5,
                        value = 1)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x  <- QCM_all[which(QCM_all$labels==input$type),"ch1p0.8"]
        bins <- seq(min(x), max(x), length.out = 10)
        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, border = 'white', main = "Histogram of Channel 1", col=rainbow(length(bins)))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
