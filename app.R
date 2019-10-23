#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(readxl)
library(knitr)
library(png)
library(ggplot2)

## Load the data




Climate_data <- read_excel("C:\\Users\\wuyf\\Desktop\\MSSP\\615\\project\\World Bank Data\\climate_change_download_0.xls", sheet = 1)
head(Climate_data)


## Data Cleaning Process



Filted_data <- filter(Climate_data, `Country name` %in%  c("China", "United States", "India", "Russian Federation", "Japan", "Germany" ))
Filted_data <- select(Filted_data, -c(7:16))
Filted_data <- na_if(Filted_data, "..")
Filted_data <- filter(Filted_data, (rowMeans(is.na(Filted_data)) < 0.3))



## Explore The Data -- Comparing total CO2 emission


CO2 <- filter(Filted_data, `Series name` %in% c('CO2 emissions, total (KtCO2)'))
CO2 <- select(CO2, -c(1:6,16:18))
CO2[nrow(CO2) + 1,] = c("2000","2001","2002","2003","2004","2005","2006","2007","2008")
rownames(CO2) <- c("CHN", "Germany", "India", "Japan", "Russia", "USA", "year")


CO2 = t(CO2)
CO2 = as.data.frame(CO2)
CO2$Japan <- as.numeric(as.character(CO2$Japan))
CO2$CHN <- as.numeric(as.character(CO2$CHN))
CO2$Germany <- as.numeric(as.character(CO2$Germany))
CO2$USA <- as.numeric(as.character(CO2$USA))
CO2$India <- as.numeric(as.character(CO2$India))
CO2$year <- as.numeric(as.character(CO2$year))
ggplot(CO2, aes(x = year)) +
    geom_line(aes(y = Japan), color = "darkred") + 
    geom_line(aes(y = CHN), color="steelblue", linetype="twodash") +
    geom_line(aes(y = USA), color = "blue") + 
    geom_line(aes(y = Germany), color="green", linetype="twodash") +
    geom_line(aes(y = India), color = "black") +
    ggtitle('Total CO2 Emission') +
    ylab("Value (ktCO2)")
CO2p <- filter(Filted_data, `Series name` %in% c('CO2 emissions per capita (metric tons)'))
CO2p <- select(CO2p, -c(1:6,16:18))
CO2p[nrow(CO2p) + 1,] = c("2000","2001","2002","2003","2004","2005","2006","2007","2008")
rownames(CO2p) <- c("CHN", "Germany", "India", "Japan", "Russia", "USA", "year")


CO2p = t(CO2p)
CO2p = as.data.frame(CO2p)
CO2p$Japan <- as.numeric(as.character(CO2p$Japan))
CO2p$CHN <- as.numeric(as.character(CO2p$CHN))
CO2p$Germany <- as.numeric(as.character(CO2p$Germany))
CO2p$USA <- as.numeric(as.character(CO2p$USA))
CO2p$India <- as.numeric(as.character(CO2p$India))
CO2p$year <- as.numeric(as.character(CO2p$year))
ggplot(CO2p, aes(x = year)) +
    geom_line(aes(y = Japan), color = "darkred") + 
    geom_line(aes(y = CHN), color="steelblue", linetype="twodash") +
    geom_line(aes(y = USA), color = "blue") + 
    geom_line(aes(y = Germany), color="green", linetype="twodash") +
    geom_line(aes(y = India), color = "black") +
    ggtitle('CO2 Emission per capita') +
    ylab("Value (tons)")


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("World Climate Change -- CO2 Emission"),

    # Sidebar with a slider input for number of bins 
    

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )


# Define server logic required to draw a histogram
server <- function(input, output) {
    

    output$CO2 <- renderPlot({
        ggplot(CO2, aes(x = year)) +
            geom_line(aes(y = Japan), color = "darkred") + 
            geom_line(aes(y = CHN), color="steelblue", linetype="twodash") +
            geom_line(aes(y = USA), color = "blue") + 
            geom_line(aes(y = Germany), color="green", linetype="twodash") +
            geom_line(aes(y = India), color = "black") +
            ggtitle('Total CO2 Emission') +
            ylab("Value (ktCO2)")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
