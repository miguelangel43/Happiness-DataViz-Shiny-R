# Import libraries
library(shiny)
library(magrittr) 
library(ggplot2)
library(maps)
library(ggiraph)
library(dplyr)
library(RColorBrewer)
library(plotly)
library(ggrepel)

# Application deployed to https://teampcm.shinyapps.io/happinessapp/

# Loading the data
dataset_path <- "world-happiness-report.csv"
data <- read.csv(dataset_path)
names(data)[1] <- "Country.name" # Fixes the error on Windows of R reading the first column wrongly
# Handle NaN values
data <- na.omit(data)
data_num <- data[,-1]


#---------------------------------------------------------------------------------------------------
# Time Series 
#---------------------------------------------------------------------------------------------------

paises_africa <- c("Algeria","Angola","Benin","Botswana","Burkina Faso",
                   "Burundi","Cameroon","Central African Republic","Chad",
                   "Comoros","Congo","Djibouti","Egypt","Eswatini","Ethiopia",
                   "Gabon","Gambia","Ghana","Guinea","Kenya","Lesotho",
                   "Liberia","Libya","Madagascar","Malawi","Mali","Mauritania",
                   "Mauritius","Morocco","Mozambique","Namibia","Niger","Nigeria",
                   "Rwanda","Senegal","Sierra Leone","South Africa","Sudan",
                   "Tanzania, United Republic of","Togo","Tunisia","Uganda",
                   "Zambia","Zimbabwe")

paises_americas <- c("Argentina","Belize","Bolivia (Plurinational State of)","Brazil",
                     "Canada","Chile","Colombia","Costa Rica","Dominican Republic","Ecuador",
                     "El Salvador","Guatemala","Guyana","Haiti","Honduras","Jamaica",
                     "Mexico","Nicaragua","Panama","Paraguay","Peru","Suriname","Trinidad and Tobago",
                     "United States of America","Uruguay","Venezuela (Bolivarian Republic of)")

paises_asia <- c("Afghanistan","Armenia","Azerbaijan","Bahrain","Bangladesh",
                 "Bhutan","Cambodia","Cyprus","Georgia","India","Indonesia",
                 "Iran (Islamic Republic of)","Iraq","Israel","Japan","Jordan",
                 "Kazakhstan","Korea, Republic of","Kuwait","Kyrgyzstan",
                 "Lao People's Democratic Republic","Lebanon","Malaysia",
                 "Mongolia","Myanmar","Nepal","Pakistan","Palestine, State of",
                 "Philippines","Qatar","Saudi Arabia","Singapore","Sri Lanka",
                 "Syrian Arab Republic","Taiwan, Province of China","Tajikistan",
                 "Thailand","Turkey","United Arab Emirates","Uzbekistan","Viet Nam",
                 "Yemen")
paises_europe <- c("Albania","Austria","Belarus","Belgium","Bosnia and Herzegovina",
                   "Bulgaria","Croatia","Czechia","Denmark","Estonia","Finland","France",
                   "Germany","Greece","Hungary","Iceland","Ireland","Italy","Latvia",
                   "Lithuania","Luxembourg","Malta","Moldova, Republic of","Montenegro",
                   "Netherlands","North Macedonia","Norway","Poland","Portugal","Romania",
                   "Russian Federation","Serbia","Slovakia","Slovenia","Spain","Sweden",
                   "Switzerland","Ukraine","United Kingdom of Great Britain and Northern Ireland")

paises_oceania <- c("New Zealand","Australia")

#---------------------------------------------------------------------------------------------------
# Map Plot
#---------------------------------------------------------------------------------------------------

# As different data sets have different formats for country names (e.g., “United Kingdom
# of Great Britain and Northern Ireland” versus “United Kingdom”), we’ll match country 
# names to ISO3 codes
# Get a Dataframe with 
iso_codes <- read.csv(url("https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv"))

# Rename some countries that appear under a different name in the datasets
old_names <- c("Bolivia","Congo (Brazzaville)","Congo (Kinshasa)","Czech Republic","Iran",
               "Ivory Coast","Laos","Moldova","Palestinian Territories","Russia","South Korea",
               "Swaziland","Syria","Taiwan Province of China","Tanzania","United Kingdom",
               "United States","Venezuela","Vietnam")
new_names <- c("Bolivia (Plurinational State of)","Congo","Congo","Czechia","Iran (Islamic Republic of)",
               "Côte d'Ivoire","Lao People's Democratic Republic","Moldova, Republic of",
               "Palestine, State of","Russian Federation","Korea, Republic of","Eswatini",
               "Syrian Arab Republic","Taiwan, Province of China","Tanzania, United Republic of",
               "United Kingdom of Great Britain and Northern Ireland","United States of America",
               "Venezuela (Bolivarian Republic of)","Viet Nam")
for (i in 1:length(old_names)){
    data$Country.name[data$Country.name == old_names[i]] <- new_names[i]
}

# Add column 'iso3' with the matching codes of the country
data['iso3'] <- iso_codes$alpha.3[match(data$Country.name, iso_codes$name)]
data['region'] <- iso_codes$region[match(data$Country.name, iso_codes$name)]

# Check that the ISO3 matching worked for every country
unique(data[which(is.na(data$ISO3)),]$Country.name)
unique(data[which(is.na(data$region)),]$Country.name)

# Get a dataset for the map plot with coordinates
world_data <- ggplot2::map_data('world')
world_data <- fortify(world_data)
old_names <- c("French Southern and Antarctic Lands", "Antigua", "Barbuda", "Saint Barthelemy", "Brunei", "Ivory Coast",
               "Democratic Republic of the Congo", "Republic of Congo", "Falkland Islands", "Micronesia", "UK", 
               "Heard Island", "Cocos Islands", "Iran", "Nevis", "Saint Kitts", "South Korea", "Laos", "Saint Martin",
               "Macedonia", "Pitcairn Islands", "North Korea", "Palestine", "Russia", "South Sandwich Islands",
               "South Georgia", "Syria", "Trinidad", "Tobago", "Taiwan", "Tanzania", "USA", "Vatican", "Grenadines",
               "Saint Vincent", "Venezuela", "Vietnam", "Wallis and Fortuna")
new_names <- c("French Southern Territories", rep("Antigua and Barbuda", 2), "Saint-Barthélemy",
               "Brunei Darussalam", "Côte d'Ivoire", "Congo, (Kinshasa)", "Congo (Brazzaville)", 
               "Falkland Islands (Malvinas)", "Micronesia, Federated States of", "United Kingdom",
               "Heard and Mcdonald Islands", "Cocos (Keeling) Islands", "Iran, Islamic Republic of",
               rep("Saint Kitts and Nevis", 2), "Korea (South)", "Lao PDR", "Saint-Martin (French part)",
               "Macedonia, Republic of", "Pitcairn", "Korea (North)", "Palestinian Territory", "Russian Federation",
               rep("South Georgia and the South Sandwich Islands", 2), 
               "Syrian Arab Republic (Syria)", rep("Trinidad and Tobago", 2), "Taiwan, Republic of China",
               "Tanzania, United Republic of", "United States of America", "Holy See (Vatican City State)",
               rep("Saint Vincent and Grenadines", 2), "Venezuela (Bolivarian Republic)", "Viet Nam", "Wallis and Futuna Islands")
for (i in 1:length(old_names)){
    world_data$region[world_data$region == old_names[i]] <- new_names[i]
}
# Add the iso3
world_data["iso3"] <- iso_codes$alpha.3[match(world_data$region, iso_codes$name)]
# Check again if the ISO3 matching worked for every country
unique(world_data[which(is.na(world_data$iso3)),]$region)

# Helper function that outputs df that will be used to plot the world map
worldMaps <- function(df, world_data, indicator, chosen_year){
    # Function for setting the aesthetics of the plot
    my_theme <- function () { 
        theme_bw() + theme(axis.text = element_text(size = 14),
                           axis.title = element_text(size = 14),
                           strip.text = element_text(size = 14),
                           panel.grid.major = element_blank(), 
                           panel.grid.minor = element_blank(),
                           panel.background = element_blank(), 
                           legend.position = "bottom",
                           panel.border = element_blank(), 
                           strip.background = element_rect(fill = 'white', colour = 'white'))
    }
    
    # Select only the data corresponding to the inputted year
    chosenData <- filter(df, year==chosen_year)
    # Select only the data that the user has selected to view
    chosenData <- chosenData[, c("iso3", indicator)]
    # Do not include countries for which we don't have an iso3
    chosenData <- chosenData[!is.na(chosenData$iso3), ]
    
    # Add the data the user wants to see to the geographical world data
    world_data["Value"] <- chosenData[[2]][match(world_data$iso3, chosenData$iso3)]
    
    # Create caption with the data source to show underneath the map
    capt <- paste0("Source: Happiness Index")
    
    # Specify the plot for the world map
    g <- ggplot() + 
        geom_polygon_interactive(data = world_data, color = 'gray70', size = 0.1,
                                 aes(x = long, y = lat, fill = Value, group = group, 
                                     tooltip = sprintf("%s<br/>%s", iso3, Value))) + 
        scale_fill_gradientn(colours = brewer.pal(5, "RdBu"), na.value = 'white') + 
        scale_y_continuous(limits = c(-60, 90), breaks = c()) + 
        scale_x_continuous(breaks = c()) + 
        labs(fill = indicator, color = indicator, title = NULL, x = NULL, y = NULL, caption = capt) + 
        my_theme()
    
    return(g)
}

# Define UI for application that draws a histogram
ui <- navbarPage(
    "Happiness Index",
    tabPanel("Introduction",
             includeHTML("introduction.html")
             ),
    tabPanel("K-Means",
        sidebarLayout(
            sidebarPanel(
                selectInput('xcol', 'X Variable', names(data_num)[names(data_num) != "year"], selected = names(data_num)[[2]]),
                selectInput('ycol', 'Y Variable', names(data_num)[names(data_num) != "year"], selected = names(data_num)[[3]]),
                numericInput('clusters', 'Cluster count', 3, min = 1, max = 9),
                sliderInput("year_clustering", "Year:",min = 2006, max = 2020, value = 2015)
            ),
            mainPanel(
               plotOutput("kmeans_plot")
            )
        )
    ),
    tabPanel("World Map",

             titlePanel("World Map"),
             sidebarPanel(
                 selectInput('indicator', 'Indicator', names(data_num)[-1], selected = names(data_num)[-1][[2]]),
                 sliderInput("year", "Year:",
                             min = 2006, max = 2020,
                             value = 2015)
             ),

             # Hide errors
             tags$style(type = "text/css",
                        ".shiny-output-error { visibility: hidden; }",
                        ".shiny-output-error:before { visibility: hidden; }"),
             
             # Output: interactive world map
             girafeOutput("distPlot"),
    ),
    tabPanel("Time Series per Region",
             p("On this tab you'll be able to analyze and compare the evolution of variables that sustain happiness of a country among them and their averages on their respective regions."),
             fluidRow(
                 column(4,
                        selectInput('ycol_region_var', 'Y Variable', names(data_num), selected = names(data_num)[[3]])
                 )
                 ,
                 column(2,
                        # Add checkbox gRoup
                        checkboxGroupInput("varSelectregion", "Continents",
                                           c("Africa","Americas","Asia","Europe","Oceania"),selected = c("Africa","Americas","Asia","Europe","Oceania"))
                 )
                 ,
                 column(6,
                        plotlyOutput("avg_region_plot")
                 )
             ),
             hr(),
             
             fluidRow(
                 column(1,
                        # Add checkbox gRoup
                        checkboxGroupInput("varSelectafrica1", "Countries in Africa",
                                           paises_africa[1:15],selected = paises_africa[1:15])),
                 column(1,
                        checkboxGroupInput("varSelectafrica2", "",
                                           paises_africa[16:30],selected = paises_africa[16:30])),
                 column(1,
                        checkboxGroupInput("varSelectafrica3", "",
                                           paises_africa[31:44],selected = paises_africa[31:44]))
                 
                 ,column(9,
                         plotlyOutput("avg_region_plot_africa")
                         
                 )),
             hr(),
             fluidRow(
                 column(1,
                        # Add checkbox gRoup
                        checkboxGroupInput("varSelectamericas1", "Countries in America",
                                           paises_americas[1:15],selected = paises_americas[1:15])),
                 column(1,
                        checkboxGroupInput("varSelectamericas2", "",
                                           paises_americas[16:26],selected = paises_americas[16:26])),
                 
                 column(10,
                        plotlyOutput("avg_region_plot_americas")
                        
                 )),
             hr(),
             
             fluidRow(
                 column(1,
                        # Add checkbox gRoup
                        checkboxGroupInput("varSelectasia1", "Countries in Asia",
                                           paises_asia[1:15],selected = paises_asia[1:15])),
                 column(1,
                        checkboxGroupInput("varSelectasia2", "",
                                           paises_asia[16:30],selected = paises_asia[16:30])),
                 column(1,
                        checkboxGroupInput("varSelectasia3", "",
                                           paises_asia[31:42],selected = paises_asia[31:42])),
                 
                 column(9,
                        plotlyOutput("avg_region_plot_asia")
                        
                 )),
             hr(),
             fluidRow(
                 column(1,
                        # Add checkbox gRoup
                        checkboxGroupInput("varSelecteurope1", "Countries in Europe",
                                           paises_europe[1:15],selected = paises_europe[1:15])),
                 column(1,
                        checkboxGroupInput("varSelecteurope2", "",
                                           paises_europe[16:30],selected = paises_europe[16:30])),
                 column(1,
                        checkboxGroupInput("varSelecteurope3", "",
                                           paises_europe[31:39],selected = paises_europe[31:39])),
                 
                 column(9,
                        plotlyOutput("avg_region_plot_europe")
                        
                 )
             ),
             hr(),   
             fluidRow(
                 column(1,
                        # Add checkbox gRoup
                        checkboxGroupInput("varSelectoceania", "Countries in Oceania",
                                           paises_oceania,selected = paises_oceania)
                        
                 ),
                 column(
                     11,
                     plotlyOutput("avg_region_plot_oceania")
                 )
             )
    )
    ,
    
    tabPanel("Scatterplot",
             sidebarPanel(
                 p('This visualization aims to help the analyst to overviewing the dataset and to characterize the distributions, but more specifically to find outliers, extreme values and to correlate the attributes.'),
                 selectInput('xcol2', 'X Variable', names(data_num), selected = names(data_num)[[2]]),
                 selectInput('ycol2', 'Y Variable', names(data_num), selected = names(data_num)[[3]]),
                 selectInput("region", 
                             label = "Choose Region",
                             choices = c("Africa", "Americas", "Asia", "Europe", "Oceania","All"),
                             selected = "All"),
                 sliderInput("year_scatterplot", "Year:",
                             min = 2006, max = 2020,
                             value = 2015),
                 
             ),
             mainPanel(
                 plotOutput("scatter2_plot")
            )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    #---------------------------------------------------------------------------------------------------
    # K-Means
    #---------------------------------------------------------------------------------------------------

    data_clustering <- reactive({ filter(data_num, year==input$year_clustering) })
    data_countries <- reactive({ filter(data, year==input$year_clustering)})
    clusters <- reactive({ kmeans(data_clustering(), input$clusters) })
    
    output$kmeans_plot <- renderPlot({
        ggplot(data_clustering(), aes_string(x=input$xcol, y=input$ycol, color=as.factor(clusters()$cluster))) + 
            geom_point() + 
            geom_text_repel(label = data_countries()$Country.name, size=3) +
            theme_classic()+scale_color_brewer(palette="Dark2")
    })
    
    #---------------------------------------------------------------------------------------------------
    # World Map
    #---------------------------------------------------------------------------------------------------
    # Create the interactive world map

    output$distPlot <- renderGirafe({
        ggiraph(code = print(worldMaps(data, world_data, input$indicator, input$year)))
    })
    
    #---------------------------------------------------------------------------------------------------
    # Time Series
    #---------------------------------------------------------------------------------------------------
    
    avg_region <- reactive({
        data_without_na = data[,c('region','year',input$ycol_region_var)]
        data_without_na = data_without_na[complete.cases(data_without_na),]
        
        df1 = group_by(data_without_na,region,year)
        
        df1 = summarize_at(df1,.vars = input$ycol_region_var,.funs = mean)
        #remove some rows
        df1 = df1[df1$region %in% input$varSelectregion,]
        
        rename_at(df1,input$ycol_region_var, ~ "mean_var")
        
        # avg region is a table of columns region, year, mean_var
    })
    
    output$avg_region_plot <-renderPlotly({ggplot(avg_region(),aes(x=year,y=mean_var,color =region )) + geom_line() })
    
    #Time series Africa
    avg_region_africa <- reactive({
        data_aux_africa = data[data$region == 'Africa',]
        data_without_na_africa = data_aux_africa[,c('Country.name','year',input$ycol_region_var)]
        data_without_na_africa = data_without_na_africa[complete.cases(data_without_na_africa),]
        
        df1_africa = group_by(data_without_na_africa,Country.name,year)
        
        df1_africa = summarize_at(df1_africa,.vars = input$ycol_region_var,.funs = mean)
        
        #remove some rows
        df1_africa = df1_africa[(df1_africa$Country.name %in% input$varSelectafrica1 |df1_africa$Country.name %in% input$varSelectafrica2|df1_africa$Country.name %in% input$varSelectafrica3) ,]
        
        rename_at(df1_africa,input$ycol_region_var, ~ "mean_var_africa")
        
        # avg region is a table of columns region, year, mean_var
    })
    
    output$avg_region_plot_africa <-renderPlotly({ggplot(avg_region_africa(),aes(x=year,y=mean_var_africa,color =Country.name )) + geom_line() + labs(title = "Selected Var in countries in Africa",x="Year",y=input$ycol_region_var )+ theme(legend.position="bottom")})
    
    #Time series Americas
    avg_region_americas <- reactive({
        data_aux_americas = data[data$region == 'Americas',]
        data_without_na_americas = data_aux_americas[,c('Country.name','year',input$ycol_region_var)]
        data_without_na_americas = data_without_na_americas[complete.cases(data_without_na_americas),]
        
        df1_americas = group_by(data_without_na_americas,Country.name,year)
        
        df1_americas = summarize_at(df1_americas,.vars = input$ycol_region_var,.funs = mean)
        
        #remove some rows
        df1_americas = df1_americas[(df1_americas$Country.name %in% input$varSelectamericas1 |df1_americas$Country.name %in% input$varSelectamericas2) ,]
        
        rename_at(df1_americas,input$ycol_region_var, ~ "mean_var_americas")
        
        # avg region is a table of columns region, year, mean_var
    })
    
    output$avg_region_plot_americas <-renderPlotly({ggplot(avg_region_americas(),aes(x=year,y=mean_var_americas,color =Country.name )) + geom_line()+ labs(title = "Selected Var in countries in America",x="Year",y=input$ycol_region_var )+ theme(legend.position="bottom")})
    
    #Time series Asia
    avg_region_asia <- reactive({
        data_aux_asia = data[data$region == 'Asia',]
        data_without_na_asia = data_aux_asia[,c('Country.name','year',input$ycol_region_var)]
        data_without_na_asia = data_without_na_asia[complete.cases(data_without_na_asia),]
        
        df1_asia = group_by(data_without_na_asia,Country.name,year)
        
        df1_asia = summarize_at(df1_asia,.vars = input$ycol_region_var,.funs = mean)
        rename_at(df1_asia,input$ycol_region_var, ~ "mean_var_asia")
        
        # avg region is a table of columns region, year, mean_var
    })
    
    output$avg_region_plot_asia <-renderPlotly({ggplot(avg_region_asia(),aes(x=year,y=mean_var_asia,color =Country.name )) + geom_line()+ labs(title = "Selected Var in countries in Asia",x="Year",y=input$ycol_region_var )+ theme(legend.position="bottom")})
    
    
    #Time series Europe
    avg_region_europe <- reactive({
        data_aux_europe = data[data$region == 'Europe',]
        data_without_na_europe = data_aux_europe[,c('Country.name','year',input$ycol_region_var)]
        data_without_na_europe = data_without_na_europe[complete.cases(data_without_na_europe),]
        
        df1_europe = group_by(data_without_na_europe,Country.name,year)
        
        df1_europe = summarize_at(df1_europe,.vars = input$ycol_region_var,.funs = mean)
        rename_at(df1_europe,input$ycol_region_var, ~ "mean_var_europe")
        
        # avg region is a table of columns region, year, mean_var
    })
    
    output$avg_region_plot_europe <-renderPlotly({ggplot(avg_region_europe(),aes(x=year,y=mean_var_europe,color =Country.name )) + geom_line()+ labs(title = "Selected Var in countries in Europe",x="Year",y=input$ycol_region_var )+ theme(legend.position="bottom")})
    
    
    #Time series Oceania
    avg_region_oceania <- reactive({
        data_aux_oceania = data[data$region == 'Oceania',]
        data_without_na_oceania = data_aux_oceania[,c('Country.name','year',input$ycol_region_var)]
        data_without_na_oceania = data_without_na_oceania[complete.cases(data_without_na_oceania),]
        
        df1_oceania = group_by(data_without_na_oceania,Country.name,year)
        
        df1_oceania = summarize_at(df1_oceania,.vars = input$ycol_region_var,.funs = mean)
        
        #remove some rows
        df1_oceania = df1_oceania[df1_oceania$Country.name %in% input$varSelectoceania,]
        
        rename_at(df1_oceania,input$ycol_region_var, ~ "mean_var_oceania")
        
        # avg region is a table of columns region, year, mean_var
    })
    
    output$avg_region_plot_oceania <-renderPlotly({ggplot(avg_region_oceania(),aes(x=year,y=mean_var_oceania,color =Country.name )) + geom_line()+ labs(title = "Selected Var in countries in Oceania",x="Year",y=input$ycol_region_var )+ theme(legend.position="bottom")})
    
    
    #---------------------------------------------------------------------------------------------------
    # Scatterplot
    #---------------------------------------------------------------------------------------------------
    selectedData2 <- reactive({
        data2<-data[, c(input$xcol2, input$ycol2, "region", "Country.name", "year")]
        data2 <- filter(data2, year == input$year_scatterplot)
        data2 <- switch(input$region, 
                        "Africa" = filter(data2, region == "Africa"),
                        "Americas" = filter(data2, region == "Americas"),
                        "Asia" = filter(data2, region == "Asia"),
                        "Europe" = filter(data2, region == "Europe"),
                        "Oceania" = filter(data2, region == "Oceania"),
                        "All" = data2)
        
    })
    output$scatter2_plot <- renderPlot({
        df<-selectedData2()
        ggplot(df, aes_string(x=input$xcol2, y=input$ycol2, color="region")) +
            geom_point() + 
            geom_text_repel(label = df$Country.name, size=3) +
            theme_classic()+scale_color_brewer(palette="Dark2")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

