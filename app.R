#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(stringr)
library(shinydashboard)
library(shinycssloaders)
library(DT)
library(plotly)
library(dplyr)
library(ggplot2)
library(leaflet)
library(shinyalert)
library(forcats)
library(purrr)
library(ggmap)
library(RColorBrewer)
library(tidyverse)

# loading AirBnB data

data <- load("C:/Users/selfe/Documents/DSTI/R_Coding/Final_Project23/AirBnB_Paris.Rdata")

# keep necessary variable and renaming some of them
new_L <- select(L, listing_id = id, host_id, host_name,        bathrooms, bedrooms, beds, bed_type, equipments= amenities, type= property_type, room= room_type, 
                              nb_of_guests= accommodates, price, guests_included, minimum_nights, 
                              maximum_nights,availability_over_one_year= availability_365, instant_bookable, 
                              cancellation_policy, city, address= street, neighbourhood=neighbourhood_cleansed, 
                              city_quarter=zipcode, latitude, longitude, security_deposit, transit, 
                              host_response_time, superhost= host_is_superhost, host_since, 
                              listing_count= calculated_host_listings_count, host_score= review_scores_rating, 
                              reviews_per_month, number_of_reviews)

# Remove duplicates by listing_id
new_L %>% distinct(listing_id, .keep_all = TRUE)

# Removing the '$' character in the price to later convert it to numeric
new_L$price <- substring(gsub(",", "", as.character(new_L$price)),2)

# Convert the necessary data types as numeric
new_L$bathrooms <- as.numeric((new_L$bathrooms))
new_L$bedrooms <- as.numeric((new_L$bedrooms))
new_L$beds <- as.numeric((new_L$beds))
new_L$price <- as.numeric((new_L$price))
new_L$guests_included <- as.numeric((new_L$guests_included))
new_L$minimum_nights <- as.numeric((new_L$minimum_nights))
new_L$maximum_nights <- as.numeric((new_L$maximum_nights))
new_L$availability_over_one_year <- as.numeric((new_L$availability_over_one_year))
new_L$security_deposit <- as.numeric((new_L$security_deposit))
new_L$listing_count <- as.numeric((new_L$listing_count))
new_L$host_score <- as.numeric((new_L$host_score))
new_L$reviews_per_month <- as.numeric((new_L$reviews_per_month))
new_L$number_of_reviews <- as.numeric((new_L$number_of_reviews))

# Convert the necessary data types as characters
new_L$neighbourhood <- as.character(new_L$neighbourhood)

# Setting the price range
new_L <- new_L %>% filter(new_L$price >= 0 & new_L$price <= 100)

# Bathroom, bedrooms and beds have missing values
## Bathrooms
x <- mean(new_L$bathrooms, na.rm = TRUE)
y <- is.na(new_L$bathrooms)
new_L$bathrooms[y] <- x

## bedrooms
x <- mean(new_L$bedrooms, na.rm = TRUE)
y <- is.na(new_L$bedrooms)
new_L$bedrooms[y] <- x

## bed
x <- mean(new_L$bed, na.rm = TRUE)
y <- is.na(new_L$bed)
new_L$bed[y] <- x

## price
x = mean(new_L$price,na.rm = TRUE) 
y = is.na(new_L$price) 
new_L$price[y] = x

#Calculs
#1. Setting the city quarters (Arrondissements)
new_L$city = str_sub(new_L$city, 1, 5)
new_L$city_quarter = str_sub(new_L$city_quarter, -2)

#2. retrieving observations of the data that don't have an empty city_quarter
new_L <- subset(new_L, new_L$city_quarter != "" & new_L$city_quarter != '00' & new_L$city_quarter != ' ')

#3. combine two data frames ('new_L' and 'R') based on a common column ("listing_id")
new_data <- inner_join(new_L, R, by = "listing_id")

#4. visit frequency of the different quarters according to time
new_data = mutate(new_data,year = as.numeric(str_extract(new_data$date, "^\\d{4}")))

#5. number of apartments per host
count_by_host_1 <- new_L %>% 
  group_by(host_id) %>%
  summarise(number_apt_by_host = n()) %>%
  ungroup() %>%
  mutate(groups = case_when(
    number_apt_by_host == 1 ~ "001",
    between(number_apt_by_host, 2,10) ~ "002-010",
    number_apt_by_host > 10 ~ "011-153"))

count_by_host_2 <- count_by_host_1 %>%
  group_by(groups) %>%
  summarise(counting = n()) %>%
  arrange(desc(counting), .by_group = TRUE)

count_by_host_3 <- new_L %>%
  group_by(host_id) %>%
  summarise(number_apt_by_host = n()) %>%
  arrange(desc(number_apt_by_host))

top_listings_by_host <- count_by_host_3 %>%
  top_n(n=20, wt = number_apt_by_host)

knit_print.data.frame <- top_listings_by_host

#6. Listings by Property type

whole_property_type_count <- table(new_L$type)
property_types_counts <- table(new_L$type,exclude=names(whole_property_type_count[whole_property_type_count[] < 4000]))

count_of_others <- sum(as.vector(whole_property_type_count[whole_property_type_count[] < 4000]))
property_types_counts['Others'] <- count_of_others
property_types <- names(property_types_counts)
counts <- as.vector(property_types_counts)
percentages <- scales::percent(round(counts/sum(counts), 2))
property_types_percentages <- sprintf("%s (%s)", property_types, percentages)
property_types_counts_df <- data.frame(group = property_types, value = counts)

# price per city quarter
average_prices_per_arrond <- aggregate(cbind(new_L$price),
                                       by = list(arrond = new_L$city_quarter),
                                       FUN = function(x) mean(x))


# Entire data map
df <- select(new_L,longitude, neighbourhood, latitude, price) #necessary variables to build the map
df %>% select(longitude, neighbourhood, latitude, price) #Remove the 'metadata' 

# host_is_superhost map
dfsuperhost <- select(new_L, longitude, neighbourhood, latitude, price)
dfsuperhost <- filter(new_L, superhost == "t")


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Exploring the AirBnB Data Paris"),

    # sidebar with widgets for selecting features to explore
    dashboardSidebar(
      sidebarMenu(
        menuItem("Prices vs Apartments",tabName="prices_apartments", icon=icon("money-bill-1-wave")),
        
        menuItem("Apartments per Owner",tabName="apartments_owners", icon=icon("house-chimney-user")),
        
        menuItem("Price per Quarter",tabName="arrondissements", icon=icon("dashboard")),
        
        menuItem("Visit Frequency",tabName="visits", icon=icon("wave-square")),
        
        menuItem("Maps",tabName="map", icon=icon("map"))
      )),
    
    dashboardBody(
      
      # A quick overview of the data
      fluidRow(tags$head(tags$style(HTML(".small-box {height: 100px}"))),
               valueBox("Paris", "France", icon = icon("location-pin"), width = 3),
               valueBoxOutput("mean_price", width = 3),
               valueBoxOutput("nb_superhosts", width = 3),
               valueBoxOutput("count_listings", width = 3)),
      
      # Build the dashboards on the corresponding menu item
      tabItems(
        
        tabItem(tabName ="prices_apartments",
                fluidRow(
                  box(title= "Listings by room type", 
                      width = 6,
                      plotOutput("room_type")%>% withSpinner(color="#971a4a"), 
                      status = "primary", 
                      solidHeader = FALSE, 
                      collapsible = TRUE),
                  
                  box(title= "Listings by property type",
                      width =6,
                      plotOutput("property_type")%>% withSpinner(color="#971a4a"), 
                      status = "primary", 
                      solidHeader = FALSE, 
                      collapsible = TRUE)),
                fluidRow(
                  box(title="Avg. price according to room type", 
                      plotOutput("price_room")%>% withSpinner(color="#971a4a"), 
                      width=6, 
                      solidHeader = FALSE, 
                      collapsible = TRUE )
                )
        ),  
        
        tabItem(tabName ="apartments_owners",
                fluidRow(
                  box(title= "Apartments per host", 
                      width = 6,  
                      plotOutput("nb_apartments")%>% withSpinner(color="#971a4a"), 
                      status = "primary", 
                      solidHeader = FALSE, 
                      collapsible = TRUE),
                  
                  box(title= "Hosts vs Superhosts",  
                      plotOutput("superhosts")%>% withSpinner(color="#971a4a"), 
                      status = "primary", 
                      solidHeader = FALSE, 
                      width = 6, 
                      collapsible = TRUE)
                ),
                
                fluidRow(
                  box(title = "Top 20 hosts in Paris", 
                      width=12, 
                      status= "success",
                      solidHeader = FALSE, 
                      collapsible = TRUE,
                      DTOutput('top20_hosts')))
        ),
        
        tabItem(tabName ="arrondissements",
                
                fluidRow(
                  box(title= "Avg. price per neighborhood",
                      width = 12, 
                      plotlyOutput("avg_price_nh")%>% withSpinner(color="#971a4a"), 
                      status = "success", 
                      solidHeader = FALSE, 
                      collapsible = TRUE)),
                
                box(title= "Top 10 neighborhoods in Paris",
                    width =6,
                    plotlyOutput("top10_nh")%>% withSpinner(color="#971a4a"), 
                    status = "primary", 
                    solidHeader = FALSE, 
                    collapsible = TRUE),
                
                fluidRow(
                  box(title= "Rented apartments in the past years",
                      width = 12, plotOutput("nb_rented")%>% withSpinner(color="#971a4a"), 
                      status = "success", 
                      solidHeader = FALSE, 
                      collapsible = TRUE)),
                
                fluidRow(
                  box(title= "Map representing price range within Paris neighborhoods", 
                      width = 12, 
                      plotOutput("price_range_nh")%>% withSpinner(color="#971a4a"),
                      status = "success", 
                      solidHeader = FALSE, 
                      collapsible = TRUE),
                )),
        
        tabItem(tabName ="visits",
                fluidRow(
                  box(title= "Listings by neighborhood and room type",
                      plotlyOutput("listings_nh")%>% withSpinner(color="#971a4a"), 
                      width = 12, 
                      status = "success", 
                      solidHeader = FALSE, 
                      collapsible = TRUE)),
                
                fluidRow(  
                  box(title= "Frequency of visits in the past years", 
                      plotlyOutput("freq_visit")%>% withSpinner(color="#971a4a"), 
                      width = 12, 
                      status = "success", 
                      solidHeader = FALSE, 
                      collapsible = TRUE)
                ),
        ),
        
        tabItem(tabName ="map",
                fluidPage(
                  box(title = "Neighborhoud listings", 
                      width = 12,
                      leafletOutput("all_map"), 
                      status = "success", 
                      solidHeader = FALSE, 
                      collapsible = TRUE),
                  
                  box(title = "Superhosts listings", 
                      width = 12,
                      leafletOutput("superhost_map"), 
                      status = "success", 
                      solidHeader = FALSE, 
                      collapsible = TRUE)
                ))
      ) 
    )  
)

# Define server 
server <- function(input, output) {
  
  # Create a Shiny alert
  shinyalert("R for Big Data [DSTI Final_Project23]",
             "by Nwabo Ndeffo Cyrille ")
  
  # Output variables; define value boxes
  output$mean_price <- renderValueBox({
    valueBox(
      round(mean(new_L$price),0), 
      "Mean Price", 
      icon = icon("hand-holding-dollar")
    )
  })
  
  output$nb_superhosts <- renderValueBox({
    valueBox(
      sum(new_L$superhost == "t"), 
      "Superhosts", 
      icon = icon("user-check")
    )
  })
  
  output$count_listings <- renderValueBox({
    valueBox(
      nrow(new_L), 
      "Listings", 
      icon = icon("list")
    )
  })
  
  # -.-.- Define the graphs
  ## -.-.-.- Listings by room type 
  output$room_type <- renderPlot ({
    room_types_counts <- table(new_L$room)
    room_types <- names(room_types_counts)
    counts <- as.vector(room_types_counts)
    percentages <- scales::percent(round(counts/sum(counts), 2))
    room_types_percentages <- sprintf("%s (%s)", room_types, percentages)
    room_types_counts_df <- data.frame(group = room_types, value = counts)
    
    ggplot(room_types_counts_df, 
           aes(x = "", 
               y = value, 
               fill = room_types_percentages)
    )+
      geom_bar(width = 1, stat = "identity")+
      coord_polar("y", start = 0)+
      scale_fill_brewer("Room types", palette ="BuPu")+
      ylab("")+
      xlab("")+
      labs(fill="")+
      geom_text(aes(label = percentages), 
                size = 4, 
                position = position_stack(vjust = 0.5))+
      theme_void()
  })
  
  ## -.-.-.- Listings by property type
  output$property_type <- renderPlot ({
    ggplot(property_types_counts_df, aes(x="",y = value, fill=property_types_percentages))+
      geom_bar(width = 1,stat = "identity")+
      coord_polar("y",start = 0)+
      scale_fill_brewer("Property types", palette ="BuPu")+
      ylab("")+
      xlab("")+
      labs(fill="")+
      geom_text(aes(label = percentages),size= 4 ,position = position_stack(vjust = 0.5))+
      theme_void()
    
  })
  
  ## -.-.-.- Mean price by room type
  output$price_room <- renderPlot ({
    ggplot(new_L)+ 
      geom_boxplot(aes(x = room,y = price, fill = room)) + 
      labs(x = "Room Type", y = "Price", fill = "Room Type")+ 
      coord_flip()
    
    new_L %>% 
      
      group_by(room) %>% 
      summarise(mean_price = mean(price, na.rm = TRUE)) %>% 
      ggplot(aes(x = reorder(room, mean_price), y = mean_price, fill = room)) +
      geom_col(stat ="identity", fill="#971a4a") +
      coord_flip() +
      theme_minimal()+
      labs(x = "Room type", y = "Price") +
      geom_text(aes(label = round(mean_price,digit = 2)), hjust = 1.0, color = "white", size = 3.5) +
      
      xlab("Room type") + 
      ylab("Mean price")
  })
  
  ## -.-.-.- Top 10 neighborhoods
  output$top10_nh <- renderPlotly ({
    p30<- new_L %>%
      group_by(neighbourhood) %>%
      dplyr::summarize(num_listings = n(), 
                       borough = unique(neighbourhood)) %>%
      top_n(n = 10, wt = num_listings) %>%
      ggplot(aes(x = fct_reorder(neighbourhood, num_listings), 
                 y = num_listings, fill = borough)) +
      scale_fill_brewer(palette ="BuPu")+
      geom_col() +
      coord_flip() +
      theme(legend.position = "none") +
      labs(x = "Neighborhood", y = "Nb. of listings")
    
    ggplotly(p30)
  })
  
  ## -.-.-.- Nb. of Apartments per host
  output$nb_apartments <- renderPlot ({
    ggplot(count_by_host_2, aes(x = "", y = counting)) +  
      geom_col(aes(fill = factor(groups)),color = "white")+
      geom_text(aes(y = counting / 1.23, label = counting),
                size = 3)+
      labs(x = "", y = "", fill = "Number of apartments per host")+
      scale_fill_brewer(palette ="BuPu") +
      coord_polar(theta = "y")+
      theme_void()
  })
  
  ## -.-.-.- Nb. of Superhosts
  output$superhosts <- renderPlot ({
    ggplot(new_L) +
      geom_bar(aes(x='' , fill=superhost)) +
      coord_polar(theta='y') +
      scale_fill_brewer(palette ="BuPu")+
      theme_void()
    
  })
  
  ## -.-.-.- Top 20 hosts
  output$top20_hosts <- renderDT(
    top_listings_by_host, 
    options = list(searching = FALSE,
                   pageLength = 10
    ))
  
  ## -.-.-.- Avg. price by Neighborhood 
  output$avg_price_nh <- renderPlotly ({
    p3 <- ggplot(data = average_prices_per_arrond, aes(x = arrond, y = V1))+
      geom_bar(stat = "identity", fill = "#971a4a", width = 0.7)+
      geom_text(aes(label = round(V1, 2)), size=4)+
      coord_flip()+
      labs(
        x = "City quarters", y = "Average daily price")+
      theme(legend.position = "bottom",axis.text.x = element_text(angle = 90, hjust = 1))+
      scale_fill_brewer(palette ="BuPu") 
    ggplotly(p3)
  })
  
  ## -.-.-.- Nb. of rented apartments 
  output$nb_rented <- renderPlot ({
    temp["date"] <- temp["date"] %>% map(., as.Date)
    
    longitudinal  <- temp %>% 
      group_by(date, neighbourhood) %>% 
      summarise(count_obs = n())
    
    ggplot(longitudinal,aes(x = date,y = count_obs,group = 1))+ 
      geom_line(size = 0.5,colour = "#FF5AAC") +  
      stat_smooth(color = "#971a4a",method = "loess")+  
      scale_x_date(date_labels = "%Y")+  
      labs(x = "Year",y = "Nb. rented apartment")+  
      facet_wrap(~ neighbourhood)
    
  })
  
  
  ## -.-.-.- Price range per neighborhood 
  output$price_range_nh <- renderPlot({
    height <- max(new_L$latitude) - min(new_L$latitude)
    width <- max(new_L$longitude) - min(new_L$longitude)
    Paris_borders <- c(bottom  = min(new_L$latitude)  - 0.1 * height, 
                       top     = max(new_L$latitude)  + 0.1 * height,
                       left    = min(new_L$longitude) - 0.1 * width,
                       right   = max(new_L$longitude) + 0.1 * width)
    map <- get_stamenmap(Paris_borders, zoom = 12)
    p8<- ggmap(map) +
      geom_point(data = new_L, 
                 mapping = aes(x = longitude, y = latitude, col = log(price))) +
      scale_color_distiller(palette ="BuPu", direction = 1)
    p8
  })
  
  ## -.-.-.- Nb. of listings per neighborhood
  output$listings_nh <- renderPlotly({
    x <- ggplot(new_L, aes(x = fct_infreq(neighbourhood), fill = room)) +
      geom_bar() +
      labs(x = "Neighborhood", y = "Nb. of listings")+
      scale_fill_brewer(palette ="BuPu") +
      theme(legend.position = "bottom", axis.text.x = element_text(angle = 90, hjust = 1))
    ggplotly(x)
  })
  
  ## -.-.-.- Visit frequency
  output$freq_visit <- renderPlotly ({
    p6 <- ggplot(new_data) +
      geom_bar(aes(y =city_quarter,
                   fill=factor(year)))+
      scale_size_area() +
      labs( x="Frequency", y="City Quarter", fill="Year")+
      scale_fill_brewer(palette ="BuPu")
    
    ggplotly(p6)
  })
  
  ## -.-.-.- Neighborhood listings map
  output$all_map <- renderLeaflet ({
    leaflet(df) %>%  
      setView(lng = 2.3488, lat = 48.8534, zoom = 12) %>%
      addTiles() %>% 
      addMarkers(clusterOptions = markerClusterOptions()) %>%
      addMiniMap()
  })
  
  ## -.-.-.- Superhost listings map
  output$superhost_map <- renderLeaflet ({
    leaflet(dfsuperhost %>% select(longitude,neighbourhood,
                                   latitude,price))%>%
      setView(lng = 2.3488, lat = 48.8534, zoom = 12) %>%
      addTiles() %>% 
      addMarkers(clusterOptions = markerClusterOptions()) %>%
      addMiniMap()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
