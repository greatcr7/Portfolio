library(shiny)
library(shinythemes)
library(shinydashboard)
library(tidyverse)
library(shinyjs)
library(gsheet)
library(wordcloud)
library(tm)
library(dendextend)
library(sp)
library(ggmap)
library(ggdendro)
library(highcharter)
library(plotly)
library(gapminder)


alexandy_315_theme <-  theme_bw() + # White background, black and white theme
  theme(axis.text = element_text(size = 12, color = "darkslategray", 
                                 face = "italic"),
        text = element_text(size = 14, face = "italic", color = "#E34234"))

ks.data <- read.csv("ks-projects.csv", encoding = 'UTF-8', fileEncoding = 'ISO8859-1')
ks.data$month <- sapply(ks.data$launched, FUN = function(date){return(strsplit(as.character(date), '/')[[1]][1])})
ks.data$year <- sapply(ks.data$launched, FUN = function(date){return(substr(strsplit(as.character(date), '/')[[1]][3], 1, 2))})
tech.subset <- ks.data[which(ks.data$main_category == "Technology"),]
tech.subset.sf <- tech.subset[which(tech.subset$state %in% c("successful","failed")),]

world_data <- map_data(map = "world")

country_names <- c("Austria", "Australia", "Belgium", "Canada", "Switzerland", 
                   "Germany", "Denmark", "Spain", "France", "UK", "Ireland", 
                   "Italy", "Luxembourg", "NULL", "Netherlands", "Norway", 
                   "New Zealand", "Sweden", "USA")
country_match <- data.frame(levels(ks.data$country), country_names)
colnames(country_match) <- c("initial", "country_name")
country_name <- country_match$country_name[match(ks.data$country, country_match)]
ks.data$region <- country_match$country_name[match(ks.data$country, country_match$initial)]

ks.data_aggregate <- ks.data %>% 
  group_by(region) %>%
  summarize(Count = n(),
            mean_backers = mean(backers, na.rm = T),
            mean_pledged = mean(usd.pledged, na.rm = T))
  
    
ks.data_aggregate$region <- as.character(ks.data_aggregate$region)


tech.subset.sf_aggregate <- tech.subset.sf %>% 
  group_by(category) %>%
  summarize(Count = n(),
            mean_pledged_per_cat = mean(pledged, na.rm = T),
            mean_backers_per_cat = mean(backers, na.rm = T))

tech.subset.sf_aggregate$category <- as.character(tech.subset.sf_aggregate$category)



world_data <- world_data %>%
  left_join(ks.data_aggregate, by = "region")



ui <- fluidPage( theme = shinytheme("cosmo"),
  #useShinyjs(),
   # Application title
  titlePanel("Dream Big - Understanding Kickstarter Projects"),
  h6("Reed Marques,  Masahiro Abe,  Alex Yu,  Brandon Jin"),
  # Side Panel
  sidebarLayout(
      sidebarPanel(
        sidebarMenu(
          #style = "position: fixed; overflow: visible;",
          # Each menu item is another bullet in the side menu
          menuItem("Overview", tabName = "html", icon = icon("th")),
          menuItem("Projects Over Time", tabName = "b", icon = icon("th")),
          menuItem("Global Breakdown", tabName = "map", icon = icon("th")),
          menuItem("Common Project Themes", tabName = "d", icon = icon("th")),
          menuItem("Finding Correlations", tabName = "e", icon = icon("th")),
          menuItem("Project Statuses", tabName = "backers", icon = icon("th")),
          menuItem("Goal & Pledge", tabName = "world", icon = icon("th")),
          menuItem("Backers vs Pledged", tabName = "filter", icon = icon("th"))
        )
      ),
      # Main Panel containing graphs
      mainPanel(
        tabItems(
          
          # HTML Plot
          tabItem(tabName = "html",
                  h3("Number of Project Backers vs Amount Pledged per Category"),
                  fluidRow(
                    box(
                      highchartOutput("hcontainer",height = "500px", width = "500px")
                      )
                  )
          ),

          # Time Series Plot
          tabItem(tabName = "b",
                  h3("Project Funding Success Based on Month Launched"),
                  fluidRow(
                    box(
                      selectInput(inputId = "tsCountryInput", label = "Select Country",
                                  choices = c("US", "CA", "GB","DE","AU","NL")),
                      plotOutput("timeseries_plot", height = 500, width = 600),

                      # Scroll through different years
                     sliderInput(inputId = "year_adjust",
                                 label = "Change Year:",
                                 min = 2009, max = 2016, value = 2009, step = 1)
                    )
                  )
          ),

          # Third tab content
          tabItem(tabName = "map",
                  h3("Kickstarter Projects Around the World"),
                  fluidRow(
                    box(
                      selectInput(inputId = "Attribute",
                                  label = "Attribute of Data",
                                  choices = c("Number of Projects", "Average Money Pledged", "Average Number of Backers"),
                                  selected = "Number of Projects")),
                    plotOutput("map_plot")
                  )
          ),
          # Word Cloud
          tabItem(tabName = "d",
                  h3("Common Project Themes"),
                  fluidRow(
                    box(
                      selectInput("categoryInput", label = "Select Category",
                                  choices = c("Technology", "Software", "Hardware",
                                              "3D Printing", "Apps", "Camera Equipment",
                                              "DIY Electronics", "Fabrication Tools",
                                              "Flight", "Gadgets", "Robots", "Sound",
                                              "Space Exploration", "Wearables",
                                              "Web", "Makerspaces")),
                      plotOutput("wordcloud", height = "400px", width = "400px")
                    )
                  )
          ),
          
          # Dendrogram
          tabItem(tabName = "e",
                  h3("Finding Correlations"),
                  fluidRow(
                    box(
                      selectInput("variableInput", label = "Select Type",
                                  choices = c("Category", "Country", "State")),
                      plotOutput("dendrogram", height = 600, width = 500),
                      sliderInput("sampleInput", label = "Sample Size", 0, 600, 200)
                    )
                  )
          ),
          
          # Backers
          tabItem(tabName = "backers",
                  h3("Funding Breakdown over Project Status"),
                  fluidRow(
                    box(
                      selectInput(inputId = "Status",
                                  label = "Attribute of Data",
                                  choices = c("Cancelled", "Failed", "Live", "Successful", "Suspended"),
                                  selected = "successful")),
                    plotOutput("backer_plot")
                  )
          ),
          
          # Plotly_1 Scatterplot
          tabItem(tabName = "world",
                  h3("Filtered Scatterplot of Goal & Pledged"),
                  fluidRow(
                    plotlyOutput("plotly_1", width = 600), 
                    box(
                      selectInput(inputId = "Countryy",
                                  label = "Select A Country",
                                  choices = c("ALL COUNTRIES", as.vector(unique(ks.data$region))),
                                  selected = "ALL COUNTRIES"),
                      selectInput(inputId = "Categoryy",
                                  label = "Select A Category",
                                  choices = c("ALL CATEGORIES", as.vector(unique(ks.data$category))),
                                  selected = "ALL CATEGORIES")
                    )
                  )
          ),
          
          # Plotly_2
          tabItem(tabName = "filter",
                  h3("Animation of Backer-Pledge Relation Over Time"),
                  fluidRow(
                    plotlyOutput("plotly_2", width = 600),
                    selectInput(inputId = "YEAR",
                                label = "Select A Year",
                                choices = c("ALL YEARS", as.vector(unique(ks.data$year))),
                                selected = "ALL YEARS"),
                    selectInput(inputId = "modee",
                                label = "Select A Graph Type",
                                choices = c("Points", "Segments"),
                                selected = "Segments")
                  )
          )

        )
      )
   )
)

########################################################################
### SERVER
########################################################################

# Define server logic required to draw a histogram
server <- function(input, output, session) {

  ## Higherchart Plot
  output$hcontainer <- renderHighchart({
    
    hc <- hchart(tech.subset.sf_aggregate, "scatter", 
                 hcaes(x = mean_backers_per_cat, 
                       y = mean_pledged_per_cat, group = category))
    
    hc %>%
      hc_xAxis(title = list(text = "Average Amount Pledged per Category")) %>%
      hc_yAxis(title = list(text = "Number of Backers")) 
    
    
    hc
  })
  
  
  ## Time Series Graph
  output$timeseries_plot <- renderPlot({
    ks.filtered <-
      ks.data %>%
      filter(
        year == substr(input$year_adjust, 3, 4),
        perc_raised < 50000,
        country == input$tsCountryInput
      )
    
    timeseries_plot <- ggplot(ks.filtered,
                              aes(x = month, y = perc_raised, alpha = 0.5,
                                  col = ifelse(perc_raised < 100, 'blue', 'red'))) +
      geom_point() + 
      geom_point(data = ks.filtered[which(ks.filtered$main_category == "Technology"),],
                 aes(pch = 10, size = 1)) + scale_shape_identity() + 
      scale_y_continuous(limits = c(0, 50000)) + scale_x_discrete(labels = c(1:12)) +
      labs(x = "Month", y = "Percent of Goal Raised",
           col = 'Above or Below Goal', size = "Technology Project") +
      guides(alpha = F) +
      scale_color_discrete(labels = c("Below", "Above")) + alexandy_315_theme

    if (length(ks.filtered) == 0){
    } else {
      timeseries_plot
    }

  })
  
  ## Chloropleth Map
  output$map_plot <- renderPlot({
    
    if (input$Attribute == "Number of Projects") {
      main_plot <- ggplot(data = world_data) + 
        geom_polygon(aes(x = long, y = lat, fill = Count, group = group), color = "white") + 
        coord_fixed(1.3) +
        theme_void() + 
        scale_fill_continuous(low = "orange", high = "red") + 
        labs(title = "Number of Projects by Country", fill = "Number of Projects") + alexandy_315_theme
    }
    
    if (input$Attribute == "Average Money Pledged") {
      main_plot <- ggplot(data = world_data) + 
        geom_polygon(aes(x = long, y = lat, fill = mean_pledged, group = group), color = "white") + 
        coord_fixed(1.3) +
        theme_void() + 
        scale_fill_continuous(low = "orange", high = "red") + 
        labs(title = "Average Money Pledged by Country, in USD", fill = "USD") + alexandy_315_theme
    }
    
    if (input$Attribute == "Average Number of Backers") {
      main_plot <- ggplot(data = world_data) + 
        geom_polygon(aes(x = long, y = lat, fill = mean_backers, group = group), color = "white") + 
        coord_fixed(1.3) +
        theme_void() + 
        scale_fill_continuous(low = "orange", high = "red") + 
        labs(title = "Average Number of Backers per Project by Country", fill = "Number of Backers") + alexandy_315_theme
    }
    main_plot
  })
  
  ## Word Cloud
  output$wordcloud <- renderPlot({
    tech.filtered <-
      tech.subset %>%
      filter(
        category == input$categoryInput
      )
    names <- Corpus(VectorSource(as.character(tech.filtered$name)))
    names_plain <- tm_map(names, PlainTextDocument)
    names_plain <- tm_map(names_plain, removePunctuation)
    names_plain <- tm_map(names_plain, removeWords, stopwords("english"))
    names_plain <- tm_map(names_plain, removeWords, c("Canceled", "The", "the",
                                                      "You", "Your", "with", "and",
                                                      "for", "For", "that", "can"))
    wordcloud <- wordcloud(names_plain[[1]][[1]], max.words = 100)
  })

  ## Dendrogram
  output$dendrogram <- renderPlot({
    
    tech.subset_sample <- sample_n(tech.subset, input$sampleInput, replace = F)
    
    ks.data_cont <- subset(tech.subset_sample, select = c(goal, pledged, backers))
    
    ks.data_cont_scale <- scale(ks.data_cont)
    
    ks.data_dist <- dist(ks.data_cont_scale)
    
    ks.data_clust <- hclust(ks.data_dist)
    
    hcdata<- dendro_data(ks.data_clust, type="rectangle")
    
    hcdata$labels <- mutate(hcdata$labels,
                            category = as.factor(tech.subset_sample$category),
                            country = as.factor(tech.subset_sample$country),
                            state = as.factor(tech.subset_sample$state))
    
    qual_palette <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c", "#fb9a99",
                      "#e31a1c","#fdbf6f", "#ff7f00", "#cab2d6", "#6a3d9a", "#ffff99",
                      "#b15928", "#8dd3c7", "#c51b7d", "#bc80bd", "#1a1a1a", "#d9d9d9", "#543005")
    
    if (input$variableInput == "Category") {
      var = hcdata$labels$category
    } else if (input$variableInput == "Country") {
      var = hcdata$labels$country
    } else {
      var = hcdata$labels$state
    }
    
    ggplot() +
      geom_segment(data=segment(hcdata), aes(x=x, y=y, xend=xend, yend=yend)) +
      geom_text(data = label(hcdata), aes(x=x, y=y, label=label, colour = var, hjust=0), size=3) +
      geom_point(data = label(hcdata), aes(x=x, y=y), size=3, shape = 21) +
      coord_flip() +
      scale_y_reverse(expand=c(0.2, 0)) +
      scale_colour_manual(values = qual_palette) + 
      theme_dendro() + alexandy_315_theme +
      labs(colour = input$variableInput)
  })
  
  ## Backers and state
  output$backer_plot <- renderPlot({
    
    if (input$Status == "Cancelled") {
      main_plot <- ggplot(data = filter(tech.subset, state == "canceled")) + 
        geom_point(aes(x = backers, y = pledged, color = country), cex = 0.9) + 
        geom_smooth(aes(x = backers, y = pledged), method = "lm") + alexandy_315_theme
        labs(title = "Amount Pledged by Number of Backers, for Cancelled Projects (as of 11/27/17)", color = "Country") 
    }
    
    if (input$Status == "Failed") {
      main_plot <- ggplot(data = filter(tech.subset, state == "failed")) + 
        geom_point(aes(x = backers, y = pledged, color = country), cex = 0.9) + 
        geom_smooth(aes(x = backers, y = pledged), method = "lm", se = F) + alexandy_315_theme
        labs(title = "Amount Pledged by Number of Backers, for Failed Projects (as of 11/27/17)", color = "Country") 
    }
    
    if (input$Status == "Live") {
      main_plot <- ggplot(data = filter(tech.subset, state == "live")) + 
        geom_point(aes(x = backers, y = pledged, color = country), cex = 0.9) + 
        geom_smooth(aes(x = backers, y = pledged), method = "lm", se = F) + alexandy_315_theme
        labs(title = "Amount Pledged by Number of Backers, for Live Projects (as of 11/27/17)", color = "Country") 
    }
    
    if (input$Status == "Successful") {
      main_plot <- ggplot(data = filter(tech.subset, state == "successful")) + 
        geom_point(aes(x = backers, y = pledged, color = country), cex = 0.9) + 
        geom_smooth(aes(x = backers, y = pledged), method = "lm", se = F) + alexandy_315_theme
        labs(title = "Amount Pledged by Number of Backers, for Successful Projects (as of 11/27/17)", color = "Country")
    }
    
    if (input$Status == "Suspended") {
      main_plot <- ggplot(data = filter(tech.subset, state == "suspended")) + 
        geom_point(aes(x = backers, y = pledged, color = country), cex = 0.9) + 
        geom_smooth(aes(x = backers, y = pledged), method = "lm", se = F) + + alexandy_315_theme
        labs(title = "Amount Pledged by Number of Backers, for Suspended Projects (as of 11/27/17)", color = "Country")
    }
    main_plot
  })
  
  ## Code for Plot 7
  
  
  output$plotly_1<- renderPlotly({
    categoryy <- input$Categoryy
    countryy <- input$Countryy
    subb <- ks.data
    if (categoryy != "ALL CATEGORIES"){
      subb <- subset(subb, subb$category == categoryy)} 
    if (countryy != "ALL COUNTRIES") {
      subb <- subset(subb, subb$region == countryy)
    }
    x <- list(
      title = "Amount Aiming For"
    )
    y <- list(
      title = "Amount Pledged"
    )
    p <- plot_ly(
      type = 'scatter',
      x = subb$goal,
      y = subb$pledged,
      text = subb$name,
      hoverinfo = 'text',
      mode = 'markers',
      transforms = list(
        list(
          type = 'filter',
          target = 'y',
          operation = '>',
          value = mean(subb$goal)
        )
      )
    ) %>% layout(
      xaxis = x, yaxis = y
    )
    p
  })
  
  ## Code for Plot 8
  output$plotly_2 <- renderPlotly({
    #ks<-subset(ks.data, ks.data$pledged < 50000)
    #kss<-subset(ks, ks$backers < 2000)
    kss<-ks.data
    kss$month <- as.numeric(kss$month)
    if (input$YEAR != "ALL YEARS") {
      kss <- subset(ks.data, ks.data$year == input$YEAR)
    }
    
    x <- list(
      title = "Number of Backers"
    )
    y <- list(
      title = "Amount Pledged"
    )
    
    if (input$modee == "Points") {
      p <- gapminder %>%
        plot_ly(
          x = ~kss$backer, 
          y = ~kss$pledged, 
          size = ~kss$goal, 
          color = ~kss$state, 
          frame = ~kss$month, 
          text = ~kss$state, 
          hoverinfo = "text",
          type = 'scatter',
          mode = 'markers', alpha = 0.5, transforms = list(
            list(
              type = 'filter',
              target = 'y',
              operation = '>',
              value = mean(kss$goal)
            )
          )
        ) %>%
        layout(
          xaxis = x, yaxis = y
        ) %>% animation_opts(
          1000, easing = "elastic", redraw = FALSE
        ) %>% 
        animation_button(
          x = 1, xanchor = "right", y = 0, yanchor = "bottom"
        ) %>%
        animation_slider(
          currentvalue = list(prefix = "Month: ", font = list(color="red"))
        )
      
    }
    else {
      q <- gapminder %>%
        plot_ly(
          x = ~kss$backer, 
          y = ~kss$pledged, 
          size = ~kss$goal, 
          color = ~kss$state, 
          frame = ~kss$month, 
          text = ~kss$state, 
          hoverinfo = "text",
          type = 'scatter',
          mode = 'lines', alpha = 0.5, transforms = list(
            list(
              type = 'filter',
              target = 'y',
              operation = '>',
              value = mean(kss$goal)
            )
          )
        ) %>%
        layout(
          xaxis = x, yaxis = y
        ) %>% animation_opts(
          1000, easing = "elastic", redraw = FALSE
        ) %>% 
        animation_button(
          x = 1, xanchor = "right", y = 0, yanchor = "bottom"
        ) %>%
        animation_slider(
          currentvalue = list(prefix = "Month: ", font = list(color="red"))
        )
      
    }
  })
  
}

# Run the application
shinyApp(ui = ui, server = server)

