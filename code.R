# Loading necessary libraries
library(shiny)
library(plotly)
library(leaflet)
library(dplyr)
library(scales)
library(sf)
library(ggplot2)
library(readr)
# Reading the data from the required files-tab1
nsw_vic_df <- read.csv("NSW_VIC_OFFENCES.csv")

# Reading required data for Victim Reports Analysis based on Relationship to Offender-tab2
vict_rel_df <- read.csv("off_vict_rel.csv")
vict_rel_df$Victim.Reports <- as.numeric(gsub(",", "", vict_rel_df$Victim.Reports)) #replacing comma with empty space
vict_rel_df$Year <- as.numeric(vict_rel_df$Year) #converting year to numeric column
vict_rel_df$`Relationship of Victim to Offender` <- as.factor(vict_rel_df$Relationship.of.Victim.to.Offender) # converting into a factor variable
vict_rel_df$Offence.Division <- as.factor(vict_rel_df$Offence.Division)
vict_rel_df$Offence.Subdivision <- as.factor(vict_rel_df$Offence.Subdivision)


# Reading the data for the sunburst plot for tab3
file_path <- "newvicts.csv"
victms_df <- read.csv(file_path)
victms_df$Victim.Reports <- as.numeric(gsub(",", "", victms_df$Victim.Reports))
victms_df$Year <- as.numeric(victms_df$Year)
mi <- min(victms_df$Year)
ma <- max(victms_df$Year)

# Reading data for map and scatter plot for tab4
# Load merged data
merged_df <- read_csv("merged_offence_income_data.csv", show_col_types = FALSE)
# Convert the geometry column to an sf object
merged_df <- st_as_sf(merged_df, wkt = "geometry") #[7]


# CSS to style the tabs
tab_css <- "
.nav-tabs {
  background-color: lightblue;
}

.nav-tabs > li > a {
  color: black;
}

.nav-tabs > li.active > a {
  background-color: #5bc0de;
  color: white;
}

.nav-tabs > li > a:hover {
  background-color: #5bc0de;
  color: white;
}
"

# Defining the UI for the webpage
ui <- fluidPage(
  tags$head(tags$style(HTML(tab_css))), # Adding CSS to the tabs
  titlePanel("Unveiling Crime Patterns"), # Title of the page
  tabsetPanel(   # Defining page into tabs. [1]
    tabPanel("Home",
             h2("Unveiling Crime Patterns: A Comparative Study of Crime Incidents in Victoria and NSW Over the Time, with a Focus on Demographics, and Geographic Dynamics in Victoria"),
             
             h4("In recent years, crime poses a multifaceted challenge in various regions, particularly in populous and urbanized areas such as the Australian states of Victoria and New South Wales (NSW).
                    This study focuses on unraveling the intricacies of crime incidents over time, drawing a comparative analysis between these two populous and urbanized regions. Specifically, 
                    our exploration targets Victoria, aiming to shed light on prevalent crime types, demographic nuances, and geographic patterns.Through this investigation, we aspire to provide valuable 
                    insights to inform law enforcement, policymakers, and the public, fostering a safer and more secure environment in Victoria. The insights gained can improve safety measures and make our living environment more secure."),
             
             h4(""),
             h4("Following are the tabs in this web page in order to navigate through them just click on that specific tab, which are on the top of this page beside Home tab:"),
             tags$ul(
               tags$li(h4("NSW and Victoria Crime Rate Analysis")),
               tags$li(h4("Relationship of Victim to Offender Analysis")),
               tags$li(h4("Victim Characteristic Analysis")),
               tags$li(h4("Analysing LGA wise Offence Count and Median Income"))
             )
    ),
    tabPanel("NSW and Victoria Crime Rate Analysis", # Tab title
             sidebarLayout(
               sidebarPanel( # side bar in the web page
                 h5("Stacked Bar chart shows crime rate trend in NSW and Victoria over the years (2019-2023) and the type of offenses occurred and their distribution too."),
                 checkboxGroupInput("div_ft", "Offense Divisions", # Check boxes
                                    choices = unique(nsw_vic_df$Offense.Division),
                                    selected = unique(nsw_vic_df$Offense.Division)),
                 sliderInput("y_ft", label = "Year",
                             min = 2019,
                             max = ma,
                             value = c(mi, ma),
                             sep = ""), # Ensuring no commas
                 h4("Additional Info:"),
                 h5("- Use the check boxes to filter the stacked bar chart."),
                 h5("- Hover to see more info on the stacked bar chart.")
               ),
               mainPanel( # The main area where our plots lie
                 plotlyOutput("tab1plot1", width = "100%", height = "100%")
               )
             )
    ),
    tabPanel("Relationship of Victim to Offender Analysis", 
             titlePanel("Victim Reports Analysis based on Relationship to Offender in Victoria State Between Years (2014-2023)"),
             h5("The following line graphs show the Relationship to Offender over the years (2014-2023) in Victoria. To see the trend animation, click the play button in the first graph. Hover over first line graph trend lines to see more details then next graph changes to show the offense division-wise victim reports, after that hover on second graph the third graph opens up to show offence sub division wise victim reports."),
             fluidRow(
               column(width = 6, 
                      h4("1. Victim Reports by Relationship of Victim to Offender over the years (2014-2023)"), 
                      plotlyOutput("line_chart", height = "350px", width = "auto")),
               column(width = 6, h4(textOutput("area_chartTitle")),
                      plotlyOutput("area_chart", height = "350px", width = "auto"))
             ),
             fluidRow(
               column(width = 6, h4(textOutput("sub_plottitle")), 
                      plotlyOutput("sub_plot", height = "350px", width = "auto"))
             )
    ),
    tabPanel("Victim Characteristic Analysis",
             titlePanel("Victim Characteristic Analysis based on Offense Division in Victoria State over the years (2014-2023)"),
             sidebarLayout(
               sidebarPanel(
                 h3("Sunburst Chart Showing Victim characteristic Analysis"),
                 helpText("The victim reports based on offense division, sex, and age group are shown in the Sunburst Chart."),
                 sliderInput("Year",
                             label = "Year",
                             min = mi,
                             max = ma,
                             value = c(mi, ma),sep=""),
                 h4("Additional Info:"),
                 h5("- Click on the Sunburst Chart to get information in detail."),
                 h5("- On clicking the second innermost level(gender), a line chart appears, showing the victims in the sub-offense division."),
                 h5("- Adjust the Year slider to change the Sunburst chart data."),
                 h5("- Here there are three different age groups: <18 years, 18-44 years, >45 years.")
               ),
               mainPanel(
                 fluidRow(h4("Sunburst Chart Showing Victim Characteristics"), plotlyOutput("sunburstPlot", width = "100%")),
                 fluidRow(h4(textOutput("sub_plot_text")),
                          plotlyOutput("sub_plot2", width = "100%"))
               )
             )
    ),
    tabPanel("Analysing LGA wise Offence Count and Median Income", 
             titlePanel("Analysing the Relationship Between LGA wise Offence Count and Median Income in Victoria State"),
             sidebarLayout(
               sidebarPanel(
                 checkboxGroupInput("Income_Level", h4("Income Level"), 
                                    choices = unique(merged_df$`Income_Level`), 
                                    selected = unique(merged_df$`Income_Level`)),
                 checkboxGroupInput("offence_division", h4("Offense Division"), 
                                    choices = unique(merged_df$`Offence Division`), 
                                    selected = unique(merged_df$`Offence Division`)),
                 h4("Additional Info:"),
                 h5("- Zoom in and out to see the map regions and bubbles clearly."),
                 h5("- Choropleth bubble map and scatter plot can be filtered by using the above checkboxes."),
                 h5("- Low Income corresponds to a median income < $50,000."),
                 h5("- Medium Income corresponds to $50,000-$60,000."),
                 h5("- High Income corresponds to > $60,000."),
                 h5("- Choropleth and bubble map can be filtered by using the above check boxes."),
                 h5("- Hover the scatter plot points to see more info."),
                 h5("- The line in the scatter plot is a best fit regression line it shows the relation between median income and offence count"),
                 h5("- Click on the map to see more info.")
               ),
               mainPanel(
                 fluidRow(h3("Choropleth Bubble Map Showing LGA Wise Offences Recorded and Median Income For Year 2023"),
                          leafletOutput("map")),
                 fluidRow(h3("Scatter Plot Showing Total Offence Count vs. Median Income For Year 2023"),
                          plotlyOutput("sc"))
               )
             )
    )
  )
)

# Defining the server function
server <- function(input, output) {
  
  # Stacked bar chart as plot1 in tab1
  # stores stacked bar chart
  output$tab1plot1 <- renderPlotly({
    # Filtering data based on checkboxes and year slider
    st_barchart_df <- nsw_vic_df %>%
      filter(Offense.Division %in% input$div_ft, Year >= input$y_ft[1], Year <= input$y_ft[2])
    
    # Plotting stacked bar chart using ggplot
    st_plot <- ggplot(st_barchart_df, aes(x = State, y = Offense.Count, fill = Offense.Division)) +
      geom_bar(stat = "identity", position = "stack", width = 0.7) + # Making stacked bar chart 
      facet_grid(~Year, labeller = labeller(Year = label_both)) + # Using year as facet grid
      theme_minimal() +
      scale_y_continuous(labels = label_number( suffix = "k", scale = 1e-3, big.mark = ",")) + # Changing y-axis labels to SI units (k)[2]
      labs(title = "Recorded Offences in NSW and Victoria between (2019-2023) years",
           x = "State",
           y = "Offense Count",
           fill = "Offense Division") +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.5),
            legend.position = "bottom")
    
    # Convert to plotly object
    ggplotly(st_plot)  #[4]
  })
  
  
  
  
  #  Victim Reports Analysis based on Relationship to Offender-TAB2 plot
  
  # defining reactive value type variables to store the hovered relationship and offence type
  hovered_relationship <- reactiveVal(NULL)
  hovered_crime <- reactiveVal(NULL)
  
  # accumulate_by function helps to animate line chart [3]
  accumulate_by <- function(dat, var) {
    var <- lazyeval::f_eval(var, dat)
    lvls <- plotly:::getLevels(var)
    dats <- lapply(seq_along(lvls), function(x) {
      cbind(dat[var %in% lvls[seq(1, x)], ], frame = lvls[[x]])
    })
    dplyr::bind_rows(dats)
  }
  # Rendering the line chart
  output$line_chart <- renderPlotly({
    # grouping data based on year for line chart
    line_df <- vict_rel_df %>%
      group_by(Year, `Relationship of Victim to Offender`) %>%
      summarise(vict_rep = sum(Victim.Reports, na.rm = TRUE), .groups = 'drop')
    
    #applying accumulateby function to line chart 
    line_df<-line_df%>% accumulate_by(~Year)
    
    # generating line chart using plotly
    p <- plot_ly(
      data = line_df,
      x = ~Year,
      y = ~vict_rep,
      frame = ~frame, # frame column returned by accumulate_by()
      color = ~`Relationship of Victim to Offender`,
      type = 'scatter',
      mode = 'lines+markers',
      hoverinfo = "text",
      text = ~paste("Year: ", Year, "<br>", #tooltip content
                    "Victim Reports: ", vict_rep, "<br>",
                    "Relationship to victim: ", `Relationship of Victim to Offender`),
      source = "line_chart" # defining source name for this plot
    ) %>%
      layout( #defining the layout structure
        xaxis = list(title = "Year"),
        yaxis = list(title = "Victim Reports"),
        legend = list(title = list(text = "Relationship to victim to offender"))
        
      )
    p <- p %>% animation_opts(
      frame = 200,     #animating speed of line graph 
      transition = 0, 
      redraw = FALSE
    )
    p <- p %>% animation_slider( #shows animation slider
      hide = T
    )
    p <- p %>% animation_button( #button position
      x = 1, xanchor = "right", y = 0, yanchor = "bottom"
    )
    p <- event_register(p, "plotly_hover") # registering hover event for plot p i.e, line chart
    p
  })
  
  # Observing the hovering events on the line chart
  observeEvent(event_data("plotly_hover", source = "line_chart"), {
    hover_df <- event_data("plotly_hover", source = "line_chart")
    if (!is.null(hover_df)) { # line chart is hovered 
      relationship_index <- hover_df$curveNumber + 1 # gives the line number
      relationships <- levels(vict_rel_df$`Relationship of Victim to Offender`)
      hovered_relationship(relationships[relationship_index]) # Getting the hovered relationship by line number
    } else {
      hovered_relationship(NULL) # if not hovered on line chart
    }
  })
  
  # Rendering the area chart between offense divisions and victim reports between years 2014-2023
  output$area_chart <- renderPlotly({
    if (is.null(hovered_relationship())) { #if any relationship is not hovered on page load
      area_df <- vict_rel_df %>% # grouping data based on hovered offense division
        group_by(Year, Offence.Division) %>%
        summarise(Reports = sum(Victim.Reports, na.rm = TRUE), .groups = 'drop')
      
      g <- plot_ly(
        data = area_df,
        x = ~Year,
        y = ~Reports,
        color = ~Offence.Division,
        type = 'scatter',
        mode = 'lines+markers',
        fill = 'tozeroy', # filling the line chart with colour to make it area chart
        hoverinfo = "text", # tooltip info
        text = ~paste("Year: ", Year, "<br>",
                      "Reports: ", Reports, "<br>",
                      "Offence Division: ", Offence.Division),
        source = "area_chart" # defining it's source as area_chart
      ) %>%
        layout(
          xaxis = list(title = "Year"), # defining x-axis labels
          yaxis = list(title = "Victim Reports"), #defining y-axis labels
          margin = list(l = 40, r = 40, b = 40, t = 40, pad = 4), #setting margin for the axes labels
          legend = list(title = list(text = "Offence Division"))
        )
      g <- event_register(g, "plotly_hover") # registering hover event for plot p [5]
      g 
    } else {
      #filtering data based on hovered relationship
      temp_df <- vict_rel_df %>% # 
        filter(`Relationship of Victim to Offender` == hovered_relationship())
      #grouping data based on hovered offense division
      area_df <- temp_df %>%
        group_by(Year, Offence.Division) %>%
        summarise(Reports = sum(Victim.Reports, na.rm = TRUE), .groups = 'drop')
      # creating area chart plots based on hovered relationship
      f <- plot_ly(
        data = area_df,
        x = ~Year,
        y = ~Reports,
        color = ~Offence.Division,
        type = 'scatter',
        mode = 'lines+markers',
        fill = 'tozeroy', # Area fill style
        hoverinfo = "text",
        text = ~paste("Year: ", Year, "<br>","Victim Reports: ", Reports, 
                      "<br>","Offence Division: ", Offence.Division),
        source = "area_chart"
      ) %>%
        layout(
          xaxis = list(title = "Year"),
          yaxis = list(title = "Victim Reports"),
          legend = list(title = list(text = "Offence Division"))  # legend title
        )
      f <- event_register(f, "plotly_hover") # registering hovering event for plot f
      f
    }
  })
  
  # observing event for area chart on hovering
  observeEvent(event_data("plotly_hover", source = "area_chart"), {
    hover_df <- event_data("plotly_hover", source = "area_chart")
    if (!is.null(hover_df)) { # when hovered on area chart
      crime_index <- hover_df$curveNumber + 1
      crimes <- levels(vict_rel_df$`Offence.Division`)
      hovered_crime(crimes[crime_index])
    } else {
      hovered_crime(NULL)
    }
  })
  
  # Title for area chart which changes based on hovered relationship
  output$area_chartTitle <- renderText({
    if (is.null(hovered_relationship())) {
      "2.Overall Victim Reports by Offence Division over the years(2014-2023)"
    } else {
      paste("2.Victim Reports by Offence Division When Offender is",
            hovered_relationship()," over the years(2014-2023)")
    }
  })
  output$sub_plottitle <- renderText({ 
    if (!is.null(hovered_crime()) && !is.null(hovered_relationship())) {
      paste("3.Victim Reports by Sub Offence Division for Offence division:", 
            hovered_crime(), ",\nWhen Offender is:", 
            hovered_relationship()," over the years(2014-2023)")
    } else {
      ""
    }
  })
  
  # Rendering the sub_plot
  output$sub_plot <- renderPlotly({
    if(!is.null(hovered_crime())&& !is.null(hovered_relationship())){ # on hovering offense division 
      temp_df <- vict_rel_df %>% #filtering data based on hovered offence and relationship
        filter(`Relationship of Victim to Offender` == hovered_relationship(), 
               Offence.Division == hovered_crime())
      # group the data based on the sub division and calculating victims reports sum based on it
      sub_df <- temp_df %>%
        group_by(Year, Offence.Subdivision) %>%
        summarise(Reports = sum(Victim.Reports, na.rm = TRUE), .groups = 'drop')
      # defining the line graph as sub plot and storing it in f 
      f <- plot_ly(
        data = sub_df,
        x = ~Year,
        y = ~Reports,
        color = ~Offence.Subdivision,
        type = 'scatter',
        mode = 'lines+markers',
        hoverinfo = "text",
        text = ~paste("Year: ", Year, "<br>",
                      "Victim Reports: ", Reports, "<br>",
                      "Offence sub-division: ", Offence.Subdivision),
        colors = c("red", "green", "blue", "brown", "magenta","yellow","black") ,
        source = "sub_plot"
      ) %>%
        layout( #defining axes labels
          xaxis = list(title = "Year"),
          yaxis = list(title = "Victim Reports"),
          legend = list(title = list(text = "Offence Sub Division")) 
        )
      f #returning the line graph plot
    }
  })
  
  
  
  
  # Sunburst plot in TAB3
  
  #Reactive value element to store the hovered Sex value
  hovered_ele <- reactiveVal(NULL)
  # stores sunburst chart
  output$sunburstPlot <- renderPlotly({
    # filtering the data for the sunburst chart based year from slider input
    data <- victms_df %>%
      filter(Year >= input$Year[1], Year <= input$Year[2])
    #grouping the data based on Offence.Division, Sex, Age.Group to get victim reports
    sunburst_data <- data %>%
      group_by(Offence.Division, Sex, Age.Group) %>%
      summarise(Reports = sum(Victim.Reports, na.rm = TRUE), .groups = 'drop')
    
    # storing unique values as labels
    offence_labels <- unique(sunburst_data$Offence.Division)
    sex_labels <- unique(sunburst_data$Sex)
    age_group_labels <- unique(sunburst_data$Age.Group)
    
    # id needed for sunburst as parameter
    ids <- c(
      offence_labels,
      paste(rep(offence_labels, each = length(sex_labels)), sex_labels, sep = "-"),
      paste(rep(offence_labels, each = length(sex_labels) * length(age_group_labels)),
            rep(sex_labels, each = length(age_group_labels)), 
            age_group_labels, sep = "-")
    )
    #labels for sunburst to plot
    labels <- c(
      "Crimes against <br>the person", "Property and <br>deception offences",  # Offence Labels (Level 1)
      "Females", "Males", "Females", "Males",  # Sex Labels (Level 2)
      "18 to 44 years", "<18 years", ">45 years",
      "18 to 44 years", "<18 years", ">45 years",
      "18 to 44 years", "<18 years", ">45 years",
      "18 to 44 years", "<18 years", ">45 years"
    )
    # defining the heirarchy
    parents <- c(
      rep("", length(offence_labels)),  # Offence divisions have no parents
      rep(offence_labels, each = length(sex_labels)),  # Each offence division is a parent to all sex categories
      paste(rep(offence_labels, each = length(sex_labels) * length(age_group_labels)),
            rep(sex_labels, each = length(age_group_labels)), sep = "-")  # Each sex category is a parent to all age groups
    )
    # defining the values it will be the size of the sunburst chart values
    values <- c(
      sunburst_data %>% group_by(Offence.Division) %>% summarise(Reports = sum(Reports)) %>% pull(Reports),
      sunburst_data %>% group_by(Offence.Division, Sex) %>% summarise(Reports = sum(Reports)) %>% pull(Reports),
      sunburst_data$Reports
    )
    # defining colours for each level 
    colors <- c("#FFCC99","#99CC99", "#FF99CC","#99CCFF", "#FF99CC","#99CCFF",
                "#CCCC99", "#CCFF99", "#CC99CC", "#CCCC99", "#CCFF99", "#CC99CC",
                "#CCCC99", "#CCFF99", "#CC99CC", "#CCCC99", "#CCFF99", "#CC99CC")
    
    hovertext <- c(
      paste("Offence Division: ", offence_labels, "<br>Reports: ", values[1:length(offence_labels)]),
      paste("Sex: ", rep(sex_labels, length(offence_labels)), "<br>Reports: ", values[(length(offence_labels) + 1):(length(offence_labels) + length(sex_labels) * length(offence_labels))]),
      paste("Age Group: ", rep(age_group_labels, length(offence_labels) * length(sex_labels)), "<br>Reports: ", values[(length(offence_labels) + length(sex_labels) * length(offence_labels) + 1):length(values)])
    )
    
    # Create the sunburst plot
    fig <- plot_ly(
      ids = ids,
      labels = labels,
      parents = parents,
      values = values,
      type = 'sunburst',
      branchvalues = 'total',
      marker = list(colors = colors),
      hoverinfo= 'text',
      hovertext =hovertext,
      source="sunburst"
    )
    
    # Defing sunburst chart height and width
    fig <- fig %>% layout(height = 400, width = 500)
    fig<-event_register(fig,"plotly_click")
    fig
  })
  # Observing the hovering events on sunburst
  observeEvent(event_data("plotly_click", source = "sunburst"), {
    hover_data <- event_data("plotly_click", source = "sunburst")
    if (!is.null(hover_data)) { # line chart is hovered 
      hovered_ele(hover_data$pointNumber) # Each level and label has a number associated with it in sunburst
    } else {
      hovered_ele(NULL) # if not hovered on line chart
    }
  })
  # The line chart title is made dynamic and changes accordingly with click event
  output$sub_plot_text<- renderText({
    if (is.null(hovered_ele())||!(hovered_ele() %in% c(2,3,4,5))) {
      ""
    } else {
      
      paste("Detailed ",ifelse(hovered_ele() %in% c(2,4), "Female", "Male"),
            "Victim Reports For Offence: ",ifelse(hovered_ele() %in% c(2,3),
                                                  "Crimes against the person", "Property and deception offences"),", By Age-group and Year(2014-2023)")
    }
  })
  # Line chart for sunburst on hovering on sex label.
  output$sub_plot2 <- renderPlotly({
    if(!is.null(hovered_ele())){ # on hovering male or female
      if(hovered_ele() %in% c(2,3,4,5)){ # point numbers of female,male levels in both the cime divisions
        temp_df <- victms_df %>% #filtering data based on hovered label
          filter(Year >= input$Year[1], Year <= input$Year[2],
                 Offence.Division == ifelse(hovered_ele() %in% c(2,3),
                                            "A Crimes against the person", "B Property and deception offences"),
                 Sex == ifelse(hovered_ele() %in% c(2,4), "Female", "Male") )
        # group the data based on the sub division,age group and calculating victims reports sum based on it
        sub_df <- temp_df %>%
          group_by(Year, Age.Group,Offence.Subdivision) %>%
          summarise(Reports = sum(Victim.Reports, na.rm = TRUE), .groups = 'drop')
        # defining the line graph as sub plot and storing it in f 
        # Converting sub_df to factor
        sub_df$Offence.Subdivision <- as.factor(sub_df$Offence.Subdivision)
        
        #Creating line plot for victim reports by sub-offense division, age group
        gg <- ggplot(sub_df, aes(x = Year, y = Reports, 
                                 color = Offence.Subdivision)) +
          geom_line() +
          geom_point() +  # adding points
          labs(x = "Year", y = "Victim Reports") +
          theme_minimal() +  
          facet_wrap(~ Age.Group)+  # Facet by Age group
          theme(legend.position = "bottom",  # shift legend to the bottom
                legend.box = "horizontal"  # positioning legend items horizontally
          )
        g<-ggplotly(gg) # converting it to plotly object
        g <- g %>%
          layout(
            legend = list(orientation = "h", x = 0, y = -0.2),
            margin = list(b = 100)  # Adjusting bottom margin to place legend
          )
        g      #returns the line graph plot
      }}
  })
  
  
  
  
  
  
  # Filtering the data based on the checkbox's inputs
  filtered_data <- reactive({
    merged_df %>%
      filter(Income_Level %in% input$Income_Level &
               `Offence Division` %in% input$offence_division)
  })
  
  # Creating the choropleth bubble map
  output$map <- renderLeaflet({
    # grouping offense data by LGA and summarizing total `Offence Count`
    merged_data <- filtered_data() %>%
      group_by(`LGA NAME`, geometry, Median_Income, Income_Level) %>%
      summarize(Total_Offence_Count = sum(`Offence Count`), .groups = 'drop')
    
    # getting coordinates for plotting bubbles at the center of map
    coords <- st_coordinates(st_centroid(merged_data))
    merged_data$longitude <- coords[, 1]
    merged_data$latitude <- coords[, 2]  #[6]
    
    # Defining color palette
    pal <- colorNumeric(palette = "YlOrRd",
                        domain = merged_data$Total_Offence_Count)
    
    # Define leaflet choropleth map
    leaflet(merged_data) %>% #[8]
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons( # Choropleth Map
        fillColor = ~pal(Total_Offence_Count), 
        color = "black", # outline color
        weight = 1,
        fillOpacity = 0.7,
        highlightOptions = highlightOptions(
          weight = 2,
          color = "white",
          fillOpacity = 1,
          bringToFront = TRUE
        ),
        popup = ~paste("LGA: ",`LGA NAME`,"<br>", # Tooltip 
                       "Total Offences: ", Total_Offence_Count, "<br>",
                       "Median Income: $", Median_Income,"<br>",
                       "Income level: ",Income_Level)
      ) %>%
      addCircles( # Adding bubbles to the choropleth
        lng = ~longitude, 
        lat = ~latitude, 
        weight = 1,
        radius = ~Median_Income / 40,  # size of the bubbles
        popup = ~paste("LGA: ", `LGA NAME`, "<br>", # tooltip text
                       "Total Offences: ", Total_Offence_Count, "<br>",
                       "Median Income: $", Median_Income),
        color = "blue",
        fillOpacity = 1,
        layerId = ~`LGA NAME`
      ) %>%
      addLegend(  # defining legend
        "bottomright",
        pal = pal,
        values = ~Total_Offence_Count,
        title = "Total Offence Count",
        opacity = 1
      )
  })
  
  # Creating the scatter plot with tooltips and specified colors
  output$sc <- renderPlotly({
    merged_data <- filtered_data() %>%
      group_by(`LGA NAME`, geometry, Median_Income, Income_Level) %>%
      summarize(Total_Offence_Count = sum(`Offence Count`), .groups = 'drop')
    
    # mapping colors for income levels in the plot
    income_colors <- c("Low Income" = "orange", "Medium Income" = "blue", 
                       "High Income" = "green")
    # defining the scatter plot with best fit regression line
    sc_plt <- ggplot(merged_data) +
      geom_point(aes(x = Median_Income, y = Total_Offence_Count, 
                     color = Income_Level, text = paste("LGA: ", `LGA NAME`, 
                                                        "<br>Median Income: $", Median_Income, "<br>Total Offence Count: ", 
                                                        Total_Offence_Count)), size = 3) +
      geom_smooth(aes(x = Median_Income, y = Total_Offence_Count), 
                  method = "lm", se = FALSE, color = "black") +
      scale_color_manual(values = income_colors) +
      labs(
        x = "Median Income",
        y = "Total Offence Count") +
      theme_minimal()
    # Converting to plotly object to add tooltip 
    ggplotly(sc_plt, tooltip = "text") 
    
  })
}

# Run the Shiny application
shinyApp(ui = ui, server = server)


#Reference
#[1]https://stackoverflow.com/questions/44222796/shiny-with-multiple-tabs-and-different-sidebar-in-each-tab
#[2]https://stackoverflow.com/questions/56758733/in-r-use-and-k-as-a-y-axis-labels-for-thousands-of-dollars
#[3]https://plotly.com/r/cumulative-animations/
#[4]https://www.tutorialspoint.com/how-to-convert-ggplot2-graph-into-a-plotly-graph-in-r
#[5]https://stackoverflow.com/questions/56173297/how-do-i-use-plotly-events-to-filter-data-event-register-error-message
#[6]https://stackoverflow.com/questions/75899357/return-centroid-of-points-using-st-centroid
#[7]https://r-spatial.github.io/sf/reference/st_as_sf.html
#[8]https://r-charts.com/spatial/interactive-maps-leaflet/