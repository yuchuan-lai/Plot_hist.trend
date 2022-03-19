library(ggplot2)
library(dplyr)

annual_plot <- function(city.ID, climate.var, use.MA) {
  
  city.name <- cities.list$Name[cities.list$ID %in% city.ID]
  city.stn <- select(filter(stn.list, ID %in% cities.list$ID[cities.list$ID %in% city.ID]), c(Stn.name, Stn.stDate, Stn.edDate))
  city.stn <- data.frame("Name" = city.stn$Stn.name, "st.yr" = lubridate::year(city.stn$Stn.stDate), "ed.yr" = lubridate::year(city.stn$Stn.edDate))
  city.stn <- cbind(city.stn, data.frame("print.date" = paste0("(", city.stn$st.yr, "-", city.stn$ed.yr, ")")))
  num.stn <- nrow(city.stn)
  
  hist.annual.obs <- read.csv(paste0("./Data/A.QA_2022/", city.name, ".csv"))[-c(1)]
  hist.annual.MA <- read.csv(paste0("./Data/A.MA_2022/", city.name, ".csv"))[-c(1)]
  
  var.idx <- c(1:length(var.list))[var.list %in% climate.var]
  
  hist.obs.df <- data.frame("year" = hist.annual.obs$year, "obs" = select(hist.annual.obs, !!as.symbol(var.ID[var.idx])))
  hist.obs.df <- cbind(hist.obs.df, data.frame("Station" = rep(NA, nrow(hist.obs.df))))
  colnames(hist.obs.df) <- c("year", "obs", "Station")
  for (ii in c(1:num.stn)) {
    hist.obs.df$Station[hist.obs.df$year >= city.stn$st.yr[ii] & hist.obs.df$year <= city.stn$ed.yr[ii]] <- paste0("Station ", ii)
  }
  hist.obs.df <- filter(hist.obs.df, is.na(obs) == FALSE)
  hist.obs.reshape <- select(hist.obs.df, c(year, obs, Station))
  
  if (var.idx <= 11) {
    my.colors <- rev(rev(RColorBrewer::brewer.pal(num.stn + 3, "Oranges"))[1:num.stn])
  } else {
    my.colors <- rev(rev(RColorBrewer::brewer.pal(num.stn + 3, "Blues"))[1:num.stn])
  }
  
  theme_set(theme_bw())
  
  if (use.MA == TRUE) {
    hist.MA.df <- data.frame("year" = hist.annual.MA$year, "MA" = select(hist.annual.MA, !!as.symbol(var.ID[var.idx])))
    colnames(hist.MA.df) <- c("year", "MA")
    hist.MA.df <- filter(hist.MA.df, is.na(MA) == FALSE)
    
    p1 <- ggplot() + geom_point(data = hist.obs.reshape, aes(x = year, y = obs, color = Station), size = 2) +
      scale_color_manual(values = my.colors) +
      geom_line(data = hist.MA.df, aes(x = year, y = MA), color = "red", size = 1) +
      xlab("Year") +
      ylab(paste0(var.unit[var.idx])) +
      ggtitle(paste0(num.stn, " station(s) used:\n", paste0(apply(city.stn[c(1, 4)], 1, toString), collapse = ";\n"))) +
      scale_x_continuous(breaks = seq(1860, 2020, 10)) +
      theme(plot.title = element_text(size = 11), legend.position = "bottom")
    
  } else {
    
    p1 <- ggplot() + geom_point(data = hist.obs.reshape, aes(x = year, y = obs, color = Station), size = 2) +
      scale_color_manual(values = my.colors) +
      xlab("Year") +
      ylab(paste0(var.unit[var.idx])) +
      ggtitle(paste0(num.stn, " station(s) used:\n", paste0(apply(city.stn[c(1, 4)], 1, toString), collapse = ";\n"))) +
      scale_x_continuous(breaks = seq(1860, 2020, 10)) +
      theme(plot.title = element_text(size = 11), legend.position = "bottom")
  }
  
  return(p1)
}
title_text <- function(climate.var, city.ID){
  en.dash <- substr(signs::signs(-2, accuracy = 1), 1, nchar(signs::signs(-2, accuracy = 1)) - 1)
  city.print.name <- paste0(cities.list$P.name[cities.list$ID %in% city.ID], " ", cities.list$State.ID[cities.list$ID %in% city.ID])
  print.text <- paste0(climate.var, " ", en.dash, " ", city.print.name)
 
  return(print.text) 
}
stn.list <- read.csv("./Data/Stn.List.93cities.csv")[-c(1)]
states.list <- unique(stn.list$State)
cities.list <- stn.list[!duplicated(stn.list$ID),]
var.list <- c("Annual average temperature", "Annual warmest daily Tmax", "Annual warmest daily Tmin", "Annual coldest daily Tmax", "Annual coldest daily Tmin",
              "Annual count of days when daily Tmax > 25ºC (77ºF)", "Annual count of days when daily Tmin > 20ºC (68ºF)", "Annual count of days when daily Tmax < 0ºC (32ºF)", "Annual count of days when Tmin <0 ºC (32ºF)",
              "Annual cooling degree days", "Annual heating degree days", 
              "Annual total precipitation", "Annual maximum 1 day precipitation", "Annual maximum consecutive 5-day precipitation", 
              "Annual count of days when Precip ≥ 10mm (0.39in.)", "Annual count of days when Precip ≥ 20mm (0.79in.)")
var.ID <- c("Avg.Temp",  "TXx", "TXn", "TNx", "TNn", "SU", "TR", "ID", "FD", "CoDD", "HDD", "Tot.Prcp", "Rx1day", "Rx5day", "R10mm", "R20mm")
var.unit <- c("Temperature (ºF)", "Temperature (ºF)", "Temperature (ºF)", "Temperature (ºF)", "Temperature (ºF)", 
              "Count of days", "Count of days", "Count of days", "Count of days", "ºF-days", "ºF-days", "Precipitation (in.)", "Precipitation (in.)", "Precipitation (in.)", 
              "Count of days", "Count of days")

ui <- fluidPage(
  titlePanel("Historical time series of annual temperature and precipitation indices at selected U.S. cities"),

  sidebarLayout(
    sidebarPanel(
      selectInput("state", "Select the state", choices = states.list, selected = states.list[1]),
      selectInput("city", "Select the city", choices = cities.list$P.name[cities.list$State == states.list[1]], selected = cities.list$P.name[2]),
      selectInput("var", "Select the annual climate variable", choices = var.list, selected = var.list[1]),
      checkboxInput("use.MA", "Include the moving 10-year averages\n(shown as the red line)", value = TRUE),
    ),
    mainPanel(
      h3(textOutput("select.var")),
      plotOutput("plot", width = "700px")
    )
  )
  
)

server <- function(input, output, session) {
  
  
  observeEvent(input$state, {
    
    x <- input$state
    updateSelectInput(session, "city",
                      choices = cities.list$P.name[cities.list$State == x],
                      selected = cities.list$P.name[cities.list$State == x][1])
  })
  
  
  d <- reactive({
    cities.list$ID[cities.list$P.name == input$city]   
  }) 
  
  output$plot <- renderPlot({
    annual_plot(d(), input$var, input$use.MA)
    })
    
  output$select.var <- renderText(title_text(input$var, d()))
}

shinyApp(ui = ui, server = server)




