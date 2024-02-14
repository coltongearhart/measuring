### --- Load packages ---- 

library(tidyverse)
library(plotly)

### --- Load data ---- 

# read and format data
data_measuring <- readxl::read_excel("measuring.xlsx") %>% 
  mutate(date = mdy(date),
         out_of_bed_time = str_c('2024-01-01', out_of_bed_time, sep = ' ') %>% ymd_hm, # set to constant day (only to plot the time info)
         bed_time = str_c('2024-01-01', bed_time, sep = ' ') %>% ymd_hm, # set to constant day (only to plot the time info)
         across(c(snooze_time, work_sessions, movement_time), as.numeric),
         work_hours = work_sessions * 2) %>% # convert to hours and change variable
  select(-work_sessions)

### --- Aggregate data ---- 

# create function to aggregate data
aggregate_data <- function(data, var, aggregate) {
  
  # conditionally check if need to aggregate based on selected variable
  if (var %in% c("out_of_bed_time", "bed_time")) {
    
    return(data)
  
  }
  
  else {
   
    # aggregate data based on different time frames
    data_agg = if (identical(aggregate, "daily")) {
      
      data
      
    } else if (identical(aggregate, "weekly")){
      
      data %>% 
        group_by(year(date), week(date)) %>% 
        summarize(date = max(date),
                  across(c(snooze_time, work_hours, movement_time), \(var) sum(var, na.rm = TRUE))) %>% 
        ungroup
      
    }else{ # monthly
      
      data %>% 
        group_by(year(date), month(date)) %>% 
        summarize(date = max(date),
                  across(c(snooze_time, work_hours, movement_time), \(var) sum(var, na.rm = TRUE))) %>% 
     
        ungroup
    }
    
    return(data_agg)
    
  }
  
}

# test function
data_measuring %>% aggregate_data(var = "snooze_time", aggregate = "monthly")

### --- Plot data ---- 

# create function to plot data
plot_data <- function(data, var, aggregate, smooth) {
  
  # conditionally create base ggplot objects
  # -> creating line plots with correct titles
  # -> adding correct reference lines too
  if (identical(var, "out_of_bed_time")) {
    
    g = data %>% 
      ggplot(aes(x = date,
                 y = out_of_bed_time,
                 group = 1)) + 
      geom_point(col = "grey50") + 
      geom_line(col = "grey50") + 
      geom_hline(yintercept = ymd_hms("2024/01/01 8:00:00") %>% as.numeric, # extra step needed for ggplotly
                 col = "darkgreen") + 
      scale_x_date(date_breaks = "1 week") + 
      scale_y_datetime(date_labels = ("%I %p")) + 
      labs(title= "Out of bed time",
           x = "Date",
           y = "Time")
    
  } else if (identical(var, "bed_time")) {
    
    g = data %>% 
      ggplot(aes(x = date,
                 y = bed_time,
                 group = 1)) + 
      geom_point() + 
      geom_line() + 
      geom_hline(yintercept = ymd_hms("2024/01/01 23:00:00") %>% as.numeric, # extra step needed for ggplotly
                 col = "darkgreen") + 
      scale_x_date(date_breaks = "1 week") + 
      scale_y_datetime(date_labels = ("%I %p")) + 
      labs(title = "Bed time",
           x = "Date",
           y = "Time")
    
  } else if (identical(var, "snooze_time")) {
    
    g = data %>% 
      ggplot(aes(x = date,
                 y = snooze_time,
                 group = 1)) + 
      geom_point(col = "grey50") + 
      geom_line(col = "grey50") + 
      geom_hline(yintercept = 30,
                 col = "darkgreen") + 
      scale_x_date(date_breaks = ifelse(aggregate %in% c("daily", "weekly"), "1 week", "1 month")) + 
      labs(title = "Snooze time",
           x = "Date",
           y = "Minutes")
    
  } else if (identical(var, "work_hours")) {
    
    g = data %>% 
      ggplot(aes(x = date,
                 y = work_hours,
                 group = 1)) + 
      geom_point(col = "grey50") + 
      geom_line(col = "grey50") + 
      geom_hline(yintercept = 8,
                 col = "darkgreen") + 
      scale_x_date(date_breaks = ifelse(aggregate %in% c("daily", "weekly"), "1 week", "1 month")) + 
      labs(title = "Work hours",
           x = "Date",
           y = "Hours")
    
  } else { # movement_time
  
    g = data %>% 
      ggplot(aes(x = date,
                 y = movement_time,
                 group = 1)) + 
      geom_point(col = "grey50") + 
      geom_line(col = "grey50") + 
      geom_hline(yintercept = 60,
                 col = "darkgreen") + 
      scale_x_date(date_breaks = ifelse(aggregate %in% c("daily", "weekly"), "1 week", "1 month")) + 
      labs(title = "Movement time",
           x = "Date",
           y = "Minutes")
    
  }
    
  # conditionally add smooth curve
  if(identical(smooth, TRUE)) {
    
    g = g + 
      geom_smooth()
  }
  
  g = g + theme_bw()
  
  ggplotly(g) %>% return()
  
}

# test function
data_measuring %>% 
  aggregate_data(var = "snooze_time", aggregate = "daily") %>% 
  plot_data(var = "snooze_time", aggregate = "daily")

### ---- Simulate app ---- 

# input list
input <- list()
var <- colnames(data_measuring)[-1]
aggregate <- c("daily", "weekly", "monthly")
input$var <- var[5]
input$aggregate <- aggregate[1]
input$smooth <- TRUE

# aggregate data and create plot
data_measuring %>% 
  aggregate_data(var = input$var, aggregate = input$aggregate) %>% 
  plot_data(var = input$var, aggregate = input$aggregate, smooth = input$smooth)

