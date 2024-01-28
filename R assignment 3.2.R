install.packages("lubridate")
library (lubridate)

# Q3.2.1 -----------------------------------------------------------------------


## remind_me() function: ##

remind_me <- function() {
  
  monday_list <- list(c("Monday to-do list:", "PIPS lecure @ 13:00", "Start PIPS assignment/study"))
  
  tuesday_list <- list(c("Tuesday to-do list:", "Study day!"))
  
  wednesday_list <- list(c("Wednesday to-do list:", "PIPs lecture @ 13:00", "Check groceries"))
  
  thursday_list <- list(c("Thursday to-do list:", "PIPS due tomorrow!", "Double-check if you work tonight!"))
  
  friday_list <- list(c("Friday to-do list:", "PIPS assignment due!", "PIPs practical @ 13:00", 
                        "Check meetings and work schedule for next week!"))
  
  saturday_list <- list(c("Saturday to-do list:", "Work @ 15:00", "Have you left anyone on 'read'?"))
  
  sunday_list <- list(c("Sunday to-do list:", "Change linens", "Laundry", "Vaccuum", "Organize", "Trash", 
                        "Double-check calendar", "Submit work availability")) 
  
  if (as.Date("2024-01-08") <= Sys.Date() & Sys.Date() <= as.Date("2024-02-02")) {
  
    if (wday(Sys.time()) == 2) {
      print(monday_list, quote = FALSE)
    }
    
    if (wday(Sys.time()) == 3) {
      print(tuesday_list, quote = FALSE)
    }
    
    if (wday(Sys.time()) == 4) {
      print(wednesday_list, quote = FALSE)
    }
    
    if (wday(Sys.time()) == 5) {
    print(thursday_list, quote = FALSE)
  }
    
    if (wday(Sys.time()) == 6) {
    print(friday_list, quote = FALSE)
  }
  
    if (wday(Sys.time()) == 7) {
    print(saturday_list, quote = FALSE)
  }
  
    if (wday(Sys.time()) == 7) {
    print(sunday_list, quote = FALSE)
  }
  } else {
  print("Your reminders list and function dates need to be updated!", quote = FALSE)
}
}

remind_me()

## cheat() function: ##

cheat <- function(question) {
  if (question == 1) {
    cat("q1data <- rnorm(66, mean = 6.5, sd = 1, min = 3, max = 9) \n hist(q1data)")
  } else if (question == 2) {
    cat(
      "q2data <- read.csv('https://bit.ly/3GLVQ86') \n
        q2data <- as.data.frame(q2data) \n
        ggplot(data = q2data) + \n
          geom_point(aes(x = DATE, y = TMIN))"
    )
  } else if (question == 3) {
    cat(
      "dead_v_alive <- factor(titanic_train$Survived) \n
        ggplot(data = titanic_train) + \n
        geom_col(aes( \n
        x = Sex, \n
        y = as.numeric(factor(Sex)), \n
        fill = dead_v_alive \n
        )) + \n
        scale_fill_discrete(labels = c('dead', 'alive')) + \n
        labs(y = 'count', fill = 'How did it go?') + \n
        ylim(0 , 600)"
    )
  }
  
  
}



# Q3.2.2 -----------------------------------------------------------------------
library(tidyverse)
library(ggplot2)

make_art <-
  function(seed = runif(1, min = 1, max = 99999)) {
    
    set.seed(seed)
    
    number <- runif(1, min = 1, max = 999)
    palette <- sample(colors(), 10)
    
    
    
    polar_plot <- function(number, palette) {
      q3data <- tibble(
        x0 = runif(number),
        y0 = runif(number),
        x1 = x0 + runif(number, min = -(runif(1)), max = runif(1)),
        y1 = y0 + runif(number, min = -(runif(1)), max = runif(1)),
        shade = runif(number),
        size = runif(number)
      )
      
      ggplot(data = q3data,
             aes(
               x = x0,
               y = y0,
               xend = x1,
               yend = y1,
               colour = shade,
               size = size
             )) +
        coord_polar() +
        scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
        scale_x_continuous(expand = c(0, 0), limits = c(0, 1)) +
        scale_colour_gradientn(colours = palette) +
        scale_size(range = c(0, 10)) +
        theme_void()
    }
    
    polar_plot1 <- function(number, palette) {
      polar_plot(number, palette) + geom_segment(show.legend = FALSE)
    }
    
    
    polar_plot2 <- function(number, palette) {
      polar_plot(number, palette) + geom_path(show.legend = FALSE)
    }
    
    polar_plot3 <- function(number, palette) {
      polar_plot(number, palette) + geom_point(show.legend = FALSE)
    }
    
    
    sample(list(polar_plot1(number, palette), polar_plot2(number, palette), 
                polar_plot3(number, palette)), size = 1)
  }


make_art()
