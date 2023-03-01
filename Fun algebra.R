type1 <- function(input){
  x <- sample(c(1:9), size = 3, replace  = FALSE)
 
   while(abs(prod(x) - input)> 1e-5){
     x <- sample(c(1:9), size = 3, replace  = FALSE)
  }
  return(x)
}

type2 <- function(input){
  x <- sample(c(1:9), size = 4, replace  = FALSE)
  
  while(abs(prod(x[1:3]) +x[4] - input)> 1e-5){
    x <- sample(c(1:9), size = 4, replace  = FALSE)
  }
  return(x)
}

type3 <- function(input){
  x <- sample(c(1:9), size = 4, replace  = FALSE)
  
  while(abs(prod(x[1:3]) -x[4] - input)> 1e-5){
    x <- sample(c(1:9), size = 4, replace  = FALSE)
  }
  return(x)
}

type4 <- function(input){
  x <- sample(c(1:9), size = 3, replace  = FALSE)
  
  while(abs(prod(x[1:2])  + x[3] - input)> 1e-5){
    x <- sample(c(1:9), size = 3, replace  = FALSE)
  }
  return(x)
}

type5 <- function(input){
  x <- sample(c(1:9), size = 3, replace  = FALSE)
  
  while(abs(prod(x[1:2])  - x[3] - input)> 1e-5){
    x <- sample(c(1:9), size = 3, replace  = FALSE)
  }
  return(x)
}


type6 <- function(input){
  x <- sample(c(1:9), size = 4, replace  = FALSE)
  
  while(abs(prod(x[1:2])  + x[3] +x[4] - input)> 1e-5){
    x <- sample(c(1:9), size = 4, replace  = FALSE)
  }
  return(x)
}


rootfunction <- function(input, type){
  if(type == 1){x <- type1(input)}
  if(type == 2){x <- type2(input)}
  if(type == 3){x <-  type3(input)}
  if(type == 4){x <-  type4(input)}
  if(type == 5){x <- type5(input)}
  if(type == 6){x <- type6(input)}
  return(x)
}

# write a GUI
dropdown <- c("xxx", "xxx+","xxx-","xx+","xx-","xx++")

#Main File for simulations from the GUI

list.of.packages <- c("gWidgets2","formattable",
                      "gWidgets2tcltk","tcltk", "lubridate", "tidyverse",
                      "reshape2", "stringr", "cowplot", "gridExtra", "corrplot",
                      "grid", "scales")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if (length(new.packages)) install.packages(new.packages)

source("./cfg/config_limit.R")


library(gWidgets2)
library(gWidgets2tcltk)
#Main File for simulations from the GUI

list.of.packages <- c("gWidgets2","formattable",
                      "gWidgets2tcltk","tcltk", "lubridate", "tidyverse",
                      "reshape2", "stringr", "cowplot", "gridExtra", "corrplot",
                      "grid", "scales")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if (length(new.packages)) install.packages(new.packages)



library(gWidgets2)
library(gWidgets2tcltk)

w <- gwindow("type Selection", visible = FALSE,toolkit =guiToolkit())
grp_name <- ggroup(container = w)
lyt <- glayout(container = grp_name)



lyt[1,1] <- "type:"
lyt[1,2] <- gcombobox(dropdown, container = lyt)

lyt[2,1] <- "result:"
lyt[2,2] <- gedit("", container = lyt)


lyt[3,3] <- gbutton(text = "calculate", 
                    border = TRUE, 
                    handler=NULL, 
                    action = NULL, 
                    container = lyt)

addHandlerChanged(lyt[3,3], # OK button is clicked
                  handler = function(h,...){
                    # call the GUI function
                    dropdown2 <-  c("xxx", "xxx+","xxx-","xx+","xx-","xx++")
                    
                    typeNumeric <- match(as.character(svalue(lyt[1,2])),dropdown2)
                    target      <- as.numeric(svalue(lyt[2,2]))
                    y <- rootfunction(target,type = typeNumeric)
                    
                    print(y)
                  }
)
visible(w) <- TRUE
























