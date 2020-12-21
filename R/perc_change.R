perc_change <- function(old.value, new.value, print.result = "value"){
  perc.change <- list()
  change <- new.value - old.value
  perc.change$value <- change/old.value
  perc.change$perc <- perc.change$value*100
  perc.change$text <- paste0("The percentage change from ", old.value,
                             " to ", new.value,
                             " is ", round(perc.change$perc, 2), "%")
  if(print.result == "value"){
    print(perc.change$value)
  }

  if(print.result == "percentage"){
    print(perc.change$perc)
  }

  if(print.result == "text"){
    print(perc.change$text)
  }
}
