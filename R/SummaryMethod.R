#summary methond for class "CarGrid"
summary.CarGrid = function(cargrid) {
  sum = list()
  sum$dimention = paste("Number of rows:", cargrid$dimension[1], 
                        "Number of columns:", cargrid$dimension[2])
  if (length(cargrid$density) == 1) {
    sum$density = paste("Density of Red:", cargrid$density, 
                        "Density of Blue:", cargrid$density)
  }
  else if (length(cargrid$density) > 1) {
    sum$density = paste("Density of Red:", cargrid$density[1], 
                        "Density of Blue:", cargrid$density[2])
  }
  sum$count = paste("Number of Red Cars:",
                    nrow(cargrid$griddf[cargrid$griddf$occupy == "Red",]),
                    " Number of Blue Cars:",
                    nrow(cargrid$griddf[cargrid$griddf$occupy == "Blue",]))
  sum$redorigin =positionprint(cargrid$origin, title = "Original Places of Cars")
  sum$redfinal = positionprint(cargrid$griddf, title = "Final Places of Cars")
  cat(unlist(sum), sep = "\n\n")
}

positionprint = function(cardf, title = NULL) {
  positions = paste("(", cardf$row, ",", cardf$column, ")", sep = "", collapse = " ")
  posprint = paste(title, positions, collapse = "\n")
  posprint
}