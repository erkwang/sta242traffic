#generate the randomly seeded matrix with red and blue cars
gridseed = function(r, j, p) {
  level = c("Red", "Blue", "Empty")
  totalnum = r * j
  if (length(p) == 1 && 2*p <= 1) {
    field = sample(x = level, size = totalnum, replace = TRUE, prob = c(p, p, 1-2*p)) 
  }
  else if (length(p) == 2 && sum(p) <= 1) {
    field = sample(x = level, size = totalnum, replace = TRUE, prob = c(p, 1-sum(p)))
  }
  else if (length(p) == 3 && sum(p) == 1) {
    field = sample(x = level, totalnum, replace = TRUE, prob = p)
  }
  else {
    stop("invalid probability")
  }
  field = dataframer(field, r, j)
  field
}

#place information in the matrix into a data frame
dataframer = function(fd, rownum, colnum) {
  row = rep(1:rownum, colnum)
  column = rep(1:colnum, each = rownum)
  df = data.frame(row, column, occupy = fd)
  df = df[(df[,3] != "Empty"), ]
  df
}


#move the "cars" according to given step length and direction
mover = function(infodf, stepup = 1, stepright = 1, move, r, j) {
  oldpos = infodf[c("row", "column")]
  newpos = oldpos
  if (move == "Red") {
    newpos$row[infodf$occupy == "Red"] = newpos$row[infodf$occupy == "Red"] - 1
  }
  else if (move == "Blue") {
    newpos$column[infodf$occupy == "Blue"] = newpos$column[infodf$occupy == "Blue"] + 1
  }
  newpos = edgecheck(newpos, r, j)
  newpos = poscheck(newpos, oldpos)
  newpos
}

#check if any car is out of edge after the movement
edgecheck = function(new, maxrow, maxcol) {
  new$row = sapply(new$row, function(rownum){
    if (rownum < 1) maxrow - (0 - rownum) else rownum
  })
  new$column = sapply(new$column, function(colnum){
    if (colnum > maxcol) colnum - maxcol else colnum
  })
  new
}

#check if there is any conflict in positions
poscheck = function(new, old) {
  new.collapse = as.numeric(paste(new$row, new$column, sep = ""))
  old.collapse = as.numeric(paste(old$row, old$column, sep = ""))
  reverse = which(new.collapse %in% old.collapse)
  new[reverse,] = old[reverse,]
  new
}

#execute move based on time
gridmove = function(infodf, t, r, j) {
  if ((t/2 - floor(t/2)) * 2) {
    newpos = mover(infodf, move = "Red", r = r, j = j)
  }
  else {
    newpos = mover(infodf, move = "Blue", r = r, j = j)
  }
  infodf = data.frame(newpos, occupy = infodf$occupy)
  infodf
}

#given the dimension, density and time, create the final grid
gridsim = function(nrow, ncol, density, time) {
  griddf = gridseed(r = nrow, j = ncol, p = density)
  origin = griddf
  for (i in 1:time) griddf = gridmove(infodf = griddf, t = i, r = nrow, j = ncol)
  cargrid = list(griddf = griddf, dimension = c(nrow, ncol), 
                 density = density, origin = origin)
  class(cargrid) = "CarGrid"
  cargrid
}