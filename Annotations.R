row_info <- chrInfo
col_info <- phen
x <- genomDat
col_color <- NULL

r <- 1

while (r <= length(col_info)) {

  if (nrow(unique(col_info[r])) == nrow(col_info)) {
    print(colnames(col_info[r]))
    r = r + 1
  }
  else {
    print(colnames(col_info[r]))
    col_var <- append(col_var, c(colnames(col_info[r])))
    r = r + 1
  }

}

w <- 1
color_vec <- c("skyblue", "blue", "yellow", "purple", "black", "red", "orange", "green", "cyan", "darkgreen")

for (v in col_var) {

  if (length(unique(col_info[,v])) > 3) {
    #print("Needs to be scales")
  }
  else if (length(unique(col_info[,v])) == 2){
    if (w > 10) {
      w <- 1
    }
    col_color <- append(col_color, color_vec[w:(w + 1)])
    w = w + 2
  }
  else if (length(unique(col_info[,v])) == 1) {
    if (w > 10) {
      w <- 1
    }
    col_color <- append(col_color, color_vec[w])
    w = w + 1
  }

}

col_color
