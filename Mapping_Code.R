DY <- AGACConversion[[2]] 
DX <- AGACConversion[[1]]

ORIGscipen <- getOption("scipen") 
options(scipen=10)
mycol <- colorRampPalette(c("green","yellow","orange","red"))
ramp <- data.frame("ID"=rev(seq(scale[length(scale)], scale[1], 1)), 
                   "col"=mycol(length(seq(scale[length(scale)], scale[1], 1)))[1:length(seq(scale[length(scale)], scale[1], 1))], 
                   stringsAsFactors=F)
ramp <- data.frame("ID"=rev(seq(0,1000,50)), "col"=mycol(21)[1:21], stringsAsFactors=F)

ramp <- data.frame("ID"=seq(0,1400,10), "col"= mycol(141)[1:141], stringsAsFactors=F)

  data <- Active
  #creates the plot, must be rerun every run through 
  par(mar=c(0,0,0,0))
  plot(NULL, ylim=c(0, (DY+60)), xlim=c(0, DX), xlab="", ylab="", xaxt="n", yaxt="n", bty="n")
  for (m in 1:DX){
    for(i in 1:DY){
      if (is.na(data[i,m])){
        NULL
      } else {
        wichColor <- max(which(data[i,m]>ramp$ID))
        colour <- paste(ramp$col[wichColor])
        rect(m-0.5, i-0.5, m+0.5, i+0.5, col=colour, border=colour)
      }
    }
  }
  
#this is the part that must get repeated every time
  L <- round(DX * .25/length(ramp$col),2)
  for(i in 1:length(ramp$col)){
    rect(0.4+i*L, DY+41, (0.4+L)+i*L, DY+45, col=paste(ramp$col[i]), border=paste(ramp$col[i]))
  }
  scale <-seq(0,1400,100) 
  lines <- seq(0.5,length(ramp$col)*L, length.out = length(scale))
  k <- 1
  for (i in lines){
    text(i, DY+40, labels=scale[k], srt=90, adj=1, cex=0.6)
    lines(x = c(i,i), y = c(DY+41.5, DY+43.5))
    k <- k+1
  }
  text((DX/2)-50, DY+9, labels=title, adj=0, cex=1)
  #ending
  if(out!=""){
    dev.off()
  }
  show(paste("round", W, "completed")) 

  
png("test.png")
plot(c(1:10),c(21:30))
dev.off()
