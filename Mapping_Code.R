{file=file.choose()
out="AGEA_week4_version5_test.pdf"
title <- "Week 4 (version5)"
North = 205 #how eastnorth corner is
East = 212 #how north east corner is
South = 106 #how eastsouth corner is
West = 115 #how north west corner is

col=c("red", "white")
ORIGscipen <- getOption("scipen")
options(scipen=10)

if(is.character(file)){
  data <- read.csv(file, stringsAsFactors=F)
  row.names(data) <- data[,1]
  data <- data[,-c(1)]
} else {data <- file}

if(out!=""){
  pdf(out)
}


mycol <- colorRampPalette(col)
ramp <- data.frame("ID"=rev(seq(50, 240, 1)), "col"=mycol(191)[1:191], stringsAsFactors=F)

par(mar=c(0,0,0,0))
plot(NULL, ylim=c(0, (nrow(data)+10)), xlim=c(0, ncol(data)), xlab="", ylab="", xaxt="n", yaxt="n", bty="n")
#points(100,100, pch=16, cex=7)

for (m in 1:ncol(data)){
  #text(m, nrow(data)+2, m, srt=90, adj=0, cex =0.4)
  for(i in 1:nrow(data)){
    wichColor <- which(data[i,m]>ramp$ID)[1]
    rect(m-0.5, i-0.5, m+0.5, i+0.5, col=ramp$col[wichColor], border=F)
  }
}

polygon(c(ncol(data)+0.5,-0.5,-0.5,ncol(data)+0.5,ncol(data)+0.5,North,ncol(data),South,0,North),
        c(nrow(data)+0.5, nrow(data)+0.5,-0.5,-0.5,nrow(data)+0.5,nrow(data),East,0,West,nrow(data)), col="white", border="white") 
for(i in 1:121){
  rect(0.45+i*0.05, nrow(data)+10, 0.5+i*0.05, nrow(data)+12, col=ramp$col[i], border=F)
}


MYlabels <- c(240, 50)

k <- 1

for (i in c(0.5, 8.5)){
  text(i, nrow(data)+9, labels=MYlabels[k], srt=90, adj=1, cex=0.9)
  k <- k+1
}
text((ncol(data)/2)-50, nrow(data)+9, labels=title, adj=0, cex=1)


if(out!=""){
  dev.off()
}
}


