Kd <- 100 
c_total_receptor <- 100
c_total_ligand <- 100
lig_range1 <- -4
lig_range2 <- 2
rec_range1 <- -2
rec_range2 <- 2
kd_range1 <- -2
kd_range2 <- 2
receptor_name <- "Receptor"
ligand_name <- "Ligand"


volume <- 1
fixed_kd <- Kd
kdcoating <- 10^(12-0) #10^(12-input$pKdcoat[1])
fixed_receptor <- c_total_receptor
fixed_ligand <- c_total_ligand
seq.lig <- seq(from=lig_range1,to=lig_range2,by=1)
seq.rec <- seq(from=rec_range1,to=rec_range2,by=0.2)
seq.kd <- seq(from=kd_range1,to=kd_range2,by=1)
min.lig <- 10^min(seq.lig)*fixed_ligand
max.lig <- 10^max(seq.lig)*fixed_ligand
min.rec <- 10^min(seq.rec)*fixed_receptor
max.rec <- 10^max(seq.rec)*fixed_receptor
plot( c(min.rec,max.rec), c(min(min.lig,min.rec),max(max.lig,max.rec)), log="xy", col=0, main=paste("Concentration ", receptor_name,"-",ligand_name, HTML(" complex for fixed KD of "), fixed_kd, " pM", sep=""), xlab=paste("Concentration of ", receptor_name," (", c_total_receptor," pM)",sep=""), ylab=paste(ligand_name,"-",receptor_name," complex",sep="") )

#lines( c(min.lig,max.lig), c(min.rec,max.rec), col=1)
rec.dots <- 10^seq.rec*fixed_receptor
leggy <- c()
my.df <- NULL
mylist <- NULL
for (lig.cnt in seq.lig) {
  this.lig <- 10^lig.cnt*fixed_ligand
  ## now each line
  occ.dots <- c()
  xaxis <- c()
  yaxis <- c()
  for (rec.cnt in seq.rec) {
    this.rec <- 10^rec.cnt*fixed_receptor
    gfun <- function(x) {c(this.rec - x[1] - x[2]*x[1]/kdcoating - x[1]*x[3]/fixed_kd, 0-x[2]-x[2]*x[1]/kdcoating, this.lig-x[3]-x[1]*x[3]/fixed_kd)}
    fred <- multiroot( gfun, start=c(this.rec/2,0,this.lig/2), positive=TRUE )
    #gfun <- function(x) {c(this.rec - x[1] - x[2]*x[1]/kdcoating - x[1]*x[3]/fixed_kd, input$c_total_coating-x[2]-x[2]*x[1]/kdcoating, this.lig-x[3]-x[1]*x[3]/fixed_kd)}
    #fred <- multiroot( gfun, start=c(this.rec/2,input$c_total_coating/2,this.lig/2), positive=TRUE )
    c_free_receptor <- fred$root[1]
    c_free_coat <- fred$root[2]
    c_free_ligand <- fred$root[3]
    c_complex <- this.lig - c_free_ligand
    pc_occ = 100*(1-c_free_ligand/this.lig)
    occ.dots <- c(occ.dots, c_complex)
    xaxis <- seq(min.rec,max.rec, length.out = length(occ.dots))
    yaxis <- seq(from = min(min.lig,min.rec), to =max(max.lig,max.rec), length.out = length(occ.dots) )
    my.df <- (cbind(xaxis, yaxis,rec.dots, this.lig, occ.dots))
  }
  leggy <- c(leggy, paste(this.lig," pM ",ligand_name,sep="") )
  lines( rec.dots, occ.dots, col=(1+lig.cnt-min(seq.lig)),lwd=2 )
  mylist <- data.frame(rbind(mylist, my.df))
}
legend(x=min.rec,y=0.5*max.rec,legend=leggy, col=1:length(seq.rec), lty=1,lwd=2 )

#temp <- melt(mylist, id.vars= "this.lig", value.name = "occ.dots")
qplot <- ggplot(mylist)
#thinned <- floor(seq(from=1,to=dim(mylist)[1],length=70))
thinned <- mylist[mylist$rec.dots %in% c(1, 10, 100, 1000, 10000), ]
qplot <- qplot + geom_line(aes(x=rec.dots, y=occ.dots, group=this.lig, colour = this.lig))+
  scale_x_continuous(trans = "log10") + 
  scale_y_continuous(trans = "log10") +
  geom_point(data=mylist[thinned,], aes(x=(rec.dots), y=occ.dots, group=this.lig, colour = this.lig))

qplot <- qplot + geom_line(aes(x=rec.dots, y=occ.dots, group=this.lig, colour = this.lig))+
  geom_point(data=mylist[thinned,], aes(x=(rec.dots), y=occ.dots, group=this.lig, colour = this.lig))
  scale_x_continuous(trans = "log10") + 
  scale_y_continuous(trans = "log10") 
qplot 
 
# ggplot2 with logarithmic scale 
ggplot(mylist, aes(x=rec.dots, y=occ.dots, group=this.lig, colour = this.lig))+geom_line() +
  geom_point(data= mylist[mylist$rec.dots %in% c(1, 10, 100, 1000, 10000), ]) + 
  scale_x_continuous(trans = "log10") + 
  scale_y_continuous(trans = "log10") 
  

thinned <- floor(seq(from=1,to=dim(mylist)[1],length=70))
ggplot(d, aes(x=x, y=y))+geom_line()+geom_point(data=d[thinned,],aes(x=x,y=y))



qplot <- ggplot(mylist)
qplot <- qplot + geom_line(aes(x=rec.dots, y=occ.dots, group=this.lig, colour = this.lig))+
  xlim(c(min.rec,max.rec)) + 
  ylim(c(min(min.lig,min.rec),max(max.lig,max.rec)))
qplot

tb_data <- mylist[ , c("rec.dots", "this.lig", "occ.dots")]
#tb_data <- round(tb_data, 2)
tb_data_new <- tb_data[tb_data$rec.dots %in% c(1, 10, 100, 1000, 10000), ]
tb_data_spread <- spread(tb_data_new, key= "this.lig", value =  "occ.dots")
tb_data_spread <- data.table(tb_data_spread)
