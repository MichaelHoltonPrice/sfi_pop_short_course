# Clear the workspace
rm(list=ls())

# Link to data:
# http://esapubs.org/archive/ecol/E084/093/Mammal_lifehistories_v2.txt
# S. K. Morgan Ernest. 2003. Life history characteristics of placental non-volant mammals. Ecology 84:3402.

read.ernest.data <- function(f) {
	ernest.data.raw <- read.csv(file=f,head=TRUE,sep="\t",skip=0,colClasses=c("factor","factor","factor","factor","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","numeric","character"))
	ernest.data<- reformat.raw.ernest.data(ernest.data.raw)
	return(ernest.data)
}

reformat.raw.ernest.data <- function(ernest.data.raw) {
	# Create blank data frame with appropriate columns
	ernest.data <- data.frame(t(rep(NA,14)))
	order <- ernest.data.raw$order
	family <- ernest.data.raw$family
	genus <- ernest.data.raw$Genus
	species <- ernest.data.raw$species

	#ind <- (1:length(y))%%2 == 1
	adult.mass <- m999toNA(ernest.data.raw$mass.g.)
	gestation.time <- m999toNA(ernest.data.raw$gestation.mo.)/12
	newborn.mass <- m999toNA(ernest.data.raw$newborn.g.)
	weaning.age <- m999toNA(ernest.data.raw$weaning.mo.)/12
	weaning.mass <- m999toNA(ernest.data.raw$wean.mass.g.)
	AFR <- m999toNA(ernest.data.raw$AFR.mo.)/12
	max.lifespan <- m999toNA(ernest.data.raw$max..life.mo.)/12
	litter.size <- m999toNA(ernest.data.raw$litter.size)
	litters.per.year <- m999toNA(ernest.data.raw$litters.year)
	refs <- m999toNA(ernest.data.raw$refs)
	ernest.data <- data.frame(order=order,family=family,genus=genus,species=species,adult.mass=adult.mass,gestation.time=gestation.time,newborn.mass=newborn.mass,weaning.age=weaning.age,weaning.mass=weaning.mass,AFR=AFR,max.lifespan=max.lifespan,litter.size=litter.size,litters.per.year=litters.per.year,refs=refs)
	gs <- paste(ernest.data$genus,ernest.data$species,sep=" ") # Genus species
	ernest.data <- cbind(ernest.data,gs)
	return(ernest.data)
}

m999toNA <- function(v) {
	# Convert all the instances of -999 in v to NA
	ind.na <- v == -999
	v[ind.na] <- NA
	return(v)
}

plotInfo <- list(szHomo=2)
plotInfo$colHomo <- 'red'
plotInfo$pchHomo <- 16

plotInfo$szApes <- 2
plotInfo$colApes <- 'green'
plotInfo$pchApes <- 15

plotInfo$szPrimates <- 1
plotInfo$colPrimates <- 'black'
plotInfo$pchPrimates <- 15

plotInfo$szOther <- 1
plotInfo$colOther <- 'gray'
plotInfo$pchOther <- 16

# Placental non-volant mammals [no bats]
ernest <- read.ernest.data('Mammal_lifehistories_v2.txt')
# From Anage Database for humans
# adult.mass		62.035 kg
# weaning.mass		11.750 kg
# AFR			13 yr
# max.lifespan		122.5 yr
# litters.per.year	0.3
# litter.size		1

homo <- data.frame(order="Primates",family="Hominidae",genus="Homo",species="sapiens",adult.mass=62035,gestation.time=NA,newborn.mass=NA,weaning.age=NA,weaning.mass=11750,AFR=13,max.lifespan=122.5,litter.size=1,litters.per.year=0.3,refs="",gs="Homo sapiens")
ernest <- rbind(ernest,homo) # p stands for prime

plotInfo$indHomo <- ernest$species == 'sapiens'
plotInfo$indApes <- ernest$family == 'Hominidae' & !plotInfo$indHomo # non-human apes
plotInfo$indPrimates <- (ernest$order == 'Primates') & !(plotInfo$indHomo | plotInfo$indApes)
plotInfo$indOther <- !(plotInfo$indHomo | plotInfo$indApes | plotInfo$indPrimates)


makePlot <- function(df,trait1,trait2,lab1,lab2,plotInfo) {
  plot(log(df[plotInfo$indOther,trait1]),log(df[plotInfo$indOther,trait2]),xlab=lab1,ylab=lab2,col=plotInfo$colOther,pch=plotInfo$pchOther,cex=plotInfo$szOther)
  points(log(df[plotInfo$indPrimates,trait1]),log(df[plotInfo$indPrimates,trait2]),xlab=lab1,ylab=lab2,col=plotInfo$colPrimates,pch=plotInfo$pchPrimates,cex=plotInfo$szPrimates)
  points(log(df[plotInfo$indApes,trait1]),log(df[plotInfo$indApes,trait2]),xlab=lab1,ylab=lab2,col=plotInfo$colApes,pch=plotInfo$pchApes,cex=plotInfo$szApes)
  points(log(df[plotInfo$indHomo,trait1]),log(df[plotInfo$indHomo,trait2]),xlab=lab1,ylab=lab2,col=plotInfo$colHomo,pch=plotInfo$pchHomo,cex=plotInfo$szHomo)
  legend('topleft',legend=c("Other","Primates",'Great Apes','Humans'),col=c(plotInfo$colOther,plotInfo$colPrimates,plotInfo$colApes,plotInfo$colHomo),pch=c(plotInfo$pchOther,plotInfo$pchPrimates,plotInfo$pchApes,plotInfo$pchHomo))
}

pdf('weaning_mass.pdf')
  makePlot(ernest,'adult.mass','weaning.mass','log(Adult Mass) [kg]','log(Weaning Mass) [kg]',plotInfo)
dev.off()

pdf('female_maturity.pdf')
  makePlot(ernest,'adult.mass','AFR','log(Adult Mass) [kg]','log(Female Maturity) [yr]',plotInfo)
dev.off()

pdf('max_lifespan.pdf')
  makePlot(ernest,'adult.mass','max.lifespan','log(Adult Mass) [kg]','log(Max Lifespan) [yr]',plotInfo)
dev.off()
