############################################################################
#### LIBRAIRIES
############################################################################
library(ggplot2)## for graphics
library(viridis)## for color (optional)
library(RColorBrewer)## for color (optional)
library(cowplot)##to combine graphics
library(DALEX)##to plot decomposition 

rm(list=ls())

############################################################################
#### FUNCTIONS FOR PLOTTING
############################################################################
source("./utilsPLOT.R") ## utilities for plotting

############################################################################
#### LEVEL 1: LOCAL ANALYSIS PER PREDICTION TIME
############################################################################

## load DOE
load(paste0("./SupplementaryMaterials/DOE_GrIS_MIROC5-RCP85",".RData"))
## doe: matrix with values of the d=9 inputs (design of experiments)

## Choose year and load Shapley effects
year_choice <- "2100"
load(paste0("./SupplementaryMaterials/Shapley_y",year_choice,".RData"))
## S: matrix N=55 cases x d+1: SHAP values for the d inputs
## YHAT: ML-based predictions of the sea level for the 55 cases
## YTRUE: true values for the 55 cases
## mae: mean absolute error

## Choose Case
case_choice = 55

## format data
S00 <- S[case_choice,] 
xinput00 <- doe[case_choice,1:9]
yhat00 <- YHAT[case_choice]
ytrue00 <- YTRUE[case_choice]
x <- conversion(S00,yhat00,xinput00)

## error filtering
E <- ytrue00 - yhat00
(ff <- which(abs(x$contribution) <= abs(round(E,3))))
if (length(ff) > 0) x$sign[ff] <- "0"

## plot
plt<-plot.break_down(x,vnames=c("µ0",as.character(x$variable[-1])))

## format
plt<-plt+theme_classic()+tt+ylim(5,20)+ylab("sea level change (cm)")+ggtitle(paste0("Case",case_choice,"\n","Error=",round(E,3)))
print(plt)

############################################################################
#### LEVEL 2: MODEL STRUCTURE AT GIVEN PREDICTION TIME
############################################################################
NAME <- names(doe)

### PLOT for CATEGORICAL VARIABLES
plt <- list()
ivar <- c(1,2,3,6,8)## which categorical inputs
ccc <- 0
for (i in ivar){

	ccc <- ccc +1
	pSLE<-S[,i+1]
	df0 <- data.frame(pSLE=pSLE,var=doe[,i])
	df0$var <- as.factor(df0$var) 

	plt[[ccc]] <- ggplot(df0, aes(x=var,y=pSLE)) + 
				geom_boxplot(width = 0.75,outlier.shape = NA)+
  				geom_jitter(size=1.5,alpha=0.25, position=position_jitter(0.1),stroke = 1.5) +
  				scale_shape_manual(values = 0:12)+
  				ylab("µ (cm SLE)")+xlab("")+
  				xlab(NAME[i])+
  				theme_bw()+
  				ylim(-1,1)+tt+
  				ggtitle(paste0("(",letters[ccc],")"))+geom_hline(yintercept = c(-mae,mae),colour="red",linetype="dashed",size=1)
}

### Combine the plots
plt.cate<-plot_grid(
			plt[[1]],
			plt[[2]],
			plt[[3]],
			plt[[4]],
			plt[[5]],
			ncol=5)
print(plt.cate)

### PLOT for CONTINUOUS VARIABLES
ivar <- c(9,7,4,5)
ccc <- 0
plt <- list()
for (i in ivar){

	ccc <- ccc +1
	pSLE<-S[,i+1]
	df0 <- data.frame(pSLE=pSLE,var=doe[,i])

	plt[[ccc]] <- ggplot(df0, aes(x=var,y=pSLE)) + 
  		geom_point(shape = 21, size = 3, stroke = 1.25, fill = "grey",color="black", alpha = 0.5)+
  		scale_shape_manual(values = 0:12)+
  		ylab("µ (cm SLE)")+xlab("")+
  		theme_bw()+
  		ylim(-3,6)+tt+
  		ggtitle(paste0("(",letters[ccc],")"))+geom_hline(yintercept = c(-mae,mae),colour="red",linetype="dashed",size=1)+
  		xlab(NAME[i])

}

### Combine the plots
plt.cont<-plot_grid(
			plt[[1]],plt[[2]],
			plt[[3]],plt[[4]])
print(plt.cont)

############################################################################
#### LEVEL 3: GLOBAL ANALYSIS OVER TIME
############################################################################
year_choice <-  c(2020, 2030, 2040, 2050, 2060, 2070, 2080, 2090, 2100)

plt <- list()

for (i in 1:9){

	df00 <-NULL
	ERR <- matrix(0,9,3)
	ccc <- 0
	for(time in year_choice){

		ccc <- ccc + 1
	
		## load data at given time
		rm(S,YTRUE,YHAT)
		load(paste0("./SupplementaryMaterials/Shapley_y",time,".RData"))

		## normalize
		pSLE<-(S[,i+1]/(YTRUE-S[,1]))
		df0 <- data.frame(pSLE=pSLE,
				var=doe[,i],
				time=rep(time,55)
				)	
		df00 <- rbind(df00,df0[1:55,])

		##error estimate
		E <- YTRUE - YHAT 
		ERR[ccc,2] <-median(abs((E)/(YTRUE-S[,1])))
		ERR[ccc,1] <-quantile(abs((E)/(YTRUE-S[,1])),0.25)
		ERR[ccc,3] <-quantile(abs((E)/(YTRUE-S[,1])),0.75)

	}##time

	df00$time <- as.factor(df00$time)

	## data.frame for error plot
	df.th <- data.frame(time=year_choice,thr1=(ERR[,1]),thr2=(ERR[,2]),thr3=(ERR[,3]))
	df.th$time <- as.factor(df.th$time)
 
	## plot
	plt[[i]] <- ggplot(df00, aes(x=time,y=abs(pSLE))) + 
			geom_boxplot(width = 0.75, position = position_dodge(0.9))+
			ylab(expression(abs(µ[n])))+
  			xlab("Year")+
  			theme_bw()+
  			ylim(0,2)+
  			ggtitle(paste0(NAME[i]))+tt
	
	## add error
	plt[[i]] <- plt[[i]]+geom_point(data=df.th,aes(x=time,y=thr1),color="red",size=5,shape="+")
	plt[[i]] <- plt[[i]]+geom_point(data=df.th,aes(x=time,y=thr3),color="red",size=5,shape="+")

}#ivariable

plt.all<-plot_grid(
			plt[[3]],plt[[6]],plt[[7]],
			plt[[1]],plt[[4]],plt[[5]],
			plt[[2]],plt[[8]],plt[[9]])

print(plt.all)

############################################################################
#### REFERENCES
############################################################################
### MIROC5,RCP8.5-forced GrIS MME
### Goelzer, Heiko, et al. "The future sea-level contribution of the Greenland ice sheet: a multi-model ensemble study of ISMIP6." The Cryosphere 14.9 (2020): 3071-3096.
### DALEX
### Biecek, P. (2018). DALEX: explainers for complex predictive models in R. The Journal of Machine Learning Research, 19(1), 3245-3249.
### iBreakDown
### Gosiewska, A., & Biecek, P. (2019). Do not trust additive explanations. arXiv preprint arXiv:1903.11420.