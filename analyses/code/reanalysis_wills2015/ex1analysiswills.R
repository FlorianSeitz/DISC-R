cat('DAU: EXE1\n')
cat('Author: Andy Wills\n')
cat('Date: 2014-11-20\n')
cde <- read.table("Experiment 1/exe1code.txt", header = TRUE, sep = "\t")
dta <- read.table("Experiment 1/exe1data.txt", header = TRUE, sep = "\t")
# The raw data file reuses subject numbers across different conditions.
# This is potentially confusing in some analyses so is fixed here.
dta$subj[dta$cond == 1024] <- dta$subj[dta$cond == 1024] + 1000 
dta$subj[dta$cond == 2048] <- dta$subj[dta$cond == 2048] + 2000 
dta$subj[dta$cond == 3072] <- dta$subj[dta$cond == 3072] + 3000 
dta$subj[dta$cond == 7500] <- dta$subj[dta$cond == 7500] + 7000 
# Model-based analysis
bigm <- array(0, dim=c(145,12))
colnames(bigm) <- c("cond","subj","hull","sail","os","ident","top1","left2","right3","model","consist","rt")
o <- 0
for (j in 1:145) {
	mdl <-c(0,0,0,0,0,0,0)
	tio <-0
	for (i in 1:48) { 
		if (cde[dta[i+o,'triad'],'hull'] == dta[i+o,'resp']) mdl[1] = mdl[1] + 1
		if (cde[dta[i+o,'triad'],'sail'] == dta[i+o,'resp']) mdl[2] = mdl[2] + 1
		if (cde[dta[i+o,'triad'],'os'] == dta[i+o,'resp']) mdl[3] = mdl[3] + 1
		if (cde[dta[i+o,'triad'],'ident'] == dta[i+o,'resp']) mdl[4] = mdl[4] + 1
		if (1 == dta[i+o,'resp']) mdl[5] = mdl[5] + 1
		if (2 == dta[i+o,'resp']) mdl[6] = mdl[6] + 1
		if (3 == dta[i+o,'resp']) mdl[7] = mdl[7] + 1
		if (0 == dta[i+o,'resp']) tio = tio + 1
	}
	mdl = mdl / (48-tio)
	bigm[j,'cond'] = dta[o+1,'cond']
	bigm[j,'subj'] = dta[o+2,'subj']
	bigm[j,3:9] = mdl
	bigm[j,'model'] = which.max(mdl)
	bigm[j,'consist'] = max(mdl)
	bigm[j,'rt'] = sum(dta[(1+o):(48+o),'rt']) / (48-tio)
	o = o + 48
}

cat('Traditional descriptives (For model consistency, see later)\n')
cat('The rows OS and IDENT correspond to the OS and ID values reported in Table 2\n')
library(psych)
bigmf <- as.data.frame(bigm)
print(describeBy(bigmf,bigmf$cond))

cat('Model-based desciptives\n')
mbt <- table(bigm[,'cond'],bigm[,'model'])
ud <- mbt[,1] + mbt[,2]
os <- mbt[,3]
id <- mbt[,4]
pos <- mbt[,5] + mbt[,6] + mbt[,7]
mbts <- cbind(ud,os,id)
mbts[1,] <- mbts[1,]/sum(mbts[1,])
mbts[2,] <- mbts[2,]/sum(mbts[2,])
mbts[3,] <- mbts[3,]/sum(mbts[3,])
mbts[4,] <- mbts[4,]/sum(mbts[4,])
mbts[5,] <- mbts[5,]/sum(mbts[5,])
print(mbts)
print('Traditional analysis Part 1')
print('1024ms vs 3072ms t-test')
tdta <- subset(bigmf, cond == 1024 | cond == 3072, select=c('cond','os','ident'))
print(t.test(os ~ cond, data = tdta,var.equal=TRUE))
print('1024ms vs 7500ms t-test')
tdta <- subset(bigmf, cond == 1024 | cond == 7500, select=c('cond','os','ident'))
print(t.test(os ~ cond, data = tdta,var.equal=TRUE))
print('1024ms vs 2048ms t-test')
tdta <- subset(bigmf, cond == 1024 | cond == 2048, select=c('cond','os','ident'))
print(t.test(os ~ cond, data = tdta,var.equal=TRUE))
print('3072 ms vs 7500 ms t-test')
tdta <- subset(bigmf, cond == 3072 | cond == 7500, select=c('cond','os','ident'))
print(t.test(os ~ cond, data = tdta,var.equal=TRUE))

print('Model-based analysis Part 1')
mbts <- cbind(ud,os,id)

print('1024ms vs 3072ms UD vs other.')
c <- rbind(c(mbts['1024','ud'],mbts['1024','os']+mbts['1024','id']),c(mbts['3072','ud'],mbts['3072','os']+mbts['3072','id']))
print(chisq.test(c,correct=FALSE))

print('1024ms vs 7500ms UD vs other.')
c <- rbind(c(mbts['1024','ud'],mbts['1024','os']+mbts['1024','id']),c(mbts['7500','ud'],mbts['7500','os']+mbts['7500','id']))
print(chisq.test(c,correct=FALSE))

print('1024ms vs 3072ms OS vs other.')
c <- rbind(c(mbts['1024','os'],mbts['1024','ud']+mbts['1024','id']),c(mbts['3072','os'],mbts['3072','ud']+mbts['3072','id']))
print(chisq.test(c,correct=FALSE))

print('1024ms vs 2048ms OS vs other.')
c <- rbind(c(mbts['1024','os'],mbts['1024','ud']+mbts['1024','id']),c(mbts['2048','os'],mbts['2048','ud']+mbts['2048','id']))
print(chisq.test(c,correct=FALSE))

print('1024ms vs 7500ms OS vs other.')
c <- rbind(c(mbts['1024','os'],mbts['1024','ud']+mbts['1024','id']),c(mbts['7500','os'],mbts['7500','ud']+mbts['7500','id']))
print(chisq.test(c,correct=FALSE))

print('3072ms vs 2048ms OS vs other.')
c <- rbind(c(mbts['3072','os'],mbts['3072','ud']+mbts['3072','id']),c(mbts['2048','os'],mbts['2048','ud']+mbts['2048','id']))
print(chisq.test(c,simulate.p.value=TRUE))

print("SUPPLEMENTARY ANALYSES (RESPONSE SET)")

# Look at consistency, removing the position bias people...
bigm <- bigm[bigm[,'model'] < 5,]
bigmf <- as.data.frame(bigm)
# ...and combining the two UD strategies
bigmf$model[bigmf$model == 1] <- 'ud'
bigmf$model[bigmf$model == 2] <- 'ud'
bigmf$model[bigmf$model == 3] <- 'os'
bigmf$model[bigmf$model == 4] <- 'id'
# Create a ud column, selecting the better of the two UD fits
bigmf$ud <- pmax(bigmf$hull,bigmf$sail)
# Closest competitor analysis
bigmf$win.margin <- 0
for (i in 1:nrow(bigmf)) {
  tmp <- c(bigmf[i,'ud'],bigmf[i,'os'],bigmf[i,'ident'])
  tmp <- tmp[order(tmp)]
  bigmf$win.margin[i] <- tmp[3] - tmp[2]
}

print('Table S1')

print('N (all cells)')
print(aggregate(consist ~ model + cond,data=bigmf,length))

print('Consistency (all cells)')
print(aggregate(consist ~ model + cond,data=bigmf,mean))

print('Consistency (collapse models)')
print(aggregate(consist ~ cond,data=bigmf,mean))

print('Consistency (collapse condition)')
print(aggregate(consist ~ model,data=bigmf,mean))

print('Margin (all cells)')
print(aggregate(win.margin ~ model + cond,data=bigmf,mean))

print('Margin (collapse models)')
print(aggregate(win.margin ~ cond,data=bigmf,mean))

print('Margin (collapse condition)')
print(aggregate(win.margin ~ model,data=bigmf,mean))

print('Response model type effect on consistency')
a1 <- aov(consist ~ model, data = bigmf)
print(summary(a1))
print(TukeyHSD(a1))

print('Response model type effect on margin')
a1 <- aov(win.margin ~ model, data = bigmf)
print(summary(a1))
print(TukeyHSD(a1))

print('Stimulus presentation time effect on consistency')
ag <- aggregate(consist ~ cond,data=bigmf,mean)
lin.mod <- lm(consist ~ cond, data = ag)
print(summary(lin.mod))

print('Stimulus presentation time effect on margin')
ag <- aggregate(consist ~ cond,data=bigmf,mean)
lin.mod <- lm(consist ~ cond, data = ag)
print(summary(lin.mod))

print('Save out model analysis')
bigmf[,'exp'] <- 'EXE1'
save(bigmf,file='exe1sum2.RData')

print('***END***')
