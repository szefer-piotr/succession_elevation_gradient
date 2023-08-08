# Multi-site beta diversity

# Upload community composition for each garden within each site.
source('code/prepare_data.R')

# Controls
ycdf <- COMMDATA[,grepl("yawan.*_c", colnames(COMMDATA))]
ycdfnz <- ycdf[rowSums(ycdf) != 0,]

ncdf <- COMMDATA[,grepl("numba.*_c", colnames(COMMDATA))]
ncdfnz <- ncdf[rowSums(ncdf) != 0,]

wcdf <- COMMDATA[,grepl("wanang.*_c", colnames(COMMDATA))]
wcdfnz <- wcdf[rowSums(wcdf) != 0,]

# Insects
yidf <- COMMDATA[,grepl("yawan.*_i", colnames(COMMDATA))]
yidfnz <- yidf[rowSums(yidf) != 0,]

nidf <- COMMDATA[,grepl("numba.*_i", colnames(COMMDATA))]
nidfnz <- nidf[rowSums(nidf) != 0,]

widf <- COMMDATA[,grepl("wanang.*_i", colnames(COMMDATA))]
widfnz <- widf[rowSums(widf) != 0,]


# Predators
ypdf <- COMMDATA[,grepl("yawan.*_p", colnames(COMMDATA))]
ypdfnz <- ypdf[rowSums(ypdf) != 0,]

npdf <- COMMDATA[,grepl("numba.*_p", colnames(COMMDATA))]
npdfnz <- npdf[rowSums(npdf) != 0,]

wpdf <- COMMDATA[,grepl("wanang.*_p", colnames(COMMDATA))]
wpdfnz <- wpdf[rowSums(wpdf) != 0,]


# Fungi
yfdf <- COMMDATA[,grepl("yawan.*_f", colnames(COMMDATA))]
yfdfnz <- yfdf[rowSums(yfdf) != 0,]

nfdf <- COMMDATA[,grepl("numba.*_f", colnames(COMMDATA))]
nfdfnz <- nfdf[rowSums(nfdf) != 0,]

wfdf <- COMMDATA[,grepl("wanang.*_f", colnames(COMMDATA))]
wfdfnz <- wfdf[rowSums(wfdf) != 0,]






library(BAT)
ybm <- beta.multi(ycdfnz)
nbm <- beta.multi(ncdfnz)
wbm <- beta.multi(wcdfnz)

ybmi <- beta.multi(yidfnz)
nbmi <- beta.multi(nidfnz)
wbmi <- beta.multi(widfnz)

ybmp <- beta.multi(ypdfnz)
nbmp <- beta.multi(npdfnz)
wbmp <- beta.multi(wpdfnz)

ybmf <- beta.multi(yfdfnz)
nbmf <- beta.multi(nfdfnz)
wbmf <- beta.multi(wfdfnz)


vardf <- data.frame("Site" = factor(c("Wanang", "Numba", "Yawan"), 
                                    levels = c("Wanang", "Numba", "Yawan")),
                    "JaccardAvg" = c(wbm[1,1], nbm[1,1], ybm[1,1]),
                    "JaccardVar" = c(wbm[1,2], nbm[1,2], ybm[1,2]),
                    "Treatment" = "Control")
vardfi <- data.frame("Site" = factor(c("Wanang", "Numba", "Yawan"), 
                                    levels = c("Wanang", "Numba", "Yawan")),
                    "JaccardAvg" = c(wbmi[1,1], nbmi[1,1], ybmi[1,1]),
                    "JaccardVar" = c(wbmi[1,2], nbmi[1,2], ybmi[1,2]),
                    "Treatment" = "Insecticide")
vardfp <- data.frame("Site" = factor(c("Wanang", "Numba", "Yawan"), 
                                    levels = c("Wanang", "Numba", "Yawan")),
                    "JaccardAvg" = c(wbmp[1,1], nbmp[1,1], ybmp[1,1]),
                    "JaccardVar" = c(wbmp[1,2], nbmp[1,2], ybmp[1,2]),
                    "Treatment" = "Predator")
vardff <- data.frame("Site" = factor(c("Wanang", "Numba", "Yawan"), 
                                    levels = c("Wanang", "Numba", "Yawan")),
                    "JaccardAvg" = c(wbmf[1,1], nbmf[1,1], ybmf[1,1]),
                    "JaccardVar" = c(wbmf[1,2], nbmf[1,2], ybmf[1,2]),
                    "Treatment" = "Fungi")

vardf_full <- rbind(vardf, vardfi, vardfp, vardff)
