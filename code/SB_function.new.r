library(tidyverse)
library(data.table)
library(Matrix)

setwd("C:/Users/Marco/")

a.non.inf.vec <- readRDS(file = "Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/working objects/a.non.inf.vec.rds")
P.prime.dt <- readRDS(file = "Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/working objects/P.prime.dt.rds")
c.vec <- readRDS(file = "Vysoká škola ekonomická v Praze/Tony Wei Tse Hung - YAY/working objects/c.vec.rds")


# New MLE iteration function by Matyas
SB_u_est_EM_mat <- function(c.vec, P.dt, a.vec, n.iter, ldt = 10^-04) {
  
  
  cdt <- data.table(c = c.vec)
  cdt <- cdt[, .(i = 1:.N, c = c)]
  pij <- cdt[P.dt, on = "i"]
  pij <- pij[c > 0] # remove those lines where c==0 because it will create 0 division
  adt <- data.table(a = a.vec)
  tiles <- adt[, .(j = 1:.N, u0 = a)]
  
  for(m in 0:(n.iter - 1)){
    cat(format(Sys.time()), paste0("---- calculating u", m + 1), "----\n")
    cols <- c("j", paste0("u", m))
    ju <- tiles[, cols, with = F]
    setnames(ju, c("j", "u"))
    pij <- ju[pij, on = "j"]
    denom <- pij[, .(sum_pik_uk = sum(u * pij)), by = i]
    pij <- denom[pij, on = "i"]
    faktor <- pij[, .(f = sum(c * pij / sum_pik_uk)), by = j]
    faktor.adj <- faktor[, f := fifelse(test = {is.na(f) | is.nan(f) | is.infinite(f)}, 1, f)] # if else to assure that the posterior is 1 to secure the same estimand value after ldt
    pij[, c("u", "sum_pik_uk") := NULL]
    tiles <- faktor.adj[tiles, on = "j"]
    tiles <- eval(parse(text = paste0("tiles[, u", m + 1, " := u", m, "* f]")))
    tiles <- eval(parse(text = paste0("tiles[,  u", m + 1, " := fifelse(u", m + 1, " < ldt, 0, u", m + 1, ")]")))
    tiles[, "f" := NULL]
  }
  
  return(tiles)
  
}

est1 <- SB_u_est_EM_mat(c.vec, P.prime.dt, a.non.inf.vec, n.iter = 10)


pij <- P.prime.dt

#convert c.vec to data.table and add an index i for mering with pij  
cdt<-data.table(c=c.vec)
cdt<-cdt[,.(i=1:.N,c=c)]
pij<-cdt[pij,on="i"]
pij <- pij[c > 0] #remove those lines where c==0 because it will create 0 division
head(pij)

#convert a.true.vec to data.table and add an index j for mering with pij  
adt<-data.table(a=a.non.inf.vec)
adt<-adt[,.(j=1:.N,a=a)]
pij<-adt[pij,on="j"]
head(pij)

#calculate the denominator
denom<-pij[,.(sum_pik_ak=sum(a*pij)),by=i]
head(denom)

#merge back to the P matrix
pij<-denom[pij,on="i"]
head(pij)

#calculate the factor which you multiply the previous value
faktor<-pij[,.(f=sum(c*pij/sum_pik_ak)),by=j]
head(faktor)

#merge back the factor and calculate the new value
adt<-faktor[adt,on="j"]
adt<-adt[,u:=a*f]
head(adt)

#make it for timing over several iterations
iterations<-2
tic()
summ<-summary(P.orcale.mat)
pij<-data.table(i = summ$i,
                j = summ$j,
                pij = summ$x)


cdt<-data.table(c=c.vec)
cdt<-cdt[,.(i=1:.N,c=c)]
pij<-cdt[pij,on="i"]
pij<-pij[c>0] #remove those lines where c==0 because it will create 0 division
adt<-data.table(a=a.true.vec)
tiles<-adt[,.(j=1:.N,u0=a)]


for(m in 0:(iterations-1)){
  cat(format(Sys.time()),paste0("---- calculating u",m+1),"----\n")
  cols<-c("j",paste0("u",m))
  ju<-tiles[,cols,with=F]
  setnames(ju,c("j","u"))
  pij<-ju[pij,on="j"]
  denom<-pij[,.(sum_pik_uk=sum(u*pij)),by=i]
  pij<-denom[pij,on="i"]
  faktor<-pij[,.(f=sum(c*pij/sum_pik_uk)),by=j]
  pij[,c("u","sum_pik_uk"):=NULL]
  tiles<-faktor[tiles,on="j"]
  tiles<-eval(parse(text=paste0("tiles[,u",m+1,":=u",m,"*f]")))
  tiles[,"f":=NULL]
}
toc()
# 9.81 sec elapsed
u.est2<-tiles[,c("u1","u2"),with=F]
u.estt<-data.table(u.est)
setnames(u.estt,c("u1","u2"))
all.equal(u.estt,u.est2)


