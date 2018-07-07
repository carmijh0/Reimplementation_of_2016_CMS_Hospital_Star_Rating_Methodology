library("tidyverse")
library("dplyr")
library("ggplot2")
require(devtools);
devtools::install_github("huangrh/rstarating");
devtools::install_github("huangrh/relvm"); #!Problems installing custom package 'relvm' for lvm model recreation
devtools::install_github("huangrh/rclus");
require(rstarating); require(relvm); require(rclus)

op <- out_dir("C:/Users/Carmijh0/Desktop/Data_Science/Capstone/data/R_ratings") #Setting up out put directory

----------------------------------------------------------------

#Replicate the original cms sas pack (with mentioned inaccuracies)
#2016 data
  
x_og <- cms2016oct_input
#View(x_og)
x_og <- mstbl(x_og)
fit_original <- relvm(x_og) # fit the non-adaptive lvm model
sr <- rating(fit_original$groups$summary_score, iter.max = 1) 

write.csv(fit_original$groups$pars, file=file.path(op,"Oct2016_par_truelvm_fit_original.csv")) #the parameters
write.csv(fit_original$groups$preds, file=file.path(op,"Oct2016_preds_truelvm_fit_original.csv")) #group scores
write.csv(sr$summary_score, file=file.path(op,"Oct2016_sum_score_truelvm_fit_original.csv")) #the summary scores & stars

----------------------------------------------------------------
  
#Correct LVM and K means(adaptive LVM by avoiding quadrature altogether and instead calculating exact 
#integrals & make sure the k means converges)
#2016 data
  
x_1 <- cms2016oct_input
x <- mstbl(x)
fit_1 <- relm(x)
sr_1 <- rating(fit2$groups$summary_score, method="kmeans", iter.max = 100)

write.csv(fit_1$groups$pars, file=file.path(op,"Oct2016_par_truelvm_fit_1.csv")) #the parameters
write.csv(fit_1$groups$preds, file=file.path(op,"Oct2016_preds_truelvm_fit_1.csv")) #group scores
write.csv(sr$summary_score, file=file.path(op,"Oct2016_sum_score_truelvm_fit_1.csv")) #the summary scores & stars

----------------------------------------------------------------

#Updated December 2017 data with corrected LVM and K means

input <- rstarating::cms_star_rating_input_2017dec
X <- mstbl(input)
fit_2 <- relvm(x)
sr_2 <- rating(x=fit_2$groups$summary_score,method="rclus2",score_col="sum_score",iter.max=5000)

write.csv(fit_2$groups$pars, file=file.path(op,"Dec2017_par_truelvm_fit_2.csv")) #the parameters
write.csv(fit_2$groups$preds, file=file.path(op,"Dec2017_preds_truelvm_fit_2.csv")) #group scores
write.csv(sr_2$summary_score, file=file.path(op,"Dec2017_sum_score_truelvm_fit_2.csv")) #the summary scores & stars



