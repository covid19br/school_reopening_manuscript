## Prepare environment

if(!require(dplyr)){install.packages("dplyr"); library(dplyr)}
if(!require(stringr)){install.packages("stringr"); library(stringr)}
if(!require(zeallot)){install.packages("zeallot"); library(zeallot)}
if(!exists("sheets_read",mode="function")) library(googlesheets4)

source('functions/parameter_calibration_functions.R')
source('functions/plots.R')
lang <- 'en'

## Run simulations
result_sp <- run_scn(estado = "SP", cidade = "Sao_Paulo")
result_rs <- run_scn(estado = "RS", cidade = "Porto_Alegre")
result_go <- run_scn(estado = "GO", cidade = "Goiania")

## Prepare plots
plots_sp <- plot_results(result_sp)
plots_rs <- plot_results(result_rs)
plots_go <- plot_results(result_go)

## Plot grid
combine.plots(plots_sp[c(1,4,5,6,7,9)], legend.pos='bottom')
combine.plots(plots_rs[c(1,4,5,6,7,9)], legend.pos='bottom')
combine.plots(plots_go[c(1,4,5,6,7,9)], legend.pos='bottom')

#############################################
### Plot interventions
#############################################

plot.interventions(estado = "SP", cidade = "Sao_Paulo")
plot.interventions(estado = "RS", cidade = "Porto_Alegre")
plot.interventions(estado = "GO", cidade = "Goiania")
