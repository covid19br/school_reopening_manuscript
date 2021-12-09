# Functions

run_scn <- function(estado, cidade) {
  ### Load model and model parameters for specified city
  source("functions/Brazilian_Model.R")
  
  # Load SRAG data for specified city
  filename_h = paste0("DATA/",estado,"/",cidade,"/srag_hospitalization.csv")
  srag.hospitalization = read.csv(file = filename_h, colClasses = c("Date","numeric"))
  now.srag.zoo <<- zoo(x = srag.hospitalization$n, order.by = srag.hospitalization$date)
  
  filename_d = paste0("DATA/",estado,"/",cidade,"/srag_deaths.csv")
  srag.deaths = read.csv(file = filename_d, colClasses = c("Date","numeric"))
  now.obito.srag.zoo <<- zoo(x = srag.deaths$n, order.by = srag.deaths$date)
  
  # Load fitting results
  filename_f = paste0("DATA/",estado,"/",cidade,"/fitting_result.csv")
  fit.results <- read.csv(file = filename_f)
  params <- fit.results[which(fit.results$residuo == min(fit.results$residuo))[1],]
  
  # Load and update fitted parameters
  base_parameters = c()
  base_parameters <- update_parameters(base_parameters, estado = estado, cidade = cidade)
  parameters <- update_params_from_fit(parameters = base_parameters, file = filename_f)
  for (i in names(params)){
    if(i %in% names(parameters))  parameters[i] <- params[[i]]
  }
  
  res <- list()
  reffs <- list()
  outs <- list()
  
  interventions <- update_scenario_parameters(parameters, estado = estado, cidade = cidade)
  
  out <- run_covid19(parameters, interventions, Y, pclin = pclin)
  list_cols <- list(I=c(Iindex,CLindex,Xindex,Hindex,HCindex,
                        ICUindex,ICUHindex,ICUCindex,QIindex,QCindex)+1,
                    E=c(Eindex,QEindex)+1,
                    R=c(Rindex, QRindex)+1,
                    C=Cindex+1,
                    H=c(Hindex, HCindex)+1,
                    U=c(ICUindex, ICUHindex, ICUCindex)+1,
                    M=CMindex+1,
                    Q=c(QSindex,QEindex,QIindex,QRindex,QCindex,HCindex,ICUCindex,Xindex)+1)
  result <- result_ts(out[[2]], parameters,parameters["startdate"], diff=F,
                      cols=list_cols)
  outs[[1]] <- out[[2]]
  result$R <- result$R/sum(Y)
  result$MD <- diff(result$M) # daily mortality
  res[[1]] <- result
  
  # put all results in a single data.frame
  results <- do.call(merge.zoo, res)
  colnames(results) <- c(paste0(c(names(list_cols), "MD"), 
                                rep(1, each=length(list_cols)+1)))
  
  final.results <- list(results = results, outs = outs)
  
  return(final.results)
}


# # Plot results

plot_results <- function(result){
  
  scn.labels <- "Simulation"
  ylabels=c("Infected",
            "Exposed",
            "Fraction recovered",
            "Total observed cases",
            "Hospitalized",
            "ICU", "Cumulative deaths", "Total deaths",'Quarantined',
            "Effective R - Cori",
            "Effective R - CoMo")
  
  # create all plots
  options(scipen=10000)
  plots <- plot_scenarios_simple_zoo(result$results, scn.labels, data='srag',
                                     cols=c('I', 'E', 'R', 'C', 'H', 'U', 'M', 'MD','Q'),#c('I', 'r', 'C', 'H', 'U', 'MD',"Reff_cori","Reff_como"),
                                     ylabels=ylabels, lang=lang,
                                     hlines=c(r=1-1/2.5,
                                              U=unname(parameters['icu_beds_available']),
                                              H=unname(parameters['beds_available'])),
                                     label_months=T, line.size=2,color.palette="Dark2")
  
  return(plots)
}

# Plot interventions

plot.interventions <- function(estado, cidade) {
  
  base_parameters = c()
  parameters <- update_parameters(base_parameters, estado = estado, cidade = cidade)
  interventions <- update_scenario_parameters(parameters, estado = estado, cidade = cidade)
  gg_interventions <- ggplot.all.interventions(interventions, parameters,
                                               linecolors = c("orange","grey","black"),
                                               lsize = 1.5)
  return(gg_interventions)
}
