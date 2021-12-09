if(!require(ggplot2)){install.packages("ggplot2"); library(ggplot2)}
if(!require(ggcorrplot)){install.packages("ggcorrplot"); library(ggcorrplot)}
if(!require(RColorBrewer)){install.packages("RColorBrewer"); library(RColorBrewer)}
if(!require(gridExtra)){install.packages("gridExtra"); library(gridExtra)}
if(!require(patchwork)){install.packages("patchwork"); library(patchwork)}
if(!require(zoo)){install.packages("zoo"); library(zoo)}
if(!require(viridis)){install.packages("viridis"); library(viridis)}
if(!require(scales)){install.packages("scales"); library(scales)}

plot_scenarios_simple <- function(res, scn.labels, cols=c('C', 'H', 'U', 'M'),
                                  base_size=18){
    nscn <- ncol(res)/(1+length(cols))
    color_seq <- colorRampPalette(brewer.pal(8, "Accent"))(nscn)
    ylabels <- c("Symptomatic Cases", "Hospitalized Cases", "ICU Cases",
                 "Cumulative Deaths")
    plot.formatos <- theme_bw(base_size=base_size)

    plots <- list()
    for (label in 1:length(cols)){
        plots[[label]] <- ggplot(data=res)
        for (i in 1:nscn){
            plots[[label]] <- plots[[label]] +
                geom_line(aes_(x=as.name("times"),
                               y=as.name(paste0(cols[label], i)),
                               color=factor(scn.labels[i])),
                          size=1, show.legend=TRUE)
        }
        plots[[label]] <- plots[[label]] +
            #scale_y_log10(limits=c(10,NA)) +
            ylab(ylabels[label]) +
            scale_color_manual(values = color_seq, name = "Cenário", labels = scn.labels) +
            theme(legend.position = "right", legend.box = "vertical") +
            plot.formatos
    }
    return(plots)
}

plot_scenarios_simple_zoo <- function(res, scn.labels, logscale=F,
                                      cols=c('C', 'H', 'U', 'M'),
                                      ylabels=c("Total Observed Cases",
                                                "Hospitalized Cases", "ICU
                                                Cases", "Cumulative Deaths"),
                                      data=NULL, hlines=c(), base_size=18,
                                      lang='pt', legend.pos='bottom',
                                      label_months=F, line.size=1, color.palette="Accent"){
    nscn <- ncol(res)/length(cols)
    color_seq <- colorRampPalette(brewer.pal(8, color.palette))(nscn)
    plot.formatos <- theme_bw(base_size=base_size)
    if (lang=='pt') {
        legend.title <- "Cenários"
        x.label <- "Data"
    }
    if (lang=='en') {
        legend.title <- "Scenarios"
        x.label <- "Date"
    }
    plots <- list()
    for (label in 1:length(cols)){
        plots[[label]] <- ggplot(data=res)
        for (i in 1:nscn){
            plots[[label]] <- plots[[label]] +
                geom_line(aes_(x=~Index,
                               y=as.name(paste0(cols[label], i)),
                               color=as.character(i)),
                          size=line.size, show.legend=TRUE)
        }
        # add hlines
        if(cols[label] %in% names(hlines)){
            plots[[label]] <- plots[[label]] +
                geom_hline(yintercept=hlines[cols[label]], linetype="dashed",
                           color="black")
        }
        if (legend.pos=="right")
            plots[[label]] <- plots[[label]] +
                theme(legend.position = "right", legend.box = "vertical")
        if (legend.pos=="bottom")
            plots[[label]] <- plots[[label]] +
                theme(legend.position = "bottom", legend.box = "horizontal")

        plots[[label]] <- plots[[label]] +
            ylab(ylabels[label]) + xlab(x.label) +
            scale_color_manual(values = color_seq, name = legend.title, labels = scn.labels) +
            plot.formatos
        if (logscale)
            plots[[label]] <- plots[[label]] + scale_y_log10(limits=c(10,NA))
        if(label_months)
            plots[[label]] <- plots[[label]] +
                scale_x_date(date_breaks = "months" , date_labels = "%b") +
                theme(axis.text.x=element_text(angle=90, hjust=1))

        if(!is.null(data)){
            # TODO: include points in the legend (shape=? + scale_shape_manual))
            if(data=='srag'){
                data_c <- now.srag.zoo
                data_cm <- now.obito.srag.zoo
            } else if(data=='covid'){
                data_c = now.covid.zoo
                data_cm = now.obito.covid.zoo
            }
            if(cols[label] == 'C'){
                plots[[label]] <- plots[[label]] +
                    geom_point(data=fortify(data_c, melt=T),
                               aes(x=Index, y=Value))
            } else if(cols[label] == 'M') {
                plots[[label]] <- plots[[label]] +
                    geom_point(data=fortify(data_cm, melt=T),
                               aes(x=Index, y=Value))
            }
        }
    }
    return(plots)
}

plot_scenarios_sensitivity <- function(QQ, scn.labels, cols=c('C', 'H', 'U', 'M'),
                                       ylabels=c("Symptomatic Cases",
                                                 "Hospitalized Cases",
                                                 "ICU Cases",
                                                 "Cumulative Deaths"),
                                       quantiles=c(0.025, 0.5, 0.975),
                                       data=NULL, hlines=c(), logscale=FALSE,
                                       base_size=18, lang='pt',
                                       legend.pos='bottom', label_months=F){
    nscn <- length(QQ)
    if(missing(scn.labels))
        scn.labels <- 1:nscn
    QQ[["suffixes"]] <- 1:nscn
    QQ <- do.call(cbind.zoo, QQ)
    color_seq <- colorRampPalette(brewer.pal(8, "Accent"))(nscn)
    plot.formatos <- theme_bw(base_size=base_size)
    if (lang=='pt')
        legend.title <- "Cenários"
    if (lang=='en')
        legend.title <- "Scenarios"

    plots <- list()
    for (label in 1:length(cols)){
        plots[[label]] <- ggplot(data=QQ)
        for (i in 1:nscn){
            plots[[label]] <- plots[[label]] +
                geom_line(aes_(x=~Index,
                               y=as.name(paste(cols[label], 100*quantiles[2], i, sep='.')),
                               color=as.character(i)), show.legend=TRUE) +
                geom_ribbon(aes_(x=~Index,
                                 ymin=as.name(paste(cols[label], 100*quantiles[1], i, sep='.')),
                                 ymax=as.name(paste(cols[label], 100*quantiles[3], i, sep='.'))),
                            fill=color_seq[i], alpha=0.3)
        }
        # add hlines
        if(cols[label] %in% names(hlines)){
            plots[[label]] <- plots[[label]] +
                geom_hline(yintercept=hlines[cols[label]], linetype="dashed",
                           color="black")
        }
        if (legend.pos=="right")
            plots[[label]] <- plots[[label]] +
                theme(legend.position = "right", legend.box = "vertical")
        if (legend.pos=="bottom")
            plots[[label]] <- plots[[label]] +
                theme(legend.position = "bottom", legend.box = "horizontal")
        if (logscale)
            plots[[label]] <- plots[[label]] + scale_y_log10(limits=c(10,NA))
        if(label_months)
            plots[[label]] <- plots[[label]] +
                scale_x_date(date_breaks = "months" , date_labels = "%b") +
                theme(axis.text.x=element_text(angle=90, hjust=1))

        plots[[label]] <- plots[[label]] +
            ylab(ylabels[label]) + xlab('date') +
            scale_color_manual(values = color_seq, name = legend.title, labels = scn.labels) +
            scale_x_date(date_labels = "%d/%b", name="") +
            plot.formatos

        if(!is.null(data)){
            # TODO: include points in the legend (shape=? + scale_shape_manual))
            if(data=='srag'){
                data_c <- now.srag.zoo
                data_cm <- now.obito.srag.zoo
            } else if(data=='covid'){
                data_c = diff(now.covid.zoo)
                data_cm = diff(now.obito.covid.zoo)
            }
            if(cols[label] == 'C'){
                plots[[label]] <- plots[[label]] +
                    geom_point(data=fortify(data_c, melt=T),
                               aes(x=Index, y=Value))
            } else if(cols[label] == 'M') {
                plots[[label]] <- plots[[label]] +
                    geom_point(data=fortify(data_cm, melt=T),
                               aes(x=Index, y=Value))
            }
        }
    }
    return(plots)
}

plot.hist.posteriors <- function(posterior){
    post.gathered <- as.data.frame(abc.out$unadj.values) %>% gather()
    p <- ggplot(post.gathered, aes(value)) +
        geom_histogram(bins=8) +
        facet_wrap(~key, scales='free_x')
}

plot.cor.posteriors <- function(posterior){
    corr<-cor(posterior)
    p <- ggcorrplot(corr, hc.order = TRUE, type = "lower", lab=T,
                    outline.col = "white")
}

# arrange plots in a grid using POG
combine.plots <- function(plots, legend.pos='bottom', margin=0.3){
    if(legend.pos == 'bottom')
        return((((plots[[1]] + theme(legend.position = "bottom", legend.direction="vertical") | plots[[2]] + theme(legend.position = "none")| plots[[3]] + theme(legend.position = "none")) / (plots[[4]]  + theme(legend.position = "none")| plots[[5]] + theme(legend.position = "none") | plots[[6]] + theme(legend.position = "none"))) / guide_area()) + plot_layout(guide = "collect", heights = c(1, 1, margin)))
    if(legend.pos == 'right')
        return(((plots[[1]] + theme(legend.position = "right") | plots[[2]] + theme(legend.position = "none")| plots[[3]] + theme(legend.position = "none")) / (plots[[4]]  + theme(legend.position = "none")| plots[[5]] + theme(legend.position = "none") | plots[[6]] + theme(legend.position = "none"))) + plot_layout(guide = "collect"))
}

combine.plots.R_eff<-function(plots,legend.pos='bottom',margin=0.3){
    if(legend.pos == 'bottom')
        return((((plots[[7]] + theme(legend.position = "bottom", legend.direction="vertical")| plots[[8]] + theme(legend.position = "none")) / guide_area()) + plot_layout(guide = "collect", heights = c(1, 1, margin))))
    if(legend.pos == 'right')
        return(((plots[[7]] + theme(legend.position = "right") | plots[[8]] + theme(legend.position = "none"))))
}


### Plot results by age group

plot.ages <- function(data, index, age_group = 1:19, plot.label = TRUE, palette = viridis(19), n.breaks = 18,
                      margins = c(4,4,2,5), popstruc = NA, control_pop = FALSE, title = "", y_lim = NA,
                      start_date = "2020-02-19", lang = "pt", ylabel_age = NA, cex.lab = 1, warn = TRUE,
                      diff = FALSE, x_lim = NA) { 
    #To-do: start_date, automatizar 
    date_parameters = c(startdate = as.Date(start_date))
    date_sequence = as.Date(date_parameters["startdate"]) + data[,1]
    
    nindex = length(index)/19
    
    # Somar grupos de idades caso mais de um compartimento seja escolhido
    if(nindex > 1) {
        sumdata = matrix(NA, nrow(data), 19)
        for(j in 1:19) sumdata[,j] = rowSums(data[,index[j+(0:(nindex-1))*19]+1])
        if(diff) subdata <- apply(subdata, 2, diff)
        z <- zoo(sumdata, date_sequence)
        colnames(z) = ((1:19)*5)-5
    } else {
    w = index+1
    subdata <- data[,w]
    if(diff) subdata <- apply(subdata, 2, diff)
    z <- zoo(subdata, date_sequence)
    colnames(z) = ((1:19)*5)-5
    }
    
    if(lang == "pt") {
        if(is.na(ylabel_age)){
        if(control_pop == FALSE) ylabel_age = "Número acumulado de mortes"
        if(control_pop == TRUE)  ylabel_age = "Proporção de mortes acumuladas por faixa etária"}
        xlabel_age = "Data"}
    
    if(lang == "en") {
        if(is.na(ylabel_age)){
        if(control_pop == FALSE) ylabel_age = "Cumulative number of deaths"
        if(control_pop == TRUE)  ylabel_age = "Proportion of cumulative deaths according to age group"}
        xlabel_age = "Date"}
    
    
    if(all(control_pop == FALSE & is.na(y_lim))) y_lim = c(0,max(z[,age_group]))
    if(all(control_pop == TRUE & is.na(y_lim))) y_lim = c(0,max( t(z[,age_group])/popstruc$pop[age_group] ))
    
    # To do: adjust so it calculates the date ranges
    month_seq = seq(as.Date("2020-03-01") ,as.Date("2021-06-01") , by = "months")
    if(is.na(x_lim)) {x_lim = date_sequence[c(1,length(date_sequence))]} else {x_lim = as.Date(x_lim)}
    
    par(mar = margins)
    plot(z[,1], type = "n", ylim = y_lim, 
         xlim = x_lim,
         xlab = xlabel_age, ylab = ylabel_age, main = title,
         cex.lab = cex.lab, xaxt = "n")
    axis(1, at = month_seq, labels = format(month_seq, "%b"), las = 2)
    
    if(control_pop == FALSE) for(i in age_group) lines(z[,i], col = palette[i])
    if(control_pop == TRUE)  for(i in age_group) lines(z[,i]/popstruc$pop[i], col = palette[i])
    # plot(z[,i]/popstruc$pop[i])
    # i = 10
    # popstruc$pop[i]
    # max(z[,i])
    # max(z[,i])/popstruc$pop[i]
    # plot(z[,i])
    
    if (plot.label) { 
        # Set up and plot labels
        if(control_pop == FALSE) finalcount <- t(z)[,nrow(z)]# t(z[nrow(z),])
        if(control_pop == TRUE) finalcount <- t(z)[age_group,nrow(z)]/popstruc$pop[age_group]
        
        if(n.breaks == 1) {
        labeldf <- data.frame(label = "0 ~ 90*", ypos = mean(finalcount))
        } else {
        dist <- hist(finalcount, breaks = seq(0,max(finalcount),length.out = n.breaks), plot = F)
        
        labeldf <- data.frame()
        for(i in 1:length(dist$breaks)){
            
            if(i < length(dist$breaks)) {
                invalue <- finalcount >= dist$breaks[i] & finalcount < dist$breaks[i+1] } else {
                    invalue <- finalcount >= dist$breaks[i]
                }
            
            if(any(invalue)==TRUE){
                
                if (sum(invalue) > 3){
                    if(all(invalue[c(min(which(invalue)):max(which(invalue)))])) {
                        age_label = paste0(names(invalue)[min(which(invalue))], " - ",  names(invalue)[max(which(invalue))])
                    } else {
                        age_label = paste0(names(invalue)[min(which(invalue))], " ~ ",  names(invalue)[max(which(invalue))], "*")
                        if(warn == TRUE) warning("* Not all age groups are represented within this range")
                    }
                    
                } else {  
                    age_label = paste(names(invalue)[invalue], collapse = ", ")
                }
                
                tempdf <- data.frame(label = age_label, ypos = mean(finalcount[invalue]))
                labeldf <- rbind(labeldf, tempdf)
            }}}
        
        for(i in 1:nrow(labeldf)) mtext(labeldf$label[i], side = 4, line = 0.5, at = labeldf$ypos[i], cex = 0.7, las = 1)
    }
    
}

# Plot number of cases according to age groups similar to SP prevalence studies

# data = out2
# index = Cindex
# ylabel_age = "Proporção de casos acumulados por faixa etária"; warn = F; control_pop = T; y_lim = c(0,1); cex.lab = 0.9
# margins = c(4,4,2,3); palette = viridis(6); title = ""; start_date = "2020-02-19"; lin.col = "red"; lin.wd = 1 ;by.age = T

plot.prev <- function(data, index, palette = viridis(6), 
                      margins = c(4,4,2,5), popstruc = NA, control_pop = FALSE, title = "",
                      start_date = "2020-02-19", lang = "pt", ylabel_age = NA, diff = FALSE,
                      cex.lab = 1, warn = TRUE, y_lim = NA, lin.col = "red", lin.wd = 1,
                      by.age = TRUE, return.max = FALSE, x_lim = NA) { 
    #To-do: start_date, automatizar
    date_parameters = c(startdate = as.Date(start_date))
    date_sequence = as.Date(date_parameters["startdate"]) + data[,1]
    
    nindex = length(index)/19
    
    # Somar grupos de idades caso mais de um compartimento seja escolhido
    if(nindex > 1) {
        sumdata = matrix(NA, nrow(data), 19)
        for(j in 1:19) sumdata[,j] = rowSums(data[,index[j+(0:(nindex-1))*19]+1])
        sumdata2 <- data.frame(a0_19 = rowSums(sumdata[,1:4]),
                               a20_29 = rowSums(sumdata[,5:6]),
                               a30_39 = rowSums(sumdata[,7:8]),
                               a40_49 = rowSums(sumdata[,9:10]),
                               a50_59 = rowSums(sumdata[,11:12]),
                               a60_99 = rowSums(sumdata[,13:19]))
        if(diff == TRUE) subdata2 <- apply(subdata2, 2, diff)
        z <- zoo(sumdata2, date_sequence)
    } else {
        w = index+1
        subdata <- data[,w]
        subdata2 <- data.frame(a0_19 = rowSums(subdata[,1:4]),
                               a20_29 = rowSums(subdata[,5:6]),
                               a30_39 = rowSums(subdata[,7:8]),
                               a40_49 = rowSums(subdata[,9:10]),
                               a50_59 = rowSums(subdata[,11:12]),
                               a60_99 = rowSums(subdata[,13:19]))
        if(diff == TRUE) subdata2 <- apply(subdata2, 2, diff)
        z <- zoo(subdata2, date_sequence)
    }
    
    if(lang == "pt") {
        if(is.na(ylabel_age)){
            if(control_pop == FALSE) ylabel_age = "Número acumulado de mortes"
            if(control_pop == TRUE)  ylabel_age = "Proporção de mortes acumuladas por faixa etária"}
        xlabel_age = "Data"}
    
    if(lang == "en") {
        if(is.na(ylabel_age)){
            if(control_pop == FALSE) ylabel_age = "Cumulative number of deaths"
            if(control_pop == TRUE)  ylabel_age = "Proportion of cumulative deaths according to age group"}
        xlabel_age = "Date"}
    
    popstruc2 <- c(sum(popstruc$pop[1:4]), sum(popstruc$pop[5:6]), sum(popstruc$pop[7:8]), sum(popstruc$pop[9:10]),
                   sum(popstruc$pop[11:12]),sum(popstruc$pop[13:19]))
    #  
    if(all(control_pop == FALSE & is.na(y_lim))) y_lim = c(0,max(z[,]))
    if(all(control_pop == TRUE & is.na(y_lim))) y_lim = c(0,max( t(z[,])/popstruc2))
    
    # if(control_pop == TRUE) {
    #     z$all <- rowSums(z)/sum(popstruc)
    # }

    ## To do: adjust to adapt according to data provided        
    month_seq = seq(as.Date("2020-03-01") ,as.Date("2021-06-01") , by = "months")
    if(is.na(x_lim)) {x_lim = date_sequence[c(1,length(date_sequence))]} else {x_lim = as.Date(x_lim)}
    
    par(mar = margins)
    plot(z[,1], type = "n", ylim = y_lim, 
         xlim = x_lim,
         xlab = xlabel_age, ylab = ylabel_age, main = title,
         cex.lab = cex.lab, xaxt = "n")
    axis(1, at = month_seq, labels = format(month_seq, "%b"), las = 2)
    if(control_pop == FALSE) for(i in 1:6) lines(z[,i], col = palette[i])
    if(control_pop == TRUE)  {
        if(by.age == TRUE) {for(i in 1:6) lines(z[,i]/popstruc2[i], col = palette[i]) } else {
        lines(z$all, col = lin.col, lwd = lin.wd)}
    }
    if(return.max) {
        whichmax <- function(column){
            which(column==max(column))
        }
        
        peak = index(z)[apply(z, 2, whichmax)]
        names(peak) = c("a0_19",  "a20_29", "a30_39", "a40_49", "a50_59", "a60_99")
       return(peak)
    }
}


## Plot Reff for different scenarios

plot_scenarios_reff <- function(result, scenarios, color.palette = "Dark2", 
                                base_size = 18, lang = "pt", scn.labels){ 
    
    Re_cori_scn <- data.frame(result$reffs[[1]]$Reff_cori_sample, 
                              Index = index(result$reffs[[1]]$Reff_cori_sample), 
                              scenario = scenarios[1])
    
    for(i in 2:length(result)) {
        Re_cori_scn <- rbind(Re_cori_scn,
                             data.frame(result$reffs[[i]]$Reff_cori_sample,
                                        Index = index(result$reffs[[i]]$Reff_cori_sample),
                                        scenario = scenarios[i]))    
    }
    
    nscn <- length(result)
    color_seq <- colorRampPalette(brewer.pal(8, color.palette))(nscn)
    
    if(missing(scn.labels)) scn.labels <- 1:nscn
    
    plot.formatos <- theme_bw(base_size=base_size)
    if (lang=='pt') {
        scn.labels <- names(scn.ALL.PT)[ match(scenarios,scn.ALL.PT) ]
        legend.title <- "Cenários"
        x.label <- "Data"
        y.label <- "Número reprodutivo efetivo"
    }
    if (lang=='en') {
        scn.labels <- names(scn.ALL)[ match(scenarios,scn.ALL) ]
        legend.title <- "Scenarios"
        x.label <- "Date"
        y.label <- "Effective reproduction number"
    }
    
    ggplot.Rt <- 
        ggplot(data = Re_cori_scn, aes(x=Index,y=Mean, group = scenario)) +
        geom_ribbon(aes(ymin = as.numeric(Lower), ymax = as.numeric(Upper), fill=factor(scenario)), 
                    alpha=0.3) +
        geom_line(aes(color = factor(scenario))) +
        scale_fill_manual(values = color_seq,  name = legend.title, labels = scn.labels) +
        scale_color_manual(values = color_seq) +
        geom_hline(yintercept=1, linetype="dashed", col="black") +
        scale_x_date( date_labels = "%d/%b", name=x.label) +
        ylab(y.label) + guides(color=FALSE)
    
    ggplot.Rt <- ggplot.Rt +
        theme(legend.position = "bottom", 
              legend.box = "horizontal",
              legend.direction = "vertical")
    
    return(ggplot.Rt)
}

#### Plotar interven??es ao longo do tempo


intervention.time.series <- function(INT, parameters) {
    
    start = INT["startdate",1]
    end = max(INT["enddate",])
    time = as.Date(parameters["startdate"]) +  c(start:end)  
   
    cov = rep(0,end - start)
    eff = rep(0,end - start)
    for(i in 1:ncol(INT)){
        time.range = INT[1:2,i] - start + 1
        cov[time.range[1]:time.range[2]] = INT[3,i]
        eff[time.range[1]:time.range[2]] = INT[4,i]
    }

    int.ts <- data.frame(cov = cov, eff = eff, prod = cov*eff)
    result <- zoo(int.ts, time)
    
    return(result)
}

plot.intervention <- function(INT, cidade = NA, main,
                              xlim = c("2020-02-20", "2021-02-01"),
                              line_colors = c("blue",2,"orange"),
                              line_types = c(1,2,2),
                              line_widths = c(2,1,1)) {
  
  INT <- INT[index(INT) >= xlim[1] & index(INT) <= xlim[2],]
  
    plot(INT$cov, ylim = c(0,1), xlab = "", ylab = "", type = "n", 
         xlim = as.Date(xlim), main = main)
    if(cidade == "Sao_Paulo"){
        
        abline(v = as.Date("2020-06-01"), col = "darkgrey", lty = 3)
        abline(v = as.Date("2020-07-01"), col = "darkgrey", lty = 3)
        abline(v = as.Date("2020-10-09"), col = "darkgrey", lty = 3)
        abline(v = as.Date("2020-12-18"), col = "darkgrey", lty = 3)
    }
    
    if(cidade == "Porto_Alegre"){
        
        abline(v = as.Date("2020-05-11"), col = "darkgrey", lty = 3)
        abline(v = as.Date("2020-06-23"), col = "darkgrey", lty = 3)
        abline(v = as.Date("2020-09-29"), col = "darkgrey", lty = 3)
        abline(v = as.Date("2020-12-01"), col = "darkgrey", lty = 3)
    }
    
    if(cidade == "Goiania"){
        
        abline(v = as.Date("2020-05-01"), col = "darkgrey", lty = 3)
        abline(v = as.Date("2020-07-14"), col = "darkgrey", lty = 3)
        abline(v = as.Date("2020-09-01"), col = "darkgrey", lty = 3)
        abline(v = as.Date("2020-11-11"), col = "darkgrey", lty = 3)
        abline(v = as.Date("2020-11-15"), col = "darkgrey", lty = 3)
        abline(v = as.Date("2021-01-01"), col = "darkgrey", lty = 3)
    }
    
    lines(INT$prod, lty = line_types[1], col = line_colors[3], lwd = line_widths[1])
    lines(INT$cov, lty = line_types[2], col = line_colors[1], lwd = line_widths[2])
    lines(INT$eff, lty = line_types[3],  col = line_colors[2], lwd = line_widths[3])
}

plot.all.interventions <- function(interventions, parameters, city_name, scenario_name,
                                   line_sce_name = -2.5, x_lim = c("2020-02-20", "2021-02-01"),
                                   cidade = NA, line_colors = c("blue",2,"orange"),
                                   line_types = c(1,2,2),
                                   line_widths = c(2,1,1)) {
    
    
    SI <- intervention.time.series(interventions$`Self Isolation`, parameters)
    SD <- intervention.time.series(interventions$`Social Distancing`, parameters)
    HW <- intervention.time.series(interventions$Handwashing[c(1,2,4,3),], parameters)
  # 
    
    SC <- intervention.time.series(interventions$`School Closing`, parameters)
    WH <- intervention.time.series(interventions$`Work From Home`, parameters)
    CE <- intervention.time.series(interventions$`Cocoon Elderly`, parameters)
    
    d = 0.45/2
    t = 0.85/3
    off = 0.0
    par(mar = c(3,3,2,1))
    par(fig = c(0,t,2*d+off,4*d+off))
    plot.intervention((SD), main = "Social Distancing", 
                      xlim = x_lim, cidade = cidade, 
                      line_colors = line_colors, line_types = line_types, line_widths = line_widths)
    par(fig = c(t,2*t,2*d+off,4*d+off), new = T)
    plot.intervention((SI), main = "Self Isolation", 
                      xlim = x_lim, cidade = cidade, 
                      line_colors = line_colors, line_types = line_types, line_widths = line_widths)
    par(fig = c(2*t,3*t,2*d+off,4*d+off), new = T)
    plot.intervention((HW), main = "Hygiene measures", 
                      xlim = x_lim, cidade = cidade, 
                      line_colors = line_colors, line_types = line_types, line_widths = line_widths)
    
    par(fig = c(0,t,0+off,2*d+off), new = T)
    plot.intervention((SC), main = "School Closing", 
                      xlim = x_lim, cidade = cidade, 
                      line_colors = line_colors, line_types = line_types, line_widths = line_widths)
    par(fig = c(t,2*t,0+off,2*d+off), new = T)
    plot.intervention((WH), main = "Work from Home", 
                      xlim = x_lim, cidade = cidade, 
                      line_colors = line_colors, line_types = line_types, line_widths = line_widths)
    
    par(fig = c(2*t,3*t,0+off,2*d+off), new = T)
        plot.intervention((CE), main = "Cocoon Elderly", 
                          xlim = x_lim, cidade = cidade, 
                          line_colors = line_colors, line_types = line_types, line_widths = line_widths)
    
    par(fig = c(3*t,1,0.2,0.8), mar = c(2,0,2,1), new = T)
    plot(c(1,1,1),c(0,3,5), xlab = "", ylab = "", xaxt = "n", 
         yaxt = "n", type = "n", frame = F, xlim = c(0,5))
    # legend(0, 1.1, legend = c("Coverage","Efficiency", "Cov vs Eff"),
    #        lty = c(2,2,1), col = c("blue",2,"orange"), lwd = 2,  box.col = 0)
    # 
    points(c(1,1,1),c(3,2,1), pch = c("-","-","-"),  col = line_colors,
           cex = 2)
    text(c(1,1,1)+0.5,c(1,2,3), c("Adherence x\nEffectiveness","Effectiveness","Adherence"), adj = 0)
    
    mtext(city_name, line = -2, at = 0.5, outer = T)
    mtext(scenario_name, line = line_sce_name, at = 0.5, outer = T, cex = 0.8)
}


ggplot.all.interventions <- function(interventions, parameters,
                                     x_lim = c("2020-03-01", "2021-02-01"),
                                     lsize = 0.9,
                                     linecolors = c("purple","#009900","black"), #c("grey60","black","black"),
                                     linevalues = c(1,1,1),
                                     set_english = TRUE,
                                     silent_warning = TRUE,
                                     text_size = 14,
                                     axis.size = 14){
  
  if(set_english & !silent_warning) {
    warning("LC_TIME set to English!")
    Sys.setlocale("LC_TIME", "English")
  }

SI <- intervention.time.series(interventions$`Self Isolation`, parameters)
SD <- intervention.time.series(interventions$`Social Distancing`, parameters)
HW <- intervention.time.series(interventions$Handwashing, parameters)
# 

SC <- intervention.time.series(interventions$`School Closing`, parameters)
WH <- intervention.time.series(interventions$`Work From Home`, parameters)
CE <- intervention.time.series(interventions$`Cocoon Elderly`, parameters)

int.df <- rbind(data.frame(date = index(SI), SI, int = "Self-Isolation"),
                data.frame(date = index(SD), SD, int = "Social Distancing"),
                data.frame(date = index(HW), HW, int = "Use of mask"),
                data.frame(date = index(SC), SC, int = "School Closure"),
                data.frame(date = index(WH), WH, int = "Work from Home"),
                data.frame(date = index(CE), CE, int = "Cocoon Elderly"))

if(is.na(x_lim)[1]) {x_lim = range(int.df$date)}

int.df <- int.df[int.df$date >= x_lim[1] & int.df$date <= x_lim[2],]
int.table <- int.df %>% gather(line, value, -date, -int)

ggint <-  ggplot(int.table, aes(x = date, y = value)) +
  geom_line(aes(linetype = line, color = line), size = lsize,
            show.legend = c(color = T, linetype = F)) +
  scale_linetype_manual(values = linevalues) +
  scale_color_manual("", labels = c("Adherence","Effectiveness","Adherence x\nEffectiveness"),
                     values = linecolors) +
  xlab("") + ylab("") + 
  facet_wrap(~ int, ncol = 3, scales = "free") +
  theme_minimal() +
  theme(legend.position = "right", strip.text = element_text(size=text_size),
        legend.text=element_text(size=text_size),
        legend.key = element_rect(color = NA, fill = NA),
        legend.key.size = unit(1, "cm"),
        legend.title.align = 0.5,
        axis.text.x = element_text(size = axis.size),
        axis.text.y = element_text(size = axis.size-2),
        axis.line = element_line(),
        panel.spacing = unit(2, "lines")) +
  scale_x_date(labels = date_format("%b"), limits = as.Date(c("2020-03-14","2020-08-30"))) +
  scale_y_continuous(limits = c(0,1)) +
  guides(color = guide_legend(override.aes = list(linetype = linevalues ) ) )

return(ggint)
}
