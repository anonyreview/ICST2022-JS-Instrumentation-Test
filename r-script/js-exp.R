library("rjson")
library(purrr)
source("util.R")

SUTs <- c("ncs-js","scs-js", "cyclotron-js", "disease-sh-api-js", "nestjs-realworld-example-app-js") #
SEED <- 30

JS_RESULTS_PATH = "../results"
JS_SAVE_PATH="../results"

GENERATED_FILES = paste(JS_SAVE_PATH, "/generated_files", sep = "")
WB_DATA_DIR = paste(JS_RESULTS_PATH, "/wb-exp", sep = "")

BB_DATA_DIR = paste(JS_RESULTS_PATH, "/bb-exp", sep = "")
ALL_DATA_FILE = paste(JS_SAVE_PATH, "/generated_files/js_results.txt", sep = "")
C8_SCRIPT_FILE = paste(JS_SAVE_PATH, "/generated_files/c8_include_scripts.txt", sep = "")

WB_REPORT_DIR = paste(WB_DATA_DIR, "/all/reports", sep = "")
WB_ZIP_FILE = paste(JS_SAVE_PATH, "/","compressedData.zip", sep = "")
WB_SNAP_ZIP_FILE = paste(JS_SAVE_PATH,"/", "snapshotCompressedData.zip", sep = "")

WB_COVERAGE_DIR = "allcoverageC8"

COLLECT_SCRIPTS <- c()

TECH_EVO_BB_TB="evo-bb-tb"
TECH_EVO_BB_FT="evo-bb-ft"
TECH_EVO_WB_FT="evo-wb-ft"
TECH_RES_TB="restler-tb"
TECH_RESTTEST_TB="resttestgen-tb"

# the scripts which are not reached due to mis-config issues of the sut
ADDITIONAL_LINE = 268+342+93+112
ADDITIONAL_BR = 148+194+14+22

techniques <-c(TECH_EVO_BB_FT, TECH_EVO_BB_TB, TECH_RES_TB, TECH_EVO_WB_FT, TECH_RESTTEST_TB)
baselineTechniques <-c(TECH_EVO_BB_FT, TECH_EVO_BB_TB, TECH_RES_TB, TECH_RESTTEST_TB)
ALGOs <- c("RANDOM", "MIO")

COLLECT_INCLUDE_SCRIPT = TRUE

FAULTS="faults"
LINES= "lines"
BRANCHES = "branches"

icst<-function(){
  # RQ1
  wbTargets()
  wbTargetsPlot()
  
  #RQ2 and RQ3
  avgTargetTable(metrcis=c(LINES,BRANCHES), metricsNames=c("\\%Lines","\\%Branches"), fileName = "_coverage")
  avgTargetTable(metrcis=c(FAULTS), metricsNames=c("\\#Faults"), fileName = "_faults")
  pairTable()
}

wbTargetsPlot <- function(){
  dt <- read.table(gzfile(WB_SNAP_ZIP_FILE), header = T)
  
  projects = sort(unique(dt$id))
  
  for (proj in projects) {
    
    baseMask = dt$id == proj
    mioMask = baseMask & dt$blackBox == "false" &dt$algorithm == "MIO"
    rsMask = baseMask & dt$blackBox == "false" & dt$algorithm == "RANDOM"
    
    
    targets = sort(unique(dt$interval))
    z = length(targets)
    
    MIO = rep(0, times = z)
    RAND = rep(0, times = z)
    
    for (i in 1 : z) {
      targetMask = dt$interval == targets[[i]]
      MIO[[i]] = mean(dt$coveredTargets[targetMask & mioMask])
      RAND[[i]] = mean(dt$coveredTargets[targetMask & rsMask])
    }

    plot_colors = c("blue", "red")
    line_width = 2
    
    pdf(paste(GENERATED_FILES, "/plot_wbtargets_",proj,".pdf", sep = ""))
    
    yMin = min(MIO,RAND)
    yMax = max(MIO,RAND)
    
    plot(MIO, ylim = c(yMin, yMax), type = "o", col = plot_colors[[1]], pch = 21, lty = 1, lwd = line_width, ylab = "Covered Targets", xlab = "Budget Percentage", xaxt = "n")
    axis(side = 1, labels = targets, at = 1 : z)
    
    lines(RAND, type = "o", col = plot_colors[[2]], pch = 22, lty = 2, lwd = line_width)

    lx = 15
    ly = yMin + 0.5 * (yMax - yMin)
    
    legend(lx, ly, c("MIO", "RAND")
           , cex = 1.2, col = plot_colors
           , pch = 21 : 22
           , lty = 1 : 2)
    
    dev.off()
  }
}

wbTargets <- function(isFault = FALSE){
  
  dt <- read.table(gzfile(WB_ZIP_FILE), header = T)
  data = dt$coveredTargets
  
  name = "all"
  
  if(isFault){
    data = dt$potentialFaults
    name = "faults"
  }
  
  TABLE = paste(GENERATED_FILES, "/table_wbtargets_",name,".tex", sep = "")
  unlink(TABLE)
  sink(TABLE, append = TRUE, split = TRUE)
  
  cat("\\begin{tabular}{ l r r r r r }\\\\ \n")
  cat("\\toprule \n")
  cat("SUT &  JS-MIO & JS-Random & $\\hat{A}_{12}$ & \\emph{p}-value  & relative \\\\ \n", sep = "")
  cat("\\midrule \n")
  
  projects = sort(unique(dt$id))
  
  for (proj in projects) {
    
    projectMask = dt$id == proj
    
    cat("\\emph{", formatedSutName(proj), "}", sep = "")
    
    ttMask = dt$useMethodReplacement == "true"
    
    rs = data[projectMask & dt$blackBox == "false" & dt$algorithm=="RANDOM"]
    mio = data[projectMask & dt$blackBox == "false" & dt$algorithm=="MIO"]
    
    all <- c(mean(rs), mean(mio))
    
    cat(" & ")
    mv<-paste(formatC(mean(mio), digits = 1, format = "f"), sep = "")
    cat(highlighBest(mv, mean(mio), all, includeRank = FALSE))
    
    cat(" & ")
    rv<-paste(formatC(mean(rs), digits = 1, format = "f"), sep = "")
    cat(highlighBest(rv, mean(rs), all, includeRank = FALSE))
    
    a12 = measureA(mio, rs)
    
    a12v=paste(formatC(a12, digits = 2, format = "f"), sep = "")
    
    w = wilcox.test(mio, rs)
    p = w$p.value
    
    
    # format p
    pv=""
    if(is.nan(p)){
      pv="NaN"
    }else{
      pv = formatedPValue(p)
    }
    
    cat(" & ")
    cat(formatedValue(a12v, a12, p))
    
    cat(" & ")
    cat(formatedValue(pv, a12, p))
    
    cat(" & ")
    cat(formatedValue(relative(mean(mio), mean(rs)), a12, p))
    cat(" \\\\ \n")
  }
  
  cat("\\bottomrule \n")
  cat("\\end{tabular} \n")
  
  sink()
}

executionInfo <- function(){
  dt <- read.table(ALL_DATA_FILE, header = T)
  projects = sort(unique(dt$id))
  
  actions = dt$evaluatedActions
  times = dt$elapsedSeconds
  
  TABLE = paste(GENERATED_FILES, "/execution_info",".tex", sep = "")
  unlink(TABLE)
  sink(TABLE, append = TRUE, split = TRUE)
  
  cat("\\begin{tabular}{ l r r r r r}\\\\ \n")
  cat("\\toprule \n")
  cat("SUT &  JS-MIO & BB$_f$ & BB$_t$ &RESTler &",formatedTechName(TECH_RESTTEST_TB)," \\\\ \n", sep = "")
  
  cat("\\midrule \n")
  
  for (proj in projects) {
    cat("\\emph{", formatedSutName(proj), "}", sep = "")
    projectMask = dt$id == proj
    
    for (tech in c(TECH_EVO_WB_FT, baselineTechniques)) {
      techMask = projectMask & dt$technique_name==tech
      if(identical(TECH_EVO_WB_FT, tech))
        techMask = techMask & dt$algorithm=="MIO"
      cat(" & ")

      ac <- formatC(mean(actions[techMask])/1000, digits = 2 ,format = "f")
      ti <- formatC(max(as.integer(times[techMask]))/60, digits = 2,format = "f")
      cat(ac,"\\textit{k}, ",ti,"m", sep = "")
    }
    
    cat(" \\\\ \n")
  }
  
  cat("\\bottomrule \n")
  cat("\\end{tabular} \n")
  
  sink()
  
}

pairTable <- function(pbranch=FALSE){
  dt <- read.table(ALL_DATA_FILE, header = T)
  
  projects = sort(unique(dt$id))
  algorithms = sort(unique(dt$algorithm))
  
  branchData <- dt$covered_branches
  if(pbranch)
    branchData <-dt$coverage_branch
  
  TABLE = paste(GENERATED_FILES, "/table_pair",".tex", sep = "")
  unlink(TABLE)
  sink(TABLE, append = TRUE, split = TRUE)
  
  cat("\\begin{tabular}{ l l r r r |r r r |r r r}\\\\ \n")
  cat("\\toprule \n")
  
  cat("SUT & JS-MIO& \\multicolumn{3}{r|}{\\%Lines} &  \\multicolumn{3}{r|}{\\%Branches} &  \\multicolumn{3}{r}{\\#Faults} \\\\ \n", sep = "")
  cat("& vs. &$\\hat{A}_{12}$ & \\emph{p}-value & relative &$\\hat{A}_{12}$ & \\emph{p}-value & relative &$\\hat{A}_{12}$ & \\emph{p}-value & relative \\\\ \n", sep = "")
  cat("\\midrule \n")
  
  
  pro_index = 0
  for (proj in projects) {
    pro_index = pro_index + 1
    cat("\\emph{", formatedSutName(proj), "}", sep = "")
    projectMask = dt$id == proj
    
    
    wb <- dt$covered_lines[projectMask & dt$technique_name==TECH_EVO_WB_FT]
    wb_br <- branchData[projectMask & dt$technique_name==TECH_EVO_WB_FT]
    wb_fa <- dt$faults[projectMask & dt$technique_name==TECH_EVO_WB_FT]
    
    for (tech in baselineTechniques) {
      cat(" & ", formatedTechName(tech), sep = "")
      
      ##### cyclotron cannot be applied by Restler
      if(identical(tech, TECH_RES_TB) & (identical(proj, "cyclotron-js"))){
        cat(" & - & - & -& - & - & -& - & - & - \\\\ \n ")
        next
      }
      
      ##### cyclotron and disease-sh-api cannot be applied by RESTTESTGEN
      if(identical(tech, TECH_RESTTEST_TB) & (identical(proj, "disease-sh-api-js") || identical(proj, "cyclotron-js"))){
        cat(" & - & - & -& - & - & -& - & - & - \\\\ \n ")
        next
      }
      
      base_line <-  dt$covered_lines[projectMask & dt$technique_name==tech]
      a12 = measureA(wb, base_line)
      a12v = formatC(a12, digits = 2, format = "f")
      
      w = wilcox.test(wb, base_line)
      p = w$p.value
      
      relative = (mean(wb) - mean(base_line))/mean(base_line) 
      relativev = paste(formatC( relative *100, digits = 2, format = "f"), "\\%",sep = "")
      
      # format p
      pv = paste(formatC(p, digits = 3, format = "f"), sep = "")
      if (p < 0.001) {
        pv = "$\\le $0.001"
      } 
      
      cat(" & ")
      cat(formatedValue(a12v, a12, p))
      
      cat(" & ")
      cat(formatedValue(pv, a12, p))
      
      cat(" & ")
      cat(formatedValue(relativev, a12, p))
      
      #branches
      base_br <- branchData[projectMask & dt$technique_name==tech]
      
      a12 = measureA(wb_br, base_br)
      a12v = formatC(a12, digits = 2, format = "f")
      
      w = wilcox.test(wb_br, base_br)
      p = w$p.value
      
      relative = (mean(wb_br) - mean(base_br))/mean(base_br) 
      relativev = paste(formatC( relative *100, digits = 2, format = "f"), "\\%",sep = "")
      
      # format p
      pv = paste(formatC(p, digits = 3, format = "f"), sep = "")
      if (p < 0.001) {
        pv = "$\\le $0.001"
      } 
      
      cat(" & ")
      cat(formatedValue(a12v, a12, p))
      
      cat(" & ")
      cat(formatedValue(pv, a12, p))
      
      cat(" & ")
      cat(formatedValue(relativev, a12, p))
      
      #Faults
      base_faults <- dt$faults[projectMask & dt$technique_name==tech]
      
      a12 = measureA(wb_fa, base_faults)
      a12v = formatC(a12, digits = 2, format = "f")
      
      w = wilcox.test(wb_fa, base_faults)
      p = w$p.value
      
      relativev="Inf"
      if(base_faults >0){
        relative = (mean(wb_fa) - mean(base_faults))/mean(base_faults) 
        relativev = paste(formatC( relative *100, digits = 2, format = "f"), "\\%",sep = "")
      }
      
      # format p
      pv=""
      if(is.nan(p)){
        pv="NaN"
      }else{
        pv = paste(formatC(p, digits = 3, format = "f"), sep = "")
        
        if (p < 0.001) {
          pv = "$\\le $0.001"
        } 
      }
      
      
      cat(" & ")
      cat(formatedValue(a12v, a12, p))
      
      cat(" & ")
      cat(formatedValue(pv, a12, p))
      
      cat(" & ")
      cat(formatedValue(relativev, a12, p))
      
      cat(" \\\\ \n")
    }
    
  }
  
  cat("\\bottomrule \n")
  cat("\\end{tabular} \n")
  
  sink()
  
}

avgTargetTable <- function(pbranch=FALSE, metrcis=c(LINES,BRANCHES,FAULTS), metricsNames=c("\\%Lines","\\%Branches","\\#Faults"), includeBBF=TRUE, fileName="all"){
  dt <- read.table(ALL_DATA_FILE, header = T)
  
  projects = sort(unique(dt$id))
  algorithms = sort(unique(dt$algorithm))
  
  branchData <- dt$covered_branches
  if(pbranch)
    branchData <-dt$coverage_branch
  
  TABLE = paste(GENERATED_FILES, "/table_avg",fileName,".tex", sep = "")
  unlink(TABLE)
  sink(TABLE, append = TRUE, split = TRUE)
  
  fbaselineTechniques <- baselineTechniques
  if(!includeBBF)
    fbaselineTechniques <- fbaselineTechniques[fbaselineTechniques != TECH_EVO_BB_FT]
  
  techs <- c(TECH_EVO_WB_FT, fbaselineTechniques)
  
  numTech <- length(techs)
  headerR <- paste(rep("r", numTech), collapse = " ")
  
  numMetrics <- length(metrcis) #line, branch, faults
  
  headerBegin <- "\\begin{tabular}{ l "
  headerText <- "SUT "
  thead <- ""
  for(i in 1:numMetrics){
    columSplit <- "|"
    if (i == numMetrics){
      columSplit <- ""
    }
    headerBegin <- paste(headerBegin, headerR, columSplit, sep = "")
    headerText <- paste(headerText, "& \\multicolumn{",numTech,"}{r",columSplit,"}{",metricsNames[i],"}", sep = "")
    
    for (tech in techs) {
      thead <- paste(thead, " & ", formatedTechName(tech), sep = "")
    }
  }
  
  headerBegin <- paste(headerBegin, "}\\\\ \n", sep = "")
  headerText <- paste(headerText, "\\\\ \n", sep = "")
  
  cat(headerBegin)
  cat("\\toprule \n")
  cat(headerText)
  
  cat(thead, " \\\\ \n", sep = "")
  cat("\\midrule \n")
  
  ranks_lines <- matrix(nrow = length(projects), ncol = length(techs))
  ranks_brs <- matrix(nrow = length(projects), ncol = length(techs))
  ranks_faults <- matrix(nrow = length(projects), ncol = length(techs))
  
  pro_index = 0
  for (proj in projects) {
    pro_index = pro_index + 1
    cat("\\emph{", formatedSutName(proj), "}", sep = "")
    projectMask = dt$id == proj
    
    tot_lines <- max(dt$total_lines[projectMask])
    tot_branches <- max(dt$total_branches[projectMask])
    
    if(identical(proj, "cyclotron-js")){
      tot_lines <- tot_lines + ADDITIONAL_LINE
      tot_branches <- tot_branches + ADDITIONAL_BR
    }
    
    
    all_lines <- unlist(techs %>% map(function(x) mean(dt$covered_lines[projectMask & dt$technique_name==x])),recursive = TRUE)
    ranks_lines[pro_index,]=rank(-all_lines,ties.method= "min")
    
    
    if(LINES %in% metrcis){
      for (l in all_lines) {
        cat(" &")
        if(is.null(l) || is.nan(l)){
          cat("-")
        }else{
          mv <- paste(formatC( (l/ tot_lines)*100, digits = 1, format = "f"), "\\%",sep = "")
          cat(highlighBest(mv, l, all_lines))
        }
      }
    }
    
    
    all_brs <- unlist(techs %>% map(function(x) mean(branchData[projectMask & dt$technique_name==x])),recursive = TRUE)
    ranks_brs[pro_index,]=rank(-all_brs, ties.method= "min")
    
    if(BRANCHES %in% metrcis){
      for (b in all_brs) {
        cat(" &" )
        if(is.null(b) || is.nan(b)){
          cat("-")
        }else{
          mv <- paste(formatC( (b/ tot_branches)*100, digits = 1, format = "f"), "\\%",sep = "")
          cat(highlighBest(mv, b, all_brs))
        }
      }
    }
    
    all_faults <- unlist(techs %>% map(function(x) mean(dt$faults[projectMask & dt$technique_name==x])),recursive = TRUE)
    ranks_faults[pro_index,]=rank(-all_faults,ties.method= "min")
    
    if(FAULTS %in% metrcis){
      for (f in all_faults) {
        cat(" &")
        if(is.null(f) || is.nan(f)){
          cat("-")
        }else{
          mv <- paste(formatC( f, digits = 2, format = "f"),sep = "")
          cat(highlighBest(mv, f, all_faults))
        }
      }
    }
    
    cat(" \\\\ \n")
  }
  cat("\\midrule \n")
  cat("\\emph{Average Rank}", sep = "")
  
  
  if(LINES %in% metrcis){
    avg_rank_lines <- unlist((1:length(techs)) %>% map(function(x) mean(ranks_lines[,x])),recursive = TRUE)
    for(avg in avg_rank_lines){
      value <- formatC(avg, digits = 2, format = "f")
      cat(" & ", highlighBest(value, avg, avg_rank_lines, includeRank = FALSE, moreBetter = FALSE) , sep = "")
    }
  }
  
  if(BRANCHES %in% metrcis){
    avg_rank_br <- unlist((1:length(techs)) %>% map(function(x) mean(ranks_brs[,x])),recursive = TRUE)
    for(avg in avg_rank_br){
      value <- formatC(avg, digits = 2, format = "f")
      cat(" & ", highlighBest(value, avg, avg_rank_br, includeRank = FALSE, moreBetter = FALSE) , sep = "")
    }
  }
  
  if(FAULTS %in% metrcis){
    avg_rank_faults <- unlist((1:length(techs)) %>% map(function(x) mean(ranks_faults[,x])),recursive = TRUE)
    for(avg in avg_rank_faults){
      if(is.null(avg) || is.nan(avg) || is.na(avg)){
        cat(" & -")
      }else{
        value <- formatC(avg, digits = 2, format = "f")
        cat(" & ", highlighBest(value, avg, avg_rank_faults, includeRank = FALSE, moreBetter = FALSE) , sep = "")
      }
    }
  }
  
  cat(" \\\\ \n")
  
  f = friedman.test(ranks_lines)
  cs = as.numeric(f$statistic)
  pv = f$p.value
  
  fb = friedman.test(ranks_brs)
  csb = as.numeric(fb$statistic)
  pvb = fb$p.value
  
  ff = friedman.test(ranks_faults)
  csf = as.numeric(ff$statistic)
  pvf = fb$p.value
  
  ftResults <- ""
  if(LINES %in% metrcis){
    ftResults <- paste(ftResults, " & \\multicolumn{",numTech,"}{r|}{${\\chi}^2$=", formatC(cs, digits = 3, format = "f"), ", $p$-value=", boldValue(formatedPValue(pv), pv), "}", sep = "")
  }
  
  if(BRANCHES %in% metrcis)
    ftResults <- paste(ftResults, " & \\multicolumn{",numTech,"}{r|}{${\\chi}^2$=", formatC(csb, digits = 3, format = "f"), ", $p$-value=", boldValue(formatedPValue(pvb), pvb), "}", sep = "")
  
  if(FAULTS %in% metrcis){
    ftResults <- paste(ftResults, " & \\multicolumn{",numTech,"}{r}{${\\chi}^2$=", formatC(csf, digits = 3, format = "f"), ", $p$-value=", boldValue(formatedPValue(pvf), pvf), "}", sep = "")
  }
  
  cat("\\midrule \n")
  cat("\\emph{Friedman test}",ftResults, " \\\\ \n",sep = "")
  
  cat("\\bottomrule \n")
  cat("\\end{tabular} \n")
  
  
  sink()
  
}

formatedPValue <- function(p){
  pv = paste(formatC(p, digits = 3, format = "f"), sep = "")
  if (p < 0.001) {
    pv = "$\\le $0.001"
  } 
  return(pv)
}

extractCoverage <-function(includeFault=TRUE){
  
  cn <- c("id", "technique", "technique_name","algorithm","blackbox","evaluatedActions","elapsedSeconds", 
          "covered_lines", "total_lines","covered_branches", "total_branches", "coverage_branch")#,"covered_functions", "total_functions")
  if(includeFault)
    cn <- c(cn, FAULTS)
  df <- data.frame(matrix(ncol = length(cn), nrow = 0))
  colnames(df) <-cn
  
  sc_cov_cn <-c("id","script")
  c8_df<-data.frame(matrix(ncol = length(sc_cov_cn), nrow = 0))

  for (sut in SUTs) {
    c8_scripts <- c()
    jest_scripts <- c()
    for (technique in techniques) {
      if(identical(sut, "cyclotron-js") & (identical(technique, TECH_RES_TB) || identical(technique, TECH_RESTTEST_TB))){
        next
      }
      
      if(identical(sut, "disease-sh-api-js") & (identical(technique, TECH_RESTTEST_TB))){
        next
      }
      
      for (i in 1:SEED) {
        
        for (algo in ALGOs) {
          general = getGeneralInfo(sut, technique, algo)
          
          if(identical(algo, "MIO") & !identical(technique, TECH_EVO_WB_FT)){
            next
          }
          if(identical(algo, "RANDOM") & identical(technique, TECH_EVO_WB_FT)){
            next
          }
          
          et_statpath = getPath(path = BB_DATA_DIR, sut= sut,technique = technique, seed =i, isStat = TRUE, algorithm = algo)
          stat_data <-extractData(et_statpath, TRUE, technique, sut, seed)
          newrow <- c(general, stat_data[["value"]])
          et_covpath = getPath(path =  BB_DATA_DIR, sut=sut, technique = technique, seed =i, isStat = FALSE, algorithm = algo)  
          cov_data <-extractData(et_covpath, FALSE, technique, sut, seed)
          if(!is.null(cov_data[["scripts"]]))
            c8_scripts <- c(c8_scripts, cov_data[["scripts"]])
          newrow <- c(newrow, cov_data[["value"]])
          if(includeFault){
            newrow <- c(newrow,stat_data[["faults"]])
          }
          if(length(newrow) == length(cn)){
            df <- rbind(df, newrow)
          }
        }
      }
    }
    
    
    for(s in unique(c8_scripts)){
      c8_df <- rbind(c8_df, c(sut, s))
    }
    
    for(s in unique(jest_scripts)){
      jest_df <- rbind(jest_df, c(sut, s))
    }
  }
  
  write.table(df, file = ALL_DATA_FILE, col.names = cn, row.names = FALSE)
  
  if(COLLECT_INCLUDE_SCRIPT){
    write.table(c8_df, file = C8_SCRIPT_FILE, col.names = sc_cov_cn, row.names = FALSE)
  }
}

extractData <- function(path, isStat, technique, sut, seed, coverageProvider = "c8", algorithm=NULL){
  if(! file.exists(path)){
    print(paste(path, " does not exist", sep = ))
    return(NULL)
  }
  
  if(isStat){
    if(identical(technique, TECH_EVO_WB_FT) || identical(technique, TECH_EVO_BB_FT) || identical(technique,TECH_EVO_BB_TB)){
      sdf <- read.csv(file = path)
      if(identical(technique, TECH_EVO_WB_FT)){
        return(multiple(c(sdf$evaluatedActions[sdf$algorithm=="MIO"], sdf$elapsedSeconds[sdf$algorithm=="MIO"]), faults = sdf$potentialFaults[sdf$algorithm=="MIO"]))
      }else{
        return(multiple(c(sdf$evaluatedActions[[1]], sdf$elapsedSeconds[[1]]), faults = sdf$potentialFaults[[1]]))
      }
    }else{
      
      if(identical(technique, TECH_RES_TB)){
        sjson <-fromJSON(paste(readLines(path), collapse=""))
        faults = sjson$bug_buckets$main_driver
        if(is.null(faults))
          faults=0
        return(multiple(c(sjson$total_requests_sent$main_driver, "SPECIFIED"), faults = faults))
      }
      
      if(identical(technique, TECH_RESTTEST_TB)){
        sjson <-fromJSON(paste(readLines(path), collapse=""))
        faults=0
        ops <- sjson$operationsResults
        if(!is.null(ops)){
          for (op in ops) {
            f500 <- op$responseCoverage$numberOfTestedErrorStatusCodes
            if(!is.null(f500) & !is.na(as.numeric(f500))){
              faults = faults + f500
            }
          }
        }
        return(multiple(c(0, "SPECIFIED"), faults = faults))
      }
    }
  }else{
    if(identical(coverageProvider, "c8") || identical(coverageProvider, "jestv8")){
      sjson <-fromJSON(paste(readLines(path), collapse=""))
      cov_data <- extractJsonCoverage(sjson, sut, identical(coverageProvider, "c8"))
      return(cov_data)
    }
    if(identical(coverageProvider, "evomaster")){
      sdf <- read.csv(file = path)
      if(is.null(algorithm))
        return(multiple(c(sdf$coveredLines[[1]], sdf$numberOfLines[[1]], sdf$coveredBranches[[1]], sdf$numberOfBranches[[1]], branchCoverageValue(sdf$coveredBranches[[1]], sdf$numberOfBranches[[1]]))))
      return(multiple(c(sdf$coveredLines[sdf$algorithm==algorithm], sdf$numberOfLines[sdf$algorithm==algorithm], 
                        sdf$coveredBranches[sdf$algorithm==algorithm], sdf$numberOfBranches[sdf$algorithm==algorithm],
                        branchCoverageValue(sdf$coveredBranches[sdf$algorithm==algorithm], sdf$numberOfBranches[sdf$algorithm==algorithm]))))
    }
  }
}

multiple <- function(value, scripts=NULL, faults=NULL){
  return(list(value=value, scripts=scripts, faults=faults))
}

extractJsonCoverage <- function(json, sut, isC8){
  total_lines <- 0
  covered_lines <- 0
  total_branches <- 0
  covered_branches <- 0
  
  scripts <- c()
  for(name in names(json)){
    include <- checkScript(sut, name)
    if(!is.null(include)){
      scripts <- c(scripts, include)
      total_lines <- total_lines + as.integer(json[[name]]$lines$total)
      covered_lines <- covered_lines + as.integer(json[[name]]$lines$covered)
      total_branches <- total_branches + as.integer(json[[name]]$branches$total)
      covered_branches <- covered_branches + as.integer(json[[name]]$branches$covered)
    }
  }
  result <- c(covered_lines, total_lines, covered_branches, total_branches, branchCoverageValue(covered_branches, total_branches, isC8, identical(sut,"cyclotron-js")))
  return(multiple(result, scripts = scripts))
}

branchCoverageValue <- function(covered_branches,total_branches, isC8=TRUE, additional_sut=FALSE){
  t <- total_branches
  if(isC8 & additional_sut)
    t <- total_branches+ ADDITIONAL_BR
  return(as.integer(formatC(covered_branches/(t) * 1000, digits = 0, format = "d")))
}

getGeneralInfo <- function(sut, technique, algorithm=NULL){
  if(identical(technique, TECH_EVO_BB_TB)){
    return(c(sut, "evomaster",technique,"Random","true"))
  }
  if(identical(technique, TECH_EVO_BB_FT)){
    return(c(sut, "evomaster",technique,"Random","true"))
  }
  
  if(identical(technique, TECH_RES_TB)){
    return(c(sut, "RESTler",technique ,"Default","true"))
  }
  if(identical(technique, TECH_EVO_WB_FT)){
    return(c(sut, "evomaster",technique,algorithm,"false"))
  }
  
  if(identical(technique, TECH_RESTTEST_TB)){
    return(c(sut, "RESTTESTGEN",technique ,"Default","true"))
  }
  
  stop("error to get general info "+technique)
}

getPath <-function(path, sut, technique, seed, isStat, algorithm=NULL, coverageProvider="c8"){
  if(isStat){
    if(identical(technique, TECH_EVO_BB_TB)){
      return( paste(BB_DATA_DIR,"/all/evo-bb-tb/reports/","statistics_", sut,"_" ,seed, ".csv",sep = ""))
    }
    if(identical(technique, TECH_EVO_WB_FT)){
      return( paste(WB_DATA_DIR,"/all/reports/","statistics_", sut,"_" ,seed, ".csv",sep = ""))
    }
    if(identical(technique, TECH_EVO_BB_FT)){
      return( paste(BB_DATA_DIR,"/all/evo-bb-ft/reports/","statistics_", sut,"_" ,seed, ".csv",sep = ""))
    }
    if(identical(technique, TECH_RES_TB) || identical(technique, TECH_RESTTEST_TB)){
      rootPath <- paste(BB_DATA_DIR, "/all/",technique,"/",sut, sep="")
      sutDirPattern = paste( sut,"_S", seed, "_","\\d+",sep = "")
      sutDir <- dir(path = rootPath, pattern = sutDirPattern, recursive = FALSE, full.names = TRUE)[1]
      if(identical(technique, TECH_RES_TB) ){
        expDir <- paste(sutDir, "/Fuzz/RestlerResults", sep="")
        summaryDir <- dir(path = expDir, pattern = "experiment\\d+", recursive = FALSE, full.names = TRUE)[1]
        covfile <- paste(summaryDir,"logs", "testing_summary.json", sep = "/")
        return(covfile)
      }
      
      if(identical(technique, TECH_RESTTEST_TB)){
        expDir <- paste(sutDir, "/reports", sep="")
        covfile <- paste(expDir, "summary.json", sep = "/")
        return(covfile)
      }
      
      stop(paste("error to get stat file for the ", technique, sep = " "))
    }
    
    
  }else{
    
    if(identical(technique, TECH_EVO_WB_FT)){
      if(identical(coverageProvider, "c8")){
        return(paste(WB_DATA_DIR, "/all/",sut, "/",WB_COVERAGE_DIR,"/EM_JS_WB_NONE_JS_JEST_False_",algorithm, "_",seed,"_TEST/coverage/coverage-summary.json",sep="")) 
      }
      if(identical(coverageProvider, "jestv8")){
        return(paste(WB_DATA_DIR, "/all/",sut, "/allcoverageV8json","/EM_",algorithm, "_FALSE_",seed,"_TEST/coverage/coverage-summary.json",sep="")) 
      }
    }
    
    rootPath <-paste(BB_DATA_DIR,"/all",sep="")
    
    if(identical(technique, TECH_EVO_BB_TB)){
      rootPath <- paste(rootPath, "/evo-bb-tb/coverage_runtime/",sut, sep="")
    }
    if(identical(technique, TECH_EVO_BB_FT)){
      rootPath <- paste(rootPath, "/evo-bb-ft/coverage_runtime/",sut, sep="")
    }
    if(identical(technique, TECH_RES_TB)||identical(technique, TECH_RESTTEST_TB)){
      rootPath <- paste(rootPath, "/",technique,"/",sut, sep="")
    }
    covfilePattern = paste( sut,"_S", seed, "_","\\d+",sep = "")
    covfile <- dir(path = rootPath, pattern = covfilePattern, recursive = FALSE, full.names = TRUE)[1]
    covfile <- paste(covfile,"coverage", "coverage-summary.json", sep = "/")
    return(covfile)
  }
  
  stop(paste("error to get file", technique, sep = " "))
}

checkScript <- function(sut, name){
  patterns <- collectScripts(sut)
  for (p in patterns) {
    include <- grepl(p, name) & !exclude(sut, name)
    if(include) {
      return(regmatches(name,regexpr(p,name)))
    }
  }
  return(NULL)
}

exclude<-function(sut, name){
  r <- FALSE
  epatterns <- excludeScripts(sut)
  for(e in epatterns){
    r <- r | grepl(e, name)
  }
  return(r)
}

excludeScripts <- function(sut){
  if(identical(sut,"ncs-js") || identical(sut, "scs-js")){
    return(c('\\\\src\\\\.*server*.js','/src/.*server*.js',
             '\\\\src\\\\.*serverWithCov.js','/src/.*serverWithCov.js',
             '\\\\src\\\\.*app.js','/src/.*app.js')) 
  }
  return(c('\\\\src\\\\.*server*.js','/src/.*server*.js',
           '\\\\src\\\\.*serverWithCov.js','/src/.*serverWithCov.js'))
}

collectScripts <- function(sut){
  return(c('\\\\src\\\\.*.js','/src/.*.js','\\\\src\\\\.*.ts','/src/.*.ts'))
}

formatedTechName <- function(name){
  if(identical(TECH_EVO_BB_FT, name))
    return("BB$_f$")
  if(identical(TECH_EVO_BB_TB, name))
    return("BB$_t$")
  if(identical(TECH_EVO_WB_FT, name))
    return("JS-MIO")
  if(identical(TECH_RES_TB, name))
    return("RESTler")
  if(identical(TECH_RESTTEST_TB, name))
    return("RESTTESTGEN")
}

formatedSutName <- function(name){
  if(identical(name, "nestjs-realworld-example-app-js"))
    return("realworld-app-js")
  return(name)
}


measureA <- function(a,b){
  
  if(length(a)==0 & length(b)==0){
    return(0.5)
  } else if(length(a)==0){
    ## motivation is that we have no data for "a" but we do for "b".
    ## maybe the process generating "a" always fail (eg out of memory)
    return(0)
  } else if(length(b)==0){
    return(1)
  }
  
  r = rank(c(a,b))
  r1 = sum(r[seq_along(a)])
  
  m = length(a)
  n = length(b)
  A = (r1/m - (m+1)/2)/n
  
  return(A)
}


## (a-b)/b
relative <- function(a, b){
  relativev="Inf"
  if(b >0){
    relative = (mean(a) - mean(b))/mean(b) 
    relativev = paste(formatC( relative *100, digits = 2, format = "f"), "\\%",sep = "")
  }
  return(relativev)
}

isBest <- function(c, vs, moreBetter){
  if(moreBetter)
    return(c >= max(vs[!is.nan(vs)]))
  return(c <= min(vs[!is.nan(vs)]))
}

highlighBest <- function(value,c, vs, includeRank=TRUE, moreBetter=TRUE){
  rankValue = NULL
  if(includeRank)
    rankValue = getRank(c, vs)
  if(isBest(c, vs, moreBetter))
    return(paste("\\textbf{",appendRankValue(value, rankValue),"}", sep = ""))
  return(appendRankValue(value, rankValue))
}

boldValue <- function(value, pvalue=NULL){
  if(is.null(pvalue) || pvalue < 0.05)
    return(paste("\\textbf{", value,"}", sep=""))
  return(value)
}

getRank <- function(c, vs){
  return(rank(-vs,ties.method= "min")[which(c==vs)[1]])
}

formatedValue <- function(value, a12, p){
  if(is.nan(p) | p >= 0.05)
    return(value)
  if(a12 > 0.5)
    return(boldValue(value))
  return(paste("\\textcolor{red}{", value,"}", sep=""))
}

appendRankValue <- function(value, rankValue=NULL){
  if(is.null(rankValue)) return(value)
  return(paste(value," (",rankValue,")", sep=""))
}

icst()