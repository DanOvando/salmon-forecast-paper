

#' Fit Dynamic Linear Sibling Model
#'
#' @param pred.year Integer indicating the prediction return year
#' @param start.year Integer for starting year of time series for input data
#' @param model.type Integer factor for log tranformation of predictors and responses: 0-NO, 1-YES
#' @param data Input data frame of return data from run reconstructions
#' @param rivers.include String indicating river systems to predict
#' @param maxit Integer specifying maximum number of MARSS EM iterations
#'
#' @return
#' @export
#'
#' @examples
fit_dlm_model <- function(pred.year, start.year, model.type, data, rivers.include, maxit=1e3) {
  
  ##### TESTING #####
  # pred.year <- 2017
  # start.year <- 1963
  # model.type <- 1
  # data <- read.csv('Data/2017.csv')
  # rivers.include <- 'Togaik'#c('Kvichak','Alagnak','Naknek','Egegik','Ugashik','Igushik','Wood','Nushagak','Togiak')
  # maxit <- 1e5
  
  
  
  ##################
  print(paste('#####','The forecast year is:', pred.year, '#####', sep=' '))
  # require(MARSS)
  
  #============
  
  #Turn return estimates into units of fish (not 1000's of fish)
  data$ret <- data$ret*1000
  
  #Organize data by brood year
  data <- data[order(data$broodYr),]
  
  if(model.type == 1) { data$ret <- log(data$ret + 0.00001) }
  
  #Freshwater and ocean ages used for analysis
  FW.ages <- c(1:2)
  O.ages <- c(2:3)
  
  #FW.ages <- 1
  #O.ages <- 3
  
  #FOR NUSHAGAK ONLY
  #FW.ages <- c(0,1)
  #O.ages <- c(3,4)
  
  
  #Create fitted model list
  fitted.mod.list <- vector('list', length=(length(rivers.include)*length(FW.ages)*length(O.ages)))
  
  #Data structure for model output
  # model.output <- matrix(nrow=0, ncol=7)
  # model.output <- rep(0,7)
  # names(model.output) <- c('system','age','model','dAIC','forecast','r2','lnSSQ')
  
  temp.river <- vector(length=0)
  temp.age <- vector(length=0)
  
  #Vectors for direct forecast information
  output.river <- vector(length=0)
  output.fwAge <- vector(length=0)
  output.oAge <- vector(length=0)
  output.model <- vector(length=0)
  output.forecast <- vector(length=0)
  
  #Counter
  counter <- 0
  
  for(river in rivers.include) {
    for(fw in FW.ages) {
      for(o in O.ages) {
        
        ###### FOR TESTING PURPOSES ONLY ######
        # river <- 'Togiak'
        # fw <- 1
        # o <- 2
        #######################################
        
        counter <- counter + 1 #Updates counter
        max.age <- fw + o + 1#The maximum age of a predictor age
        
        #Available return data for this river and age class across years
        # If currently on Kvichak 2.2, what were all Kvichak 2.2's across time
        retData <- data$ret[data$System == river
                            & data$fwAge == fw
                            & data$oAge == o
                            & data$broodYr < (pred.year-max.age) #Insures that data included is from earlier than the current age class and desired prediction year
                            & data$broodYr >= start.year] #Insures that 
        n.retData <- length(retData) #Number of years of data from which to predict
        
        
        #Temporary brood years
        temp.brood.years <- start.year:(pred.year - max.age-1)                                  
        
        ##### PREDICTOR AGES #####
        
        counter.pred <- 0 #Counter for NUMBER of applicable predictor ages
        
        # for(i in 0:2) { #Freshwater
        # for(j in 1:4) { #Ocean
        for(i in fw) { #Freshwater
          for(j in (o-1)) { #Ocean
            if((i + j) < fw + o) { #Individuals used for prediction are in fact YOUNGER than
              #Predictor Data (for glm fitting):
              # For Kvichak 2.2's - this is all relevant predictor age classes (younger) (col)
              #                       for each year (row)
              predData.temp <- data$ret[data$System == river
                                        & data$fwAge == i
                                        & data$oAge == j
                                        & data$broodYr < (pred.year - max.age)
                                        & data$broodYr >= start.year]
              #Returns of in previous year of age classes one younger than that in question:
              acData.temp <- data$ret[data$System == river
                                      & data$fwAge == i
                                      & data$oAge == j
                                      & data$broodYr == (pred.year - max.age)]
              
              #               if(length(predData.temp) > 0 & sum(predData.temp) != 0) { 
              if(length(predData.temp) == n.retData & sum(predData.temp) != 0) {
                counter.pred <- counter.pred + 1 #Updates counter
                if(counter.pred == 1) { 
                  predData <- data.frame(predData.temp) #Creates new data frame
                  acData <- acData.temp #Creates vector of return numbers by brood year
                  predData.names <- paste(i, '.', j, sep='')
                }else { 
                  predData <- data.frame(predData, predData.temp) #Updates established data frame
                  acData <- c(acData, acData.temp) #Returns 
                  predData.names <- c(predData.names, paste(i, '.', j, sep='')) 
                }
                #names(predData)[counter.pred] <- paste(i, '.', j, sep='') #Names the column in data frame
                names(acData)[counter.pred] <- paste(i, '.', j, sep='')
                #########
                #print(predData)
              }  	  	       
            }#If age less than  	
          }#next j
        }#next i
        
        #At this point we have:
        #predData
        #acData
        #predData.names
        
        #Names the colums of the predictor data (predData) data frame with their age class
        names(predData) <- predData.names
        n.pred <- length(predData.names)
        ##########################
        #RUN DLM MODEL
        df.data <- data.frame(retData,predData)
        names(df.data)[1]<- c('retData')
        df.pred <- data.frame(t(acData))
        names(df.pred) <- names(df.data)[-1]
        
        #1. standardize the data
        
        std.df.data <- scale(df.data)
        
        ##########################
        #2. Fit the model
        TT = nrow(std.df.data)
        # get response data
        resp.data <- std.df.data[,1]
        pred.data <- matrix(std.df.data[,2], nrow=1)
        # number of regr params (slope + intercept)
        m = dim(pred.data)[1] + 1
        
        #Set up proper matrices for vectors for MARSS
        #PROCESS EQUATION
        B = diag(m) # 2x2; Identity
        U = matrix(0,nrow=m,ncol=1) # 2x1; both elements = 0 --- WE ONLY DO THIS IF WE HAVE DE-MEANED THE DATA
        Q = matrix(list(0),m,m) # 2x2; all 0 for now
        diag(Q) = c("q.alpha","q.beta") # 2x2; diag = (q1,q2)
        
        #DEFINE OBSERVATION MODEL
        # for observation eqn
        Z = array(NA, c(1,m,TT)) # NxMxT; empty for now
        Z[1,1,] = rep(1,TT) # Nx1; 1's for intercept
        Z[1,2,] = pred.data # Nx1; regr variable
        A = matrix(0) # 1x1; scalar = 0
        R = matrix("r") # 1x1; scalar = r
        
        #DEFINE STARTING VALUES
        # only need starting values for regr parameters
        #inits.list = list(x0=matrix(c(mean(resp.data), mean(resp.data/(pred.data+0.00001))), nrow=m))
        inits.list <- list(x0=matrix(c(0,0),nrow=m))
        
        # list of model matrices & vectors
        mod.list = list(B=B, U=U, Q=Q, Z=Z, A=A, R=R)
        
        #FIT THE MODEL
        # fit univariate DLM
        dlm <- MARSS(resp.data, inits=inits.list, model=mod.list, control=list(maxit=maxit), silent=TRUE)
        fitted.mod.list[[counter]] <- dlm
        ###### PREDICTION COMPONENT
        
        #4. Standardize Predictors
        std.df.pred <- scale(df.pred, center=attr(std.df.data,"scaled:center")[-1], 
                             scale=attr(std.df.data,"scaled:scale")[-1])
        
        #5. Determine last estimated slope and intercept for prediction
        curr.intercept <- dlm$states[1,TT]
        curr.slope <- dlm$states[2,TT]
        
        #6. Generate Predictions
        raw.pred <- curr.intercept + curr.slope*data.frame(std.df.pred)[,ncol(std.df.pred)]
        dist.pred <- (raw.pred * attr(std.df.data,"scaled:scale")[1]) + attr(std.df.data,"scaled:center")[1]
        pred <- median(dist.pred)
        ##########################
        if(model.type==1) { pred <- exp(pred) }
        
        ##########################
        # print(paste(river, ' ', fw, '.', o, ' ', round(pred, 0), sep=''))
        
        #Plotting Section
        
        
        #OUTPUTS
        output.river <- c(output.river, river)
        output.fwAge <- c(output.fwAge, fw)
        output.oAge <- c(output.oAge, o)
        # i <- 1
        # for(i in 1:n.pred) {
        #   if(i==1) {
        #     # name.model <- paste(round(importance(rf.mod)[i,1],1), "% " , "(", names(predData)[i], ")", sep="")
        #     name.model <- paste("(", names(predData)[i], ")", sep="")
        #   }else {
        #     # name.model <- paste(name.model, " + ", round(importance(rf.mod)[i,1],1), "% " , "(", names(predData)[i], ")", sep="")
        #     name.model <- paste(name.model, " + ", "(", names(predData)[i], ")", sep="")
        #   }
        # }#next i
        
        name.model <- paste0("DLM: ", names(df.pred)[length(names(df.pred))])
        output.model <- c(output.model, name.model)
        output.forecast <- c(output.forecast, round(pred, 0))
        
      }#next o	
    }#next fw
  }#next river
  
  #OUTPUT SECTION
  short.output <- data.frame(output.river, output.fwAge, output.oAge, output.model, output.forecast)
  names(short.output) <- c('System', 'fwAge', 'oAge', 'Model', 'Forecast')
  output <- NULL
  output$short <- short.output
  output$fitted.mod.list <- fitted.mod.list
  return(output)
}