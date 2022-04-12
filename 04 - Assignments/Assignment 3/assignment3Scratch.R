# ==============================================================================
# TASK 1
# ==============================================================================

# The data input
y = c( rep(1,9),rep(0,3) , rep(1,45),rep(0,15) , rep(1,3),rep(0,9) ) 
s = c( rep("A",12) , rep("B",60) , rep("C",12) )
fileName = "Ass3.1.csv"
write.csv( data.frame(y=y,s=s) , file=fileName , row.names=FALSE )


# Example for Jags-Ydich-XnomSsubj-Mbernbeta.R 
#------------------------------------------------------------------------------- 
# Optional generic preliminaries:
graphics.off() # This closes all of R's graphics windows.
# rm(list=ls())  # Careful! This clears all of R's memory!

#------------------------------------------------------------------------------- 
# Load The data 
myData = read.csv(fileName)

# Include head of data
head(myData)

# N.B.: The functions below expect the data to be a data frame, 
# with one component named y being a vector of integer 0,1 values,
# and one component named s being a factor of subject identifiers.
myData$s = factor( myData$s )

#------------------------------------------------------------------------------- 
# Load the relevant model into R's working memory:
source("Jags-Ydich-XnomSsubj-MbernBeta.R")

#------------------------------------------------------------------------------- 
# Optional: Specify filename root and graphical format for saving output.
# Otherwise specify as NULL or leave saveName and saveType arguments 
# out of function calls.
dir.create('Output')
fileNameRoot = "Output//"
graphFileType = "png" 

#------------------------------------------------------------------------------- 
# Generate the MCMC chain:
mcmcCoda = genMCMC( data=myData , numSavedSteps=50000 , saveName=fileNameRoot )

#------------------------------------------------------------------------------- 
# Display diagnostics of chain, for specified parameters:
parameterNames = varnames(mcmcCoda) # get all parameter names
for ( parName in parameterNames ) {
  diagMCMC( codaObject=mcmcCoda , parName=parName , 
            saveName=fileNameRoot , saveType=graphFileType )
}
#------------------------------------------------------------------------------- 
# Get summary statistics of chain:
summaryInfo = smryMCMC( mcmcCoda , compVal=NULL , rope=c(0.45,0.55) ,
                        compValDiff=0.0 , ropeDiff = c(-0.05,0.05) ,
                        saveName=fileNameRoot )

# Display posterior information:
plotMCMC( mcmcCoda , data=myData , compVal=NULL , rope=c(0.45,0.55) ,
          compValDiff=0.0 , ropeDiff = c(-0.05,0.05) ,
          saveName=fileNameRoot , saveType=graphFileType )

# Add the histogram
plotPost(mcmcCoda[,'theta[1]'], xlim = c(0,1), xlab= expression(theta[1]),
         main = 'Daniel Carpenter', col = 'darkseagreen3',
                      showCurve = FALSE)

# Add the density line
paramSampleVec = as.matrix(mcmcCoda[,'theta[1]'])
densCurve = density( paramSampleVec , adjust=2 )
lines( densCurve$x , densCurve$y , type="l" , lwd=2.5 , col='darkseagreen4' , bty="n")


graphics.off() # This closes all of R's graphics windows.
