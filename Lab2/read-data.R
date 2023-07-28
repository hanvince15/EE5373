

# Functions to read in the CSV table that contains all of the raw data.
# Before running these functions, make sure the file "all-data.csv" is
# in the local directory.
# Also, within the R environment, change the working directory to the directory
# that contains the data file using the toolbar menu:
# File -> Change dir
#

# Read the data from the csv file.
processors <- read.csv("C:/Users/1hanv/OneDrive/Documents/EE5373/Lab2/all-data.csv")


################################################################
#
# This function returns the data from the desired column.
# Example:  clock<-get_column("Fp2000","Processor.Clock..MHz.")

get_column <- function(x,y) {
  
  # x = string with the name of the desired benchmark
  # y = desired column
  #
  # Find the indices of all rows that have an entry for the  
  # indicated benchmark
  benchmark <- paste(paste("Spec",x,sep=""),"..average.base.",
                     sep="")
  ix <- !is.na(processors[,benchmark])
  return(processors[ix,y])
}
################################################################




################################################################
# This function extracts the interesting data columns for the given benchmark
# program and returns a dataframe with these columns.

extract_data <- function(benchmark) {
  
  temp <- paste(paste("Spec",benchmark,sep=""),"..average.base.", sep="")
  
  # perf = the performance reported in the database
  perf <- get_column(benchmark,temp)
  
  #nperf = performance normalized to the overall range
  max_perf <- max(perf)
  min_perf <- min(perf)
  range <- max_perf - min_perf
  nperf <- 100 * (perf - min_perf) / range
  
  clock <- get_column(benchmark,"Processor.Clock..MHz.")
  threads <- get_column(benchmark,"Threads.core")
  cores <- get_column(benchmark,"Cores")
  TDP <- get_column(benchmark,"TDP")
  transistors <- get_column(benchmark,"Transistors..millions.")
  dieSize <- get_column(benchmark,"Die.size..mm.2.")
  voltage <- get_column(benchmark,"Voltage..low.")
  featureSize <- get_column(benchmark,"Feature.Size..microns.")
  channel <- get_column(benchmark,"Channel.length..microns.")
  FO4delay <- get_column(benchmark,"FO4.Delay..ps.")
  L1icache <- get_column(benchmark,"L1..instruction...on.chip.")
  L1dcache <- get_column(benchmark,"L1..data...on.chip.")
  L2cache <- get_column(benchmark,"L2..on.chip.")
  L3cache <- get_column(benchmark,"L3..on.chip.")
  
  return(data.frame(nperf,perf,clock,threads,cores,TDP,transistors,dieSize,voltage,featureSize,channel,FO4delay,L1icache,L1dcache,L2cache,L3cache))
  
}
################################################################


# Extract a new data frame for each of the benchmark programs available in the data set.

int92.dat <- extract_data("Int1992")
fp92.dat <- extract_data("Fp1992")
int95.dat <- extract_data("Int1995")
fp95.dat <- extract_data("Fp1995")
int00.dat <- extract_data("Int2000")
fp00.dat <- extract_data("Fp2000")
int06.dat <- extract_data("Int2006")
fp06.dat <- extract_data("Fp2006")


# Calculate and print statistics for each column in the data frames

calculate_statistics <- function(data) {
  col_names <- colnames(data)
  
  for (col in col_names) {
    col_data <- data[[col]]
    mean_val <- mean(col_data)
    variance_val <- var(col_data)
    min_val <- min(col_data)
    max_val <- max(col_data)
    
    cat("Column:", col, "\n")
    cat("Mean:", mean_val, "\n")
    cat("Variance:", variance_val, "\n")
    cat("Minimum:", min_val, "\n")
    cat("Maximum:", max_val, "\n")
    cat("\n")
  }
}

# Call the calculate_statistics function for each data frame

cat("int92.dat statistics:\n")
calculate_statistics(int92.dat)
cat("fp92.dat statistics:\n")
calculate_statistics(fp92.dat)
cat("int95.dat statistics:\n")
calculate_statistics(int95.dat)
cat("fp95.dat statistics:\n")
calculate_statistics(fp95.dat)
cat("int00.dat statistics:\n")
calculate_statistics(int00.dat)
cat("fp00.dat statistics:\n")
calculate_statistics(fp00.dat)
cat("int06.dat statistics:\n")
calculate_statistics(int06.dat)
cat("fp06.dat statistics:\n")
calculate_statistics(fp06.dat)

