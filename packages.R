# A short script to help installing packages on the go
# Most useful if you are distributing  a set of script files to people who may not be aware that the needed packages are not installed
# Also useful if you use many packages and want to organise their loading at the beginning of a script
# Source: https://www.r-bloggers.com/install-and-load-missing-specifiedneeded-packages-on-the-fly/
need<-c("arulesViz") #needed packages for a job
ins<-installed.packages()[,1] #find out which packages are installed
(Get<-need[which(is.na(match(need,ins)))]) # check if the needed packages are installed
if(length(Get)>0){install.packages(Get)} #install the needed packages if they are not-installed
eval(parse(text=paste("library(",need,")")))#load the needed packages