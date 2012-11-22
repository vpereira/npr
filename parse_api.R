#install packages first
library(RCurl)
library(RJSONIO)

#it returns a json with
#$downstream
#$upstream
#$cpu
#...
getStates <- function()
{
  fromJSON(RCurl::getURI('http://apiuser:abc123@192.168.255.1/api/state.json'))
}

getTime <- function()
{
  Sys.time()
}

getDownstream <- function(json) {
  mean(json$downstream)
}

getUpstream <- function(json) {
  mean(json$upstream)
}

getCPU <- function(json) {
  mean(json$cpu)
}

downstream <- c()
upstream <- c()
timespace <- c()
cpu <- c()

for(a in 1:30) {
  json = getStates()
  timespace  <- union(timespace,getTime())
  upstream   <- union(upstream,getUpstream(json))
  downstream <- union(downstream,getDownstream(json))
  cpu <- union(cpu,getCPU(json))
  Sys.sleep(30)
}

data = data.frame(time=timespace,downstream=downstream,upstream=upstream,cpu=cpu)
par(mfrow=c(2, 1))
plot.new()
plot.window(ylim=range(data$downstream),xlim=range(data$time))
lines(data$time,data$downstream,type="l",col="red")
plot.window(ylim=range(data$upstream),xlim=range(data$time))
lines(data$time,data$upstream,type="l",col="blue")
legend("topright",legend=c("upstream","downstream"),       
       lty=c(1,1), # gives the legend appropriate symbols (lines)
       ,lwd=c(2.5,2.5),col=c("blue","red")) # gives the legend lines the correct color and width

axis(1)
axis(2)
box()
plot(data$time,data$cpu, main="cpu usage",type="l",ylab="cpu usage (%)",xlab="time range")