# set working directory to the location of your song file
setwd("YourWD")

# load required packages
library(seewave)
library(tuneR)

# read in your song file and normalize amplitude
a<-readWave("YourWave.wav")
normalize(a,
          unit=c("Bit"))

# get temporal measuremnts for your song
aNotes<-timer(a,
              dmin=0.01, # minimum duration for signal detection
              envt="hil", #Hilbert envelope type
              msmooth=c(512, 95), # smoothing the amplitude envelope
              threshold=10) # minimum amplitude (in % of total) for signal detection
aNotes # looking at what measurements are generated

# getting a small portion of the file for a spectrogram check
a1<-cutw(a,
         from=5, # start time (s)
         to=8, # end time (s)
         output="Wave")

# checking the accuracy of timer() measurements by overlaying them on a spectrogram
# adjust dmin and threshold to improve the fit of the timer() brackets on each spectrogram trace
spectro(a1,
        scale=F)
par(new=T)
timer(a1,
      dmin=0.01, 
      envt="hil", 
      msmooth=c(512, 95), 
      threshold=10)


# making a dataframe in which to deposit measurements
aDF<-data.frame(c(1:length(aNotes$s)))
colnames(aDF)<-"SyllableNumber"

# this for loop generates the following measurements and adds them to aDF:
## syllable duration
## maximum dominant frequency
## minimum dominant frequency
## mean dominant frequency
## dominant frequency range
## maximum total frequency
## minimum total frequency
## mean total frequency
## total frequency range
## total entropy

for (i in 1:length(aNotes$s.start)) {
  aDF$Duration[i]<-aNotes$s[i]
  aDF$MaxDomFreq[i]<-(max(dfreq(
    cutw(a,
         from=aNotes$s.start[i],
         to=aNotes$s.end[i],
         output="Wave"),
    plot = F)[,2]))
  aDF$MinDomFreq[i]<-(min(dfreq(
    cutw(a,
         from=aNotes$s.start[i],
         to=aNotes$s.end[i],
         output="Wave"),
    plot = F)[,2]))
  aDF$MeanDomFreq[i]<-(mean(dfreq(
    cutw(a,
         from=aNotes$s.start[i],
         to=aNotes$s.end[i],
         output="Wave"),
    plot = F)[,2]))
  aDF$DomFreqRange[i]<-aDF$MaxFrequency[i]-aDF$MinFrequency[i]
  crit = -24
  try(aDF$MaxFreq[i]<-max(meanspec(cutw(a,
                                        from=aNotes$s.start[i],
                                        to=aNotes$s.end[i],
                                        output="Wave"),
                                          dB='max0')[,1][meanspec(cutw(a,
                                                                       from=aNotes$s.start[i],
                                                                       to=aNotes$s.end[i],
                                                                       output="Wave"),
                                                                  dB='max0')[,2]>crit]))
  try(aDF$MinFreq[i]<-min(meanspec(cutw(a,
                                        from=aNotes$s.start[i],
                                        to=aNotes$s.end[i],
                                        output="Wave"),
                                          dB='max0')[,1][meanspec(cutw(a,
                                                                       from=aNotes$s.start[i],
                                                                       to=aNotes$s.end[i],
                                                                       output="Wave"),
                                                                  dB='max0')[,2]>crit]))
  try(aDF$MeanFreq[i]<-mean(meanspec(cutw(a,
                                          from=aNotes$s.start[i],
                                          to=aNotes$s.end[i],
                                          output="Wave"),
                                            dB='max0')[,1][meanspec(cutw(a,
                                                                         from=aNotes$s.start[i],
                                                                         to=aNotes$s.end[i],
                                                                         output="Wave"),
                                                                    dB='max0')[,2]>crit]))
  try(aDF$PeakFreq[i]<-meanspec(cutw(a,
                                     from=aNotes$s.start[i],
                                     to=aNotes$s.end[i],
                                     output="Wave"),
                                       dB='max0')[,1][match(c(0),
                                                            meanspec(cutw(a,
                                                                          from=aNotes$s.start[i],
                                                                          to=aNotes$s.end[i],
                                                                          output="Wave"), dB='max0')[,2])])
  try(aDF$FreqRange[i]<-aDF$MaxFreq[i]-aDF$MinFreq[i])
  aDF$TotalEntropy[i]<-H(cutw(a,
                              from=aNotes$s.start[i],
                              to=aNotes$s.end[i],
                              output="Wave"))
}