#### The purpose of this script is to generate acoustic measurements of individual syllables of bird vocalizations that have been segmented ("chopped") into a series of .wav files at the song level.

# set working directory to the location of your song files
setwd("C:/Users/Shelby Palmer/Desktop/PracticeData")

# load required packages
library(seewave)
library(tuneR)

# We will practice on 1 song file first
a<-readWave("Gm_GO_1.wav") # read in your song file
a # looking at the properties of the file

# filtering
a<-fir(a,
       from = 2500, #lower frequency limit
       to = 15000, #upper frequency limit
       bandpass = T,
       output = "Wave")

# get temporal measuremnts for your song
aTime<-timer(a,
              dmin=0.05, # minimum duration for signal detection
              envt="hil", #Hilbert envelope type
              msmooth=c(512, 95), # smoothing the amplitude envelope
              threshold=7) # minimum amplitude (in % of total) for signal detection
aTime # looking at what measurements are generated

# checking the accuracy of timer() measurements by overlaying them on a spectrogram
# adjust dmin and threshold to improve the fit of the timer() brackets on each spectrogram trace
spectro(a,
        wl = 512,
        ovlp = 95,
        collevels = seq(-42,0,6),
        flim = c(0, 10), # frequency axis limits
        osc = F,
        scale = F,
        colgrid = "gray",
        cexlab = 0.8,
        cexaxis = 0.7)
par(new=T)
timer(a,
      dmin=0.05, 
      envt="hil", 
      msmooth=c(512, 95), 
      threshold=7)

#### MEASUREMENT EXTRACTION FUNCTIONS ####
# making functions that extract frequency measurements from the mean frequency spectrum using the function meanspec()

crit = -24 # critical amplitude value when we use dB='max0'

MaxFreq<-function(x) {
  max(meanspec(x, 
               flim=c(0,10), # defining frequency limits
               wl=512, 
               dB='max0', 
               plot=F)[,1][meanspec(x, 
                                    flim=c(0,10), 
                                    wl = 512, 
                                    dB='max0',
                                    plot=F)[,2]>crit])
}

MinFreq<-function(x) {
  min(meanspec(x, 
               flim=c(0,10), 
               wl=512, 
               dB='max0',
               plot=F)[,1][meanspec(x, 
                                    flim=c(0,10), 
                                    wl = 512, 
                                    dB='max0',
                                    plot=F)[,2]>crit])
}

MeanFreq<-function(x) {
  mean(meanspec(x, 
                flim=c(0,10), 
                wl=512, 
                dB='max0',
                plot=F)[,1][meanspec(x, 
                                     flim=c(0,10), 
                                     wl = 512, 
                                     dB='max0',
                                     plot=F)[,2]>crit])
}

PeakFreq<-function(x) {
  meanspec(x, 
           flim=c(0,10), 
           wl = 512, 
           dB='max0',
           plot=F)[,1][match(c(0),
                             meanspec(x, 
                                      flim=c(0,10), 
                                      wl = 512, 
                                      dB='max0',
                                      plot=F)[,2])]
}

AbsPFmaxslope<-function(x) {
  max(abs(diff(dfreq(x, 
                     ovlp=95, 
                     threshold=5, 
                     plot=F)[,2])))
}


#### Generating a dataframe for our practice file ####
Notes<-data.frame(note_num=seq(1:length(aTime$s.start)), 
                    file_name=rep("Gm_GO_1.wav"))

# this for loop generates the following measurements and adds them to aDF:
## note duration
## maximum frequency
## minimum frequency
## mean frequency
## frequency bandwidth
## peak frequency
## absolute value of the maximum peak frequency slope
## total entropy

for (i in 1:length(aTime$s.start)) {
  Notes$Duration[i]<-aTime$s[i]
  Notes$Max_Freq[i]<-MaxFreq(cutw(a,
                                    from=aTime$s.start[i],
                                    to=aTime$s.end[i],
                                    output="Wave",
                                    plot=F))
  Notes$Min_Freq[i]<-MinFreq(cutw(a,
                                    from=aTime$s.start[i],
                                    to=aTime$s.end[i],
                                    output="Wave",
                                    plot=F))
  Notes$Freq_Range[i]<-Notes$Max_Freq[i]-Notes$Min_Freq[i]
  Notes$Mean_Freq[i]<-MeanFreq(cutw(a,
                                      from=aTime$s.start[i],
                                      to=aTime$s.end[i],
                                      output="Wave",
                                      plot=F))
  Notes$Peak_Freq[i]<-PeakFreq(cutw(a,
                                      from=aTime$s.start[i],
                                      to=aTime$s.end[i],
                                      output="Wave",
                                      plot=F))
  Notes$Abs_Max_PF_Slope[i]<-AbsPFmaxslope(cutw(a,
                                                  from=aTime$s.start[i],
                                                  to=aTime$s.end[i],
                                                  output="Wave",
                                                  plot=F))
  Notes$Entropy[i]<-H(cutw(a,
                             from=aTime$s.start[i],
                             to=aTime$s.end[i],
                             output="Wave",
                             plot=F),
                        msmooth=c(512,90))
}
View(Notes)

#### All Measurements ####
# loop that binds every other set of measurements to the first dataframe

for (i in 2:length(list.files())) {
  a<-readWave(list.files()[i])
  a1<-fir(a,
          from=2500,
          to=15000,
          output="Wave")
  aTime<-timer(a1,
                dmin=0.05, 
                envt="hil", 
                msmooth=c(512, 95), 
                threshold=7,
                plot=F)
  b<-data.frame(note_num=seq(1:length(aTime$s.start)), 
                file_name=rep(list.files()[i]))
  for (i in 1:length(aTime$s.start)) {
    b$Duration[i]<-aTime$s[i]
    b$Max_Freq[i]<-MaxFreq(cutw(a1,
                                from=aTime$s.start[i],
                                to=aTime$s.end[i],
                                output="Wave",
                                plot=F))
    b$Min_Freq[i]<-MinFreq(cutw(a1,
                                from=aTime$s.start[i],
                                to=aTime$s.end[i],
                                output="Wave",
                                plot=F))
    b$Freq_Range[i]<-b$Max_Freq[i]-b$Min_Freq[i]
    b$Mean_Freq[i]<-MeanFreq(cutw(a1,
                                  from=aTime$s.start[i],
                                  to=aTime$s.end[i],
                                  output="Wave",
                                  plot=F))
    b$Peak_Freq[i]<-PeakFreq(cutw(a1,
                                  from=aTime$s.start[i],
                                  to=aTime$s.end[i],
                                  output="Wave",
                                  plot=F))
    b$Abs_Max_PF_Slope[i]<-AbsPFmaxslope(cutw(a1,
                                              from=aTime$s.start[i],
                                              to=aTime$s.end[i],
                                              output="Wave",
                                              plot=F))
    b$Entropy[i]<-H(cutw(a1,
                         from=aTime$s.start[i],
                         to=aTime$s.end[i],
                         output="Wave",
                         plot=F),
                    msmooth=c(512,90))
  }
  Notes<-rbind(Notes, b)
}
