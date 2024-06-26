#this script is getting the acoustic features for multiple examples of the same call type to get the standard error ect.

#TODO: add chopchop, roar, snort, squawk, squeak

#test
library(seewave)
library(tuneR)
library(ggplot2)
library(DT)
library(plyr)
library(vioplot)
library(RColorBrewer)

# Function to calculate the Signal-to-Noise Ratio (SNR)
calculate_snr <- function(signal) {
  signal_power <- mean(signal^2)
  noise <- signal - mean(signal) # Assuming the noise is the deviation from the mean
  noise_power <- mean(noise^2)
  snr <- 10 * log10(signal_power / noise_power)
  return(snr)
}


#colour palette for spectrogram
jet.colors <- colorRampPalette(c("white","white", "seashell","seashell1", "plum1", "deeppink4", "darkblue", "black"))

plot.colors <- brewer.pal(n = 11, name = "Paired")

#list of features to keep
keep <- c("mode", "median", "sh", "Q25", "Q75","IQR", "skewness", "kurtosis", "cent", "sfm")

#calls <- c("growl", "chirp", "chitter")

# mean	- mean frequency (see mean)
# sd	- standard deviation of the mean (see sd)
# sem	- standard error of the mean
# median	- median frequency (see median)
# mode	- mode frequency, i.e. the dominant frequency
# Q25	- first quartile (see quantile)
# Q75	- third quartile (see quantile)
# IQR	- interquartile range (see IQR)
# cent	- centroid
# skewness	- skewness, a measure of asymmetry
# kurtosis	- kurtosis, a measure of peakedness. if K > 3 when the spectrum is leptokurtic, i.e. it has more items near the center and at the tails, with fewer items in the shoulders relative to normal distribution with the same mean and variance.
# sfm	- spectral flatness measure (see sfm) This function estimates the flatness of a frequency spectrum. The SFM of a noisy signal will tend towards 1 whereas the SFM of a pure tone signal will tend towards 0.
# sh	- spectral entropy (see sh)
# prec	- frequency precision of the spectrum

# shannon entropy - The Shannon spectral entropy of a noisy signal will tend towards 1 whereas the Shannon spectral entropy of a pure tone signal will tend towards 0

#ACI - computes the variability of the intensities registered in audio-recordings


#growl

dir <- "C:/Users/egrout/Dropbox/calls/each call type/growl/"

filenames <- list.files(dir, pattern=".wav", full.names=TRUE)
unique(filenames)


all_names = basename(filenames)
lwf <- lapply(filenames, readWave)


#make empty dataframe to add the features too
prop <- data.frame(matrix(ncol = (length(keep) + 6), nrow = 0))

#this forloop is reading in each call (cut into single wave file from adobe audition) and extracting acoustic features, and putting it into the prop dataframe

for (i in 1:length(lwf)) {
  
  #spectro(lwf[[i]],f=22050,ovlp=85,zp=16, palette=jet.colors, osc=TRUE, collevels=seq(-29,0), grid = F)
  f = lwf[[i]]@samp.rate
  dat <- meanspec(lwf[[i]],f=f,plot=F)
  dat2 <- as.data.frame(dat)
  #excluding rows containing 1.0000
  dat2 <- subset(dat2, y != 1)
  #exclude rows where x is 0
  dat2 <- subset(dat2, x != 0)
  #excluding values greater than 4kHz
  dat2 <- subset(dat2, x < 4)
  #dominant frequency is the frequency of max amplitude, which is x for the max of y in chirp_dat2
  dom_freq <- dat2[dat2$y == max(dat2$y),1]
  prop1 <- as.data.frame(specprop(dat, f=lwf[[i]]@samp.rate))
  prop1 <- prop1[, keep] 
  prop1$dom_freq <- dom_freq*1000
  #Acoustic complexity index
  #prop1$ACI <- ACI(lwf[[i]], lwf[[i]]@samp.rate, channel = 1, wl = 512, ovlp = 0,  wn = "hamming", flim = NULL, nbwindows = 1)
  #Entropy
  prop1$entropy <- H(lwf[[i]], lwf[[i]]@samp.rate, channel = 1, wl = 512, envt="hil", msmooth = NULL, ksmooth = NULL)
  prop1$duration <- seewave::duration(lwf[[i]], lwf[[i]]@samp.rate, channel=1)
  
  # Calculate SNR
  if (lwf[[i]]@stereo) {
    signal <- lwf[[i]]@right
  } else {
    signal <- lwf[[i]]@left
  }
  prop1$snr <- calculate_snr(signal)
  
  
  prop1$name <- "growl"
  prop1$file <- all_names[i]
  prop1$sum_calls <- length(all_names)
  prop <- rbind(prop, prop1)

}


#chirp

dir <- "C:/Users/egrout/Dropbox/calls/each call type/chirp/"
filenames <- list.files(dir, pattern=".wav", full.names=TRUE)
all_names = basename(filenames)
lwf <- lapply(filenames, readWave)

i = 36
for (i in 1:length(lwf)) {
  #spectro(lwf[[i]],f=lwf[[i]]@samp.rate,ovlp=85,zp=16, palette=jet.colors, osc=TRUE, collevels=seq(-29,0), grid = F)
  f = lwf[[i]]@samp.rate
  #incase we wanted to resample but I don't think this is necessary 
  #if (f > 100000){
  #  lwf[[i]] <- resamp(lwf[[i]], f = lwf[[i]]@samp.rate, g = 50000, output = "Wave")
  #}
  #dev.off()
  dat <- meanspec(lwf[[i]],f= lwf[[i]]@samp.rate , plot= F)
  dat2 <- as.data.frame(dat)
  #excluding rows containing 1.0000
  dat2 <- subset(dat2, y != 1)
  #exclude rows where x is 0
  dat2 <- subset(dat2, x != 0)
  #excluding values less than 4kHz
  dat2 <- subset(dat2, x > 4)
  #dominant frequency is the frequency of max amplitude, which is x for the max of y in chirp_dat2
  dom_freq <- dat2[dat2$y == max(dat2$y),1]
  prop2 <- as.data.frame(specprop(dat,f= lwf[[i]]@samp.rate))
  prop2 <- prop2[, keep] 
  prop2$dom_freq <- dom_freq*1000
  #Acoustic complexity index
  #prop2$ACI <- ACI(lwf[[i]], lwf[[i]]@samp.rate, channel = 1, wl = 512, ovlp = 0,  wn = "hamming", flim = NULL, nbwindows = 1)
  #Entropy
  prop2$entropy <- H(lwf[[i]], lwf[[i]]@samp.rate, channel = 1, wl = 512, envt="hil", msmooth = NULL, ksmooth = NULL)
  prop2$duration <- seewave::duration(lwf[[i]], lwf[[i]]@samp.rate, channel=1)
  
  # Calculate SNR
  if (lwf[[i]]@stereo) {
    signal <- lwf[[i]]@right
  } else {
    signal <- lwf[[i]]@left
  }
  prop2$snr <- calculate_snr(signal)
  
  
  prop2$name <- "chirp"
  prop2$file <- all_names[i]
  prop2$sum_calls <- length(all_names)
  prop <- rbind(prop, prop2)
}


#chitter

dir <- "C:/Users/egrout/Dropbox/calls/each call type/chitter/"
filenames <- list.files(dir, pattern=".wav", full.names=TRUE)
all_names = basename(filenames)
lwf <- lapply(filenames, readWave)

#df <- data.frame(matrix(ncol = 2, nrow = length(lwf)))

i = 15
for (i in 1:length(lwf)) {
  
  #spectro(lwf[[i]],f=lwf[[i]]@samp.rate,ovlp=85,zp=16, palette=jet.colors, osc=TRUE, collevels=seq(-29,0), grid = F)
  
  f = lwf[[i]]@samp.rate
  
  #df[i,1] <- filenames[i]
  #df[i,2] <- f
  
  dat <- meanspec(lwf[[i]],f= lwf[[i]]@samp.rate , plot= F)
  dat2 <- as.data.frame(dat)
  #excluding rows where y is 1.0000
  dat2 <- subset(dat2, y != 1)
  #exclude rows where x is 0
  dat2 <- subset(dat2, x != 0)
  #excluding values less than 4kHz
  #dat2 <- subset(dat2, x > 4)
  #dominant frequency is the frequency of max amplitude, which is x for the max of y in chirp_dat2
  dom_freq <- dat2[dat2$y == max(dat2$y), 1]
  prop3 <- as.data.frame(specprop(dat,f=lwf[[i]]@samp.rate))
  prop3 <- prop3[, keep] 
  prop3$dom_freq <- dom_freq*1000
  #Acoustic complexity index
  #prop3$ACI <- ACI(lwf[[i]], channel = 1, wl = 512, ovlp = 0,  wn = "hamming", flim = NULL, nbwindows = 1)
  #Entropy
  prop3$entropy <- H(lwf[[i]], lwf[[i]]@samp.rate, channel = 1, wl = 512, envt="hil", msmooth = NULL, ksmooth = NULL)
  prop3$duration <- seewave::duration(lwf[[i]], lwf[[i]]@samp.rate, channel=1)
  
  # Calculate SNR
  if (lwf[[i]]@stereo) {
    signal <- lwf[[i]]@right
  } else {
    signal <- lwf[[i]]@left
  }
  prop3$snr <- calculate_snr(signal)
  
  
  prop3$name <- "chitter"
  prop3$file <- all_names[i]
  prop3$sum_calls <- length(all_names)
  prop <- rbind(prop, prop3)
  
}


#click

dir <- "C:/Users/egrout/Dropbox/calls/each call type/click/"
filenames <- list.files(dir, pattern=".wav", full.names=TRUE)
all_names = basename(filenames)
lwf <- lapply(filenames, readWave)

i = 54
for (i in 1:length(lwf)) {
  #spectro(lwf[[i]],f=lwf[[i]]@samp.rate,ovlp=85,zp=16, palette=jet.colors, osc=TRUE, collevels=seq(-29,0), grid = F)
  f = lwf[[i]]@samp.rate
  dat <- meanspec(lwf[[i]],f=lwf[[i]]@samp.rate , plot=F)
  dat2 <- as.data.frame(dat)
  #excluding rows where y is 1.0000
  dat2 <- subset(dat2, y != 1)
  #exclude rows where x is 0
  dat2 <- subset(dat2, x != 0)
  #excluding values less than 4kHz
  #dat2 <- subset(dat2, x > 4)
  #dominant frequency is the frequency of max amplitude, which is x for the max of y in chirp_dat2
  dom_freq <- dat2[dat2$y == max(dat2$y), 1]
  prop4 <- as.data.frame(specprop(dat,f=lwf[[i]]@samp.rate))
  prop4 <- prop4[, keep] 
  prop4$dom_freq <- dom_freq*1000
  #Acoustic complexity index
  #prop4$ACI <- ACI(lwf[[i]], channel = 1, wl = 512, ovlp = 0,  wn = "hamming", flim = NULL, nbwindows = 1)
  #Entropy
  prop4$entropy <- H(lwf[[i]], lwf[[i]]@samp.rate, channel = 1, wl = 512, envt="hil", msmooth = NULL, ksmooth = NULL)
  prop4$duration <- seewave::duration(lwf[[i]], lwf[[i]]@samp.rate, channel=1)
  
  # Calculate SNR
  if (lwf[[i]]@stereo) {
    signal <- lwf[[i]]@right
  } else {
    signal <- lwf[[i]]@left
  }
  prop4$snr <- calculate_snr(signal)
  
  prop4$name <- "click"
  prop4$file <- all_names[i]
  prop4$sum_calls <- length(all_names)
  
  prop <- rbind(prop, prop4)
  
  
  
}


#grunt

dir <- "C:/Users/egrout/Dropbox/calls/each call type/grunt/"
filenames <- list.files(dir, pattern=".wav", full.names=TRUE)
all_names = basename(filenames)
lwf <- lapply(filenames, readWave)

for (i in 1:length(lwf)) {
  #spectro(lwf[[i]],f=lwf[[i]]@samp.rate,ovlp=85,zp=16, palette=jet.colors, osc=TRUE, collevels=seq(-29,0), grid = F)
  f = lwf[[i]]@samp.rate
  dat <- meanspec(lwf[[i]],f=lwf[[i]]@samp.rate , plot=F, flim = c(0,2))
  dat2 <- as.data.frame(dat)
  #excluding rows where y is 1.0000
  dat2 <- subset(dat2, y != 1)
  #exclude rows where x is 0
  dat2 <- subset(dat2, x != 0)
  #excluding values greater than 1.5kHz
  dat2 <- subset(dat2, x < 1.5)
  #dominant frequency is the frequency of max amplitude, which is x for the max of y in chirp_dat2
  dom_freq <- dat2[dat2$y == max(dat2$y), 1]
  prop5 <- as.data.frame(specprop(dat,f=lwf[[i]]@samp.rate))
  prop5 <- prop5[, keep] 
  prop5$dom_freq <- dom_freq*1000
  #Acoustic complexity index
  #prop5$ACI <- ACI(lwf[[i]], channel = 1, wl = 512, ovlp = 0,  wn = "hamming", flim = NULL, nbwindows = 1)
  #Entropy
  prop5$entropy <- H(lwf[[i]], lwf[[i]]@samp.rate, channel = 1, wl = 512, envt="hil", msmooth = NULL, ksmooth = NULL)
  prop5$duration <- seewave::duration(lwf[[i]], lwf[[i]]@samp.rate, channel=1)
  
  if ("WaveMC" %in% class(lwf[[i]])) {
    # For multi-channel WaveMC files, use both channels for SNR calculation
    signal <- lwf[[i]]
    
  } else if (lwf[[i]]@stereo) {
    signal <- lwf[[i]]@right
  } else {
    signal <- lwf[[i]]@left
  }
  
  prop5$snr <- calculate_snr(signal)
  
  prop5$name <- "grunt"
  prop5$file <- all_names[i]
  prop5$sum_calls <- length(all_names)
  prop <- rbind(prop, prop5)
  
}


#dc

dir <- "C:/Users/egrout/Dropbox/calls/each call type/dc/"
filenames <- list.files(dir, pattern=".wav", full.names=TRUE)
all_names = basename(filenames)
lwf <- lapply(filenames, readWave)

for (i in 1:length(lwf)) {
  #spectro(lwf[[i]],f=lwf[[i]]@samp.rate,ovlp=85,zp=16, palette=jet.colors, osc=TRUE, collevels=seq(-29,0), grid = F)
  f = lwf[[i]]@samp.rate
  dat <- meanspec(lwf[[i]],f=lwf[[i]]@samp.rate , plot=F)
  dat2 <- as.data.frame(dat)
  #excluding rows where y is 1.0000
  dat2 <- subset(dat2, y != 1)
  #exclude rows where x is 0
  dat2 <- subset(dat2, x != 0)
  #dominant frequency is the frequency of max amplitude, which is x for the max of y in chirp_dat2
  dom_freq <- dat2[dat2$y == max(dat2$y), 1]
  prop6 <- as.data.frame(specprop(dat,f=lwf[[i]]@samp.rate))
  prop6 <- prop6[, keep] 
  prop6$dom_freq <- dom_freq*1000
  #Acoustic complexity index
  #prop6$ACI <- ACI(lwf[[i]], channel = 1, wl = 512, ovlp = 0,  wn = "hamming", flim = NULL, nbwindows = 1)
  #Entropy
  prop6$entropy <- H(lwf[[i]], lwf[[i]]@samp.rate, channel = 1, wl = 512, envt="hil", msmooth = NULL, ksmooth = NULL)
  prop6$duration <- seewave::duration(lwf[[i]], lwf[[i]]@samp.rate, channel=1)
  
  # Calculate SNR
  if (lwf[[i]]@stereo) {
    signal <- lwf[[i]]@right
  } else {
    signal <- lwf[[i]]@left
  }
  prop6$snr <- calculate_snr(signal)
  
  
  prop6$name <- "dc"
  prop6$file <- all_names[i]
  prop6$sum_calls <- length(all_names)
  
  prop <- rbind(prop, prop6)
  
}

#vibrate

dir <- "C:/Users/egrout/Dropbox/calls/each call type/vibrate/"
filenames <- list.files(dir, pattern=".wav", full.names=TRUE)
all_names = basename(filenames)
lwf <- lapply(filenames, readWave)

for (i in 1:length(lwf)) {
  #spectro(lwf[[i]],f=lwf[[i]]@samp.rate,ovlp=85,zp=16, palette=jet.colors, osc=TRUE, collevels=seq(-29,0), grid = F)
  f = lwf[[i]]@samp.rate
  dat <- meanspec(lwf[[i]],f=lwf[[i]]@samp.rate , plot=F)
  dat2 <- as.data.frame(dat)
  #excluding rows where y is 1.0000
  dat2 <- subset(dat2, y != 1)
  #exclude rows where x is 0
  dat2 <- subset(dat2, x != 0)
  #dominant frequency is the frequency of max amplitude, which is x for the max of y in chirp_dat2
  dom_freq <- dat2[dat2$y == max(dat2$y), 1]
  prop7 <- as.data.frame(specprop(dat,f=lwf[[i]]@samp.rate))
  prop7 <- prop7[, keep] 
  prop7$dom_freq <- dom_freq*1000
  #Acoustic complexity index
  #prop7$ACI <- ACI(lwf[[i]], channel = 1, wl = 512, ovlp = 0,  wn = "hamming", flim = NULL, nbwindows = 1)
  #Entropy
  prop7$entropy <- H(lwf[[i]], lwf[[i]]@samp.rate, channel = 1, wl = 512, envt="hil", msmooth = NULL, ksmooth = NULL)
  prop7$duration <- seewave::duration(lwf[[i]], lwf[[i]]@samp.rate, channel=1)
  
  # Calculate SNR
  if (lwf[[i]]@stereo) {
    signal <- lwf[[i]]@right
  } else {
    signal <- lwf[[i]]@left
  }
  prop7$snr <- calculate_snr(signal)
  
  
  prop7$name <- "vibrate"
  prop7$file <- all_names[i]
  prop7$sum_calls <- length(all_names)
  
  prop <- rbind(prop, prop7)
  
}

#squeal

dir <- "C:/Users/egrout/Dropbox/calls/each call type/squeal/"
filenames <- list.files(dir, pattern=".wav", full.names=TRUE)
all_names = basename(filenames)
lwf <- lapply(filenames, readWave)

for (i in 1:length(lwf)) {
  #spectro(lwf[[i]],f=lwf[[i]]@samp.rate,ovlp=85,zp=16, palette=jet.colors, osc=TRUE, collevels=seq(-29,0), grid = F)
  f = lwf[[i]]@samp.rate
  dat <- meanspec(lwf[[i]],f=lwf[[i]]@samp.rate , plot=T)
  dat2 <- as.data.frame(dat)
  #excluding rows where y is 1.0000
  dat2 <- subset(dat2, y != 1)
  #exclude rows where x is 0
  dat2 <- subset(dat2, x != 0)
  #dominant frequency is the frequency of max amplitude, which is x for the max of y in chirp_dat2
  dom_freq <- dat2[dat2$y == max(dat2$y), 1]
  prop8 <- as.data.frame(specprop(dat,f=lwf[[i]]@samp.rate))
  prop8 <- prop8[, keep] 
  prop8$dom_freq <- dom_freq*1000
  #Acoustic complexity index
  #prop8$ACI <- ACI(lwf[[i]], channel = 1, wl = 512, ovlp = 0,  wn = "hamming", flim = NULL, nbwindows = 1)
  #Entropy
  prop8$entropy <- H(lwf[[i]], lwf[[i]]@samp.rate, channel = 1, wl = 512, envt="hil", msmooth = NULL, ksmooth = NULL)
  prop8$duration <- seewave::duration(lwf[[i]], lwf[[i]]@samp.rate, channel=1)
  
  # Calculate SNR
  if (lwf[[i]]@stereo) {
    signal <- lwf[[i]]@right
  } else {
    signal <- lwf[[i]]@left
  }
  prop8$snr <- calculate_snr(signal)
  
  
  prop8$name <- "squeal"
  prop8$file <- all_names[i]
  prop8$sum_calls <- length(all_names)
  
  prop <- rbind(prop, prop8)
  
}


#bark



dir <- "C:/Users/egrout/Dropbox/calls/each call type/bark/"
filenames <- list.files(dir, pattern=".wav", full.names=TRUE)
all_names = basename(filenames)
lwf <- lapply(filenames, readWave)

for (i in 1:length(lwf)) {
  #spectro(lwf[[i]],f=lwf[[i]]@samp.rate,ovlp=85,zp=16, palette=jet.colors, osc=TRUE, collevels=seq(-29,0), grid = F)
  f = lwf[[i]]@samp.rate
  dat <- meanspec(lwf[[i]],f=lwf[[i]]@samp.rate , plot=F)
  dat2 <- as.data.frame(dat)
  #excluding rows where y is 1.0000
  dat2 <- subset(dat2, y != 1)
  #exclude rows where x is 0
  dat2 <- subset(dat2, x != 0)
  
  #might need to filter off the lower frequencies as the bark call dom freq is at around 4-5kHz 
  #excluding values less than 1kHz - but this might not be a good idea as some of the barks have a grunt at the same time (but they either bark or bark grunt, so filtering for the grunt would then allow us to only see the features of the bark)
  dat2 <- subset(dat2, x > 1)

  #dominant frequency is the frequency of max amplitude, which is x for the max of y in chirp_dat2
  dom_freq <- dat2[dat2$y == max(dat2$y), 1]
  prop9 <- as.data.frame(specprop(dat,f=lwf[[i]]@samp.rate))
  prop9 <- prop9[, keep] 
  prop9$dom_freq <- dom_freq*1000
  #Acoustic complexity index
  #prop9$ACI <- ACI(lwf[[i]], channel = 1, wl = 512, ovlp = 0,  wn = "hamming", flim = NULL, nbwindows = 1)
  #Entropy
  prop9$entropy <- H(lwf[[i]], lwf[[i]]@samp.rate, channel = 1, wl = 512, envt="hil", msmooth = NULL, ksmooth = NULL)
  prop9$duration <- seewave::duration(lwf[[i]], lwf[[i]]@samp.rate, channel=1)
  
  if ("WaveMC" %in% class(lwf[[i]])) {
    # For multi-channel WaveMC files, use both channels for SNR calculation
    signal <- lwf[[i]]
    
  } else if (lwf[[i]]@stereo) {
    signal <- lwf[[i]]@right
  } else {
    signal <- lwf[[i]]@left
  }
  prop9$snr <- calculate_snr(signal)
  
  prop9$name <- "bark"
  prop9$file <- all_names[i]
  prop9$sum_calls <- length(all_names)
  
  prop <- rbind(prop, prop9)
  
}


#hum

dir <- "C:/Users/egrout/Dropbox/calls/each call type/hum/"
filenames <- list.files(dir, pattern=".wav", full.names=TRUE)
all_names = basename(filenames)
lwf <- lapply(filenames, readWave)

for (i in 1:length(lwf)) {
  #spectro(lwf[[i]],f=lwf[[i]]@samp.rate,ovlp=85,zp=16, palette=jet.colors, osc=TRUE, collevels=seq(-29,0), grid = F)
  f = lwf[[i]]@samp.rate
  dat <- meanspec(lwf[[i]],f=lwf[[i]]@samp.rate , plot=F)
  dat2 <- as.data.frame(dat)
  #excluding rows where y is 1.0000
  dat2 <- subset(dat2, y != 1)
  #exclude rows where x is 0
  dat2 <- subset(dat2, x != 0)
  #dominant frequency is the frequency of max amplitude, which is x for the max of y in chirp_dat2
  dom_freq <- dat2[dat2$y == max(dat2$y), 1]
  prop10 <- as.data.frame(specprop(dat,f=lwf[[i]]@samp.rate))
  prop10 <- prop10[, keep] 
  prop10$dom_freq <- dom_freq*1000
  #Acoustic complexity index
  #prop10$ACI <- ACI(lwf[[i]], channel = 1, wl = 512, ovlp = 0,  wn = "hamming", flim = NULL, nbwindows = 1)
  #Entropy
  prop10$entropy <- H(lwf[[i]], lwf[[i]]@samp.rate, channel = 1, wl = 512, envt="hil", msmooth = NULL, ksmooth = NULL)
  prop10$duration <- seewave::duration(lwf[[i]], lwf[[i]]@samp.rate, channel=1)
  
  # Calculate SNR
  if (lwf[[i]]@stereo) {
    signal <- lwf[[i]]@right
  } else {
    signal <- lwf[[i]]@left
  }
  prop10$snr <- calculate_snr(signal)
  
  
  prop10$name <- "hum"
  prop10$file <- all_names[i]
  prop10$sum_calls <- length(all_names)
  
  prop <- rbind(prop, prop10)
  
}

#chopchop

dir <- "C:/Users/egrout/Dropbox/calls/each call type/chopchop/"
filenames <- list.files(dir, pattern=".wav", full.names=TRUE)
all_names = basename(filenames)
lwf <- lapply(filenames, readWave)

for (i in 1:length(lwf)) {
  #spectro(lwf[[i]],f=lwf[[i]]@samp.rate,ovlp=85,zp=16, palette=jet.colors, osc=TRUE, collevels=seq(-29,0), grid = F)
  f = lwf[[i]]@samp.rate
  dat <- meanspec(lwf[[i]],f=lwf[[i]]@samp.rate , plot=F)
  dat2 <- as.data.frame(dat)
  #excluding rows where y is 1.0000
  dat2 <- subset(dat2, y != 1)
  #exclude rows where x is 0
  dat2 <- subset(dat2, x != 0)
  #dominant frequency is the frequency of max amplitude, which is x for the max of y in chirp_dat2
  dom_freq <- dat2[dat2$y == max(dat2$y), 1]
  prop11 <- as.data.frame(specprop(dat,f=lwf[[i]]@samp.rate))
  prop11 <- prop11[, keep] 
  prop11$dom_freq <- dom_freq*1000
  #Acoustic complexity index
  #prop11$ACI <- ACI(lwf[[i]], channel = 1, wl = 512, ovlp = 0,  wn = "hamming", flim = NULL, nbwindows = 1)
  #Entropy
  prop11$entropy <- H(lwf[[i]], lwf[[i]]@samp.rate, channel = 1, wl = 512, envt="hil", msmooth = NULL, ksmooth = NULL)
  prop11$duration <- seewave::duration(lwf[[i]], lwf[[i]]@samp.rate, channel=1)
  
  if ("WaveMC" %in% class(lwf[[i]])) {
    # For multi-channel WaveMC files, use both channels for SNR calculation
    signal <- lwf[[i]]
    
  } else if (lwf[[i]]@stereo) {
    signal <- lwf[[i]]@right
  } else {
    signal <- lwf[[i]]@left
  }
  prop11$snr <- calculate_snr(signal)
  
  
  prop11$name <- "chop chop"
  prop11$file <- all_names[i]
  prop11$sum_calls <- length(all_names)
  
  prop <- rbind(prop, prop11)
  
}

#roar

dir <- "C:/Users/egrout/Dropbox/calls/each call type/roar/"
filenames <- list.files(dir, pattern=".wav", full.names=TRUE)
all_names = basename(filenames)
lwf <- lapply(filenames, readWave)

for (i in 1:length(lwf)) {
  #spectro(lwf[[i]],f=lwf[[i]]@samp.rate,ovlp=85,zp=16, palette=jet.colors, osc=TRUE, collevels=seq(-29,0), grid = F)
  f = lwf[[i]]@samp.rate
  dat <- meanspec(lwf[[i]],f=lwf[[i]]@samp.rate , plot=F)
  dat2 <- as.data.frame(dat)
  #excluding rows where y is 1.0000
  dat2 <- subset(dat2, y != 1)
  #exclude rows where x is 0
  dat2 <- subset(dat2, x != 0)
  #dominant frequency is the frequency of max amplitude, which is x for the max of y in chirp_dat2
  dom_freq <- dat2[dat2$y == max(dat2$y), 1]
  prop12 <- as.data.frame(specprop(dat,f=lwf[[i]]@samp.rate))
  prop12 <- prop12[, keep] 
  prop12$dom_freq <- dom_freq*1000
  #Acoustic complexity index
  #prop12$ACI <- ACI(lwf[[i]], channel = 1, wl = 512, ovlp = 0,  wn = "hamming", flim = NULL, nbwindows = 1)
  #Entropy
  prop12$entropy <- H(lwf[[i]], lwf[[i]]@samp.rate, channel = 1, wl = 512, envt="hil", msmooth = NULL, ksmooth = NULL)
  prop12$duration <- seewave::duration(lwf[[i]], lwf[[i]]@samp.rate, channel=1)
  
  # Calculate SNR
  if (lwf[[i]]@stereo) {
    signal <- lwf[[i]]@right
  } else {
    signal <- lwf[[i]]@left
  }
  prop12$snr <- calculate_snr(signal)
  
  
  prop12$name <- "roar"
  prop12$file <- all_names[i]
  prop12$sum_calls <- length(all_names)
  
  prop <- rbind(prop, prop12)
  
}

#snort

dir <- "C:/Users/egrout/Dropbox/calls/each call type/snort/"
filenames <- list.files(dir, pattern=".wav", full.names=TRUE)
all_names = basename(filenames)
lwf <- lapply(filenames, readWave)

for (i in 1:length(lwf)) {
  #spectro(lwf[[i]],f=lwf[[i]]@samp.rate,ovlp=85,zp=16, palette=jet.colors, osc=TRUE, collevels=seq(-29,0), grid = F)
  f = lwf[[i]]@samp.rate
  dat <- meanspec(lwf[[i]],f=lwf[[i]]@samp.rate , plot=F)
  dat2 <- as.data.frame(dat)
  #excluding rows where y is 1.0000
  dat2 <- subset(dat2, y != 1)
  #exclude rows where x is 0
  dat2 <- subset(dat2, x != 0)
  #dominant frequency is the frequency of max amplitude, which is x for the max of y in chirp_dat2
  dom_freq <- dat2[dat2$y == max(dat2$y), 1]
  prop13 <- as.data.frame(specprop(dat,f=lwf[[i]]@samp.rate))
  prop13 <- prop13[, keep] 
  prop13$dom_freq <- dom_freq*1000
  #Acoustic complexity index
  #prop13$ACI <- ACI(lwf[[i]], channel = 1, wl = 512, ovlp = 0,  wn = "hamming", flim = NULL, nbwindows = 1)
  #Entropy
  prop13$entropy <- H(lwf[[i]], lwf[[i]]@samp.rate, channel = 1, wl = 512, envt="hil", msmooth = NULL, ksmooth = NULL)
  prop13$duration <- seewave::duration(lwf[[i]], lwf[[i]]@samp.rate, channel=1)
  
  if ("WaveMC" %in% class(lwf[[i]])) {
    # For multi-channel WaveMC files, use both channels for SNR calculation
    signal <- lwf[[i]]
    
  } else if (lwf[[i]]@stereo) {
    signal <- lwf[[i]]@right
  } else {
    signal <- lwf[[i]]@left
  }
  prop13$snr <- calculate_snr(signal)
  
  
  prop13$name <- "snort"
  prop13$file <- all_names[i]
  prop13$sum_calls <- length(all_names)
  
  prop <- rbind(prop, prop13)
  
}

#squawk
# 
# dir <- "C:/Users/egrout/Dropbox/calls/each call type/squawk/"
# filenames <- list.files(dir, pattern=".wav", full.names=TRUE)
# all_names = basename(filenames)
# lwf <- lapply(filenames, readWave)
# 
# for (i in 1:length(lwf)) {
#   #spectro(lwf[[i]],f=lwf[[i]]@samp.rate,ovlp=85,zp=16, palette=jet.colors, osc=TRUE, collevels=seq(-29,0), grid = F)
#   f = lwf[[i]]@samp.rate
#   dat <- meanspec(lwf[[i]],f=lwf[[i]]@samp.rate , plot=F)
#   dat2 <- as.data.frame(dat)
#   #excluding rows where y is 1.0000
#   dat2 <- subset(dat2, y != 1)
#   #exclude rows where x is 0
#   dat2 <- subset(dat2, x != 0)
#   #dominant frequency is the frequency of max amplitude, which is x for the max of y in chirp_dat2
#   dom_freq <- dat2[dat2$y == max(dat2$y), 1]
#   prop14 <- as.data.frame(specprop(dat,f=lwf[[i]]@samp.rate))
#   prop14 <- prop14[, keep] 
#   prop14$dom_freq <- dom_freq*1000
#   #Acoustic complexity index
#   #prop14$ACI <- ACI(lwf[[i]], channel = 1, wl = 512, ovlp = 0,  wn = "hamming", flim = NULL, nbwindows = 1)
#   #Entropy
#   prop14$entropy <- H(lwf[[i]], lwf[[i]]@samp.rate, channel = 1, wl = 512, envt="hil", msmooth = NULL, ksmooth = NULL)
#   prop14$duration <- seewave::duration(lwf[[i]], lwf[[i]]@samp.rate, channel=1)
#   
#   # Calculate SNR
#   if (lwf[[i]]@stereo) {
#     signal <- lwf[[i]]@right
#   } else {
#     signal <- lwf[[i]]@left
#   }
#   prop14$snr <- calculate_snr(signal)
#   
#   
#   prop14$name <- "squawk"
#   prop14$file <- all_names[i]
#   prop14$sum_calls <- length(all_names)
#   
#   prop <- rbind(prop, prop14)
#   
# }

#squeak

dir <- "C:/Users/egrout/Dropbox/calls/each call type/squeak/"
filenames <- list.files(dir, pattern=".wav", full.names=TRUE)
all_names = basename(filenames)
lwf <- lapply(filenames, readWave)

for (i in 1:length(lwf)) {
  #spectro(lwf[[i]],f=lwf[[i]]@samp.rate,ovlp=85,zp=16, palette=jet.colors, osc=TRUE, collevels=seq(-29,0), grid = F)
  f = lwf[[i]]@samp.rate
  dat <- meanspec(lwf[[i]],f=lwf[[i]]@samp.rate , plot=F)
  dat2 <- as.data.frame(dat)
  #excluding rows where y is 1.0000
  dat2 <- subset(dat2, y != 1)
  #exclude rows where x is 0
  dat2 <- subset(dat2, x != 0)
  #dominant frequency is the frequency of max amplitude, which is x for the max of y in chirp_dat2
  dom_freq <- dat2[dat2$y == max(dat2$y), 1]
  prop15 <- as.data.frame(specprop(dat,f=lwf[[i]]@samp.rate))
  prop15 <- prop15[, keep] 
  prop15$dom_freq <- dom_freq*1000
  #Acoustic complexity index
  #prop15$ACI <- ACI(lwf[[i]], channel = 1, wl = 512, ovlp = 0,  wn = "hamming", flim = NULL, nbwindows = 1)
  #Entropy
  prop15$entropy <- H(lwf[[i]], lwf[[i]]@samp.rate, channel = 1, wl = 512, envt="hil", msmooth = NULL, ksmooth = NULL)
  prop15$duration <- seewave::duration(lwf[[i]], lwf[[i]]@samp.rate, channel=1)
  
  # Calculate SNR
  if (lwf[[i]]@stereo) {
    signal <- lwf[[i]]@right
  } else {
    signal <- lwf[[i]]@left
  }
  prop15$snr <- calculate_snr(signal)
  
  
  prop15$name <- "squeak"
  prop15$file <- all_names[i]
  prop15$sum_calls <- length(all_names)
  
  prop <- rbind(prop, prop15)
  
}


#cluck

# dir <- "C:/Users/egrout/Dropbox/calls/each call type/cluck/"
# filenames <- list.files(dir, pattern=".wav", full.names=TRUE)
# all_names = basename(filenames)
# lwf <- lapply(filenames, readWave)
# 
# for (i in 1:length(lwf)) {
#   #spectro(lwf[[i]],f=lwf[[i]]@samp.rate,ovlp=85,zp=16, palette=jet.colors, osc=TRUE, collevels=seq(-29,0), grid = F)
#   f = lwf[[i]]@samp.rate
#   dat <- meanspec(lwf[[i]],f=lwf[[i]]@samp.rate , plot=F)
#   dat2 <- as.data.frame(dat)
#   #excluding rows where y is 1.0000
#   dat2 <- subset(dat2, y != 1)
#   #exclude rows where x is 0
#   dat2 <- subset(dat2, x != 0)
#   #dominant frequency is the frequency of max amplitude, which is x for the max of y in chirp_dat2
#   dom_freq <- dat2[dat2$y == max(dat2$y), 1]
#   prop16 <- as.data.frame(specprop(dat,f=lwf[[i]]@samp.rate))
#   prop16 <- prop16[, keep] 
#   prop16$dom_freq <- dom_freq*1000
#   #Acoustic complexity index
#   #prop15$ACI <- ACI(lwf[[i]], channel = 1, wl = 512, ovlp = 0,  wn = "hamming", flim = NULL, nbwindows = 1)
#   #Entropy
#   prop16$entropy <- H(lwf[[i]], lwf[[i]]@samp.rate, channel = 1, wl = 512, envt="hil", msmooth = NULL, ksmooth = NULL)
#   prop16$duration <- seewave::duration(lwf[[i]], lwf[[i]]@samp.rate, channel=1)
#   
#   # Calculate SNR
#   if (lwf[[i]]@stereo) {
#     signal <- lwf[[i]]@right
#   } else {
#     signal <- lwf[[i]]@left
#   }
#   prop16$snr <- calculate_snr(signal)
#   
#   
#   prop16$name <- "cluck"
#   prop16$file <- all_names[i]
#   prop16$sum_calls <- length(all_names)
#   
#   prop <- rbind(prop, prop16)
#   
# }


#bop

dir <- "C:/Users/egrout/Dropbox/calls/each call type/bop/"
filenames <- list.files(dir, pattern=".wav", full.names=TRUE)
all_names = basename(filenames)
lwf <- lapply(filenames, readWave)

for (i in 1:length(lwf)) {
  #spectro(lwf[[i]],f=lwf[[i]]@samp.rate,ovlp=85,zp=16, palette=jet.colors, osc=TRUE, collevels=seq(-29,0), grid = F)
  f = lwf[[i]]@samp.rate
  dat <- meanspec(lwf[[i]],f=lwf[[i]]@samp.rate , plot=F)
  dat2 <- as.data.frame(dat)
  #excluding rows where y is 1.0000
  dat2 <- subset(dat2, y != 1)
  #exclude rows where x is 0
  dat2 <- subset(dat2, x != 0)
  #dominant frequency is the frequency of max amplitude, which is x for the max of y in chirp_dat2
  dom_freq <- dat2[dat2$y == max(dat2$y), 1]
  prop17 <- as.data.frame(specprop(dat,f=lwf[[i]]@samp.rate))
  prop17 <- prop17[, keep] 
  prop17$dom_freq <- dom_freq*1000
  #Acoustic complexity index
  #prop15$ACI <- ACI(lwf[[i]], channel = 1, wl = 512, ovlp = 0,  wn = "hamming", flim = NULL, nbwindows = 1)
  #Entropy
  prop17$entropy <- H(lwf[[i]], lwf[[i]]@samp.rate, channel = 1, wl = 512, envt="hil", msmooth = NULL, ksmooth = NULL)
  prop17$duration <- seewave::duration(lwf[[i]], lwf[[i]]@samp.rate, channel=1)
  
  # Calculate SNR
  if (lwf[[i]]@stereo) {
    signal <- lwf[[i]]@right
  } else {
    signal <- lwf[[i]]@left
  }
  prop17$snr <- calculate_snr(signal)
  
  
  prop17$name <- "bop"
  prop17$file <- all_names[i]
  prop17$sum_calls <- length(all_names)
  
  prop <- rbind(prop, prop17)
  
}







#this works, now need to think about what other features should be extracted

#visualising spectral properties between call types
boxplot(dom_freq ~ name, prop)   
boxplot(sfm ~ name, prop)   
boxplot(kurtosis ~ name, prop)   


#--------------------------------------------------------------------------
#make a dataframe with the standard deviation of each feature for each call type to add to paper


#make empty dataframe with same number of columns in prop and same number of rows as call type
stand_dev <- data.frame(matrix(ncol = ((length(keep) + 7)), nrow = length(unique(prop$name))))
colnames(stand_dev) <- colnames(prop)
#remove column with filename
stand_dev <- stand_dev[ , !names(stand_dev) %in% c("file")] 

calls <- unique(prop$name)
stand_dev$name <- calls

#for loop to extract mean and sd for each call feature for each call type and puts info in stand_dev dataframe
for (i in 1:nrow(stand_dev)){
  
  call_name <- stand_dev$name[i]
  call_type <- prop[prop$name == call_name,]
  #removing filename here, otherwise adds extra column in second forloop
  call_type <- call_type[ , !names(call_type) %in% c("file", "sum_calls")] 
  sum_calls <- length(call_type[,1])
  
  for (j in 1:ncol(call_type)){
    
    call_feature <- call_type[,j]
    mean_call_feature <- round(mean(call_feature), digits = 3)
    stand_dev_feature <- round(sd(call_feature), digits = 3)
    
    #stand_dev_feature <- paste("?", stand_dev_feature, sep = " ")
    #because the ? isn't recognised in excel, need to change to different digit for now
    stand_dev_feature <- paste(":", stand_dev_feature, sep = " ")
    
    
    #joining the mean and sd to the same cell
    mean_sd <- paste(mean_call_feature, stand_dev_feature, sep = " ")
    #saving mean and standard deviation back into stand_dev dataframe
    stand_dev[i,j] <- mean_sd
    
  }
 #adding name back in, otherwise gets overrun in second forloop
  stand_dev$name[i] <- call_name
 #adding sum of calls for each call type
  stand_dev$sum_calls[i] <- sum_calls
  
}


#remove columns not wanted in paper and reordering call types
stand_dev2 <- stand_dev[,c("name","sum_calls","duration", "dom_freq", "sh", "sfm", "Q25", "Q75", "IQR")]

#manually order the table by name
stand_dev2 <- stand_dev2 %>% arrange(factor(name, levels = c("chirp", "click", "grunt", "chitter", "squeal", "growl", "bark", "dc", "hum", "bop", "cluck", "vibrate" )))

#colnames(filt2)[colnames(filt2) == "mode"] <- "Dominant Frequency (Hz)"

write.table(stand_dev2,  file = "C:/Users/egrout/Dropbox/calls/results/call_descriptions3.csv", quote = FALSE, sep ="\t" ,row.names = TRUE, col.names = TRUE)
write.table(stand_dev,  file = "C:/Users/egrout/Dropbox/calls/results/call_descriptions2.csv", quote = FALSE, sep ="\t" ,row.names = TRUE, col.names = TRUE)

#make a box and whisker plot for the duration of calls for each call type

dur_df <- prop[,c("duration", "name")]
png(height = 900, width = 1200, units = 'px', filename = "C:/Users/egrout/Dropbox/calls/results/durations.png")
par(mar = c(10,15,5,5), mgp=c(6,2.5,0)) #c(bottom, left, top, right)
boxplot(dur_df$duration ~dur_df$name, ylab = "", xlab = "Call Duration (s)",col = plot.colors, method = "jitter", vertical = F, pch = 1, las = 1, horizontal = TRUE, cex.axis = 3, cex.lab = 3)
dev.off()

#make a box and whisker plot for dominant frequency for each call type
dur_df <- prop[,c("dom_freq", "name")]
png(height = 900, width = 1200, units = 'px', filename = "C:/Users/egrout/Dropbox/calls/results/dom_freq.png")
par(mar = c(10,15,5,5), mgp=c(6,2.5,0)) #c(bottom, left, top, right)
boxplot(dur_df$dom_freq ~dur_df$name, ylab = "", xlab = "Dominant Frequency (Hz)",col = plot.colors, method = "jitter", vertical = F, pch = 1, las = 1, horizontal = TRUE, cex.axis = 3, cex.lab = 3)
dev.off()


# Define the filename for saving the plot
filename <- "C:/Users/egrout/Dropbox/calls/results/durations_and_dom_freq.png"

# Set up the plot
png(filename, height = 900, width = 1200, units = 'px')
par(mar = c(12, 8, 2, 10), mgp = c(6, 1, 0))  # c(bottom, left, top, right))

# Plot the data
dur_df <- prop[, c("duration", "dom_freq", "name")]
#change dc to dolphin-call
dur_df$name[dur_df$name == "dc"] <- "dolphin call"

#save df for Alie to look at
write.csv(dur_df, "C:/Users/egrout/Dropbox/calls/results/duration_domfreq_allcalls.csv", row.names=FALSE)

# Calculate mean duration for each call type
mean_durations <- tapply(dur_df$duration, dur_df$name, mean)

# Order call types by mean duration and handle ties by adding a small unique value
ordered_names <- names(sort(mean_durations, decreasing = TRUE))

# Create a factor with the ordered levels
dur_df$name <- factor(dur_df$name, levels = ordered_names)


# Scale the duration values for better visualization
scale <- 50000
scaled_duration <- dur_df$duration * scale  # Adjust the scaling factor as needed

# Create the plot with the scaled duration values
boxplot(scaled_duration ~ dur_df$name, ylab = " ", xlab = "", col = "plum", border = "black", method = "jitter", pch = 1, las = 2, horizontal = FALSE, boxwex = 0.3, cex.axis = 2, cex.lab = 2, yaxt = "n")
mtext("Duration (s)", side = 2, line = 5, cex = 2)
mtext("Call Type", side = 1, line = 10.5, cex = 2)

# Calculate positions for the second boxplot
num_categories <- length(unique(dur_df$name))
width <- 0.4  # Adjust as needed
positions <- seq(1, by = 1, length.out = num_categories) + width

# Overlay the boxplots for dominant frequency
boxplot(dur_df$dom_freq ~ dur_df$name, add = TRUE, col = "paleturquoise", border = "black", method = "jitter", boxwex = 0.3, pch = 1, horizontal = FALSE, axes = FALSE, at = positions)

# Add secondary y-axis for dominant frequency
axis(side = 4, las = 2, cex.axis = 2) #need to fix this axis

# Add legend
legend("topright", legend = c("Duration", "Dominant Frequency"), fill = c("plum", "paleturquoise"),  border = NA, bty = "n", cex = 2)
mtext("Frequency (Hz)", side = 4, line = 8, cex = 2)

# Change y-axis labels
axis(side = 2, at = c(0, 5000, 10000, 15000, 20000, 25000, 30000), labels = c(0, (5000/scale), (10000/scale), (15000/scale), (20000/scale), (25000/scale), (30000/scale)), las = 2, cex.axis = 2)


# Turn off the device
dev.off()



#can see that the lower frequency calls have longer durations!

dur_dom <- prop[,c("duration", "name", "dom_freq")]

plot(dur_dom$duration, dur_dom$dom_freq)




