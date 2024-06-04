#this code is for making all plot the same for the paper (by reading in all wave files into a list then running the spectro function over each call type in a forloop)

library(seewave)
library(tuneR)
library(ggplot2)
library(DT)
library(plyr)

plot_dir <- "C:/Users/egrout/Dropbox/calls/results/spectrograms/stretched/"

jet.colors <- colorRampPalette(c("white","white", "plum1", "deeppink4", "blue", "black"))
jet.colors <- colorRampPalette(c("white","plum1", "deeppink4", "black"))


Growl <- readWave('D:/PhD/All things coati/Edic mini calls/each call type/all/growl cut.wav')
Squeal <- readWave('D:/PhD/All things coati/Edic mini calls/each call type/all/squeal_down1.wav')
Squeal_grunt <- readWave('D:/PhD/All things coati/Edic mini calls/each call type/all/squeal grunt.wav')
Chittering1 <- readWave('D:/PhD/All things coati/Edic mini calls/each call type/all/chittering.wav')
Chittering <- readWave('D:/PhD/All things coati/Edic mini calls/each call type/all/chittering_ramseycanyon.wav')
Chitter <- cutw(Chittering, f=48000,from=0.1, to = 0.28 ,plot=F, output = "Wave")
Long_chitter <- readWave('D:/PhD/All things coati/Edic mini calls/each call type/all/long chitter.wav')
Squawk <- readWave('D:/PhD/All things coati/Edic mini calls/each call type/all/squawk5_ASDM_121217.wav')

#CONTACT
Chirp_grunt <- readWave('D:/PhD/All things coati/Edic mini calls/each call type/all/chirpgrunt_cut.wav')
Chirp_click_grunt <- readWave('D:/PhD/All things coati/Edic mini calls/each call type/all/chirpclickgrunt2.wav')
Chirp_click_grunt_snort <- readWave('D:/PhD/All things coati/Edic mini calls/each call type/all/chirpgruntsnort.wav')#from file 3 6887 edic mini
Grunt <- readWave('D:/PhD/All things coati/Edic mini calls/each call type/all/grunt.wav') #from 9480_1_FL8
Clicks <- readWave('D:/PhD/All things coati/Edic mini calls/each call type/all/clicks.wav') ##from Chris Hass Ramsey Canyon Coatis video
Click <- readWave('D:/PhD/All things coati/Edic mini calls/each call type/all/click2.wav') #from 9467_2_FL3
#Click <- cutw(Click,f=24000,from=0.02, to = 0.14 ,plot=T, output = "Wave")
Chirp2 <-readWave('D:/PhD/All things coati/Edic mini calls/each call type/all/chirp2.wav') #from 9480_1_FL3

Squeak1 <- readWave('D:/PhD/All things coati/Edic mini calls/each call type/all/squeak1_ramseycanyon.wav')
Squeak2 <- readWave('D:/PhD/All things coati/Edic mini calls/each call type/all/squeak2_ramseycanyon.wav')
Squeak3 <- readWave('D:/PhD/All things coati/Edic mini calls/each call type/all/squeak3_ramseycanyon.wav')
Squeak4 <- readWave('D:/PhD/All things coati/Edic mini calls/each call type/all/squeak4_ramseycanyon.wav')
Squeak5 <- readWave('D:/PhD/All things coati/Edic mini calls/each call type/all/squeak5_ramseycanyon.wav')
Squeak <- readWave('D:/PhD/All things coati/Edic mini calls/each call type/all/squeak6_ramseycanyon.wav')

#AGGRESSIVE CALL SEQUENCES
agg_seq4 <- readWave('D:/PhD/All things coati/Edic mini calls/each call type/all/aggsequence4.wav')
agg_seq5 <- readWave('D:/PhD/All things coati/Edic mini calls/each call type/all/aggsequence5.wav')
Squeal_chittering <- readWave('D:/PhD/All things coati/Edic mini calls/each call type/all/aggsequence6.wav')

#ALARM
Chop_chop <- readWave('D:/PhD/All things coati/Edic mini calls/each call type/all/chop chop.wav') 
Chop <- cutw(Chop_chop,f=22050,from=0.1, to = 0.4 ,plot=F, output = "Wave")
Barking1 <- readWave('D:/PhD/All things coati/Edic mini calls/each call type/all/barking.wav') #from Chris Hass Ramsey Canyon Coatis video
Barking <- readWave('D:/PhD/All things coati/Edic mini calls/each call type/all/barking5.wav') #from Chris Hass Ramsey Canyon Coatis video
Barking2 <- readWave('D:/PhD/All things coati/Edic mini calls/each call type/all/barks_ramseycanyon.wav') #from Chris Hass Ramsey Canyon Coatis video
Barking <- readWave('D:/PhD/All things coati/Edic mini calls/each call type/all/grunting_9467_2_FL7.wav') #from 9467_2_FL6

Chirp_bark <- readWave('D:/PhD/All things coati/Edic mini calls/each call type/all/barkcomplex.wav') #from Chris Hass Ramsey Canyon Coatis video
Bark <- readWave('D:/PhD/All things coati/Edic mini calls/each call type/all/bark_ramseycanyon.wav') #from Chris Hass Ramsey Canyon Coatis video
Roar <- readWave('D:/PhD/All things coati/Edic mini calls/each call type/all/roar20_LuceroTrap.wav')

#LOST CALL
Lost_call <- readWave('D:/PhD/All things coati/Edic mini calls/each call type/all/lostcall_benvid1383.wav')


#MATING
Dolphin_call <- readWave('D:/PhD/All things coati/Edic mini calls/each call type/all/dolphin call2_amplified.wav') #call comes from Galaxy 9463_2_FL1
Dolphin_call1 <- cutw(Dolphin_call,f=24000,from=0.65, to = 0.8 ,plot=F, output = "Wave")

Dolphin_call <- readWave('D:/PhD/All things coati/Edic mini calls/each call type/all/dolphin call2.wav') #call comes from Galaxy 9463_2_FL1

#SHORT CALLS - UNK FUNCTION
Bop <- readWave('D:/PhD/All things coati/Edic mini calls/each call type/all/bop.wav') #call comes from 9471_1_FL4 
Vibrate <-  readWave('D:/PhD/All things coati/Edic mini calls/each call type/all/vibrate2_6887_6.wav')

#MECHANICAL SOUNDS - UNK FUNCTION
Snorting <- readWave('D:/PhD/All things coati/Edic mini calls/each call type/all/snorts.wav') ##from file 3 6887 edic mini
Snort <- readWave('D:/PhD/All things coati/Edic mini calls/each call type/all/snort_Lucerotrap.wav')


#SLEEP SOUNDS
Snore_hum <- readWave('D:/PhD/All things coati/Edic mini calls/each call type/all/snore.wav') #from 9480_1_FL8
Hum <- readWave('D:/PhD/All things coati/Edic mini calls/each call type/all/sleephum.wav') #from 9480_1_FL8

#BABY COATI SOUNDS
Chitters <- readWave('D:/PhD/All things coati/Edic mini calls/each call type/all/babycoati.wav') #from Lydia's whatsapp recordings
Chitters1 <- readWave('D:/PhD/All things coati/Edic mini calls/each call type/all/babychitters.wav') #from Chris recordings
Mew <- readWave('D:/PhD/All things coati/Edic mini calls/each call type/all/babymews.wav') #from Chris recordings
Purr <- readWave('D:/PhD/All things coati/Edic mini calls/each call type/all/babypurr.wav') #from Chris recordings
Whine <- readWave('D:/PhD/All things coati/Edic mini calls/each call type/all/babywhines.wav') #from Chris recordings
Whistle <- readWave('D:/PhD/All things coati/Edic mini calls/each call type/all/babywhistle.wav') #from Chris recordings

#more calls to make spectrograms
Cackle <- readWave('D:/PhD/All things coati/Edic mini calls/each call type/all/cackle2.wav')
Cut_cackle <- cutw(Cackle,f=96000,from=0.1, to = 0.545 ,plot=F, output = "Wave")
Chuckle1 <- readWave('D:/PhD/All things coati/Edic mini calls/each call type/all/chuckles.wav')
Chuckle2 <- readWave('D:/PhD/All things coati/Edic mini calls/each call type/all/chuckle2.wav')
Chuckling <- readWave('D:/PhD/All things coati/Edic mini calls/each call type/all/chuckle3.wav')
Cut_Chuckle <- cutw(Chuckle2,f=96000,from=0.1, to = 0.6 ,plot=F)
Excite <- readWave('D:/PhD/All things coati/Edic mini calls/each call type/all/excitementchirps.wav')
Chirp_squeal <- cutw(Excite,f=96000,from=0.1, to = 0.3 ,plot=F,  output = "Wave")
Chirp <- readWave('D:/PhD/All things coati/Edic mini calls/each call type/all/chirpultra3_clipped.wav')
Chitter_Ultra <- readWave('D:/PhD/All things coati/Edic mini calls/each call type/all/chitter_ultra.wav')
#Chitter <- cutw(Chitter_Ultra, f=250000,from=0.15, to = 0.25 ,plot=T, output = "Wave")
 
Chittering <- readWave('D:/PhD/All things coati/Edic mini calls/each call type/all/ramsey_chitters2.wav')

Cluck <- readWave('D:/PhD/All things coati/Edic mini calls/each call type/all/cluck2_FL.wav')
Clucking <- readWave('D:/PhD/All things coati/Edic mini calls/each call type/all/clucking.wav') #from FL0912

Giggling1 <- readWave('D:/PhD/All things coati/Edic mini calls/each call type/all/giggle_ASDM1305.wav')
Giggling <- readWave('D:/PhD/All things coati/Edic mini calls/each call type/all/giggle2.wav')


Chirp_click <- readWave('D:/PhD/All things coati/Edic mini calls/each call type/all/chirp_click.wav')

Chirp_click2 <- readWave('D:/PhD/All things coati/Edic mini calls/each call type/all/chirp_click2.wav')

#only have wave files wanted in the list in the global environemt
waves <- Filter(function(x) is(x, "Wave"), mget(ls()))
#waves <- within(waves, rm("Excite", "Chuckle"))

dur <- data.frame()

for (i in 1:length(waves)){

  call = waves[[i]]
  f = call@samp.rate
  dur = rbind(dur, seewave::duration(call))
  name = names(waves[i])
  
  name <- gsub("_", "-", name)
  #if wanting all call names to be in lowercase
  #tolower(name)
  
  
  
png(height = 1000, width = 3200, units = 'px', filename = paste0(plot_dir, name, ".png"))

#if don't want amplitude scale, put scale = F
par(mgp=c(3,2.5,0))
spectro(call, f=f, scale = F, ovlp=85, zp=16, flog = F, palette=jet.colors, osc=T, collevels=seq(-50,0), grid = F, cexlab = 4, cexaxis = 4, scalecexlab = 3, width = c(9,1), oma = rep(6,4), 
        tlab = " ",
        flab = " ",
        alab = " ")

#adding axis labels because default settings cut them off when I want to zoom the spectrogram to be more readable
mtext(name, side = 1, line = -43, cex = 4)
#if running the plots for the no_scale, need to change line from -55 to -42
#mtext("Time (s)", side = 3, line = -58, cex = 4, las = 1) #if making the plot for the no_scale, need to change line = -74 to -58

mtext(" ", side = 2, line = 6, cex = 4, las = 3) #Frequency (kHz)
mtext(" ", side = 2, line = 6, cex = 3, las = 3, at = -1.5) #-1.5 when flog = F and -0.4 when flog = T #Amplitude

dev.off()

}

