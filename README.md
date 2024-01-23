# audio.vadwebrtc

This repository contains an R package which is an Rcpp wrapper around the [webrtc](https://webrtc.googlesource.com/src/) Voice Activity Detection module.

https://github.com/bnosac/audio.vadwebrtc/assets/1710810/6086999a-6348-460a-8fd6-2ab9fd2a5d17

The package was created with as main goal to remove non-speech audio segments before doing an automatic transcription using [audio.whisper](https://github.com/bnosac/audio.whisper) to avoid transcription hallucinations. It contains

- functions to detect the location of voice in audio using a Gaussian Mixture Model implemented in [webrtc](https://webrtc.googlesource.com/src/)
- functions to extract audio where there is voice / silence in a new audio file
- functionality to rewrite the timepoints of transcribed sentences where specific sections with non-audio are removed to make sure the timepoints of the transcriptions without silences align with the original audio signal

### Installation

- The package is currently not on CRAN
- For the *development* version of this package: `remotes::install_github("bnosac/audio.vadwebrtc")`

Look to the documentation of the functions: `help(package = "audio.vadwebrtc")`

## Example

Get a audio file in 16 bit with mono PCM samples (pcm_s16le codec) with a sampling rate of either 8Khz, 16KHz or 32Khz 

```{r}
library(audio.vadwebrtc)
file <- system.file(package = "audio.vadwebrtc", "extdata", "test_wav.wav")
vad  <- VAD(file, mode = "normal")
vad
Voice Activity Detection 
  - file: D:/Jan/R/win-library/4.1/audio.vadwebrtc/extdata/test_wav.wav 
  - sample rate: 16000 
  - VAD type: webrtc-gmm, VAD mode: normal, VAD by milliseconds: 10, VAD frame_length: 160
    - Percent of audio containing a voiced signal: 90.2% 
    - Seconds voiced: 6.3 
    - Seconds unvoiced: 0.7
vad$vad_segments
 vad_segment start  end has_voice
           1  0.00 0.08     FALSE
           2  0.09 3.30      TRUE
           3  3.31 3.71     FALSE
           4  3.72 6.78      TRUE
           5  6.79 6.99     FALSE
```

Example of a simple plot of these audio and voice segments

```{r}
library(av)
x <- read_audio_bin(file)
plot(seq_along(x) / 16000, x, type = "l", xlab = "Seconds", ylab = "Signal")
abline(v = vad$vad_segments$start, col = "red", lwd = 2)
abline(v = vad$vad_segments$end, col = "blue", lwd = 2)
```

![](tools/example-detection.png)

Or show it interactively alongside R package wavesurfer: [wavesurfer](https://github.com/Athospd/wavesurfer)

```{r}
library(wavesurfer)
library(shiny)
file <- system.file(package = "audio.vadwebrtc", "extdata", "test_wav.wav")
vad  <- VAD(file, mode = "lowbitrate")
anno <- data.frame(audio_id = vad$file, 
                   region_id = vad$vad_segments$vad_segment, 
                   start = vad$vad_segments$start, 
                   end = vad$vad_segments$end, 
                   label = ifelse(vad$vad_segments$has_voice, "Voiced", "Silent"))
anno <- subset(anno, label %in% "Silent")
  
wavs_folder <- system.file(package = "audio.vadwebrtc", "extdata")
shiny::addResourcePath("wav", wavs_folder)
ui <- fluidPage(
  wavesurferOutput("my_ws", height = "128px"),
  tags$p("Press spacebar to toggle play/pause."),
)
server <- function(input, output, session) {
  output$my_ws <- renderWavesurfer({
    wavesurfer(audio = paste0("wav/", "test_wav.wav"), annotations = anno) %>%
      ws_set_wave_color('#5511aa') %>%
      ws_cursor()
  })
}
shinyApp(ui = ui, server = server)
```

## Support in text mining

Need support in text mining?
Contact BNOSAC: http://www.bnosac.be


