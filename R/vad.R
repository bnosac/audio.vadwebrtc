#' @title Voice Activity Detection
#' @description Detect the location of active voice in audio. 
#' The Voice Activity Detection is implemented using a Gaussian Mixture Model from the "webrtc" framework. 
#' It works with .wav audio files with a sample rate of 8, 16 or 32 Khz an can be applied over a window of eiher 10, 20 or 30 milliseconds.
#' @param file the path to an audio file which should be a file in 16 bit with mono PCM samples (pcm_s16le codec) with a sampling rate of either 8Khz, 16KHz or 32Khz
#' @param mode character string with the type of voice detection, either 'normal', 'lowbitrate', 'aggressive' or 'veryaggressive' where 'veryaggressive' means more silences are detected
#' @param milliseconds integer with the number of milliseconds indicating to compute by this number of milliseconds the VAD signal. Can only be 10, 20 or 30. Defaults to 10.
#' @param type character string with the type of VAD model. Only 'webrtc' currently.
#' @return an object of class \code{VAD} which is a list with elements
#' \itemize{
#' \item{file: the path to the file}
#' \item{sample_rate: the sample rate of the audio file in Hz}
#' \item{channels: the number of channels in the audio - as the algorithm requires the audio to be mono this should only be 1}
#' \item{samples: the number of samples in the data}
#' \item{bitsPerSample: the number of bits per sample}
#' \item{bytesPerSample: the number of bytes per sample}
#' \item{type: the type of VAD model - currently only 'webrtc-gmm'}
#' \item{mode: the provided VAD mode}
#' \item{milliseconds: the provided milliseconds - either by 10, 20 or 30 ms frames}
#' \item{frame_length: the frame length corresponding to the provided milliseconds}
#' \item{vad: a data.frame with columns millisecond, has_voice and vad_segment indicating if the audio contains an active voice signal at that millisecond}
#' \item{vad_segments: a data.frame with columns vad_segment, start, end and has_voice where the start/end values are in seconds}
#' \item{vad_stats: a list with elements n_segments, n_segments_has_voice, n_segments_has_no_voice, seconds_has_voice, seconds_has_no_voice, pct_has_voice indicating the number of segments with voice and the duration of the voice/non-voice in the audio}
#' }
#' @export
#' @examples 
#' file <- system.file(package = "audio.vadwebrtc", "extdata", "test_wav.wav")
#' vad  <- VAD(file, mode = "normal", milliseconds = 30)
#' vad
#' vad  <- VAD(file, mode = "lowbitrate", milliseconds = 20)
#' vad
#' vad  <- VAD(file, mode = "aggressive", milliseconds = 20)
#' vad
#' vad  <- VAD(file, mode = "veryaggressive", milliseconds = 20)
#' vad
#' vad  <- VAD(file, mode = "normal", milliseconds = 10)
#' vad
#' vad$vad_segments
#' 
#' \dontrun{
#' library(av)
#' x <- read_audio_bin(file)
#' plot(seq_along(x) / 16000, x, type = "l")
#' abline(v = vad$vad_segments$start, col = "red", lwd = 2)
#' abline(v = vad$vad_segments$end, col = "blue", lwd = 2)
#' 
#' ##
#' ## If you have audio which is not in mono or another sample rate
#' ## consider using R package av to convert to the desired format
#' av_media_info(file)
#' av_audio_convert(file, output = "audio_pcm_16khz.wav", 
#'                  format = "wav", channels = 1, sample_rate = 16000)
#' vad <- VAD("audio_pcm_16khz.wav", mode = "normal")
#' }
#' 
#' file <- system.file(package = "audio.vadwebrtc", "extdata", "leak-test.wav")
#' vad  <- VAD(file, mode = "normal")
#' vad
#' vad$vad_segments
#' vad$vad_stats
VAD <- function(file, mode = c("normal", "lowbitrate", "aggressive", "veryaggressive"), milliseconds = 10L, type = "webrtc"){
    type <- match.arg(type)
    mode <- match.arg(mode)
    stopifnot(file.exists(file))
    milliseconds <- as.integer(milliseconds)
    stopifnot(milliseconds %in% c(10, 20, 30))
    msg <- vad_webrtc_detection(file, 
                                mode = switch(mode,
                                              normal = 0,
                                              lowbitrate = 1,
                                              aggressive = 2,
                                              veryaggressive = 3),
                                milliseconds = milliseconds)
    ## Get groups of sequences of voice/non-voice
    grp <- rle(msg$vad$has_voice)
    msg$type <- "webrtc-gmm"
    msg$mode <- mode
    msg$vad$vad_segment <- rep(seq_along(grp$lengths), grp$lengths)
    segments <- tapply(X = msg$vad$millisecond, INDEX = msg$vad$vad_segment, FUN = range, simplify = F)   
    segments <- data.frame(vad_segment = as.integer(names(segments)),
                           start = vapply(segments, FUN = function(x) x[1], FUN.VALUE = integer(1), USE.NAMES = FALSE) / 1000,
                           end = vapply(segments, FUN = function(x) x[2], FUN.VALUE = integer(1), USE.NAMES = FALSE) / 1000,
                           has_voice = grp$values)
    msg$vad_segments <- segments
    ## Calculate the percentage of voiced signal
    vad_segments_info <- list(
        n_segments = nrow(segments), 
        n_segments_has_voice = sum(segments$has_voice, na.rm = TRUE), 
        n_segments_has_no_voice = sum(!segments$has_voice, na.rm = TRUE),
        seconds_has_voice = sum((segments$end - segments$start)[segments$has_voice], na.rm = TRUE),
        seconds_has_no_voice = sum((segments$end - segments$start)[!segments$has_voice], na.rm = TRUE))
    vad_segments_info$pct_has_voice = vad_segments_info$seconds_has_voice / (vad_segments_info$seconds_has_voice + vad_segments_info$seconds_has_no_voice)
    msg$vad_stats <- vad_segments_info
    class(msg) <- c("VAD", "webrtc-gmm")
    msg
}

#' @export
print.VAD <- function(x, ...){
    cat("Voice Activity Detection", "\n")
    cat("  - file:", x$file, "\n")
    cat("  - sample rate:", x$sample_rate, "\n")
    cat("  - VAD type: ", x$type, ", VAD mode: ", x$mode, ", VAD by milliseconds: ", x$milliseconds, ", VAD frame_length: ", x$frame_length, "\n", sep = "")
    if("channel" %in% names(x$vad_segments)){
        x$vad_segments <- x$vad_segments[x$vad_segments$channel %in% 0, ]
        x$vad_stats <- x$vad_stats[x$vad_stats$channel %in% 0, ]
    }
    cat("    - Percent of audio containing a voiced signal:", paste(round(100*x$vad_stats$pct_has_voice, digits = 1), "%", sep = ""), "\n")
    cat("    - Seconds voiced:", round(x$vad_stats$seconds_has_voice, digits = 1), "\n")
    cat("    - Seconds unvoiced:", round(x$vad_stats$seconds_has_no_voice, digits = 1), "\n")
}





#' @title Get from a Voice Activity Detection (VAD object) the segments which are voiced
#' @description Postprocessing the Voice Activity Detection whereby sequences of 
#' voiced/non-voiced segments are collapsed by 
#' \enumerate{
#' \item{first considering all non-voiced segments which are small in duration (default < 1 second) voiced}
#' \item{next considering voiced segments with length less than a number of seconds (default < 1 second) non-voiced}
#' }
#' @param x an object of class VAD as returned by \code{\link{VAD}} or \code{\link{VAD_channel}}
#' @param channel integer with the channel, showing the voiced section of that channel only. Only used for segments extracted with \code{\link{VAD_channel}}
#' @param units character string with the units to use for the output and thresholds used in the function - either 'seconds' or 'milliseconds'  
#' @param ... further arguments passed on to the function
#' @return A data.frame with columns vad_segment, start, end, duration, has_voice indicating where in the audio voice is detected
#' @export
#' @examples 
#' file   <- system.file(package = "audio.vadwebrtc", "extdata", "test_wav.wav")
#' vad    <- VAD(file, mode = "normal", milliseconds = 30)
#' vad$vad_segments
#' voiced <- is.voiced(vad, silence_min = 0.2, voiced_min = 1)
#' voiced
#' voiced <- is.voiced(vad, silence_min = 200, units = "milliseconds")
#' voiced
is.voiced <- function(x, channel = 0, units = "seconds", ...){
    UseMethod("is.voiced")
}

#' @param channel integer with the channel, showing the voiced section of that channel only
#' @param silence_min minimum duration of a segment with only silence 
#' @param voiced_min minimum duration of a voiced segment 
#' @export
"is.voiced.webrtc-gmm" <- function(x, channel = 0, units = c("seconds", "milliseconds"), silence_min = ifelse(units == "milliseconds", 1000, 1), voiced_min = ifelse(units == "milliseconds", 1000, 1), ...){
    x <- x$vad_segment
    units <- match.arg(units)
    silence_min <- silence_min / 1000
    voiced_min  <- voiced_min / 1000
    ## Consider silences smaller than 1 second as voiced
    x$has_voice   <- ifelse(x$has_voice, x$has_voice, ifelse((x$end - x$start) < silence_min, TRUE, x$has_voice))
    grp           <- rle(x$has_voice)
    x$vad_segment <- rep(seq_along(grp$lengths), grp$lengths)
    x             <- segment_collapse(x)
    ## Consider voiced elements smaller than 1 second as silences
    x$has_voice   <- ifelse((x$end - x$start) < voiced_min & x$has_voice, FALSE, x$has_voice)
    grp           <- rle(x$has_voice)
    x$vad_segment <- rep(seq_along(grp$lengths), grp$lengths)
    x             <- segment_collapse(x)
    if(units == "seconds"){
    }else if(units == "milliseconds"){
        x$start <- x$start * 1000
        x$end   <- x$end * 1000
    }
    x$duration <- x$end - x$start
    x <- x[, c("vad_segment", "start", "end", "duration", "has_voice"), drop = FALSE]
    x
}

#' @param channel integer with the channel, showing the voiced section of that channel only
#' @param silence_min minimum duration of a segment with only silence 
#' @param voiced_min minimum duration of a voiced segment 
#' @export
"is.voiced.webrtc-gmm-bychannel" <- function(x, channel = 0, units = c("seconds", "milliseconds"), silence_min = ifelse(units == "milliseconds", 1000, 1), voiced_min = ifelse(units == "milliseconds", 1000, 1), ...){
    x$vad_segments <- x$vad_segments[x$vad_segments$channel %in% channel, ]
    "is.voiced.webrtc-gmm"(x, channel = channel, units = units, silence_min = silence_min, voiced_min = voiced_min)
}

#' @param channel integer with the channel, showing the voiced section of that channel only
#' @param silence_min minimum duration of a segment with only silence 
#' @param voiced_min minimum duration of a voiced segment 
#' @export
is.voiced.default <- function(x, channel = 0, units = c("seconds", "milliseconds"), silence_min = ifelse(units == "milliseconds", 1000, 1), voiced_min = ifelse(units == "milliseconds", 1000, 1), ...){
    "is.voiced.webrtc-gmm"(x, channel = channel, units = units, silence_min = silence_min, voiced_min = voiced_min, ...)
}


segment_collapse <- function(x){
    x <- do.call(rbind, lapply(split(x, list(x$vad_segment, x$has_voice), drop = TRUE), FUN = function(x){
        data.frame(vad_segment = head(x$vad_segment, n = 1), 
                   start       = min(x$start), 
                   end         = max(x$end), 
                   has_voice   = head(x$has_voice, n = 1), 
                   stringsAsFactors = FALSE)
    }))
    x <- x[order(x$vad_segment, decreasing = FALSE), ]
    rownames(x) <- NULL
    x
}



#' @title Voice Activity Detection per channel
#' @description Voice Activity Detection per channel. 
#' Transforms the audio file to a wav file with the provided \code{sample_rate} and perform the voice activity detection per channel.
#' @param file the path to an audio file
#' @param sample_rate integer with the \code{sample_rate} to convert the file to. Passed on to \code{\link[av]{av_audio_convert}}
#' @param channels character string - either 'default' or 'all' indicating to do the voice activity detection for each channel independently ('default') or for all channels independently as well as all channels together ('all')
#' @param ... further arguments passed on to \code{\link{VAD}}
#' @return an object of class \code{webrtc-gmm-bychannel} which is a list with elements
#' \itemize{
#' \item{file: the path to the file}
#' \item{duration_secs: seconds}
#' \item{sample_rate: the sample rate of the audio file in Hz}
#' \item{channels: the number of channels in the audio}
#' \item{samples: the number of samples in the data}
#' \item{bitsPerSample: the number of bits per sample}
#' \item{bytesPerSample: the number of bytes per sample}
#' \item{type: the type of VAD model - currently only 'webrtc-gmm'}
#' \item{mode: the provided VAD mode}
#' \item{milliseconds: the provided milliseconds - either by 10, 20 or 30 ms frames}
#' \item{frame_length: the frame length corresponding to the provided milliseconds}
#' \item{vad_segments: a data.frame with columns channel, vad_segment, start, end and has_voice where the start/end values are in seconds}
#' \item{vad_stats: a list with elements channel, n_segments, n_segments_has_voice, n_segments_has_no_voice, seconds_has_voice, seconds_has_no_voice, pct_has_voice indicating the number of segments with voice and the duration of the voice/non-voice in the audio}
#' }
#' Channel 0 means all audio combined in 1 channel.
#' @export
#' @examples 
#' library(audio)
#' library(av)
#' file <- system.file(package = "audio.vadwebrtc", "extdata", "stereo.mp3")
#' vad  <- VAD_channel(file, sample_rate = 32000, 
#'                     mode = "normal", milliseconds = 10, channels = "all")
#' vad
#' vad$vad_segments
#' voiced <- is.voiced(vad, channel = 0, silence_min = 0.2, voiced_min = 1)
#' voiced
#' voiced <- is.voiced(vad, channel = 1, silence_min = 0.2, voiced_min = 1)
#' voiced
#' voiced <- is.voiced(vad, channel = 2, silence_min = 0.2, voiced_min = 1)
#' voiced
VAD_channel <- function(file, sample_rate = 16000, channels = c("default", "all"), ...){
    items <- match.arg(channels)
    requireNamespace("av")
    i        <- av::av_media_info(file)
    duration <- i$duration
    channels <- i$audio$channels
    #tempfile_wav_allchannels <- tempfile(pattern = "allchannels_", fileext = ".wav", tmpdir = tempdir(check = TRUE))
    #tempfile_wav_onechannel  <- tempfile(pattern = "onechannel_", fileext = ".wav", tmpdir = tempdir(check = TRUE))
    tempfile_wav_allchannels <- tempfile(pattern = "allchannels_", fileext = ".wav")
    tempfile_wav_onechannel  <- tempfile(pattern = "onechannel_", fileext = ".wav")
    on.exit({
        els <- c(tempfile_wav_allchannels, tempfile_wav_onechannel)
        invisible(file.remove(els[file.exists(els)]))   
    })
    av::av_audio_convert(audio = file, output = tempfile_wav_allchannels, format = "wav", sample_rate = sample_rate, verbose = FALSE)
    av::av_audio_convert(audio = file, output = tempfile_wav_onechannel, format = "wav", sample_rate = sample_rate, verbose = FALSE, channels = 1)
    ## By channel - VAD
    out <- list()
    if(channels > 1){
        requireNamespace("audio")
        audio_content <- audio::load.wave(tempfile_wav_allchannels)
        out <- lapply(seq_len(nrow(audio_content)), FUN = function(i, ...){
            ## Get audio of that channel + save to file
            audio_content_channel <- audio_content[i, , drop = FALSE]
            audio::save.wave(audio_content_channel, tempfile_wav_onechannel)
            ## Do the voice activity detection, add channel information to identified voiced/non-voiced segments
            vad <- audio.vadwebrtc::VAD(tempfile_wav_onechannel, ...)
            vad$vad_segments$channel <- rep(i, nrow(vad$vad_segments))
            vad$vad_stats$channel    <- i
            vad[c("vad_segments", "vad_stats", "samples", "bitsPerSample", "bytesPerSample", "milliseconds", "frame_length", "type", "mode")]
        }, ...)
    }
    if(items == "all"){
        ## Do the voice activity detection for all channels combined and call it channel 0
        vad <- audio.vadwebrtc::VAD(tempfile_wav_allchannels, ...)
        vad$vad_segments$channel <- rep(0, nrow(vad$vad_segments))
        vad$vad_stats$channel    <- 0
        vad                      <- vad[c("vad_segments", "vad_stats", "samples", "bitsPerSample", "bytesPerSample", "milliseconds", "frame_length", "type", "mode")]
        out[[length(out) + 1]]   <- vad
    }
    #lapply(out, FUN = function(x) x[c("samples", "bitsPerSample", "bytesPerSample", "milliseconds", "frame_length", "type", "mode")])
    settings <- out[[length(out)]][c("samples", "bitsPerSample", "bytesPerSample", "milliseconds", "frame_length", "type", "mode")]
    settings$samples <- NA
    out <- lapply(out, FUN = function(x) x[c("vad_segments", "vad_stats")]) 
    out <- lapply(out, unclass)
    
    results <- list(file = file,
                    duration_secs = duration,
                    sample_rate = i$audio$sample_rate, 
                    channels = channels,
                    samples = settings$samples,
                    bitsPerSample = settings$bitsPerSample,
                    bytesPerSample = settings$bytesPerSample,
                    milliseconds = settings$milliseconds,
                    frame_length = settings$frame_length,
                    type = settings$type,
                    mode = settings$mode,
                    vad_segments = do.call(rbind, lapply(out, FUN = function(x) x$vad_segments)),
                    vad_stats = do.call(rbind, lapply(out, FUN = function(x) as.data.frame(x$vad_stats))))
    results$vad_segments <- results$vad_segments[, c('channel', 'vad_segment', 'start', 'end', 'has_voice')]
    results$vad_stats <- results$vad_stats[, c('channel', 'n_segments', 'n_segments_has_voice', 'n_segments_has_no_voice', 'seconds_has_voice', 'seconds_has_no_voice', 'pct_has_voice')]
    class(results) <- c("VAD", "webrtc-gmm-bychannel")
    results
}


