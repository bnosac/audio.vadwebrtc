#' @title Voice Activity Detection
#' @description Detect the location of active voice in audio. 
#' The Voice Activity Detection is implemented using a Gaussian Mixture Model from the "webrtc" framework. 
#' It works with .wav audio files with a sample rate of 8, 16 or 32 Khz an can be applied over a window of eiher 10, 20 or 30 milliseconds.
#' @param file the path to an audio file which should be a file in 16 bit with mono PCM samples (pcm_s16le codec) with a sampling rate of either 8Khz, 16KHz or 32Khz
#' @param mode character string with the type of voice detection, either 'normal', 'lowbitrate', 'aggressive' or 'veryaggressive' where 'veryaggressive' means more silences are detected
#' @param milliseconds integer with the number of milliseconds indicating to compute by this number of milliseconds the VAD signal. Can only be 10, 20 or 30. Defaults to 10.
#' @param type character string with the type of VAD model. Only 'webrtc' currently.
#' @return an objec of class \code{VAD} which is a list with elements
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
    cat("    - Percent of audio containing a voiced signal:", paste(round(100*x$vad_stats$pct_has_voice, digits = 1), "%", sep = ""), "\n")
    cat("    - Seconds voiced:", round(x$vad_stats$seconds_has_voice, digits = 1), "\n")
    cat("    - Seconds unvoiced:", round(x$vad_stats$seconds_has_no_voice, digits = 1), "\n")
}





# collapse_segments <- function(x, collapse_silence_secs = 1){
#     x$has_voice <- ifelse(x$has_voice, x$has_voice, ifelse((x$end - x$start) < collapse_silence_secs, TRUE, x$has_voice))
#     grp <- rle(x$has_voice)
#     x$vad_segment <- rep(seq_along(grp$lengths), grp$lengths)
#     x <- data.table::as.data.table(x)
#     x <- x[, list(start = min(start), end = max(end)), by = list(vad_segment, has_voice)]
#     x
# }
#voiced <- collapse_segments(vad$vad_segments)