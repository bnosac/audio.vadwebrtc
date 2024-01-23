#' @title Voice Activity Detection
#' @description Detect the location of active Voice Activity Detection using webrtc 
#' @param file the path to an audio file which should be a file in 16 bit with mono PCM samples (pcm_s16le codec) with a sampling rate of either 8Khz, 16KHz or 32Khz
#' @param mode the type of voice detection, either 'normal', 'lowbitrate', 'aggressive' or 'veryaggressive'
#' @param type character string with the type of VAD model. Only 'webrtc' currently.
#' @return a list with elements
#' \itemize{
#' \item{file}
#' \item{sample_rate}
#' \item{channels}
#' \item{samples}
#' \item{format}
#' \item{bitsPerSample}
#' \item{bytesPerSample}
#' \item{vad: a data.frame with columns millisecond, has_voice and vad_segment indicating if the audio contains an active voice signal at that millisecond}
#' \item{vad_segments: a data.frame with columns vad_segment, start, end and has_voice where the start/end values are in seconds}
#' }
#' @export
#' @examples 
#' file <- system.file(package = "audio.vadwebrtc", "extdata", "test_wav.wav")
#' vad  <- VAD(file, mode = "normal")
#' vad$vad_segments
#' \dontrun{
#' library(av)
#' x <- read_audio_bin(file)
#' plot(seq_along(x) / 16000, x, type = "l")
#' abline(v = vad$vad_segments$start, col = "red", lwd = 2)
#' abline(v = vad$vad_segments$end, col = "blue", lwd = 2)
#' 
#' av_media_info(file)
#' av_audio_convert(file, output = "audio_pcm_16khz.wav", 
#'                  format = "wav", channels = 1, sample_rate = 16000)
#' vad <- VAD("audio_pcm_16khz.wav", mode = "normal")
#' }
VAD <- function(file, mode = c("normal", "lowbitrate", "aggressive", "veryaggressive"), type = "webrtc"){
    type <- match.arg(type)
    mode <- match.arg(mode)
    mode <- switch(mode,
                   normal = 0,
                   lowbitrate = 1,
                   aggressive = 2,
                   veryaggressive = 3)
    stopifnot(file.exists(file))
    msg <- vad_webrtc_detection(file, mode = mode)
    ## Get groups
    grp <- rle(msg$vad$has_voice)
    msg$vad$vad_segment <- rep(seq_along(grp$lengths), grp$lengths)
    segments <- tapply(X = msg$vad$millisecond, INDEX = msg$vad$vad_segment, FUN = range, simplify = F)   
    segments <- data.frame(vad_segment = as.integer(names(segments)),
                           start = vapply(segments, FUN = function(x) x[1], FUN.VALUE = integer(1), USE.NAMES = FALSE) / 1000,
                           end = vapply(segments, FUN = function(x) x[2], FUN.VALUE = integer(1), USE.NAMES = FALSE) / 1000,
                           has_voice = grp$values)
    msg$vad_segments <- segments
    msg
}