#include <Rcpp.h>
#include <stdio.h>
#include "common_audio/vad/include/webrtc_vad.h"
#define DR_WAV_IMPLEMENTATION
#include "dr_wav.h"

Rcpp::List read_dr_wav(const std::string & fname, std::vector<int16_t>& pcm16) {
    drwav wav;
    std::vector<uint8_t> wav_data; // used for pipe input from stdin
    
    if (fname == "-") {
        {
            uint8_t buf[1024];
            while (true)
            {
                const size_t n = fread(buf, 1, sizeof(buf), stdin);
                if (n == 0) {
                    break;
                }
                wav_data.insert(wav_data.end(), buf, buf + n);
            }
        }
        
        if (drwav_init_memory(&wav, wav_data.data(), wav_data.size(), nullptr) == false) {
            Rprintf("error: failed to open WAV file from stdin\n");
            return false;
        }
        
        Rprintf("%s: read %zu bytes from stdin\n", __func__, wav_data.size());
    }
    else if (drwav_init_file(&wav, fname.c_str(), nullptr) == false) {
        Rprintf("error: failed to open '%s' as WAV file\n", fname.c_str());
        return false;
    }
    const uint64_t n = wav_data.empty() ? wav.totalPCMFrameCount : wav_data.size()/(wav.channels*wav.bitsPerSample/8);
    pcm16.resize(n*wav.channels);
    drwav_read_pcm_frames_s16(&wav, n, pcm16.data());
    drwav_uninit(&wav);
    Rcpp::List output = Rcpp::List::create(Rcpp::Named("file") = fname,
                                           Rcpp::Named("sample_rate") = wav.sampleRate,
                                           Rcpp::Named("channels") = wav.channels,
                                           Rcpp::Named("samples") = pcm16.size(),
                                           //Rcpp::Named("format") = wav.translatedFormatTag,
                                           Rcpp::Named("bitsPerSample") = wav.bitsPerSample,
                                           Rcpp::Named("bytesPerSample") = wav.bitsPerSample / 8);
    return output;
}


void vad_free(VadInst *vadptr){
    WebRtcVad_Free(vadptr);
}
VadInst *vad_create(){
    VadInst* vadptr = WebRtcVad_Create();
    return vadptr;
}
VadInst *vad_init(VadInst *vadptr){
    if (WebRtcVad_Init(vadptr)) {
        return NULL;
    }
    return vadptr;
}

// [[Rcpp::export]]
Rcpp::List vad_webrtc_detection(std::string file, int mode = 3, size_t frame_length = 160) {
    VadInst* vad = vad_create();
    vad = vad_init(vad);
    if (vad) {
        WebRtcVad_set_mode(vad, mode);
    } else {
        Rcpp::stop("Failed to init vad!");
    }
    // Read in the voice file - which should be a file in 16 bit, 8Khz/16KHz/32Khz, with mono PCM samples
    std::vector<int16_t> pcm16;
    Rcpp::List output = read_dr_wav(file, pcm16);
    // Check on the combination of the frame length and the sample_rate
    Rcpp::LogicalVector detections;
    Rcpp::IntegerVector detections_ms;
    int ok = WebRtcVad_ValidRateAndFrameLength(output["sample_rate"], frame_length);
    if (ok < 0) {
        Rcpp::stop("Invalid combination of Hz and frame_length. We support 10, 20 and 30 ms frames and the rates 8000, 16000 and 32000 Hz.");
    }
    //int step = 160;
    const int16_t * temp = pcm16.data();
    // currently checking in 10ms frames, most likely to change
    for(unsigned int i=0, ms=0; i < pcm16.size(); i+=160, ms+=10){
        int isActive = WebRtcVad_Process(vad, output["sample_rate"], temp, frame_length); // 1 = voice , 0 = not voice
        temp = temp + 160;
        if(isActive < 0){
            detections.push_back(NA_LOGICAL);
        }else{
            detections.push_back(isActive);
        }
        detections_ms.push_back(ms);
    }
    vad_free(vad);
    output["vad"] = Rcpp::DataFrame::create(
        Rcpp::Named("millisecond") = detections_ms, 
        Rcpp::Named("has_voice") = detections);
    return output;
}
