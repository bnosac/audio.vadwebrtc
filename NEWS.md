## CHANGES IN audio.vadwebrtc VERSION 0.2

- Added function VAD_channel to detect voice in audio by channel which depends on reading/writing audio data with R package audio and converting an audio file to a specific sample_rate with R package av

## CHANGES IN audio.vadwebrtc VERSION 0.1

- Added function VAD to detect voice in audio
- Added is.voiced generic and is.voiced.webrtc-gmm to collapse voiced/non-voiced segments which are too short
- Initial version based on webrtc commit 7f457533a2ee582865f50210e7460af90f78f0b6 (Wed Jan 17 19:03:20 2024)


