package com.example.fahad.testapp1

import android.util.Log
import io.agora.rtc2.video.VideoCanvas
import kotlinx.coroutines.flow.MutableSharedFlow
import kotlinx.coroutines.flow.SharedFlow
import javax.inject.Inject
import javax.inject.Singleton

@Singleton
class VideoCallingSDKManagerkt @Inject constructor(
    private val sdKkt: VideoCallingSDKkt
) {
    private val _remoteUserJoinedFlow = MutableSharedFlow<Int>(replay = 0)
    val remoteUserJoinedFlow: SharedFlow<Int> = _remoteUserJoinedFlow

    private val _remoteUserLeftFlow = MutableSharedFlow<Int>(replay = 0)
    val remoteUserLeftFlow: SharedFlow<Int> = _remoteUserLeftFlow

    init {
        sdKkt.callback = object : VideoCallingSDKkt.Callback {
            override fun onUserJoined(uid: Int) {
                Log.d("Manager", "User joined: $uid")
                _remoteUserJoinedFlow.tryEmit(uid)
            }

            override fun onUserLeft(uid: Int) {
                Log.d("Manager", "User left: $uid")
                _remoteUserLeftFlow.tryEmit(uid)
            }

        }
    }

    fun startVideoCalling(videoCallingViewkt: VideoCallingViewkt) {
        sdKkt.initializeAgoraVideoSDK()
        sdKkt.enableVideo()

        videoCallingViewkt.setupLocalVideo()
        sdKkt.setLocalView(videoCallingViewkt.localView)

        sdKkt.joinChannel()
    }

    fun setRemoteView(remoteView: VideoCanvas) {
        sdKkt.setRemoteView(remoteView)
    }

    fun endCall() {
        sdKkt.onDestroy()
    }

    fun toggleCamera(isCameraOn: Boolean) {
        sdKkt.toggleCamera(isCameraOn)
    }

    fun toggleMic(isMicMute: Boolean) {
        sdKkt.toggleMic(isMicMute)
    }

    fun onDestroy() {
        sdKkt.onDestroy()
    }
}