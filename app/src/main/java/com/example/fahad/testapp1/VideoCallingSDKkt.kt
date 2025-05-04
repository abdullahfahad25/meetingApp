package com.example.fahad.testapp1

import android.content.Context
import android.util.Log
import dagger.hilt.android.qualifiers.ApplicationContext
import io.agora.rtc2.ChannelMediaOptions
import io.agora.rtc2.Constants
import io.agora.rtc2.IRtcEngineEventHandler
import io.agora.rtc2.RtcEngine
import io.agora.rtc2.RtcEngineConfig
import io.agora.rtc2.video.VideoCanvas
import javax.inject.Inject
import javax.inject.Singleton

@Singleton
class VideoCallingSDKkt @Inject constructor(
    @ApplicationContext private val context: Context
) {
    companion object {
        private const val myAppId = "215516a171ee4e85bc03b96b5c39ca5c"
        private const val channelName = "Channel TestApp1"
        private const val token = "007eJxTYPh156ez1W7BsGbXD1GLfpgEPpq0U6M+6PG/xW5Pf0VcCutVYDAyNDU1NEs0NDdMTTVJtTBNSjYwTrI0SzJNNrZMTjRNPiG6Lb0hkJGhUvkbCyMDBIL4AgzOGYl5eak5CiGpxSWOBQWGDAwA7YImiA=="
    }

    lateinit var callback: Callback
    lateinit var mRtcEngine: RtcEngine
    private val rtcEngineEventHandler: IRtcEngineEventHandler = object: IRtcEngineEventHandler() {
        override fun onJoinChannelSuccess(channel: String?, uid: Int, elapsed: Int) {
            super.onJoinChannelSuccess(channel, uid, elapsed)
            Log.d("VideoCallingSDK", "onJoinChannelSuccess, uid: $uid, channel: $channel")
        }

        override fun onUserJoined(uid: Int, elapsed: Int) {
            super.onUserJoined(uid, elapsed)
            Log.d("VideoCallingSDK", "onUserJoined: $uid")
            callback.onUserJoined(uid)
        }

        override fun onUserOffline(uid: Int, reason: Int) {
            super.onUserOffline(uid, reason)
            Log.d("VideoCallingSDK", "onUserOffline: $uid")
            callback.onUserLeft(uid)
        }
    }

    interface Callback {
        fun onUserJoined(uid: Int)
        fun onUserLeft(uid: Int)
    }

    fun initializeAgoraVideoSDK() {
        try {
            val config: RtcEngineConfig = RtcEngineConfig().apply {
                mContext = this@VideoCallingSDKkt.context
                mAppId = myAppId
                mEventHandler = rtcEngineEventHandler
            }

            mRtcEngine = RtcEngine.create(config)
        } catch (e: Exception) {
            throw RuntimeException("Error initializing RTC engine: ${e.message}")
        }
    }

    fun setLocalView(localview: VideoCanvas) {
        mRtcEngine.setupLocalVideo(localview)
    }

    fun setRemoteView(remoteView: VideoCanvas) {
        mRtcEngine.setupRemoteVideo(remoteView)
    }

    fun joinChannel() {
        val options = ChannelMediaOptions().apply {
            clientRoleType = Constants.CLIENT_ROLE_BROADCASTER
            channelProfile = Constants.CHANNEL_PROFILE_COMMUNICATION
            publishCameraTrack = true
            publishMicrophoneTrack = true
        }

        mRtcEngine.joinChannel(token, channelName, 0, options)
    }

    fun enableVideo() {
        mRtcEngine.enableVideo()
        mRtcEngine.startPreview()
    }

    fun toggleCamera(isCameraOn: Boolean) {
        mRtcEngine.muteLocalVideoStream(isCameraOn)
    }

    fun toggleMic(isMicOn: Boolean) {
        mRtcEngine.muteLocalAudioStream(isMicOn)
    }

    private fun cleanupAgoraEngine() {
        if (::mRtcEngine.isInitialized) {
            mRtcEngine.stopPreview()
            mRtcEngine.leaveChannel()
        }
    }

    fun onDestroy() {
        cleanupAgoraEngine()
    }
}