package com.example.fahad.testapp1

import androidx.lifecycle.LiveData
import androidx.lifecycle.MutableLiveData
import androidx.lifecycle.ViewModel
import androidx.lifecycle.asLiveData
import androidx.lifecycle.viewModelScope
import dagger.hilt.android.lifecycle.HiltViewModel
import io.agora.rtc2.video.VideoCanvas
import kotlinx.coroutines.flow.collect
import kotlinx.coroutines.launch
import javax.inject.Inject

@HiltViewModel
class VideoCallingViewModelkt @Inject constructor(
   private val manager: VideoCallingSDKManagerkt
) : ViewModel() {

    private val _isCallEnded = MutableLiveData<Boolean>()
    val isCallEnded: LiveData<Boolean> get() = _isCallEnded

    private val _isMicMute = MutableLiveData<Boolean>()
    val isMicMute: LiveData<Boolean> get() = _isMicMute

    private val _isCameraOn = MutableLiveData<Boolean>()
    val isCameraOn: LiveData<Boolean> get() = _isCameraOn

    private val _remoteUserJoined = MutableLiveData<Int>()
    val remoteUserJoined: LiveData<Int> = _remoteUserJoined

    val remoteUserLeft: LiveData<Int> = manager.remoteUserLeftFlow.asLiveData()

    init {
        _isMicMute.value = true
        _isCallEnded.value = false
        _isCameraOn.value = false

        viewModelScope.launch {
            manager.remoteUserJoinedFlow.collect {
                _remoteUserJoined.value = it
            }
        }
    }

    fun startVideoCall(videoCallingViewkt: VideoCallingViewkt) {
        manager.startVideoCalling(videoCallingViewkt)
    }

    fun setRemoteView(remoteView: VideoCanvas) {
        manager.setRemoteView(remoteView)
    }

    fun endCall() {
        manager.endCall()
        _isCallEnded.value = true
    }

    fun toggleCamera() {
        _isCameraOn.value = !_isCameraOn.value!!
        isCameraOn.value?.let { manager.toggleCamera(it) }
    }

    fun toggleMic() {
        _isMicMute.value = !_isMicMute.value!!
        isMicMute.value?.let { manager.toggleMic(it) }
    }

    fun destroy() {
        manager.onDestroy()
    }
}