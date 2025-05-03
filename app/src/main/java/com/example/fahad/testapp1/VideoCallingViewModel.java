package com.example.fahad.testapp1;

import androidx.lifecycle.LiveData;
import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.ViewModel;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import io.agora.rtc2.video.VideoCanvas;

@HiltViewModel
public class VideoCallingViewModel extends ViewModel {

    private final MutableLiveData<Boolean> isCallEnded = new MutableLiveData<>();
    private final MutableLiveData<Boolean> isMicMute = new MutableLiveData<>();
    private final MutableLiveData<Boolean> isCameraOn = new MutableLiveData<>();

    private VideoCallingSDKManager manager;

    @Inject
    public VideoCallingViewModel(VideoCallingSDKManager sdkManager) {
        this.manager = sdkManager;

        isMicMute.setValue(true);
        isCallEnded.setValue(false);
        isCameraOn.setValue(false);
    }

    public void setManager(VideoCallingSDK sdk) {
        manager = new VideoCallingSDKManager(sdk);
    }

    public void startVideoCall(VideoCallingView videoCallingView) {
        manager.startVideoCalling(videoCallingView);
    }

    public void setRemoteView(VideoCanvas remoteView) {
        manager.setRemoteView(remoteView);
    }

    public LiveData<Boolean> getIsCallEnded() {
        return isCallEnded;
    }

    public LiveData<Boolean> getIsMicMute() {
        return isMicMute;
    }

    public LiveData<Boolean> getIsCameraOn() {
        return isCameraOn;
    }

    public LiveData<Integer> getRemoteUserJoinedLiveData() {
        return manager.getRemoteUserJoinedLiveData();
    }

    public LiveData<Integer> getRemoteUserLeftLiveData() {
        return manager.getRemoteUserLeftLiveData();
    }

    public void endCall() {
        manager.endCall();
        isCallEnded.setValue(true);
    }

    public void toggleCamera() {
        isCameraOn.setValue(Boolean.FALSE.equals(isCameraOn.getValue()));
        manager.toggleCamera(Boolean.TRUE.equals(isCameraOn.getValue()));
    }

    public void toggleMic() {
        isMicMute.setValue(Boolean.FALSE.equals(isMicMute.getValue()));
        manager.toggleMic(Boolean.TRUE.equals(isMicMute.getValue()));
    }

    public void destroy() {
        manager.onDestroy();
    }
}
