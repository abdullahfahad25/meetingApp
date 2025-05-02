package com.example.fahad.testapp1;

import androidx.lifecycle.LiveData;
import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.ViewModel;

import io.agora.rtc2.video.VideoCanvas;

public class VideoCallingViewModel extends ViewModel {

    private VideoCallingSDKManager manager;

    public VideoCallingViewModel() {}

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
        return manager.getIsCallEnded();
    }

    public LiveData<Boolean> getIsMicMute() {
        return manager.getIsMicMute();
    }

    public LiveData<Boolean> getIsCameraOn() {
        return manager.getIsCameraOn();
    }

    public LiveData<Integer> getRemoteViewLiveData() {
        return manager.getRemoteViewLiveData();
    }

    public void endCall(VideoCallingSDK sdk) {
        manager.endCall();
    }

    public void toggleCamera(VideoCallingSDK sdk) {
        manager.toggleCamera();
    }

    public void toggleMic(VideoCallingSDK sdk) {
        manager.toggleMic();
    }
}
