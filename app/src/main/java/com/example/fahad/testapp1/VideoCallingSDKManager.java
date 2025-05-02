package com.example.fahad.testapp1;

import android.util.Log;

import androidx.lifecycle.LiveData;
import androidx.lifecycle.MutableLiveData;

import io.agora.rtc2.video.VideoCanvas;

public class VideoCallingSDKManager {
    private final VideoCallingSDK sdk;
    private final MutableLiveData<Integer> remoteViewLiveData = new MutableLiveData<>();
    private final MutableLiveData<Boolean> isCallEnded = new MutableLiveData<>();
    private final MutableLiveData<Boolean> isMicMute = new MutableLiveData<>();
    private final MutableLiveData<Boolean> isCameraOn = new MutableLiveData<>();

    public VideoCallingSDKManager(VideoCallingSDK sdk) {
        this.sdk = sdk;

        isMicMute.setValue(true);
        isCallEnded.setValue(false);
        isCameraOn.setValue(false);

        sdk.setCallback(new VideoCallingSDK.Callback() {
            @Override
            public void onUserJoined(int uid) {
                Log.d("Manager", "User joined: " + uid);
                remoteViewLiveData.setValue(uid);
            }
        });
    }

    public void startVideoCalling(VideoCallingView videoCallingView) {
        sdk.initializeAgoraVideoSDK();
        sdk.enableVideo();

        videoCallingView.setupLocalVideo();
        sdk.setLocalView(videoCallingView.getLocalview());

        sdk.joinChannel();
    }

    public void setRemoteView(VideoCanvas remoteView) {
        sdk.setRemoteView(remoteView);
    }

    public LiveData<Integer> getRemoteViewLiveData() {
        return remoteViewLiveData;
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

    public void endCall() {
        sdk.onDestroy();
        isCallEnded.setValue(true);
    }

    public void toggleCamera() {
        isCameraOn.setValue(Boolean.FALSE.equals(isCameraOn.getValue()));
        sdk.toggleCamera(Boolean.TRUE.equals(isCameraOn.getValue()));
    }

    public void toggleMic() {
        isMicMute.setValue(Boolean.FALSE.equals(isMicMute.getValue()));
        sdk.toggleMic(Boolean.TRUE.equals(isMicMute.getValue()));
    }
}
