package com.example.fahad.testapp1;

import android.util.Log;

import androidx.lifecycle.LiveData;
import androidx.lifecycle.MutableLiveData;

import javax.inject.Inject;
import javax.inject.Singleton;

import io.agora.rtc2.video.VideoCanvas;

@Singleton
public class VideoCallingSDKManager {
    private final VideoCallingSDK sdk;
    private final MutableLiveData<Integer> remoteUserJoinedLiveData = new MutableLiveData<>();
    private final MutableLiveData<Integer> remoteUserLeftLiveData = new MutableLiveData<>();

    @Inject
    public VideoCallingSDKManager(VideoCallingSDK sdk) {
        this.sdk = sdk;
        sdk.setCallback(new VideoCallingSDK.Callback() {
            @Override
            public void onUserJoined(int uid) {
                Log.d("Manager", "User joined: " + uid);
                remoteUserJoinedLiveData.setValue(uid);
            }

            @Override
            public void onUserLeft(int uid) {
                Log.d("Manager", "User left: " + uid);
                remoteUserLeftLiveData.setValue(uid);
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

    public LiveData<Integer> getRemoteUserJoinedLiveData() {
        return remoteUserJoinedLiveData;
    }

    public MutableLiveData<Integer> getRemoteUserLeftLiveData() {
        return remoteUserLeftLiveData;
    }

    public void endCall() {
        sdk.onDestroy();
    }

    public void toggleCamera(boolean isCameraOn) {
        sdk.toggleCamera(isCameraOn);
    }

    public void toggleMic(boolean isMicMute) {
        sdk.toggleMic(isMicMute);
    }

    public void onDestroy() {
        sdk.onDestroy();
    }
}
