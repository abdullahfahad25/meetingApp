package com.example.fahad.testapp1;

import androidx.lifecycle.LiveData;
import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.ViewModel;

public class VideoCallingViewModel extends ViewModel {
    private final MutableLiveData<Boolean> isCallEnded = new MutableLiveData<>();
    private final MutableLiveData<Boolean> isMicMute = new MutableLiveData<>();
    private final MutableLiveData<Boolean> isCameraOn = new MutableLiveData<>();

    public VideoCallingViewModel() {
        isMicMute.setValue(true);
        isCallEnded.setValue(false);
        isCameraOn.setValue(false);
    }

    public LiveData<Boolean> getIsCallEnded() {
        return isCallEnded;
    }

    public MutableLiveData<Boolean> getIsMicMute() {
        return isMicMute;
    }

    public MutableLiveData<Boolean> getIsCameraOn() {
        return isCameraOn;
    }

    public void onCallEnded(VideoCallingSDK sdk) {
        sdk.onDestroy();
        isCallEnded.setValue(true);
    }

    public void toggleCamera(VideoCallingSDK sdk) {
        isCameraOn.setValue(Boolean.FALSE.equals(isCameraOn.getValue()));
        sdk.toggleCamera(Boolean.TRUE.equals(isCameraOn.getValue()));
    }

    public void toggleMic(VideoCallingSDK sdk) {
        isMicMute.setValue(Boolean.FALSE.equals(isMicMute.getValue()));
        sdk.toggleMic(Boolean.TRUE.equals(isMicMute.getValue()));
    }
}
