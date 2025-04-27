package com.example.fahad.testapp1;

import androidx.lifecycle.LiveData;
import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.ViewModel;

public class VideoCallingViewModel extends ViewModel {
    private final MutableLiveData<Boolean> isCallEnded = new MutableLiveData<>();

    public LiveData<Boolean> getIsCallEnded() {
        return isCallEnded;
    }

    public void onCallEnded(boolean callEnded) {
        isCallEnded.setValue(callEnded);
    }
}
