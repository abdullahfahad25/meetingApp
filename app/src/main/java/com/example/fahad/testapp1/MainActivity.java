package com.example.fahad.testapp1;

import android.content.pm.PackageManager;
import android.os.Bundle;
import android.Manifest;
import android.util.Log;

import androidx.annotation.NonNull;
import androidx.appcompat.app.AppCompatActivity;
import androidx.core.app.ActivityCompat;
import androidx.core.content.ContextCompat;
import androidx.lifecycle.Observer;
import androidx.lifecycle.ViewModelProvider;

import com.google.android.material.bottomnavigation.BottomNavigationView;

public class MainActivity extends AppCompatActivity {

    private static final int PERMISSION_REQ_ID = 22;

    private VideoCallingSDK sdk;
    private VideoCallingViewModel videoCallingViewModel;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        videoCallingViewModel = new ViewModelProvider(this).get(VideoCallingViewModel.class);

        sdk = new VideoCallingSDK(this, videoCallingViewModel);

        if (checkPermissions()) {
            sdk.startVideoCalling();
        } else {
            requestPermissions();
        }

        videoCallingViewModel.getIsCallEnded().observe(this, new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean value) {
                Log.d("MainActivity", "Call Ended: " + value);
                if (value) {
//                    onDestroy();
                    finish();
                }
            }
        });
    }

    private void requestPermissions() {
        ActivityCompat.requestPermissions(this, getRequiredPermissions(), PERMISSION_REQ_ID);
    }

    private boolean checkPermissions() {
        for (String permission : getRequiredPermissions()) {
            if (ContextCompat.checkSelfPermission(this, permission) != PackageManager.PERMISSION_GRANTED) {
                return false;
            }
        }
        return true;
    }

    private String[] getRequiredPermissions() {
        if (android.os.Build.VERSION.SDK_INT >= android.os.Build.VERSION_CODES.S) {
            return new String[]{
                    android.Manifest.permission.RECORD_AUDIO,
                    android.Manifest.permission.CAMERA,
                    android.Manifest.permission.READ_PHONE_STATE,
                    android.Manifest.permission.BLUETOOTH_CONNECT
            };
        } else {
            return new String[]{
                    android.Manifest.permission.RECORD_AUDIO,
                    Manifest.permission.CAMERA
            };
        }
    }

    @Override
    public void onRequestPermissionsResult(int requestCode, @NonNull String[] permissions, @NonNull int[] grantResults) {
        super.onRequestPermissionsResult(requestCode, permissions, grantResults);
        if (requestCode == PERMISSION_REQ_ID && checkPermissions()) {
            sdk.startVideoCalling();
        }
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();
        videoCallingViewModel.getIsCallEnded().removeObservers(this);
        sdk.onDestroy();
    }
}