package com.example.fahad.testapp1;

import android.content.pm.PackageManager;
import android.os.Bundle;
import android.Manifest;
import android.util.Log;
import android.widget.Toast;

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

    private BottomNavigationView bottomNavigationView;

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

        videoCallingViewModel = new ViewModelProvider(this).get(VideoCallingViewModel.class);

        sdk = new VideoCallingSDK(this, videoCallingViewModel);

        setupUI();
        setupObservers();

        if (checkPermissions()) {
            sdk.startVideoCalling();
        } else {
            requestPermissions();
        }
    }

    private void setupUI() {
        bottomNavigationView = findViewById(R.id.bottom_nav);
        bottomNavigationView.setOnItemSelectedListener(item -> {
            if (item.getItemId() == R.id.page_1) {
                //WIP
                showToast("Options Selected");
                return true;
            } else if (item.getItemId() == R.id.page_2) {
                //This is for Video
                //Icon update is not done yet
                videoCallingViewModel.toggleCamera(sdk);
                return true;
            } else if (item.getItemId() == R.id.page_3) {
                //This is for Audio/Mic
                //Icon update is not done yet
                videoCallingViewModel.toggleMic(sdk);
                return true;
            } else if (item.getItemId() == R.id.page_4) {
                //This is for ending call
                videoCallingViewModel.onCallEnded(sdk);
                return true;
            } else {
                return false;
            }
        });
    }

    private void setupObservers() {
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

        videoCallingViewModel.getIsCameraOn().observe(this, new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean aBoolean) {
                Log.d("MainActivity", "isCameraOn: " + aBoolean);
                //Update icon
            }
        });

        videoCallingViewModel.getIsMicMute().observe(this, new Observer<Boolean>() {
            @Override
            public void onChanged(Boolean aBoolean) {
                Log.d("MainActivity", "isMicMute: " + aBoolean);
                //Update icon
            }
        });
    }

    private void showToast(String msg) {
        Toast.makeText(this, msg, Toast.LENGTH_SHORT).show();
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
        videoCallingViewModel.getIsMicMute().removeObservers(this);
        videoCallingViewModel.getIsCameraOn().removeObservers(this);
        sdk.onDestroy();
    }
}