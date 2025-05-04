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
import androidx.recyclerview.widget.GridLayoutManager;
import androidx.recyclerview.widget.RecyclerView;

import com.google.android.material.bottomnavigation.BottomNavigationView;

import java.util.ArrayList;
import java.util.List;

import javax.inject.Inject;

import dagger.hilt.android.AndroidEntryPoint;
import io.agora.rtc2.video.VideoCanvas;

@AndroidEntryPoint
public class MainActivity extends AppCompatActivity {

    private static final int PERMISSION_REQ_ID = 22;

//    private VideoCallingView callingView;
    private VideoCallingViewkt callingViewkt;

    private VideoCallingViewModel videoCallingViewModel;

    private BottomNavigationView bottomNavigationView;
    private RecyclerView remoteRecyler;
    private RemoteRecyclerAdapter adapter;
    private List<Integer> remoteUids = new ArrayList<>();

    @Override
    protected void onCreate(Bundle savedInstanceState) {
        super.onCreate(savedInstanceState);
        setContentView(R.layout.activity_main);

//        callingView = new VideoCallingView(this);
        callingViewkt = new VideoCallingViewkt(this);

        videoCallingViewModel = new ViewModelProvider(this).get(VideoCallingViewModel.class);
//        videoCallingViewModel.setManager(new VideoCallingSDKkt(getApplicationContext()));

        setupUI();
        setupObservers();

        if (checkPermissions()) {
            videoCallingViewModel.startVideoCall(callingViewkt);
        } else {
            requestPermissions();
        }
    }

    private void setupUI() {
        remoteRecyler = findViewById(R.id.remote_recycler);
        remoteRecyler.setLayoutManager(new GridLayoutManager(this, 2));
        adapter = new RemoteRecyclerAdapter(remoteUids, this);
        adapter.setCallback(new RemoteRecyclerAdapter.Callback() {
            @Override
            public void onBindViewReady(VideoCanvas view) {
                Log.d("MainActivity", "Remote view is ready");
                videoCallingViewModel.setRemoteView(view);
            }
        });
        remoteRecyler.setAdapter(adapter);

        bottomNavigationView = findViewById(R.id.bottom_nav);
        bottomNavigationView.setOnItemSelectedListener(item -> {
            if (item.getItemId() == R.id.page_1) {
                //WIP
                showToast("Options Selected");
                return true;
            } else if (item.getItemId() == R.id.page_2) {
                //This is for Video
                //Icon update is not done yet
                videoCallingViewModel.toggleCamera();
                return true;
            } else if (item.getItemId() == R.id.page_3) {
                //This is for Audio/Mic
                //Icon update is not done yet
                videoCallingViewModel.toggleMic();
                return true;
            } else if (item.getItemId() == R.id.page_4) {
                //This is for ending call
                videoCallingViewModel.endCall();
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

        videoCallingViewModel.getRemoteUserJoinedLiveData().observe(this, new Observer<Integer>() {
            @Override
            public void onChanged(Integer uid) {
                Log.d("MainActivity", "remote joined user id: " + uid.toString());
//                videoCallingViewModel.setRemoteView(callingView.getRemoteView(integer));
                if (!remoteUids.contains(uid)) {
                    remoteUids.add(uid);
                    adapter.notifyItemInserted(remoteUids.indexOf(uid));
                }
            }
        });

        videoCallingViewModel.getRemoteUserLeftLiveData().observe(this, new Observer<Integer>() {
            @Override
            public void onChanged(Integer uid) {
                Log.d("MainActivity", "remote left user id: " + uid.toString());
                if (remoteUids.contains(uid)) {
                    int pos = remoteUids.indexOf(uid);
                    remoteUids.remove(uid);
                    adapter.notifyItemRemoved(pos);
                }
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
            videoCallingViewModel.startVideoCall(callingViewkt);
        }
    }

    @Override
    protected void onDestroy() {
        super.onDestroy();

        remoteRecyler.setAdapter(null);

        videoCallingViewModel.getIsCallEnded().removeObservers(this);
        videoCallingViewModel.getIsMicMute().removeObservers(this);
        videoCallingViewModel.getIsCameraOn().removeObservers(this);
        videoCallingViewModel.getRemoteUserJoinedLiveData().removeObservers(this);
        videoCallingViewModel.getRemoteUserLeftLiveData().removeObservers(this);
        videoCallingViewModel.destroy();
    }
}