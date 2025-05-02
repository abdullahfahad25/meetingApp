package com.example.fahad.testapp1;

import android.app.Activity;
import android.view.MenuItem;
import android.view.SurfaceView;
import android.widget.FrameLayout;
import android.widget.Toast;

import androidx.annotation.NonNull;

import com.google.android.material.bottomnavigation.BottomNavigationView;
import com.google.android.material.navigation.NavigationBarView;

import io.agora.rtc2.ChannelMediaOptions;
import io.agora.rtc2.Constants;
import io.agora.rtc2.IRtcEngineEventHandler;
import io.agora.rtc2.RtcEngine;
import io.agora.rtc2.RtcEngineConfig;
import io.agora.rtc2.video.VideoCanvas;

public class VideoCallingSDK {

    // Fill in the app ID from Agora Console
    private String myAppId = "215516a171ee4e85bc03b96b5c39ca5c";
    private RtcEngine mRtcEngine;

    // Fill in the channel name
    private String channelName = "Channel TestApp1";

    // Fill in the temporary token generated from Agora Console
    //This token gets expired in 1 day. So, always need to generate token
    private String token = "007eJxTYPh156ez1W7BsGbXD1GLfpgEPpq0U6M+6PG/xW5Pf0VcCutVYDAyNDU1NEs0NDdMTTVJtTBNSjYwTrI0SzJNNrZMTjRNPiG6Lb0hkJGhUvkbCyMDBIL4AgzOGYl5eak5CiGpxSWOBQWGDAwA7YImiA==";

    private final Activity context;

    private Callback callback;

    private final IRtcEngineEventHandler mRtcEventHandler = new IRtcEngineEventHandler() {
        // Triggered when the local user successfully joins the specified channel.
        @Override
        public void onJoinChannelSuccess(String channel, int uid, int elapsed) {
            super.onJoinChannelSuccess(channel, uid, elapsed);
            showToast("Host User " + uid + " has joined channel " + channel);
        }


        // Triggered when a remote user/host joins the channel.
        @Override
        public void onUserJoined(int uid, int elapsed) {
            super.onUserJoined(uid, elapsed);
            context.runOnUiThread(() -> {
                // Initialize and display remote video view for the new user.
//                setupRemoteVideo(uid);
                showToast("Remote User joined: " + uid);

                callback.onUserJoined(uid);
            });
        }

        // Triggered when a remote user/host leaves the channel.
        @Override
        public void onUserOffline(int uid, int reason) {
            super.onUserOffline(uid, reason);
            context.runOnUiThread(() -> {
                showToast("User offline: " + uid);
            });
        }
    };

    public VideoCallingSDK(Activity context) {
        this.context = context;
    }

//    public void startVideoCalling() {
//        initializeAgoraVideoSDK();
//        enableVideo();
//        setupLocalVideo();
//        joinChannel();
//    }

    public void initializeAgoraVideoSDK() {
        try {
            RtcEngineConfig config = new RtcEngineConfig();
            config.mContext = context.getBaseContext();
            config.mAppId = myAppId;
            config.mEventHandler = mRtcEventHandler;
            mRtcEngine = RtcEngine.create(config);
        } catch (Exception e) {
            throw new RuntimeException("Error initializing RTC engine: " + e.getMessage());
        }
    }

    public void setLocalView(VideoCanvas localView) {
        mRtcEngine.setupLocalVideo(localView);
    }

    public void joinChannel() {
        ChannelMediaOptions options = new ChannelMediaOptions();
        options.clientRoleType = Constants.CLIENT_ROLE_BROADCASTER;
        options.channelProfile = Constants.CHANNEL_PROFILE_COMMUNICATION;
        options.publishCameraTrack = true;
        options.publishMicrophoneTrack = true;
        mRtcEngine.joinChannel(token, channelName, 0, options);
    }

    public void enableVideo() {
        mRtcEngine.enableVideo();
        mRtcEngine.startPreview();
    }

//    private void setupLocalVideo() {
//        FrameLayout container = context.findViewById(R.id.local_video_view_container);
//        SurfaceView surfaceView = new SurfaceView(context.getBaseContext());
//        container.addView(surfaceView);
//        mRtcEngine.setupLocalVideo(new VideoCanvas(surfaceView, VideoCanvas.RENDER_MODE_FIT, 0));
//    }

    public void toggleCamera(boolean isCameraOn) {
        mRtcEngine.muteLocalVideoStream(isCameraOn);
    }

    public void toggleMic(boolean isMicOn) {
        mRtcEngine.muteLocalAudioStream(isMicOn);
    }

//    private void setupRemoteVideo(int uid) {
//        FrameLayout container = context.findViewById(R.id.remote_video_view_container);
//        SurfaceView surfaceView = new SurfaceView(context.getBaseContext());
//        surfaceView.setZOrderMediaOverlay(true);
//        container.addView(surfaceView);
//        mRtcEngine.setupRemoteVideo(new VideoCanvas(surfaceView, VideoCanvas.RENDER_MODE_FIT, uid));
//    }

    public void setRemoteView(VideoCanvas remoteView) {
        mRtcEngine.setupRemoteVideo(remoteView);
    }

    public void setCallback(Callback callback) {
        this.callback = callback;
    }

    private void showToast(String message) {
        context.runOnUiThread(() -> Toast.makeText(context, message, Toast.LENGTH_SHORT).show());
    }

    private void cleanupAgoraEngine() {
        if (mRtcEngine != null) {
            mRtcEngine.stopPreview();
            mRtcEngine.leaveChannel();
            mRtcEngine = null;
        }
    }

    public void onDestroy() {
        cleanupAgoraEngine();
    }

    interface Callback {
        void onUserJoined(int uid);
    }
}
