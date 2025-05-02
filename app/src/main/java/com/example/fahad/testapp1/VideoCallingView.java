package com.example.fahad.testapp1;

import android.app.Activity;
import android.view.SurfaceView;
import android.widget.FrameLayout;

import io.agora.rtc2.video.VideoCanvas;

public class VideoCallingView {

    private final Activity activity;
    private VideoCanvas localview;

    public VideoCallingView(Activity activity) {
        this.activity = activity;
    }

    public void setupLocalVideo() {
        FrameLayout container = activity.findViewById(R.id.local_video_view_container);
        SurfaceView surfaceView = new SurfaceView(activity.getBaseContext());
        container.addView(surfaceView);
        localview = new VideoCanvas(surfaceView, VideoCanvas.RENDER_MODE_FIT, 0);
    }

    public VideoCanvas getLocalview() {
        return localview;
    }

    public VideoCanvas getRemoteView(int uid) {
        FrameLayout container = activity.findViewById(R.id.remote_video_view_container);
        SurfaceView surfaceView = new SurfaceView(activity.getBaseContext());
        surfaceView.setZOrderMediaOverlay(true);
        container.addView(surfaceView);
        return new VideoCanvas(surfaceView, VideoCanvas.RENDER_MODE_FIT, uid);
    }
}
