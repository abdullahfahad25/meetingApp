package com.example.fahad.testapp1

import android.app.Activity
import android.view.SurfaceView
import android.widget.FrameLayout
import io.agora.rtc2.video.VideoCanvas

class VideoCallingViewkt(
    private val activity: Activity
) {

    lateinit var localView: VideoCanvas

    fun setupLocalVideo() {
        val container: FrameLayout = activity.findViewById(R.id.local_video_view_container)
        val surfaceView = SurfaceView(activity.baseContext)
        container.addView(surfaceView)
        localView = VideoCanvas(surfaceView, VideoCanvas.RENDER_MODE_FIT, 0)
    }

    fun getRemoteView(uid: Int): VideoCanvas {
        val container: FrameLayout = activity.findViewById(R.id.remote_video_view_container)
        val surfaceView = SurfaceView(activity.baseContext)
        surfaceView.setZOrderMediaOverlay(true)
        container.addView(surfaceView)
        return VideoCanvas(surfaceView, VideoCanvas.RENDER_MODE_FIT, uid)
    }
}