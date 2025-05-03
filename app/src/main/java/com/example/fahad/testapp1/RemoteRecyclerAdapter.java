package com.example.fahad.testapp1;

import android.content.Context;
import android.view.SurfaceView;
import android.view.View;
import android.view.ViewGroup;
import android.widget.FrameLayout;

import androidx.annotation.NonNull;
import androidx.recyclerview.widget.RecyclerView;

import java.util.List;

import io.agora.rtc2.video.VideoCanvas;

public class RemoteRecyclerAdapter extends RecyclerView.Adapter<RemoteRecyclerAdapter.ViewHolder> {

    private final List<Integer> users;
    private Context context;

    private Callback callback;

    public RemoteRecyclerAdapter(List<Integer> users, Context context) {
        this.users = users;
        this.context = context;
    }

    @NonNull
    @Override
    public ViewHolder onCreateViewHolder(@NonNull ViewGroup parent, int viewType) {
        FrameLayout layout = new FrameLayout(context);
        layout.setLayoutParams(new ViewGroup.LayoutParams(
                ViewGroup.LayoutParams.MATCH_PARENT,
                500
        ));
        return new ViewHolder(layout);
    }

    @Override
    public void onBindViewHolder(@NonNull RemoteRecyclerAdapter.ViewHolder holder, int position) {
        int uid = users.get(position);
        holder.bind(uid);
        callback.onBindViewReady(holder.getVideoCanvas());
    }

    @Override
    public int getItemCount() {
        return users.size();
    }

    public void setCallback(Callback callback) {
        this.callback = callback;
    }

    public static class ViewHolder extends RecyclerView.ViewHolder {
        FrameLayout container;
        VideoCanvas videoCanvas;

        public ViewHolder(@NonNull View itemView) {
            super(itemView);
            container = (FrameLayout) itemView;
        }

        public void bind(int uid) {
            container.removeAllViews();
            SurfaceView surfaceView = new SurfaceView(container.getContext());
            container.addView(surfaceView);
            videoCanvas = new VideoCanvas(surfaceView, VideoCanvas.RENDER_MODE_HIDDEN, uid);
        }

        public VideoCanvas getVideoCanvas() {
            return videoCanvas;
        }
    }

    interface Callback {
        void onBindViewReady(VideoCanvas view);
    }
}
