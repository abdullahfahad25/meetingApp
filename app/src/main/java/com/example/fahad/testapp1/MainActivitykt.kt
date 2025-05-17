package com.example.fahad.testapp1

import android.Manifest
import android.content.pm.PackageManager
import android.os.Build
import android.os.Bundle
import android.util.Log
import android.view.MenuItem
import android.widget.Toast
import androidx.activity.viewModels
import androidx.appcompat.app.AppCompatActivity
import androidx.core.app.ActivityCompat
import androidx.core.content.ContextCompat
import androidx.lifecycle.Observer
import androidx.recyclerview.widget.GridLayoutManager
import androidx.recyclerview.widget.RecyclerView
import com.google.android.material.bottomnavigation.BottomNavigationView
import com.google.android.material.navigation.NavigationBarView
import dagger.hilt.android.AndroidEntryPoint

@AndroidEntryPoint
class MainActivitykt : AppCompatActivity() {
    companion object {
        private const val PERMISSION_REQ_ID: Int = 22
    }

    private lateinit var callingViewkt: VideoCallingViewkt
    private lateinit var remoteRecyler: RecyclerView
    private lateinit var adapter: RemoteRecyclerAdapter
    private lateinit var bottomNavigationView: BottomNavigationView

    private val videoCallingViewModelkt: VideoCallingViewModelkt by viewModels()
    private val remoteUids: MutableList<Int> = mutableListOf()

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)

        setContentView(R.layout.activity_main)

        callingViewkt = VideoCallingViewkt(this)

//        videoCallingViewModelkt = ViewModelProvider(this).get(VideoCallingViewModelkt::class.java)

        setupUI()
        setupObservers()

        if (checkPermissions()) {
            videoCallingViewModelkt.startVideoCall(callingViewkt)
        } else {
            requestPermissions()
        }
    }

    private fun setupUI() {
        remoteRecyler = findViewById(R.id.remote_recycler)
        remoteRecyler.layoutManager = GridLayoutManager(this, 2)
        adapter = RemoteRecyclerAdapter(remoteUids, this)
        adapter.setCallback { view ->
            Log.d("MainActivity", "Remote view is ready")
            videoCallingViewModelkt.setRemoteView(view)
        }
        remoteRecyler.adapter = adapter

        bottomNavigationView = findViewById<BottomNavigationView>(R.id.bottom_nav)
        bottomNavigationView.setOnItemSelectedListener(NavigationBarView.OnItemSelectedListener {item: MenuItem ->
            if (item.itemId == R.id.page_1) {
                //WIP
                showToast("Options Selected")
                true
            } else if (item.itemId == R.id.page_2) {
                //This is for Video
                //Icon update is not done yet
                videoCallingViewModelkt.toggleCamera()
                true
            } else if (item.itemId == R.id.page_3) {
                //This is for Audio/Mic
                //Icon update is not done yet
                videoCallingViewModelkt.toggleMic()
                true
            } else if (item.itemId == R.id.page_4) {
                //This is for ending call
                videoCallingViewModelkt.endCall()
                true
            } else {
                false
            }
        })
    }

    private fun setupObservers() {
        videoCallingViewModelkt.isCallEnded.observe(this, object : Observer<Boolean> {
            override fun onChanged(value: Boolean) {
                Log.d("MainActivity", "Call Ended: $value")
                if (value) {
//                    onDestroy();
                    finish()
                }
            }
        })

        videoCallingViewModelkt.isCameraOn.observe(this
        ) { value ->
            Log.d("MainActivity", "isCameraOn: $value")
            //Update icon
        }

        videoCallingViewModelkt.isMicMute.observe(this
        ) { value ->
            Log.d("MainActivity", "isMicMute: $value")
            //Update icon
        }

        videoCallingViewModelkt.remoteUserJoined.observe(this, object : Observer<Int> {
            override fun onChanged(uid: Int) {
                Log.d("MainActivity", "remote joined user id: $uid")
                //                videoCallingViewModel.setRemoteView(callingView.getRemoteView(integer));
                if (!remoteUids.contains(uid)) {
                    remoteUids.add(uid)
                    adapter.notifyItemInserted(remoteUids.indexOf(uid))
                }
            }
        })

        videoCallingViewModelkt.remoteUserLeft.observe(this
        ) { uid ->
            Log.d("MainActivity", "remote left user id: $uid")
            if (remoteUids.contains(uid)) {
                val pos = remoteUids.indexOf(uid)
                remoteUids.remove(uid)
                adapter.notifyItemRemoved(pos)
            }
        }
    }

    private fun showToast(msg: String) = Toast.makeText(this, msg, Toast.LENGTH_SHORT).show()

    private fun requestPermissions() {
        ActivityCompat.requestPermissions(
            this,
            getRequiredPermissions(),
            PERMISSION_REQ_ID
        )
    }

    private fun checkPermissions(): Boolean {
        for (permission in getRequiredPermissions()) {
            if (ContextCompat.checkSelfPermission(
                    this,
                    permission
                ) != PackageManager.PERMISSION_GRANTED) {
                return false
            }
        }
        return true
    }

    private fun getRequiredPermissions(): Array<String> {
        return if (Build.VERSION.SDK_INT >= Build.VERSION_CODES.S) {
            arrayOf(
                Manifest.permission.RECORD_AUDIO,
                Manifest.permission.CAMERA,
                Manifest.permission.READ_PHONE_STATE,
                Manifest.permission.BLUETOOTH_CONNECT
            )
        } else {
            arrayOf(
                Manifest.permission.RECORD_AUDIO,
                Manifest.permission.CAMERA
            )
        }
    }

    override fun onRequestPermissionsResult(
        requestCode: Int,
        permissions: Array<out String>,
        grantResults: IntArray
    ) {
        super.onRequestPermissionsResult(requestCode, permissions, grantResults)

        if (requestCode == PERMISSION_REQ_ID && checkPermissions()) {
            videoCallingViewModelkt.startVideoCall(callingViewkt)
        }
    }

    override fun onDestroy() {
        super.onDestroy()

        remoteRecyler.adapter = null

        videoCallingViewModelkt.isCallEnded.removeObservers(this)
        videoCallingViewModelkt.isMicMute.removeObservers(this)
        videoCallingViewModelkt.isCameraOn.removeObservers(this)
        videoCallingViewModelkt.remoteUserJoined.removeObservers(this)
        videoCallingViewModelkt.remoteUserLeft.removeObservers(this)
        videoCallingViewModelkt.destroy()
    }
}