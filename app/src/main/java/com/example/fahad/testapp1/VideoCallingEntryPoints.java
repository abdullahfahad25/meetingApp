package com.example.fahad.testapp1;

import dagger.hilt.EntryPoint;
import dagger.hilt.InstallIn;
import dagger.hilt.components.SingletonComponent;

@EntryPoint
@InstallIn(SingletonComponent.class)
public interface VideoCallingEntryPoints {
    VideoCallingSDK getSDK();
    VideoCallingSDKkt getSDKkt();
    VideoCallingSDKManager getSDKManager();
}
