package com.unclebobsdevblog.pattern

import java.nio.ByteBuffer
import java.security.MessageDigest

import org.apache.commons.codec.digest.DigestUtils
import scalafx.application
import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.canvas.Canvas
import scalafx.scene.paint.Color


object PatternDemo extends JFXApp {
  stage = new application.JFXApp.PrimaryStage {
    title.value = "com.unclebobsdevblog.pattern.PatternDemo"
    width = 400
    height = 400
    scene = new Scene {
      val md = MessageDigest.getInstance("SHA-1")
      val hash = DigestUtils.sha1Hex(System.currentTimeMillis().toString)
      val pattern = Patterns(hash)
      pattern.width <== width
      pattern.height <== height
      content = pattern
    }
  }
}
