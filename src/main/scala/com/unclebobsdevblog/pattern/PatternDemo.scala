package com.unclebobsdevblog.pattern

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
      val pattern = Patterns("0FABE9711834AEEB96E5B6B3350FC8EB8B1D9D91")
      pattern.width <== width
      pattern.height <== height
      content = pattern
    }
  }
}
