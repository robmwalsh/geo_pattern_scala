package com.unclebobsdevblog.pattern

import scalafx.application
import scalafx.application.JFXApp
import scalafx.scene.Scene
import scalafx.scene.canvas.Canvas
import scalafx.scene.paint.Color


object PatternDemo extends JFXApp {
  stage = new application.JFXApp.PrimaryStage {
    title.value = "com.unclebobsdevblog.pattern.PatternDemo"
    width = 800
    height = 800
    scene = new Scene {
      val pattern = Patterns("BFABE9711834AEEB96E5B6B3350FC8EB8B1D9D91")
      pattern.draw()
      content = pattern
    }
  }
}
