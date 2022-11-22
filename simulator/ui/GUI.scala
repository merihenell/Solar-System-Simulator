package simulator.ui

import simulator._

import java.awt.event.ActionListener
import java.awt.{Color, Graphics2D, RenderingHints}
import javax.swing.UIManager
import scala.swing._
import scala.swing.event.ButtonClicked

object GUI extends SimpleSwingApplication {

  UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName)

  /** Converts the given string into a double, if possible */
  def toDouble(string: String): Double = {
    val double = string.trim.toDoubleOption
    if (double.isEmpty) {
      throw new InvalidInputException("Invalid input.")
    }
    double.get
  }

  /** Converts the given string into an instance of Vector3D, if possible */
  def toVector(string: String): Vector3D = {
    val coordinates = string.trim.init.tail.split(",").map(_.trim)
    if (coordinates.length != 3) {
      throw new InvalidInputException("Invalid input. The position and velocity should be given as vectors of the form (x, y, z).")
    }
    Vector3D(toDouble(coordinates(0)), toDouble(coordinates(1)), toDouble(coordinates(2)))
  }

  val solarSystem = new SolarSystem

  def top: MainFrame = new MainFrame {

    title = "Solar System Simulator"
    resizable = false
    preferredSize = new Dimension(FullWidth, Height + 36)

    /** Simulation: */

    val simulation = new Panel {
      override def paintComponent(g: Graphics2D) = {
        g.setColor(new Color(29, 41, 81))
        g.fillRect(0, 0, SimulationWidth, Height)

        g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)

        solarSystem.draw(g)
      }
      preferredSize = new Dimension(SimulationWidth, Height)
    }

    /** Inputs: */

    val mass = new TextField("", 15)
    val position = new TextField("", 15)
    val velocity = new TextField("", 15)

    val addSatelliteButton = new Button("Add satellite")

    val duration = new TextField(DefaultDuration.toString, 15)
    val timeStep = new TextField(DefaultTimeStep.toString, 15)
    val file = new TextField("simple-solar-system.json", 15)

    val startButton = new Button("Start simulation")

    /** Layout: */

    val inputs = new GridBagPanel {
      def constraints(x: Int, y: Int, gridwidth: Int = 1, gridheight: Int = 1,
                      weightx: Double = 0.0, weighty: Double = 0.0,
                      fill: GridBagPanel.Fill.Value = GridBagPanel.Fill.None,
                      insets: Insets = new Insets(2, 2, 2, 2)): Constraints = {
        val c = new Constraints
        c.gridx = x
        c.gridy = y
        c.gridwidth = gridwidth
        c.gridheight = gridheight
        c.weightx = weightx
        c.weighty = weighty
        c.fill = fill
        c.insets = insets
        c
      }
      add(new Label("Mass (kg):"), constraints(0, 0))
      add(mass, constraints(1, 0))
      add(new Label("Position (m):"), constraints(0, 1))
      add(position, constraints(1, 1))
      add(new Label("Velocity (m/s):"), constraints(0, 2))
      add(velocity, constraints(1, 2))
      add(addSatelliteButton, constraints(0, 3, gridwidth = 2, fill = GridBagPanel.Fill.Horizontal, insets = new Insets(10, 2, 40, 2)))
      add(new Label("Duration (d):"), constraints(0, 4))
      add(duration, constraints(1, 4))
      add(new Label("Time step (h):"), constraints(0, 5))
      add(timeStep, constraints(1, 5))
      add(new Label("File name:"), constraints(0, 6))
      add(file, constraints(1, 6))
      add(startButton, constraints(0, 7, gridwidth = 2, fill = GridBagPanel.Fill.Horizontal, insets = new Insets(10, 2, 2, 2)))
    }

    val frame = new BoxPanel(Orientation.Horizontal) {
      contents += simulation
      contents += inputs
    }

    contents = frame

    Dialog.showMessage(frame, "A satellite can be added by defining its mass,\nand position and velocity in vector form\nand then pressing the \"Add satellite\" button.\n" +
                              "The default duration, time step length, and file\ncontaining the initial states of the bodies can be\nmodified by providing new input values and then\npressing the \"Start simulation\" button.", "For the user")

    /** Events: */

    val timer = new javax.swing.Timer(4, null)
    timer.addActionListener(new ActionListener() {
      def actionPerformed(e: java.awt.event.ActionEvent) = {
        solarSystem.advance()
        simulation.repaint()
        if (solarSystem.isOver) {
          timer.stop()
          Dialog.showMessage(frame, "Simulation ended.", "End")
        }
      }
    })

    listenTo(addSatelliteButton)
    listenTo(startButton)

    reactions += {
      case clickEvent: ButtonClicked if clickEvent.source == addSatelliteButton =>
        try {
          val m = toDouble(mass.text)
          val s = toVector(position.text)
          val v = toVector(velocity.text)
          solarSystem.addSatellite(m, s, v)
        } catch {
          case InvalidInputException(text) => Dialog.showMessage(frame, text, "Error")
        }
      case clickEvent: ButtonClicked if clickEvent.source == startButton =>
        try {
          val d = toDouble(duration.text)
          val t = toDouble(timeStep.text)
          solarSystem.setDuration(toDouble(duration.text))
          solarSystem.setTimeStep(toDouble(timeStep.text))
          solarSystem.addBodies(FileReader.readFile(file.text))
          timer.start()
        } catch {
          case InvalidInputException(text) => Dialog.showMessage(frame, text, "Error")
        }
    }

  }

}
