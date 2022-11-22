package simulator

import scala.collection.mutable.Buffer
import scala.math._
import java.awt.Graphics2D

class SolarSystem {

  val bodies = Buffer[Body]()

  /** Start with the default values for duration and time step length converted to seconds for the calculations */
  private var duration: Double = DefaultDuration * DaysToSeconds
  private var timeStep: Double = DefaultTimeStep * HoursToSeconds
  private var timeLeft = duration

  def welcomeMessage = "\nWelcome to solar system simulator. Use the help command to find out what actions are available."

  def goodbyeMessage = {
    "End. " + (if (timeLeft <= 0) "The simulated time frame ended." else "There was a collision.")
  }

  def fullDescription = {
    "Current state:\n" + (if (bodies.nonEmpty) bodies.foldLeft("\n")(_ + _.description) else "There are no bodies in the solar system.\n\n")
  }

  /** Sets the duration of the simulation given in days */
  def setDuration(newDuration: Double) = {
    duration = newDuration * DaysToSeconds
    timeLeft = duration
    s"Duration was set to $newDuration days."
  }

  /** Sets the step length for the simulation given in hours */
  def setTimeStep(newStep: Double) = {
    timeStep = newStep * HoursToSeconds
    s"Time step was set to $newStep hours."
  }

  /** Adds the bodies to the solar system */
  def addBodies(list: Buffer[Body]) = {
    for (b <- list) {
      bodies += b
    }
    val names = bodies.map(_.name).mkString(", ")
    s"Bodies $names were added to the solar system."
  }

  /** Adds a satellite to the solar system with the given mass (kg), position (m), and velocity (m/s),
    * and the default name "satellite" and radius equal to SatelliteRadius (100 m) */
  def addSatellite(mass: Double, position: Vector3D, velocity: Vector3D) = {
    bodies += new Body("satellite", mass, SatelliteRadius, position, velocity)
    "New satellite was added to the solar system."
  }

  /** Advances the simulation by moving all the bodies using the current time step length */
  def advance() = {
    val rk = bodies.map(b => RungeKutta(b, bodies.filterNot(_ == b).toVector, timeStep))
    (bodies zip rk.map(_.acceleration)).foreach( p => p._1.setAcceleration(p._2) )
    bodies.foreach(_.move(timeStep))
    timeLeft -= timeStep
  }

  /** Advances the simulation by specified number of times */
  def advanceTimes(n: Int) = {
    for (x <- 1 to n) {
      advance()
    }
    s"The solar system was advanced $n times."
  }

  /** Executes and returns the string response to the input command */
  def act(command: String) = {
    val action = new Action(command)
    val report = action.execute(this)
    report.getOrElse("Unknown command: \"" + command + "\".")
  }

  /** Checks if a collision has happended or the simulated time frame has ended */
  def isOver: Boolean = {
    for (b1 <- bodies) {
      for (b2 <- bodies) {
        if (b1 != b2 && b1.collidedWith(b2)) {
          return true
        }
      }
    }
    timeLeft <= 0
  }

  /** Calculates the ratio for scaling the distances for the GUI */
  lazy val ratio = (min(SimulationWidth / 2, Height / 2) - 20) / bodies.map(_.position.length).max

  /** Draws the solar system using a Graphics2D object */
  def draw(g: Graphics2D) = {
    bodies.foreach( b => b.draw(g, ratio) )
  }

}