package simulator.ui

import simulator._
import scala.io.StdIn._

object TextUI extends App {

  val solarSystem = new SolarSystem
  this.run()

  /** Runs the simulation */
  def run() = {
    println(solarSystem.welcomeMessage)
    while (!solarSystem.isOver) {
      printInfo()
      act()
    }
    printInfo()
    println(solarSystem.goodbyeMessage)
  }

  /** Prints out a description of the current state of the solar system */
  private def printInfo() = {
    println("\n\n" + solarSystem.fullDescription)
  }

  /** Requests a command from the user and prints out a report of what happened */
  private def act() = {
    val command = readLine("Command: ")
    val report = solarSystem.act(command)
    if (report.nonEmpty) {
      println("\n" + report)
    }
  }

}
