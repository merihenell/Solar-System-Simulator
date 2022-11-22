package simulator

class Action(input: String) {

  val command = input.trim.toLowerCase
  val verb = command.takeWhile(_ != ' ')
  val userInput = command.drop(verb.length).trim

  /** Converts the given string into a double, if possible */
  def toDouble(string: String): Double = {
    val double = string.trim.toDoubleOption
    if (double.isEmpty) {
      throw new InvalidInputException("Invalid input. Input should be numbers.")
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

  /** Converts the given string to a tuple representing the mass, position, and velocity of a body, if possible */
  def toInputs(string: String): (Double, Vector3D, Vector3D) = {
    val inputs = string.split(";")
    if (inputs.length != 3) {
      throw new InvalidInputException("Invalid input. Input should be of the form [mass]; [position]; [velocity].")
    }
    (toDouble(inputs(0)), toVector(inputs(1)), toVector(inputs(2)))
  }

  /** Causes the command to be executed, if possible, returning a description of what happened */
  def execute(system: SolarSystem): Option[String] = {
    try {
      this.verb match {
        case "help" => Some("Available commands:\n" +
                            s"duration [duration (d)] -> set the duration of the simulation (default value is $DefaultDuration days)\n" +
                            s"timestep [step length (h)] -> set the time step of the simulation (default value is $DefaultTimeStep hours)\n" +
                            "addFrom [file] -> add bodies to the solar system\n" +
                            "addSatellite [mass (kg)]; [position (m) as a vector]; [velocity (m/s) as a vector] -> add a satellite to the solar system\n" +
                            "advance [n] -> advance the solar system n times")
        case "duration" => Some(system.setDuration(toDouble(userInput)))
        case "timestep" => Some(system.setTimeStep(toDouble(userInput)))
        case "addfrom" => Some(system.addBodies(FileReader.readFile(userInput)))
        case "addsatellite" => {
          val result = toInputs(userInput)
          Some(system.addSatellite(result._1, result._2, result._3))
        }
        case "advance" => Some(system.advanceTimes(toDouble(userInput).toInt))
        case other => None
      }
    } catch {
      case InvalidInputException(text) => Some(text)
    }
  }

}
