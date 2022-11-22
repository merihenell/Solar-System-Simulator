package simulator

import scala.math.pow

case class Euler(val body: Body, val otherBodies: Vector[Body]) extends IntegrationMethod {

  /** Computes the acceleration of the body due to other bodies */
  def acceleration = {
    otherBodies.map(b => (b.position - body.position) * (G * b.mass / pow(body.distance(b), 3))).reduce(_ + _)
  }

}