package simulator

import scala.math.pow

case class RungeKutta(val body: Body, val otherBodies: Vector[Body], val dt: Double) extends IntegrationMethod {

  /** Computes the acceleration of the body due to other bodies using the fourth order Runge-Kutta algorithm */
  def acceleration = {
    var a = Vector3D(0, 0, 0)
    for (b <- otherBodies) {
      val factor = G * b.mass
      var s = Vector3D(0, 0, 0)
      var v = Vector3D(0, 0, 0)
      val a1 = (b.position - body.position) * (factor / pow(body.distance(b), 3))
      v = body.velocity + (a1 * 0.5 * dt)
      s = body.position + (v * 0.5 * dt)
      val a2 = (b.position - s) * (factor / pow((b.position - s).length, 3))
      v = body.velocity + (a2 * 0.5 * dt)
      s = body.position + (v * 0.5 * dt)
      val a3 = (b.position - s) * (factor / pow((b.position - s).length, 3))
      v = body.velocity + (a3 * dt)
      s = body.position + (v * dt)
      val a4 = (b.position - s) * (factor / pow((b.position - s).length, 3))
      a = a + ((a1 + a2 * 2 + a3 * 2 + a4) * (1.0 / 6.0))
    }
    a
  }

}