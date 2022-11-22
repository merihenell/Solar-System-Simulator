package simulator

import java.awt.{Color, Graphics2D}
import java.awt.geom._
import java.awt.Polygon
import scala.math._

class Body(val name: String, val mass: Double, val radius: Double, val initialPosition: Vector3D, val initialVelocity: Vector3D) {

  /** Initialize the position, velocity, and acceleration */
  private var s = initialPosition
  private var v = initialVelocity
  private var a = Vector3D(0, 0, 0)

  def position = s
  def velocity = v

  def acceleration = a
  def setAcceleration(acceleration: Vector3D) = {
    a = acceleration
  }

  def description = {
    name + s":\n  position: (${position.x}, ${position.y}, ${position.z})\n  velocity: (${velocity.x}, ${velocity.y}, ${velocity.z})\n  acceleration: (${acceleration.x}, ${acceleration.y}, ${acceleration.z})\n\n"
  }

  /** Computes the distance between this body and another body */
  def distance(another: Body) = (this.position - another.position).length

  /** Updates the position and velocity of the body using the given time step */
  def move(dt: Double) = {
    v = v + (a * dt)
    s = s + (v * dt)
  }

  /** Checks if this body has collided with another body */
  def collidedWith(another: Body) = this.distance(another) <= (this.radius + another.radius)

  /** Draws this body and arrows representing the velocity and acceleration using a Graphics2D object and ratio for scaling down the values */
  def draw(g: Graphics2D, ratio: Double) = {
    def scale(value: Double) = ratio * value
    def drawArrow(g: Graphics2D, x1: Double, y1: Double, x2: Double, y2: Double, size: Double) = {
      val dx = x2 - x1
      val dy = y2 - y1
      val D = sqrt(dx * dx + dy * dy)
      val sin = dy / D
      val cos = dx / D
      val xm = (D - size) * cos - size * sin + x1
      val ym = (D - size) * sin + size * cos + y1
      val xn = (D - size) * cos + size * sin + x1
      val yn = (D - size) * sin - size * cos + y1
      g.draw(new Line2D.Double(x1, y1, x2, y2))
      g.fill(new Polygon(Array(x2.toInt, xm.toInt, xn.toInt), Array(y2.toInt, ym.toInt, yn.toInt), 3))
    }
    val r = (if (scale(radius) >= (MinRadius / ScaleR) && scale(radius) <= (MaxRadius / ScaleR)) scale(radius) * ScaleR else if (scale(radius) < (MinRadius / ScaleR)) MinRadius else MaxRadius)
    val x1 = scale(s.x) + (SimulationWidth / 2)
    val y1 = scale(s.y) + (Height / 2)
    val x2 = x1 + (scale(v.x) * ScaleV)
    val y2 = y1 + (scale(v.y) * ScaleV)
    val x3 = x1 + (scale(a.x) * ScaleA)
    val y3 = y1 + (scale(a.y) * ScaleA)
    name.trim.toLowerCase match {
      case "sun" => g.setColor(new Color(252, 212, 64))
      case "star" => g.setColor(new Color(255, 110, 0))
      case "mercury" => g.setColor(new Color(173, 168, 165))
      case "venus" => g.setColor(new Color(211, 165, 103))
      case "earth" => g.setColor(new Color(107, 147, 214))
      case "mars" => g.setColor(new Color(193, 68, 14))
      case "moon" => g.setColor(Color.gray)
      case "satellite" => g.setColor(Color.white)
      case "asteroid" => g.setColor(Color.darkGray)
      case _ => g.setColor(new Color(197, 171, 110))
    }
    g.fill(new Ellipse2D.Double(x1 - r, y1 - r, 2 * r, 2 * r))
    g.setColor(Color.black)
    if ((v * ratio * ScaleV).length > 1) {
      drawArrow(g, x1, y1, x2, y2, ArrowSize)
    }
    if ((a * ratio * ScaleA).length > 1) {
      drawArrow(g, x1, y1, x3, y3, ArrowSize)
    }
  }

}