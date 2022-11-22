package simulator
import scala.math._

case class Vector3D(x: Double, y: Double, z: Double) {

  def length = sqrt(x * x + y * y + z * z)

  def *(t: Double) = new Vector3D(t * x, t * y, t * z)

  def +(another: Vector3D) = new Vector3D(this.x + another.x, this.y + another.y, this.z + another.z)

  def -(another: Vector3D) = new Vector3D(this.x - another.x, this.y - another.y, this.z - another.z)

}