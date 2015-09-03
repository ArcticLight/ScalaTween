package me.arcticlight.animations

import scala.runtime.RichFloat


object ScalaTween {

  /**
   * TweenOps defines the verious bare-minimum operations that a type needs to have
   * in order to have linear interpolation applied to it. Specifically, we need
   * scalar multiplication, addition, and subtraction.
   *
   * In general, an implementor of TweenOps only needs to provide the mult, sub, and add operations.
   *
   * A default <code>interp()</code> implementation is provided, and <b>this method should be used</b>
   * when performing interpolation with a [[TweenOps]], rather than constructing the default linear interpolation
   * formula manually with mult, add, and sub. This is for those cases where the interpolation is NOT linear,
   * such as when interpolating between color values, which requires a specific conversion in order to
   * preserve variables such as brightness during animation.
   * @tparam T The type that this TweenOps applies to
   */
  trait TweenOps[T] extends Any {
    def mult(a: T, b: Float): T

    def sub(a: T, b: T): T

    def add(a: T, b: T): T

    def interp(a: T, b: T, much: Float) = add(a, mult(sub(b,a), much))
  }

  object TweenOps {
    implicit object FloatIsTweenOps extends TweenOps[Float] {
      override def mult(a: Float, b: Float): Float = a * b
      override def sub(a: Float, b: Float): Float = a - b
      override def add(a: Float, b: Float): Float = a + b
    }


    implicit object DoubleIsTweenOps extends TweenOps[Double] {
      override def mult(a: Double, b: Float): Double = a * b
      override def sub(a: Double, b: Double): Double = a - b
      override def add(a: Double, b: Double): Double = a + b
    }

    //The default behavior is to use rounding when interpolating integers.
    implicit object IntIsTweenOpsWithRounding extends TweenOps[Int] {
      override def mult(a: Int, b: Float): Int = Math.round(a * b)
      override def sub(a: Int, b: Int): Int = a - b
      override def add(a: Int, b: Int): Int = a + b
    }
  }

  /**
   * A generic *target* for Tween Operations that handles
   * Tween Operations for an underlying value.
   * @param target The target which is wrapped by this AnimationTarget
   * @tparam T The raw type of the target, for which there are TweenOps available for it.
   */
  case class AnimationTarget[T: TweenOps](var target: T)
  implicit def AnimationTargetIsItself[T](x: AnimationTarget[T]): T = x.target

  private def clamp(x: Float, min: Float, max: Float) = if (x < min) min else if (x > max) max else x

  trait AnimationOps {
    val cycleDuration: Float
    val cycles: Int
    def seekTo(utime: Float): Unit
    def duration: Float = cycleDuration * cycles

    def ease(e: Float => Float): AnimationOps = new WithEase(e)

    private class WithEase(e: Float => Float) extends AnimationOps {
      override val cycleDuration = AnimationOps.this.cycleDuration
      override val cycles = AnimationOps.this.cycles
      override def seekTo(utime:Float): Unit = {
        AnimationOps.this.seekTo(e(clamp(utime, 0, super.duration)/super.duration)*super.duration)
      }
    }
  }

  class Tween[T: TweenOps](val target: AnimationTarget[T],
                           val start: T,
                           val end: T,
                           override val cycleDuration: Float = 1,
                           override val cycles: Int = 1)
  extends AnimationOps {
    lazy val looping: Boolean = cycles > 1

    def seekTo(utime: Float): Unit = {
      target.target = implicitly[TweenOps[T]].interp(start, end, (clamp(utime, 0, duration)%cycleDuration)/cycleDuration)
    }
  }

  class SeqTimeline(val timeline: Seq[_ <: AnimationOps], override val cycles: Int = 1) extends AnimationOps {
    require(timeline.hasDefiniteSize, "The Timeline Seq must be of finite size")
    override val cycleDuration = timeline.foldLeft(0f)((accum, elem) => accum + elem.duration)

    private[this] val dtable = {
      val t: Seq[Float] = Seq[Float](0f) ++ timeline.scanLeft[Float,Seq[Float]](0f)((acc, x) => acc+x.duration)
      timeline.zipWithIndex.map({case (x,i:Int) => (t(i), t(i) + x.duration)})
    }

    private var currentTime: Float = 0
    override def seekTo(utime: Float): Unit = {
      val htime = clamp(utime, 0, duration)%cycleDuration
      timeline.zipWithIndex.dropWhile {
                case (x,i) =>
                  val (startTime,_) = dtable(i)
                  startTime <= utime
              }
              .takeWhile {
                case (x,i) =>
                  val (_, endTime) = dtable(i)
                  utime <= endTime
              }
              .foreach {
                case (x,i) =>
                  val (startTime, _) = dtable(i)
                  x.seekTo(htime - startTime)
              }
    }
  }

  class ParTimeline(val timeline: Seq[_ <: AnimationOps], override val cycles: Int = 1) extends AnimationOps {
    require(timeline.hasDefiniteSize, "The Timeline Seq must be of finite size")
    override val cycleDuration = timeline.maxBy(_.duration).duration

    private var currentTime: Float = 0

    override def seekTo(utime: Float): Unit = {
      val htime = clamp(utime, 0, duration)%cycleDuration
      timeline.foreach{_.seekTo(htime)}
    }
  }
}
