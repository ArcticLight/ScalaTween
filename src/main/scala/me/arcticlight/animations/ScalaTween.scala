package me.arcticlight.animations

import scala.language.{implicitConversions, existentials}

object ScalaTween {

  /**
    * Trait which defines scalar multiplication and vector addition for a type.
    * Any type which has these operations (and can therefore define VectorLike)
    * can be automatically turned into an Interpolatable.
    */
  trait VectorLike[T] extends Interpolatable[T] {
    /**
      * Defines Scalar Multiplication on a [[VectorLike]].
      *
      * Basically this should be the result of multiplying your type [[T]] by a Float.
      * So if I want <code>[[T]] * 0.36</code> that should have some meaning for your type.
      *
      * Additionally, it is <strong>an important implementation detail</strong> that
      * <code>add([[T]], mult([[T]], -1)) == Zero</code> (for whatever kind of object Zero is in [[T]]).
      *
      *
      * @param a The [[T]] to scale
      * @param b The float to scale by.
      * @return The new [[T]]
      */
    def mult(a: T, b: Float): T

    /**
      * Defines the addition of two objects of type [[T]]
      * @param a The first object to add
      * @param b The second object to add
      * @return <code>a + b</code>, or whatever that means for your type.
      */
    def add(a: T, b: T): T

    override def interp(a: T, b: T, much: Float): T = add(a, mult(add(a,mult(b,-1)),much))
  }

  /**
    * Defines the (linear) interpolation method, <code>interp(a, b, much)</code> for a type [[T]].
    *
    * @tparam T The type that has the interpolation ability
    */
  trait Interpolatable[T] {
    def interp(a: T, b: T, much: Float): T
  }

  object DefaultInterpolations {
    implicit object FloatIsVectorLike extends VectorLike[Float] {
      override def mult(a: Float, b: Float): Float = a * b
      override def add(a: Float, b: Float): Float = a + b
    }


    implicit object DoubleIsVectorLike extends VectorLike[Double] {
      override def mult(a: Double, b: Float): Double = a * b
      override def add(a: Double, b: Double): Double = a + b
    }

    //The default behavior is to use rounding when interpolating integers.
    implicit object IntIsVectorLikeWithRounding extends VectorLike[Int] {
      override def mult(a: Int, b: Float): Int = Math.round(a * b)
      override def add(a: Int, b: Int): Int = a + b
    }
  }

  /**
    * A generic target for Tween operations. Carries a var of the underlying value
    * so that the value can be manipulated by Tweens which only have a reference
    * to the AnimationTarget.
    *
    * This probably makes more sense with an example; you use AnimationTarget like this:
    * <code>
    *   val xPosition = AnimationTarget(0f) //Type is AnimationTarget[Float]
    *
    *   val tween = Tween(xPosition, 0f, 1f) //Tween from 0 -> 1
    *   tween.seek(.5) //Update the tween to be exactly half executed; the value of xPosition is now .5
    * </code>
    *
    * @param target The thing which is the subject of the target
    * @tparam T The type of the target
    */
  class AnimationTarget[T: Interpolatable](var target: T)
  implicit def AnimationTargetIsItself[T](x: AnimationTarget[T]): T = x.target
  object AnimationTarget{
    def apply[T: Interpolatable](target: T): AnimationTarget[T] = new AnimationTarget(target)
  }

  /**
    * Trait [[Animatable]] abstracts away the bare minimum functionality required to animate something.
    * An animation can be seeked and has a duration.
    *
    * The trait also defines a universal operation for applying an easing to an animation: the ease function
    * is sealed and applies an easing to the underlying Animatable's seekTo function.
    */
  trait Animatable {
    /**
      * The total duration of this Animatable. Negative numbers are not allowed.
      *
      * In the future, PositiveInfinity might be allowed in order to allow arbitrarily repeating
      * animations, but currently this is not allowed.
      */
    val duration: Float

    /**
      * Seek to a specific point in this animation, given as a float.
      * @param utime The exact point in time to seek to.
      */
    def seekTo(utime: Float): Unit
    sealed def ease(e: (Float, Float, Float, Float) => Float): Animatable = new WithEase(e)

    private class WithEase(e: (Float, Float, Float, Float) => Float) extends Animatable {
      override val duration: Float = Animatable.this.duration
      override def seekTo(utime: Float): Unit = {
        Animatable.this.seekTo(e(utime, 0, duration, duration))
      }
    }
  }

  class Tween[T: Interpolatable](val target: AnimationTarget[T],
                                 val start: T,
                                 val end: T,
                                 override val duration: Float = 1f) extends Animatable {
    def seekTo(utime: Float): Unit = {
      target.target = implicitly[Interpolatable[T]].interp(start, end, utime/duration)
    }
  }

  object Tween {
    def apply[T: Interpolatable](target: AnimationTarget[T],
                                 start: T,
                                 end: T,
                                 duration: Float = 1f): Tween[T] = new Tween(target, start, end, duration)
  }

  class SeqTimeline(val timeline: Seq[_ <: Animatable]) extends Animatable {
    require(timeline.hasDefiniteSize, "The Timeline Seq must be of finite size")
    override val duration = timeline.foldLeft(0f)((accum, element) => accum + element.duration)

    //dtable: Build a table of the start and end times of every animation in the timelines.
    //Speeds up future calculations.
    private[this] lazy val dtable = {
      val t: Seq[Float] = timeline.scanLeft[Float,Seq[Float]](0f)((accum, element) => accum + element.duration)
      timeline.zipWithIndex.map({
        case (x,i:Int) => (t(i), t(i) + x.duration)
      })
    }

    private var currentTime: Float = 0

    override def seekTo(utime: Float): Unit = {
      val qCurrentTime = currentTime
      this.currentTime = clamp(utime, 0, duration)
      val delta = currentTime - qCurrentTime
      if(delta == 0) return

      val lowTime = if (delta < 0) currentTime else qCurrentTime
      val highTime = if (delta < 0) qCurrentTime else currentTime

      val updateList = timeline.zipWithIndex.dropWhile({
        //Drop entries whose time is less than the lowTime;
        //they ended earlier than the region we care about
        case (x,i) =>
          val (_, endTime) = dtable(i)
          endTime < lowTime
      }).takeWhile({
        //Take only the entries whose startTime is less than
        //highTime; they started on or before we close the
        //time region we care about
        case (x,i) =>
          val (startTime, _) = dtable(i)
          startTime <= highTime
      })

      updateList.foreach {
        case (x,i) =>
          val (startTime, _) = dtable(i)
          x.seekTo(clamp(currentTime - startTime, 0, x.duration))
      }
    }
  }

  object SeqTimeline {
    def apply[A](timeline: A forSome { type A <: Animatable }*): SeqTimeline = new SeqTimeline(timeline)
  }


  class ParTimeline(val timeline: Seq[_ <: Animatable]) extends Animatable {
    require(timeline.hasDefiniteSize, "Timeline Seq must have definite size")
    override val duration = timeline.maxBy(_.duration).duration

    override def seekTo(utime: Float): Unit = {
      timeline.foreach(x =>
        x.seekTo(clamp(utime, 0, x.duration))
      )
    }
  }

  object ParTimeline {
    def apply[A](timeline: A forSome { type A <: Animatable }*): ParTimeline = new ParTimeline(timeline)
  }
}
