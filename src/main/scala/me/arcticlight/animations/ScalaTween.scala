package me.arcticlight.animations
import scala.language.implicitConversions

object ScalaTween {
  trait TweenOps[T <: TweenOps[T]]  {
    /**
     * Scalar multiply (Tween operation) multiplies this value by a fraction and returns the result
     * @param fraction A [[Float]] value between 0 and 1, inclusive
     * @return a T scaled by multiplying it with the scalar `fraction` amount
     */
    def *(fraction: Float): T

    /**
     * Add (Tween operation) adds together this object and the parameter and returns the result
     * @param other Another [[T]] to add to this one
     * @return The result of adding together `this` and `other`
     */
    def +(other: T): T

    /**
     * Subtract (Tween operation) subtracts the parameter from this object and returns the result.
     * @param other Another [[T]] to subtract from this one
     * @return The result of subtracting `other` from `this`
     */
    def -(other: T): T

    /**
     * Perform linear interpolation using TweenOps.
     * @param other the other TweenOps with which to lerp
     */
    def lerp(other: T, fraction: Float): T
      = this * fraction + other * (1-fraction)

    def lease(other: T, fraction: Float, fease: (Float) => Float): T
      = this + (other - this) * fease(fraction)
  }

  /**
   * Implicitly convert any `TweenOps[T]` back to the underlying `T`.
   *
   * This implicit conversion is basically a reminder to the compiler
   * that any `TweenOps` for some `T <: TweenOps` is also a `T`, which the
   * compiler can't seem to prove on its' own. Since the type param on
   * `TweenOps` gets erased at compile-time, this function never actually
   * gets called.
   *
   * Also, note that while IntelliJ thinks this is wrong, the actual Scala
   * compiler understands it. Probably should disable that warning.
   *
   * @param ops an instance of [[TweenOps]]
   * @tparam T the underlying type on which that [[TweenOps]] operates
   * @return the given `TweenOps`, but converted so that the compiler
   *         can understand it as a `T`
   */
  implicit def unwrapTweenOps[T <: TweenOps[T]](ops: TweenOps[T]): T = ops

  /**
   * Unpack a [[TwopsyNumeric]] to get back the underlying [[Numeric]].
   *
   * @param ops A [[TwopsyNumeric]]
   * @tparam A the underlying type `A : Numeric`
   * @return the underlying value wrapped by `ops`
   */
  implicit def detwopsNumeric[A](ops: TwopsyNumeric[A]): A = ops.value

  /**
   * Implicitly adds [[TweenOps]][A] to any `A` with [[Numeric]].
   *
   * This is possible because any typeclass implementing [[Numeric]] should
   * already have `+`, `-`, and `*` operations that [[TweenOps]] can use.
   *
   * @param value the underlying numeric value
   * @tparam A a type extending [[Numeric]]
   * @author Hawk Weisman
   */
  implicit class TwopsyNumeric[A : Numeric](val value: A)
  extends TweenOps[TwopsyNumeric[A]] {
    // Basically, just wrap all of the underlying numeric value's
    // pre-existing mathematical operations. I wish this wasn't necessary,
    // but apparently Scalac is not quite smart enough to construct a type
    // proof that         +, - :: (Numeric a) => a -> a -> a
    // is equivalent to   +, - :: (TweenOps a) => a -> a -> a.
    //
    // So we have to do it this way.
    //  – Hawk
    override def *(fraction: Float): TwopsyNumeric[A]
      = value * fraction
    override def +(other: TwopsyNumeric[A]): TwopsyNumeric[A]
      = value + other
    override def -(other: TwopsyNumeric[A]): TwopsyNumeric[A]
      = value - other

  }
  class AnimationTarget[T <: TweenOps[T]](var value: T) {
  }
}
