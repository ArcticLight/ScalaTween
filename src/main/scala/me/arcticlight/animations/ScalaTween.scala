package me.arcticlight.animations
import scala.language.implicitConversions

object ScalaTween {

  trait TweenOps[T]  {
    /**
     * Scalar multiply (Tween operation) multiplies this value by a fraction and returns the result
     * @param a The value to multiply
     * @param f A [[Float]] value between 0 and 1, inclusive
     * @return a T scaled by multiplying it with the scalar `fraction` amount
     */
    def mult(a: T, f: Float): T

    /**
     * Add (Tween operation) adds together this object and the parameter and returns the result
     * @param a The first value to work with
     * @param b Another [[T]] to add to this one
     * @return The result of adding together `a` and `b`
     */
    def add(a: T, b: T): T

    /**
     * Subtract (Tween operation) subtracts the parameter from this object and returns the result.
     * @param a The first value to work with
     * @param b Another [[T]] to subtract from this one
     * @return The result of subtracting `b` from `a`
     */
    def subt(a: T, b: T): T

    /**
     * Perform linear interpolation using TweenOps.
     * @param a The value to start with
     * @param b the other [[T]] with which to lerp to
     */
    def lerp(a: T, b: T, fraction: Float): T
      = add(a, mult(subt(b,a), fraction))

    def withEase(ease: (Float) => Float): WithEase = new WithEase(ease)

    class WithEase(ease: (Float) => Float) extends TweenOps[T] {
      /**
       * Scalar multiply (Tween operation) multiplies this value by a fraction and returns the result
       * @param a The value to multiply
       * @param f A [[Float]] value between 0 and 1, inclusive
       * @return a T scaled by multiplying it with the scalar `f` amount
       */
      override def mult(a: T, f: Float): T = TweenOps.this.mult(a,f)

      /**
       * Subtract (Tween operation) subtracts the parameter from this object and returns the result.
       * @param a The first value to work with
       * @param b Another [[T]] to subtract from this one
       * @return The result of subtracting `b` from `a`
       */
      override def subt(a: T, b: T): T = TweenOps.this.subt(a,b)

      /**
       * Add (Tween operation) adds together this object and the parameter and returns the result
       * @param a The first value to work with
       * @param b Another [[T]] to add to this one
       * @return The result of adding together `a` and `b`
       */
      override def add(a: T, b: T): T = TweenOps.this.add(a,b)

      override def lerp(a: T, b: T, f: Float): T = add(a, mult(subt(b,a), ease(f)))
    }
  }

  implicit class FloatHasTweenOps(value: Float) extends TweenOps[Float] {
    /**
     * Scalar multiply (Tween operation) multiplies this value by a fraction and returns the result
     * @param a The value to multiply
     * @param f A [[Float]] value between 0 and 1, inclusive
     * @return a T scaled by multiplying it with the scalar `fraction` amount
     */
    override def mult(a: Float, f: Float): Float = a * f

    /**
     * Subtract (Tween operation) subtracts the parameter from this object and returns the result.
     * @param a The first value to work with
     * @param b Another [[Float]] to subtract from this one
     * @return The result of subtracting `b` from `a`
     */
    override def subt(a: Float, b: Float): Float = a * b

    /**
     * Add (Tween operation) adds together this object and the parameter and returns the result
     * @param a The first value to work with
     * @param b Another [[Float]] to add to this one
     * @return The result of adding together `this` and `other`
     */
    override def add(a: Float, b: Float): Float = a + b
  }

  class AnimationTarget[T](var value: T)(implicit to: TweenOps[T]) extends TweenOps[T] {
    /**
     * Scalar multiply (Tween operation) multiplies this value by a fraction and returns the result
     * @param a The value to multiply
     * @param f A [[Float]] value between 0 and 1, inclusive
     * @return a T scaled by multiplying it with the scalar `fraction` amount
     */
    override def mult(a: T, f: Float): T = to.mult(a,f)

    /**
     * Subtract (Tween operation) subtracts the parameter from this object and returns the result.
     * @param a The first value to work with
     * @param b Another [[T]] to subtract from this one
     * @return The result of subtracting `b` from `a`
     */
    override def subt(a: T, b: T): T = to.subt(a,b)

    /**
     * Add (Tween operation) adds together this object and the parameter and returns the result
     * @param a The first value to work with
     * @param b Another [[T]] to add to this one
     * @return The result of adding together `a` and `b`
     */
    override def add(a: T, b: T): T = to.add(a,b)

    def +(b: T): T = add(this.value, b)
    def -(b: T): T = subt(this.value,b)
    def *(f: Float): T = mult(this.value, f)

    def +=(b: T): Unit = {
      this.value = add(this.value, b)
    }

    def -=(b: T): Unit = {
      this.value = subt(this.value, b)
    }

    def *=(f: Float): Unit = {
      this.value = mult(this.value,f)
    }
  }

  def main(args: Array[String]) {
    new AnimationTarget[Float](1.2f)
  }
}
