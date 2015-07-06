package me.arcticlight.animations

import me.arcticlight.animations.TweenOps.FloatHasTweenOps

import scala.language.implicitConversions

object ScalaTween {
  // TODO: finish this, requires TweenOps.mult() to take a
  //Fractional or Numeric as the left scalar.
  // class FractionalsAreTweenOpsable[A : Fractional]
  // extends TweenOps[A] {
  //   /**
  //    * Scalar multiply (Tween operation) multiplies this value by a fraction
  //    * and returns the result
  //    * @param a The value to multiply
  //    * @param f A [[Float]] value between 0 and 1, inclusive
  //    * @return a T scaled by multiplying it with the scalar `fraction` amount
  //    */
  //   override def mult(a: A, b: A): A
  //     = implicitly[Fractional[A]].times(a,b)
  //
  //   /**
  //    * Subtract (Tween operation) subtracts the parameter from this object
  //    * and returns the result.
  //    * @param a The first value to work with
  //    * @param b Another [[Float]] to subtract from this one
  //    * @return The result of subtracting `b` from `a`
  //    */
  //   override def subt(a: A, b: A): A
  //     = implicitly[Fractional[A]].minus(a,b)
  //
  //   /**
  //    * Add (Tween operation) adds together this object and the parameter and
  //    * returns the result
  //    * @param a The first value to work with
  //    * @param b Another [[Float]] to add to this one
  //    * @return The result of adding together `this` and `other`
  //    */
  //   override def add(a: A, b: A): A
  //     = implicitly[Fractional[A]].plus(a,b)
  // }

  class AnimationTarget[A : TweenOps](var value: A)
  extends TweenOps[A] {
    /**
     * Scalar multiply (Tween operation) multiplies this value by a fraction
     * and returns the result
     * @param a The value to multiply
     * @param f A [[Float]] value between 0 and 1, inclusive
     * @return a T scaled by multiplying it with the scalar `fraction` amount
     */
    override def mult[B : Fractional](a: A, b: B): A
      = implicitly[TweenOps[A]].mult(a,b)

    /**
     * Subtract (Tween operation) subtracts the parameter from this object
     * and returns the result.
     * @param a The first value to work with
     * @param b Another [[T]] to subtract from this one
     * @return The result of subtracting `b` from `a`
     */
    override def subt(a: A, b: A): A
      = implicitly[TweenOps[A]].subt(a,b)

    /**
     * Add (Tween operation) adds together this object and the parameter
     * and returns the result
     * @param a The first value to work with
     * @param b Another [[T]] to add to this one
     * @return The result of adding together `a` and `b`
     */
    override def add(a: A, b: A): A
      = implicitly[TweenOps[A]].add(a,b)

    def +(b: A): A = add(this.value, b)
    def -(b: A): A = subt(this.value,b)
    def *[B : Fractional](b: B): A = mult(this.value, b)

    def +=(b: A): Unit = {
      this.value = add(this.value, b)
    }

    def -=(b: A): Unit = {
      this.value = subt(this.value, b)
    }

    def *=[B : Fractional](b: B): Unit = {
      this.value = mult(this.value,b)
    }
  }

  def main(args: Array[String]) {
    new AnimationTarget[Float](1.2f)
  }
}
