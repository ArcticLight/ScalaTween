package me.arcticlight.animations

object ScalaTween {
  trait TweenOps[T <: TweenOps[T]] {
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
     */
    def lerp(other: T, fraction: Float): T = {
      this * fraction + other * (1-fraction)
    }

    def lease(other: T, fraction: Float, fease: (Float) => Float) = {
      this + (other - this) * fease(fraction)
    }
  }

  class AnimationTarget[T <: TweenOps[T]](var value: T) {
  }
}