package me.arcticlight.animations

trait TweenOps[A]  {
  /**
   * Scalar multiply (Tween operation) multiplies this value by a fraction
   * and returns the result
   * @param a The value to multiply
   * @param f A [[Float]] value between 0 and 1, inclusive
   * @return a T scaled by multiplying it with the scalar `fraction` amount
   */
  def mult[B : Fractional](a: A, b: B): A

  /**
   * Add (Tween operation) adds together this object and the parameter and
   * returns the result
   * @param a The first value to work with
   * @param b Another [[T]] to add to this one
   * @return The result of adding together `a` and `b`
   */
  def add(a: A, b: A): A

  /**
   * Subtract (Tween operation) subtracts the parameter from this object
   * and returns the result.
   * @param a The first value to work with
   * @param b Another [[T]] to subtract from this one
   * @return The result of subtracting `b` from `a`
   */
  def subt(a: A, b: A): A

  /**
   * Perform linear interpolation using TweenOps.
   * @param a The value to start with
   * @param b the other [[T]] with which to lerp to
   */
  def lerp(a: A, b: A, fraction: Float): A
    = add(a, mult(subt(b,a), fraction))

  def withEase(ease: (Float) => Float): WithEase = new WithEase(ease)

  class WithEase(ease: (Float) => Float) extends TweenOps[A] {
    /**
     * Scalar multiply (Tween operation) multiplies this value by a fraction
     * and returns the result
     * @param a The value to multiply
     * @param f A [[Float]] value between 0 and 1, inclusive
     * @return a T scaled by multiplying it with the scalar `f` amount
     */
    override def mult[B : Fractional](a: A, b: B): A
      = TweenOps.this.mult(a,b)

    /**
     * Subtract (Tween operation) subtracts the parameter from this object
     * and returns the result.
     * @param a The first value to work with
     * @param b Another [[T]] to subtract from this one
     * @return The result of subtracting `b` from `a`
     */
    override def subt(a: A, b: A): A
      = TweenOps.this.subt(a,b)

    /**
     * Add (Tween operation) adds together this object and the parameter
     * and returns the result
     * @param a The first value to work with
     * @param b Another [[T]] to add to this one
     * @return The result of adding together `a` and `b`
     */
    override def add(a: A, b: A): A
      = TweenOps.this.add(a,b)

    override def lerp(a: A, b: A, f: Float): A
      = add(a, mult(subt(b,a), ease(f)))
  }
}
object TweenOps {
  implicit object FloatHasTweenOps extends TweenOps[Float] {
    /**
     * Scalar multiply (Tween operation) multiplies this value by a fraction
     * and returns the result
     * @param a The value to multiply
     * @param f A [[Float]] value between 0 and 1, inclusive
     * @return a T scaled by multiplying it with the scalar `fraction` amount
     */
    override def mult[B : Fractional](a: Float, b: B): Float
      = a * implicitly[Numeric[B]].toFloat(b)

    /**
     * Subtract (Tween operation) subtracts the parameter from this object
     * and returns the result.
     * @param a The first value to work with
     * @param b Another [[Float]] to subtract from this one
     * @return The result of subtracting `b` from `a`
     */
    override def subt(a: Float, b: Float): Float = a * b

    /**
     * Add (Tween operation) adds together this object and the parameter and
     * returns the result
     * @param a The first value to work with
     * @param b Another [[Float]] to add to this one
     * @return The result of adding together `this` and `other`
     */
    override def add(a: Float, b: Float): Float = a + b
  }
}
