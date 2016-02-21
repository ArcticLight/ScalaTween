package me.arcticlight

/**
  * Created by Max on 2/20/2016.
  */
package object animations {

  /**
    * Clamp a value to ensure that it is between min and max
    * @param x The value
    * @param min The minimum that x should be
    * @param max The maximum that x should be
    * @return x, unless x is below the minimum or above the maximum. If it is, return min or max instead.
    */
  def clamp(x: Float, min: Float, max: Float) = if (x < min) min else if (x > max) max else x
  def clamp(x: Double, min: Double, max: Double) = if (x < min) min else if (x > max) max else x
  def clamp(x: Int, min: Int, max: Int) = if (x < min) min else if (x > max) max else x
}
