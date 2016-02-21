package me.arcticlight.animations

/**
  * Created by Max on 2/20/2016.
  */
object Eases {
  final def EaseInQuad(t: Float, b: Float, c: Float, d: Float): Float = {
    val f = t/d
    c*f*f + b
  }

  final def EaseOutQuad(t: Float, b: Float, c: Float, d: Float): Float = {
    val f = t/d
    -c * f * (f-2) + b
  }

  final def EaseInOutQuad(t: Float, b: Float, c: Float, d: Float): Float = {
    val e = t/(d/2f)
    val f = if (e < 1) e - 1 else e
    if (e < 1) c/2*f*f + b else -c/2 * (t*(t-2) - 1) + b
  }

  final def EaseOutElastic(t: Float, b: Float, c: Float, d: Float): Float = {
    if (t==0) return b else if ((t/d)==1) return b+c
    val f = t/d
    val p = d*.3f
    val s = p/4
    c*Math.pow(2,-10*f).toFloat * Math.sin( (f*d-s)*(2*Math.PI.toFloat)/p).toFloat + c + b
  }
}
