package me.arcticlight.animations

import me.arcticlight.animations.ScalaTween._
import me.arcticlight.animations.ScalaTween.DefaultInterpolations._
import org.scalatest._
import org.scalatest.junit.JUnitRunner
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class TweenSpec extends FlatSpec with Matchers {
  "A Tween" should "interpolate from 0 -> 1 in a full step" in {
    val v = AnimationTarget(0f)
    Tween(v, 0f, 1f).seekTo(1f)
    v.target shouldBe 1f
  }

  it should "interpolate from 0 -> .5 -> 1 (seeked halfway)" in {
    val v = AnimationTarget(0f)
    Tween(v, 0f, 1f).seekTo(.5f)
    v.target shouldBe   .5f
  }

  it should "interpolate from 0 -> .5 -> 1 (seeked twice)" in {
    val v = AnimationTarget(0f)
    val t = Tween(v, 0f, 1f)
    t.seekTo(.5f)
    v.target shouldBe   .5f
    t.seekTo(1f)
    v.target shouldBe   1f
  }

  "A SeqTimeline" should "interpolate (0,0,0) to (1,.5,0) when seeked halfway" in {
    val v1 = AnimationTarget(0f)
    val v2 = AnimationTarget(0f)
    val v3 = AnimationTarget(0f)
    val t = SeqTimeline(
      Tween(v1, 0f, 1f),
      Tween(v2, 0f, 1f),
      Tween(v3, 0f, 1f)
    )

    t.seekTo(t.duration/2f)

    v1.target shouldBe   1f
    v2.target shouldBe   .5f
    v3.target shouldBe   0f
  }

  it should "interpolate (0,0,0) to (1,1,.5) when seeked to 5/6ths" in {
    val v1 = AnimationTarget(0f)
    val v2 = AnimationTarget(0f)
    val v3 = AnimationTarget(0f)
    val t = SeqTimeline(
      Tween(v1, 0f, 1f),
      Tween(v2, 0f, 1f),
      Tween(v3, 0f, 1f)
    )

    t.seekTo(t.duration * (5f/6f))

    v1.target shouldBe   1f
    v2.target shouldBe   1f
    v3.target shouldBe   .5f
  }

  it should "interpolate (0,0,0) to (1,1,1) and back to (0,0,0) when seeked 0->1->0" in {
    val v1 = AnimationTarget(0f)
    val v2 = AnimationTarget(0f)
    val v3 = AnimationTarget(0f)
    val t = SeqTimeline(
      Tween(v1, 0f, 1f),
      Tween(v2, 0f, 1f),
      Tween(v3, 0f, 1f)
    )

    t.seekTo(t.duration)

    v1.target shouldBe   1f
    v2.target shouldBe   1f
    v3.target shouldBe   1f

    t.seekTo(0f)

    v1.target shouldBe 0f
    v2.target shouldBe 0f
    v3.target shouldBe 0f
  }

  it should "interpolate (0,0,0) to (1,1,0) on an edge" in {
    val v1 = AnimationTarget(0f)
    val v2 = AnimationTarget(0f)
    val v3 = AnimationTarget(0f)
    val t = SeqTimeline(
      Tween(v1, 0f, 1f),
      Tween(v2, 0f, 1f),
      Tween(v3, 0f, 1f)
    )

    t.seekTo(t.duration/3f*2f)

    v1.target shouldBe 1f
    v2.target shouldBe 1f
    v3.target shouldBe 0f
  }

  it should "interpolate (0,0,0)->(1,1,0)->(1,0,0) on animation edges" in {
    val v1 = AnimationTarget(0f)
    val v2 = AnimationTarget(0f)
    val v3 = AnimationTarget(0f)
    val t = SeqTimeline(
      Tween(v1, 0f, 1f),
      Tween(v2, 0f, 1f),
      Tween(v3, 0f, 1f)
    )

    t.seekTo(t.duration/3f*2f)
    t.seekTo(t.duration/3f)
    
    v1.target shouldBe 1f
    v2.target shouldBe 0f
    v3.target shouldBe 0f
  }
}