package me.arcticlight.animations

import me.arcticlight.animations.ScalaTween.{AnimationTarget, Tween}
import org.scalatest._

class TweenSpec extends fixture.FlatSpec with Matchers {
  type FixtureParam = AnimationTarget[Float]

  override def withFixture(test: OneArgTest) = {
    val target = new AnimationTarget(0.0f)
    try test(target)
  }

  "A Tween" should "interpolate a Float from 0->1" in { x =>
    new Tween(x, 0f, 1f).seekTo(1) shouldBe 1f
  }
}