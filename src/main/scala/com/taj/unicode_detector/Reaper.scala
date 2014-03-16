/*
 * The MIT License (MIT)
 *
 * Copyright (c) 2014. TAJ - Société d'avocats
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 *
 * EXCEPT AS CONTAINED IN THIS NOTICE, THE NAME OF TAJ - Société d'avocats SHALL
 * NOT BE USED IN ADVERTISING OR OTHERWISE TO PROMOTE THE SALE, USE OR OTHER
 * DEALINGS IN THIS SOFTWARE WITHOUT PRIOR WRITTEN AUTHORIZATION FROM
 * TAJ - Société d'avocats.
 */

package com.taj.unicode_detector

import akka.actor._
import scala.collection.mutable.ArrayBuffer
import com.typesafe.scalalogging.slf4j.Logging
import com.taj.unicode_detector.ActorLifeOverview.RegisterRootee
import com.taj.unicode_detector.ActorLifeOverview.RegisterRooter
import akka.actor.Terminated
import scala.Some
import com.taj.unicode_detector.ActorLifeOverview.KillAkka

object ActorLifeOverview {

  case class StartRegistration(registrer: ActorRef)

  case class RegisterRootee(ref: ActorRef)

  case class RegisterRooter(ref: ActorRef)

  case class RegisterMe(ref: ActorRef)

  case class KillAkka()

}

/**
 * Watch actor and kill them when the operation is finished.
 */
object Reaper {
  def apply(name: String)(implicit system: ActorSystem): ActorRef = {
    system.actorOf(Props(new Reaper()), name)
  }
}

/**
 * Watch actor and kill them when the operation is finished.
 */
class Reaper() extends Actor with Logging {


  val watchedRootee = ArrayBuffer.empty[ActorRef]
  var rooter: Option[ActorRef] = None
  var orderToKillAkka = false

  def stopAkka() {
    if (rooter.isEmpty) {
      logger.debug("*** EVERY BODY IS GONE ***")
      context.system.shutdown()
    }
  }

  final def receive = {
    case RegisterRootee(refRootee) =>
      logger.debug(s"*** register rootee ${refRootee.path} ***")
      context.watch(refRootee)
      watchedRootee += refRootee
    case RegisterRooter(refRooter) =>
      logger.debug(s"*** register rooter ${refRooter.path} ***")
      context.watch(refRooter)
      rooter = Some(refRooter)
    case Terminated(ref) =>
      logger.debug(s"*** has been removed ${ref.path} ***")
      if (watchedRootee.contains(ref)) watchedRootee -= ref
      if (rooter.isDefined && rooter.get.equals(ref)) rooter = None
      if (orderToKillAkka) stopAkka()
    case KillAkka() => orderToKillAkka = true
      if (orderToKillAkka) stopAkka()
    case a => throw new IllegalArgumentException(s"Sent bad argument to ${self.path}: ${a.toString}")
  }
}
