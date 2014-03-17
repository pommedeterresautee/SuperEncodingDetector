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
import com.typesafe.scalalogging.slf4j.Logging
import com.taj.unicode_detector.ActorLife.RegisterRootee

import akka.actor.Terminated
import com.taj.unicode_detector.ActorLife.KillAkka
import scala.collection.mutable

object ActorLife {

  case class StartRegistration(registrer: ActorRef)

  case class RegisterRootee(parent: ActorRef)

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
  val watched: mutable.Set[ActorRef] = mutable.Set()
  var orderToKillAkka = false

  def receive = {
    case RegisterRootee(parent) =>
      logger.debug(s"*** Register rootee ${parent.path} ***")
      context.watch(parent)
      watched += parent
    case Terminated(ref) =>
      logger.debug(s"*** Has been removed ${ref.path} ***")
      watched -= ref
      byeBye()
    case KillAkka() => orderToKillAkka = true
      byeBye()
    case a => throw new IllegalArgumentException(s"Sent bad argument to ${self.path}: ${a.toString}")
  }

  def byeBye() {
    if (orderToKillAkka && watched.isEmpty) {
      logger.debug("*** Stop the Akka system ***")
      context.system.shutdown()
    }
  }
}