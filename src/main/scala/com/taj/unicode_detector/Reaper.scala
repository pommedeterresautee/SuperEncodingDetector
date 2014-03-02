package com.taj.unicode_detector

import akka.actor.{PoisonPill, Terminated, ActorRef, Actor}
import scala.collection.mutable.ArrayBuffer
import com.taj.unicode_detector.ActorLifeOverview._

object ActorLifeOverview {
  case class StartRegistration(registrer:ActorRef)
  case class RegisterRootee(ref: ActorRef)
  case class RegisterRooter(ref: ActorRef)
  case class RegisterMe(ref: ActorRef)
  case class KillAkka()
}

class Reaper(verbose:Boolean) extends Actor {
  
  
  val watchedRootee = ArrayBuffer.empty[ActorRef]
  var rooter: Option[ActorRef] = None
  var orderToKillAkka = false

  def cleanAkka(authorisedToCloseAkka:Boolean) {
    if (rooter.isEmpty && authorisedToCloseAkka) {
      if (verbose) println("*** EVERY BODY IS GONE ***")
      context.system.shutdown()
    }
  }
  
  final def receive = {
    case RegisterRootee(refRootee) =>
      if (verbose) println(s"*** register rootee ${refRootee.path} ***")
      context.watch(refRootee)
      watchedRootee += refRootee
    case RegisterRooter(refRooter) =>
      if (verbose) println(s"*** register rooter ${refRooter.path} ***")
      context.watch(refRooter)
      rooter = Some(refRooter)
    case Terminated(ref) =>
      if (verbose) println(s"*** has been removed ${ref.path} ***")
      if(watchedRootee.contains(ref)) watchedRootee -= ref
      if(rooter.isDefined && rooter.get.equals(ref) ) rooter = None
      cleanAkka(orderToKillAkka)
    case KillAkka() => orderToKillAkka = true
      cleanAkka(orderToKillAkka)
  }
}
