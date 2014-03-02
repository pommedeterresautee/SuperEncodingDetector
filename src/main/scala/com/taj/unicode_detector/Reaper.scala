package com.taj.unicode_detector

import akka.actor.{Terminated, ActorRef, Actor}
import scala.collection.mutable.ArrayBuffer


object ActorLifeOverview {
  case class StartRegistration(registrer:ActorRef)
  case class RegisterRootee(ref: ActorRef)
  case class RegisterRooter(ref: ActorRef)
  case class RegisterMe(ref: ActorRef)
}

class Reaper(verbose:Boolean) extends Actor {
  import com.taj.unicode_detector.ActorLifeOverview._
  // Keep track of what we're watching
  val watchedRootee = ArrayBuffer.empty[ActorRef]
  var rooter: Option[ActorRef] = None

  def allSoulsReaped():Unit = {
    if (verbose) println("*** EVERY BODY IS GONE ***")
    context.system.shutdown()
  }

  // Watch and check for termination
  final def receive = {
    case RegisterRootee(refRootee) =>
      if (verbose) println(s"*** register rootee ${refRootee.path} ***")
      context.watch(refRootee)
      watchedRootee += refRootee
    case RegisterRooter(refRooter) =>
      if (verbose) println(s"*** register rooter ${refRooter.path} ***")
      rooter = Some(refRooter)
    case Terminated(ref) =>
      if (verbose) println(s"*** has been removed ${ref.path} ***")
      watchedRootee -= ref
      if (watchedRootee.isEmpty) allSoulsReaped()
  }
}
