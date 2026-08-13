package myfunlaby

import com.funlabyrinthe.core.*
import com.funlabyrinthe.core.scene.*
import com.funlabyrinthe.mazes.*
import com.funlabyrinthe.mazes.std.*

import user.sjrd.floorleveledgrounds.*
import user.sjrd.viewrestriction.*

object MursMuretsEtTunnels extends Module:
  override def initialize()(using Universe): Unit =
    darkWall.painter = darkWall.painter.empty + "Plain/Black"
  end initialize

  override def startGame()(using universe: Universe): Unit =
    for map <- universe.components[Map] do
      map.outside(0) = darkWall
      for ref <- map.minRef until map.maxRef.withZ(1) do
        if !ref().field.isInstanceOf[Tunnel] then
          ref() += darkWall
  end startGame
end MursMuretsEtTunnels

@definition def allTimePlugin(using Universe) = new AllTimePlugin
@definition def stepLadderPlugin(using Universe) = new StepLadderPlugin
@definition def stepLadder(using Universe) = new StepLadder

@definition def tunnelViewRestrictionPlugin(using Universe) = new TunnelViewRestrictionPlugin

@definition def highWall(using Universe) = new HighWall

@definition def torch(using Universe) = new Torch

@definition def signTemplate(using Universe) = new Sign().asTemplate()

@definition def porch(using Universe) = new Porch

@definition def darkWall(using Universe) = new Wall

class AllTimePlugin(using ComponentInit) extends PlayerPlugin:
  override def perform(player: CorePlayer) = {
    case FallLevelDown(1) => ()
  }
end AllTimePlugin

class StepLadderPlugin(using ComponentInit) extends PlayerPlugin:
  override def perform(player: CorePlayer) = {
    case ClimbLevelUp(1) => ()
  }
end StepLadderPlugin

class StepLadder(using ComponentInit) extends Effect:
  painter += "Ladders/StepLadder"

  override def entered(context: EnteredContext): Unit = {
    context.player.plugins += stepLadderPlugin
  }

  override def execute(context: ExecuteContext): Unit = {
    context.player.showMessageOnce(
      "Depuis cet escabeau, tu peux monter sur le muret. "
        + "Tu peux sauter du muret quand tu veux, mais l'escabeau reste ici."
    )
  }

  override def exited(context: ExitedContext): Unit = {
    context.player.plugins -= stepLadderPlugin
  }
end StepLadder

class TunnelViewRestrictionPlugin(using ComponentInit) extends ViewRestrictionPlugin:
  override def presentView(corePlayer: CorePlayer, viewSize: Size): SceneUpdateFragment = {
    if corePlayer.reified[Player].position.exists(_().field.isInstanceOf[Tunnel]) then
      super.presentView(corePlayer, viewSize)
    else
      SceneUpdateFragment.empty
  }
end TunnelViewRestrictionPlugin

class HighWall(using ComponentInit) extends FloorLeveledGround:
  painter += "Fields/HighWallBase"

  @transient @noinspect
  val crenellations: List[Painter] =
    Direction.values.toList.map(d => universe.EmptyPainter + s"Fields/HighWall$d")

  override protected def doPresent(context: PresentSquareContext): Batch[SceneNode] = {
    import context.*

    var result = super.doPresent(context)

    for dir <- Direction.values do
      if !where.exists(pos => (pos +> dir)().field == this) then
        result ++= context.presentTiled(crenellations(dir.ordinal))

    result
  }
end HighWall

class Torch(using ComponentInit) extends Tool:
  painter += "Miscellaneous/TorchOn"

  override def find(context: ExecuteContext): Unit = {
    import context.*

    pos() += noTool

    player.showMessage("Avec cette torche, tu verras mieux dans les tunnels.")
    val newRadius = player.attributes(viewRestrictionRadius) + 30

    pos().field match
      case _: Tunnel =>
        while player.attributes(viewRestrictionRadius) < newRadius do
          player.attributes(viewRestrictionRadius) += 2
          player.sleep(100)
      case _ =>
        player.attributes(viewRestrictionRadius) = newRadius
    end match
  }
end Torch

class Sign(using ComponentInit) extends Obstacle {
  painter += "Signs/WoodenSign"

  var message: String = ""

  override def pushing(context: EnteringContext): Unit = {
    super.pushing(context)
    context.player.showMessage(message)
  }
}

class Porch(using ComponentInit) extends Obstacle:
  painter += "Gates/ClosedPorch"
end Porch
