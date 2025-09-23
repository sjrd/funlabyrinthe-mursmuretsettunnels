package myfunlaby

import com.funlabyrinthe.core.*
import com.funlabyrinthe.core.graphics.*
import com.funlabyrinthe.mazes.*
import com.funlabyrinthe.mazes.std.*

import user.sjrd.floorlevelledgrounds.*
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

@definition def tunnelHintSign(using Universe) = new TunnelHintSign
@definition def torchHintSign(using Universe) = new TorchHintSign
@definition def gateOnHighWallHintSign(using Universe) = new GateOnHighWallHintSign

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

  override def entered(context: MoveContext): Unit = {
    context.player.plugins += stepLadderPlugin
  }

  override def exited(context: MoveContext): Unit = {
    context.player.plugins -= stepLadderPlugin
  }
end StepLadder

class TunnelViewRestrictionPlugin(using ComponentInit) extends ViewRestrictionPlugin:
  override def drawView(corePlayer: CorePlayer, context: DrawContext): Unit =
    if corePlayer.reified[Player].position.exists(_().field.isInstanceOf[Tunnel]) then
      super.drawView(corePlayer, context)
  end drawView
end TunnelViewRestrictionPlugin

class HighWall(using ComponentInit) extends FloorLevelledGround:
  painter += "Fields/HighWallBase"

  @transient @noinspect
  val crenellations: List[Painter] =
    Direction.values.toList.map(d => universe.EmptyPainter + s"Fields/HighWall$d")

  override protected def doDraw(context: DrawSquareContext): Unit =
    import context.*

    super.doDraw(context)

    for dir <- Direction.values do
      if !where.exists(pos => (pos +> dir)().field == this) then
        crenellations(dir.ordinal).drawTo(context)
  end doDraw
end HighWall

class Torch(using ComponentInit) extends Tool:
  painter += "Miscellaneous/TorchOn"

  override def find(context: MoveContext): Unit = {
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

class TunnelHintSign(using ComponentInit) extends Obstacle:
  painter += "Signs/WoodenSign"

  override def pushing(context: MoveContext): Unit = {
    super.pushing(context)
    context.player.showMessage(
      "Voici l'entrée d'un tunnel. "
        + "Tu ne peux voir l'entrée d'un tunnel que si tu es au même niveau que celui-ci. "
        + "Une fois à l'intérieur, fais attention où tu mets les pieds, car il y fait très sombre."
    )
  }
end TunnelHintSign

class TorchHintSign(using ComponentInit) extends Obstacle:
  painter += "Signs/WoodenSign"

  override def pushing(context: MoveContext): Unit = {
    super.pushing(context)
    context.player.showMessage(
      "Dans les tunnels, rien ne vaut une torche pour y voir plus clair."
    )
  }
end TorchHintSign

class GateOnHighWallHintSign(using ComponentInit) extends Obstacle:
  painter += "Signs/WoodenSign"

  override def pushing(context: MoveContext): Unit = {
    super.pushing(context)
    context.player.showMessage(
      "Les entrées de tunnels ne se voient pas bien sur les murailles. "
        + "Observe bien pour les repérer."
    )
  }
end GateOnHighWallHintSign

class Porch(using ComponentInit) extends Obstacle:
  painter += "Gates/ClosedPorch"
end Porch
