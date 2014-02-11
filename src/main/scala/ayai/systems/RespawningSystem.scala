package ayai.systems

import crane.{Entity, World}
import crane.EntityProcessingSystem

import ayai.components._

class RespawningSystem() extends EntityProcessingSystem(include=List(classOf[Room], classOf[Character], classOf[Respawn])) { 
  override def processEntity(e : Entity, deltaTime : Int) {
  	var respawn = e.getComponent(classOf[Respawn]) match {
  		case (Some(r : Respawn)) => r
  	}
  	val health = e.getComponent(classOf[Health]) match {
  		case(Some(h : Health)) => h
  	}

    if(respawn.isReady(System.currentTimeMillis())) {
  		if(health.currentHealth <= 0) {
  			health.refill()
	  	}
	}
  }
}