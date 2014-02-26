package ayai.components

import ayai.components._
import crane.Component


class Equipment() extends Component {
	var helmet: Item = null
	var weapon1: Item = null
	var weapon2: Item = null
	var torso: Item = null
	var legs: Item = null
	var feet: Item = null

	def equipWeapon1(weapon: Item) {
		weapon.itemType match {
			case weaponType: Weapon => 
				weapon1 = weapon
			case _ =>
		}
	}
	def equipWeapon2(weapon: Item) {
		weapon.itemType match {
			case weaponType: Weapon => 
				weapon2 = weapon
			case _ =>
		}
	}
}