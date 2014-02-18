package ayai.components

import crane.Component

class Time(msActive : Int, startTime : Long ) extends Component {
  def isReady(endTime : Long) : Boolean = {
    if(endTime - startTime >= msActive) {
      return true
    }
    return false
  }
}