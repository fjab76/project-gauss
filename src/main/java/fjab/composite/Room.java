package fjab.composite;

public class Room implements Component {

	private boolean lightsOn = false;
	private int roomNumber;
	
	public Room(int roomNumber){
		this.roomNumber=roomNumber;
	}
	
	@Override
	public void switchLightsOn() {
		lightsOn = true;
	}

	@Override
	public void switchLightsOff() {
		lightsOn = false;
	}

	public boolean isLightsOn() {
		return lightsOn;
	}
}
