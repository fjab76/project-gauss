package fjab.composite;

import java.util.ArrayList;

/**
 * 
 * The Composite class simply inherits ArrayList in order to gain its containment abilities.
 * 
 * @author fjab
 *
 */
public class Floor extends ArrayList<Room> implements Component {
	
	private int floorNumber;
	
	public Floor(int floorNumber){
		this.floorNumber=floorNumber;
	}

	@Override
	public void switchLightsOn() {
		for (Room room : this) {
			room.switchLightsOn();
		}
	}

	@Override
	public void switchLightsOff() {
		for (Room room : this) {
			room.switchLightsOff();
		}
	}
}
