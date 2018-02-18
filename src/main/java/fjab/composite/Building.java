package fjab.composite;

import java.util.ArrayList;

/**
 * The Building class simply inherits ArrayList in order to gain its containment abilities.
 *
 */
public class Building extends ArrayList<Floor> implements Component{

	@Override
	public void switchLightsOn() {
		for (Floor floor : this) {
			floor.switchLightsOn();
		}
	}

	@Override
	public void switchLightsOff() {
		for (Floor floor : this) {
			floor.switchLightsOff();
		}
	}

}
