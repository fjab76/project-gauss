package fjab.composite;

import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class CompositeTest {

	private Building building;
	
	@Before
	public void createBuilding(){
		
		building = new Building();
		
		//1st floor
		Floor floor = new Floor(1);
		floor.add(new Room(11));
		floor.add(new Room(12));
		building.add(floor);
		
		//2nd floor
		floor = new Floor(2);
		floor.add(new Room(21));
		floor.add(new Room(22));
		floor.add(new Room(23));
		building.add(floor);
		
		//3rd floor
		floor = new Floor(3);
		floor.add(new Room(31));
		floor.add(new Room(32));
		floor.add(new Room(33));
		building.add(floor);
		
	}

	
	@Test
	public void buildingLightsAreOn(){

	    //checking that all rooms are off
		for(Floor floor : building){
			for(Room room : floor){
				assertEquals(false,room.isLightsOn());
			}
		}

		building.switchLightsOn();

        //checking that all rooms are on
		for(Floor floor : building){
			for(Room room : floor){
				assertEquals(true,room.isLightsOn());
			}
		}
	}
}
