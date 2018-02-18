package fjab.composite;

/**
 * There are times when a program needs to manipulate a tree data structure and it is necessary to 
 * treat both Branches as well as Leaf Nodes uniformly.
 * 
 * The important thing here is that all elements in the part-whole have operations, and that performing an 
 * operation on a node/composite also performs that operation on any children of that node/composite.
 * 
 * Branches and Leaf Nodes are Components. In this example, there are 3 types of components: Building, Floor and
 * Room. Building is a composite of Floors and Floor is a composite of Rooms.
 * 
 * @author fjab
 *
 */
public interface Component {
	void switchLightsOn();
	void switchLightsOff();
}