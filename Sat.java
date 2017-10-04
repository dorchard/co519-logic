// Compile and run by doing "javac Sat.java; java Sat"
public class Sat {

    public static void main(String[] args) {
	boolean a = false;
	boolean b = false;
	boolean c = false;

	// What is a satisfying assignment for this formula?
	boolean p =   (a || b || c)
	      	   && (!a || b || c)
	       	   && (!a || !b || c);

	// Let's see if we picked the right assignment.
	System.out.println(p);
    }
}
