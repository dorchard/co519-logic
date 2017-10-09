class BindingExample {

    static int x = 42;

    public static int fun1(int y) {
	return (x * y);
    }

    // Obtained by renaming y to z in fun1
    // Equivalent to fun1
    public static int fun2(int z) {
	return (x * z);
    }

    // Obtained by renaming y to x in fun1
    // Not equivalent to fun1 due to capture of free variable 'x'
    // which is bound globally
    public static int fun3(int x) {
	return (x * x);
    }

    public static void main(String[] args) {
	System.out.println("Fun 1 = " + fun1(2));
	System.out.println("Fun 2 = " + fun2(2));
	System.out.println("Fun 3 = " + fun3(2));
    }

}
