namespace tc {
public class TestClass : GLib.Object {

    /* Fields */
    public int first_data = 0;
    private int second_data;

    /* Constructor */
    public TestClass(int foo) {
        this.second_data = foo;
    }

    /* Method */
    public int method_1() {
        stdout.printf("private data: %d\n", this.second_data);
        return this.second_data;
    }

    public int method_2(TestClass oth) {
        return this.first_data + oth.first_data;
    }

    public signal void some_event();

    public void method () {
        some_event ();
    }
}
}
