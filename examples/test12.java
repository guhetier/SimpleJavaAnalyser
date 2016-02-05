
class Test {
    static int a;
    static int b;
    static int c;
    static boolean d;

    static void f(){
        assert false;
        c = 5;
    }

    static void main(){
        f();
        a = 3;
        d = false;
    }
}
