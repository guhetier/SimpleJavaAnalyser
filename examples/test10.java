
class Test {
    static int a;
    static int b;
    static int c = 5;
    static boolean d = true;

    static void f(){
            a = a * 0;
            a = a + 1;
    }

    static void main(){
        a = 3;
        d = true;
       
        if(d){
            a = a * 0;
            a = a + 1;
        }
        f();
    }
}
