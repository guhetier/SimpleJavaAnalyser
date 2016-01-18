class Test {
    static int a = 2;
    static int b = 3;
    static int c;

    static void main(){
        c = a + b;
        a = c - a;
        b = b / a;
    }
}
    

