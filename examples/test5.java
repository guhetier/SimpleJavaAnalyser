class Test {
    static int a = 2;
    static int b = 1;

    static void f(){
        a = 10;
    }

    static void main()
    {
        f();
        while(0 < a){
            a = a - 1;
            b = b * 2;
        }
    }
}
