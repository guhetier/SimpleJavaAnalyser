
class Test {
    static int a = 2;
    static int b;
    static boolean c;
    static int d = 0;

    static void f(){
        b = Support.random(0, 2) - 1;
    }

    static void main()
    {
        f();
        a = 10;

        while(0 < a){
            if(0 < b){
                c = true;

                if(c){
                    a = a-1;
                }
                else{
                    a = 666;
                }
            }
            else {
                a = a + 1;
            }

            f();
        }
    }

}

