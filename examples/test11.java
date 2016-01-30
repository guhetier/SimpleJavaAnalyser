
class Test {

    static int a = 0;
    static int b;
    static boolean c = 5;
    static boolean d = true;

    static void main(){
        a = true;

        while(a < d){
            if(c)
                a = 5 + c + b;
            else
                a = b;
        }

    }
}
