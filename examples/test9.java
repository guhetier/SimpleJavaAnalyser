

class Test {
    static boolean b;
    static int c;

    static void main(){
        c = Support.random(1,3);
        if(c<2)
            b = true;
        else
            b = !b;
    }
}
    

