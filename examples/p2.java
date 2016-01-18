class A {
  static int f(int x) {
    boolean a=true;
    //return 0;
  }

  static boolean g(int x, int y) {
    int z = 3+4*5+2*4;
    //return true;
  }
}

class B {
  static int z;

  static void foo() {
    if(true) {}//return;
    while(false || true) {
  //    return 0/1;
    }
  }
}
