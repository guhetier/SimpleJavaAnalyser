class A {
  static int x, y;
  static boolean b;

  static void f() {
    x=3;
    b = x < y;
    return;
  }

  static int g(int x, int y) {
    return x;
  }
}
