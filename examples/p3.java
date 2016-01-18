class A {
  static int x, y;
  static boolean b;

  static void f() {
    b = x < y;
    return;
  }

  static void main() {
      f();
  }
}
