#include <iostream>
#include <cstdio>
using namespace std;

int max_of_four(int a, int b , int c, int d) {
  int r;
  if (a >= b) {
    r = a;
  } else {
    r = b;
  }
  if (r <= c) {
    r = c;
  }
  if (r <= d) {
    r = d;
  }
  return r;
}

int main(int argc, char *argv[])
{
  int a, b, c, d;
  scanf("%d %d %d %d", &a, &b, &c, &d);
  int ans = max_of_four(a, b, c, d);
  printf("%d\n", ans);
  return 0;
}
