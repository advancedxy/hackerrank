#include <iostream>
#include <cstdio>
using namespace std;

int main() {
  int i;
  long l;
  long long ll;
  char c;
  float f;
  double d;
  scanf("%d %ld %lld %c %f %lf", &i, &l, &ll, &c, &f, &d);
  printf("%d\n", i);
  printf("%ld\n", l);
  printf("%lld\n", ll);
  printf("%c\n", c);
  printf("%.3f\n", f);
  printf("%.9lf\n", d);
  return 0;
}
