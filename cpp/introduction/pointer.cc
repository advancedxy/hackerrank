#include <stdio.h>

void update(int *a,int *b) {
  int ta = *a;
  int tb = *b;
  ta = ta + tb;
  tb = (*a) > (*b) ? (*a) - (*b) : (*b) - (*a);
  *a = ta;
  *b = tb;
}

int main() {
  int a, b;
  int *pa = &a, *pb = &b;
    
  scanf("%d %d", &a, &b);
  update(pa, pb);
  printf("%d\n%d", a, b);

  return 0;
}
