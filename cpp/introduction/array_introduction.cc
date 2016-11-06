#include <cmath>
#include <cstdio>
#include <vector>
#include <iostream>
#include <algorithm>
using namespace std;


int main() {
  int n;
  cin >> n;
  int ints[n];
  for (int i = 0; i < n; i++) {
    cin >> ints[i];
  }
  for (int i = 0; i < n; i++) {
    cout << ints[n - i - 1];
    if (i != n - 1)
      cout << " ";
  }

  return 0;
}
