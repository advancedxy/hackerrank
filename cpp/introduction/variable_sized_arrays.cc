#include <iostream>
using namespace std;

int main() {
  int n, q;
  cin >> n >> q;
  int* abuffer[n];
  for (int i = 0; i < n; i++) {
    int len, num;
    cin >> len;
    abuffer[i] = new int[len];
    for (int j = 0; j < len; j++) {
      cin >> num;
      abuffer[i][j] = num;
    }
  }
  int indexi, indexj;
  for (int i = 0; i < q; i++) {
    cin >> indexi >> indexj;
    cout << abuffer[indexi][indexj] << endl;
  }
  return 0;
}
