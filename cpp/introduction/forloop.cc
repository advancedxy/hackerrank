#include <iostream>
using namespace std;

int main(int argc, char *argv[])
{
  int a, b;
  string digitMap[9] = {"one", "two", "three", "four", "five", "six", "seven",
                        "eight", "nine"};
  cin >> a >> b;
  for (int i = a; i <= b; i++) {
    if (i >= 1 && i <= 9) {
      cout << digitMap[i - 1] << endl;
    } else {
      if (i % 2 == 0) {
        cout << "even" << endl;
      } else {
        cout << "odd" << endl;
      }
    }
  }
  return 0;
}
