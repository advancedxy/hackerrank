#include <iostream>
using namespace std;

int main(int argc, char *argv[])
{
  int n;
  string digitMap[9] = {"one", "two", "three", "four", "five", "six", "seven",
                        "eight", "nine"};
  cin >> n;
  if (n >= 1 && n <= 9) {
    cout << digitMap[n - 1];
  } else {
    cout << "Greater than 9";
  }
  return 0;
}
