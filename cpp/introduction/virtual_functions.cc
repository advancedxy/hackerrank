#include <cmath>
#include <cstdio>
#include <vector>
#include <iostream>
#include <algorithm>
using namespace std;

class Person {
public:
  string name;
  int age;
  virtual void getdata(){};
  virtual void putdata(){};
};

class Professor: public Person {
public:
  virtual void getdata() {
    cin >> name >> age >> publications;
  }
  virtual void putdata() {
    cout << name << " " << age << " " << publications << " " << cur_id << endl;
  }
  Professor(): cur_id(id++) {
  }
private:
  int publications;
  static int id;
  const int cur_id;
};
int Professor::id = 1;

class Student: public Person {
public:
  virtual void getdata() {
    cin >> name >> age;
    int mark;
    for (int i = 0; i < 6; i++) {
      cin >> mark;
      mark_sum += mark;
      marks[i] = mark;
    }
  }
  virtual void putdata() {
    cout << name << " " << age << " "<< mark_sum << " " << cur_id << endl;
  }
  Student(): cur_id(id++) {
    mark_sum = 0;
  }
private:
  int marks[6];
  int mark_sum;
  static int id;
  const int cur_id;
};
int Student::id = 1;

int main(){

  int n, val;
  cin>>n; //The number of objects that is going to be created.
  Person *per[n];

  for(int i = 0;i < n;i++){

    cin>>val;
    if(val == 1){
      // If val is 1 current object is of type Professor
      per[i] = new Professor;

    }
    else per[i] = new Student; // Else the current object is of type Student

    per[i]->getdata(); // Get the data from the user.

  }

  for(int i=0;i<n;i++)
    per[i]->putdata(); // Print the required output for each object.

  return 0;

}
