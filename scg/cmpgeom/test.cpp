#include <iostream>
#include "point.hpp"

using namespace std;

int main(void)
{
  Point a(1.0, 2.0);
  Point b(2.0, 1.0);
  Point c(1.0, 2.0);

  if (a == c)
    {
      cout << "OK" << endl;
    }
  else
    {
      cout << "Bad" << endl;
    }

  return 0;
}
