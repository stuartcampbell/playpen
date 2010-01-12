#include <iostream>
#include "vertex.hpp"
#include "polygon.hpp"
#include "helpers.hpp"

using namespace std;

int main(void)
{
  cout << "Hello" << endl;

  Vertex *plist = new Vertex(-3.0, -3.0);
  plist->insert(new Vertex(-0.5, -3.0));
  plist->insert(new Vertex(0.5, -1.0));
  plist->insert(new Vertex(-2.0, -1.0));

  Polygon parallelogram(plist);

  Vertex *s1list = new Vertex(-1.0, -4.0);
  s1list->insert(new Vertex(-1.0, -3.0));
  s1list->insert(new Vertex(-2.0, -3.0));
  s1list->insert(new Vertex(-2.0, -4.0));

  Polygon rect1(s1list);

  Vertex *s2list = new Vertex(1.0, -1.0);
  s2list->insert(new Vertex(1.0, 3.0));
  s2list->insert(new Vertex(-4.0, 3.0));
  s2list->insert(new Vertex(-4.0, -1.0));

  Polygon rect2(s2list);

  Polygon *inter1 = convexPolygonIntersect(parallelogram, rect1);
  cout << "Overlap 1" << endl;
  printOverlap(*inter1);
  cout << endl;
  parallelogram.advance(CLOCKWISE);
  Polygon *inter2 = convexPolygonIntersect(rect2, parallelogram);
  cout << "Overlap 2" << endl;
  printOverlap(*inter2);
  cout << endl;
  Polygon *inter3 = convexPolygonIntersect(parallelogram, rect2);
  cout << "Overlap 3" << endl;
  printOverlap(*inter3);

  return 0;
}
