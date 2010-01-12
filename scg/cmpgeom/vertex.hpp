#ifndef _VERTEX_HPP
#define _VERTEX_HPP 1

#include "node.hpp"
#include "point.hpp"

enum eRotDir {
  CLOCKWISE,
  COUNTER_CLOCKWISE
};

class Vertex : public Node, public Point 
{
public:
  Vertex(double x, double y);
  Vertex(Point&);
  Vertex *cw(void);
  Vertex *ccw(void);
  Vertex *neighbor(eRotDir rotation);
  Point point(void);
  Vertex *insert(Vertex*);
  Vertex *remove(void);
  void splice(Vertex*);
  Vertex *split(Vertex*);
  friend class Polygon;
};

#endif //_VERTEX_HPP
