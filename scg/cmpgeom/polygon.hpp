#ifndef _POLYGON_HPP
#define _POLYGON_HPP 1

#include "vertex.hpp"

class Point;
class Edge;

class Polygon 
{
private:
  Vertex *_v;
  int _size;
  void resize(void);
public:
  Polygon(void);
  Polygon(Polygon&);
  Polygon(Vertex*);
  ~Polygon(void);
  Vertex *v(void);
  int size(void);
  Point point(void);
  Edge *edge(void);
  Vertex *cw(void);
  Vertex *ccw(void);
  Vertex *neighbor(eRotDir rotation);
  Vertex *advance(eRotDir rotation);
  Vertex *setV(Vertex *);
  Vertex *insert(Point&);
  void remove(void);
  Polygon *split(Vertex*);
};

#endif //_POLYGON_HPP
