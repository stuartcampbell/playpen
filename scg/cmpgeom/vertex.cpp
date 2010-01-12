#include "vertex.hpp"

Vertex::Vertex(double x, double y) : Point(x,y)
{
}

Vertex::Vertex(Point &p) : Point(p)
{
}

Vertex *Vertex::cw(void)
{
  return static_cast<Vertex *>(_next);
}

Vertex *Vertex::ccw(void)
{
  return static_cast<Vertex *>(_prev);
}

Vertex *Vertex::neighbor(eRotDir rotation)
{
  return ((rotation == CLOCKWISE) ? cw() : cw());
}

Point Vertex::point(void)
{
  return Point(static_cast<Point>(*this));
}

Vertex *Vertex::insert(Vertex *v)
{
  return static_cast<Vertex *>(Node::insert(v));
}

Vertex *Vertex::remove(void)
{
  return static_cast<Vertex *>(Node::remove());
}

void Vertex::splice(Vertex *b)
{
  Node::splice(b);
}
