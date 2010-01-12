#include <cstdlib>
#include <cmath>
#include <iostream>
#include "polygon.hpp"
#include "edge.hpp"

Polygon::Polygon(void) : _v(NULL), _size(0)
{
}

Polygon::Polygon(Polygon &p)
{
  _size = p._size;
  if (_size == 0)
    {
      _v = NULL;
    }
  else
    {
      Point q = p.point();
      _v = new Vertex(q.x, q.y);
      for(int i = 1; i < _size; ++i)
        {
          p.advance(CLOCKWISE);
          Point s = p.point();
          _v = _v->insert(new Vertex(s.x, s.y));
        }
      p.advance(CLOCKWISE);
      _v = _v->cw();
    }
}

Polygon::Polygon(Vertex *v) : _v(v)
{
  resize();
}

void Polygon::resize(void)
{
  if (_v == NULL)
    {
      _size = 0;
    }
  else
    {
      Vertex *v = _v->cw();
      for(_size = 1; v != _v; ++_size, v= v->cw())
        {
          ;
        }
    }
}

Polygon::~Polygon(void)
{
  if (_v) 
    {
      Vertex *w = _v->cw();
      while(_v != w)
        {
          delete w->remove();
          w = _v->cw();
        }
      delete _v;
    }
}

Vertex *Polygon::v(void)
{
  return _v;
}

int Polygon::size(void)
{
  return _size;
}

Point Polygon::point(void)
{
  return _v->point();
}

Edge *Polygon::edge(void)
{
  Point o = point();
  Point d = _v->cw()->point();
  return new Edge(o.x, o.y, d.x, d.y);
}

Vertex *Polygon::cw(void)
{
  return _v->cw();
}

Vertex *Polygon::ccw(void)
{
  return _v->ccw();
}

Vertex *Polygon::neighbor(eRotDir rotation)
{
  return _v->neighbor(rotation);
}

Vertex *Polygon::advance(eRotDir rotation)
{
  return _v = _v->neighbor(rotation);
}

Vertex *Polygon::setV(Vertex *v)
{
  return _v = v;
}

Vertex *Polygon::insert(Point &p)
{
  if (_size++ == 0)
    {
      _v = new Vertex(p);
    }
  else
    {
      _v = _v->insert(new Vertex(p));
    }
  return _v;
}

void Polygon::remove(void)
{
  Vertex *v = _v;
  _v = (--_size == 0) ? NULL : _v->ccw();
  delete v->remove();
}

double Polygon::area(const bool signed_area)
{
  double area = 0.0;
  for (int i = 1; i <= this->_size; ++i)
    {
      Vertex *v = this-> advance(CLOCKWISE);

      Point orig = v->point();
      Point prev = v->ccw()->point();
      Point next = v->cw()->point();
      //std::cout << "Orig:" << orig << std::endl;
      //std::cout << "Prev:" << prev << std::endl;
      //std::cout << "Next:" << next << std::endl;

      area += (orig.x * (next.y - prev.y));
    }
  area *= 0.5;
  if (signed_area)
    {
      return area;
    }
  else
    {
      return std::fabs(area);
    }
}

