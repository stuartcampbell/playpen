#include "edge.hpp"
#include "compare.hpp"

double dotProduct(Point &p, Point &q)
{
  return (p.x * q.x + p.y * q.y);
}

Edge::Edge(Point &_orig, Point &_dest) : orig(_orig), dest(_dest)
{
}

Edge::Edge(void) : orig(Point(0,0)), dest(Point(1,0))
{
}

Edge::Edge(double origx, double origy, double destx, double desty) : orig(Point(origx, origy)), dest(Point(destx, desty))
{
}

Edge &Edge::rot(void)
{
  Point q = (orig + dest);
  Point m = 0.5 * q;
  Point v = dest - orig;
  Point n(v.y, -v.x);
  Point z = 0.5 * n;
  orig = m - z;
  dest = m + z;
  return *this;
}

Edge &Edge::flip(void)
{
  return rot().rot();
}

Point Edge::point(double t)
{
  Point z = (dest - orig);
  Point zz = t * z;
  return Point(orig + z);
}

eEdgeOrient Edge::intersect(Edge &e, double &t)
{
  Point a = orig;
  Point b = dest;
  Point c = e.orig;
  Point d = e.dest;

  Point n = Point((d-c).y, (c-d).x);
  Point z = b - a;
  double denom = dotProduct(n, z);
  if (compare(denom, 0.0) == 0)
    {
      eEdgeClass aclass = orig.classify(e);
      if ((aclass == LEFT) || (aclass == RIGHT))
        {
          return PARALLEL;
        }
      else
        {
          return COLLINEAR;
        }
    }
  Point zz = a - c;
  double num = dotProduct(n, zz);
  t = -num / denom;
  return SKEW;
}

eEdgeOrient Edge::cross(Edge &e, double &t)
{
  double s;
  eEdgeOrient crossType = e.intersect(*this, s);
  if ((crossType == COLLINEAR) || (crossType == PARALLEL))
    {
      return crossType;
    }
  if ((s < 0.0) || (s > 1.0))
    {
      return SKEW_NO_CROSS;
    }
  intersect(e, t);
  if ((0.0 <= t) && (t <= 1.0))
    {
      return SKEW_CROSS;
    }
  else
    {
      return SKEW_NO_CROSS;
    }
}


