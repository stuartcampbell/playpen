#include <cmath>
#include "point.hpp"
#include "edge.hpp"
#include "compare.hpp"

Point::Point(double _x, double _y) : x(_x), y(_y)
{
}

Point Point::operator+(const Point &p)
{
  return Point(x + p.x, y + p.y);
}

Point Point::operator-(const Point &p)
{
  return Point(x - p.x, y - p.y);
}

Point operator*(double s, const Point &p)
{
  return Point(s * p.x, s * p.y);
}

double Point::operator[](int i)
{
  return (i == 0) ? x : y;
}

bool Point::operator==(const Point &p)
{
  return (compare(x, p.x) == 0) && (compare(y, p.y) == 0);
}

bool Point::operator!=(const Point &p)
{
  return !(*this == p);
}

bool Point::operator<(const Point &p)
{
  return ((x < p.x) || (compare(x, p.x) == 0 && (y < p.y)));
}

bool Point::operator>(const Point &p)
{
  return ((x > p.x) || (compare(x, p.x) == 0 && (y > p.y)));
}

eEdgeClass Point::classify(Point &p0, Point &p1)
{
  Point p2 = *this;
  Point a = p1 - p0;
  Point b = p2 - p0;
  double sa = a.x * b.y - b.x * a.y;
  if (sa > 0.0)
    {
      return LEFT;
    }
  if (sa < 0.0)
    {
      return RIGHT;
    }
  if ((a.x * b.x < 0.0) || (a.y * b.y < 0.0))
    {
      return BEHIND;
    }
  if (a.length() < b.length())
    {
      return BEYOND;
    }
  if (p0 == p2)
    {
      return ORIGIN;
    }
  if (p1 == p2)
    {
      return DESTINATION;
    }
  return BETWEEN;
}

eEdgeClass Point::classify(Edge &e)
{
  return classify(e.orig, e.dest);
}

double Point::length(void)
{
  return std::sqrt(x*x + y*y);
}

std::ostream& operator<<(std::ostream &os, const Point &p)
{
  os << "(" << p.x << "," << p.y << ")";
  return os;
}
