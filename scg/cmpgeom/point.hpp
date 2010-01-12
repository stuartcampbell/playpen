#ifndef _POINT_HPP
#define _POINT_HPP 1

class Edge;

enum eEdgeClass {
  LEFT,         /**< Point is to left of edge */
  RIGHT,        /**< Point is to right of edge */
  BEYOND,       /**< Point is right of edge destination */
  BEHIND,       /**< Point is left of edge origin */
  BETWEEN,      /**< Point is between edge origin and destination */
  ORIGIN,       /**< Point equals edge origin */
  DESTINATION   /**< Point equals edge destination */
};

class Point
{
public:
  double x;
  double y;
  Point(double _x = 0.0, double _y = 0.0);
  Point operator+(const Point &p);
  Point operator-(const Point &p);
  friend Point operator*(const double s, const Point &p);
  double operator[](int);
  bool operator==(const Point &p);
  bool operator!=(const Point &p);
  bool operator>(const Point &p);
  bool operator<(const Point &p);
  eEdgeClass classify(Point &p0, Point &p1);
  eEdgeClass classify(Edge &e);
  double polarAngle(void);
  double length(void);
  double distance(const Edge &e);
};

#endif //_POINT_HPP
