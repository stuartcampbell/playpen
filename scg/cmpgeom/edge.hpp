#ifndef _EDGE_HPP
#define _EDGE_HPP 1

#include "point.hpp"

enum eEdgeOrient {
  COLLINEAR,      /**< Edges lie on the same line */
  PARALLEL,       /**< Edges point in the same direction */
  SKEW,           /**< Edges are at an angle to each other */
  SKEW_CROSS,     /**< Edges are at an angle and intersect */
  SKEW_NO_CROSS   /**< Edges are at an angle and do not intersect */
};

class Edge 
{
public:
  Point orig;
  Point dest;
  Edge(Point &_orig, Point &_dest);
  Edge(double orgx, double orgy, double destx, double desty);
  Edge(void);
  Edge &rot(void);
  Edge &flip(void);
  Point point(double);
  eEdgeOrient intersect(Edge&, double&);
  eEdgeOrient cross(Edge&, double&);
  bool isVertical(void);
  double slope(void);
  double y(double);
};

#endif //_EDGE_HPP
