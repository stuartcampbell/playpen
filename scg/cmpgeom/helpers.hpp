#ifndef _HELPERS_HPP
#define _HELPERS_HPP 1

#include "edge.hpp"

class Edge;
class Point;
class Polygon;

enum eEdgeIn { 
  UNKNOWN,        /**< Which edge is inside the other is not known */
  P_IS_INSIDE,    /**< Edge P is inside of edge Q */
  Q_IS_INSIDE     /**< Edge Q is inside of edge P */
};

bool pointInConvexPolygon(Point &s, Polygon &p);
bool aimsAt(Edge &a, Edge &b, eEdgeClass aclass, eEdgeOrient crossType);
void advance(Polygon &A, Polygon &R, eEdgeIn inside);
eEdgeOrient crossingPoint(Edge &e, Edge &f, Point &p);
Polygon *convexPolygonIntersect(Polygon &P, Polygon &Q);
void printOverlap(Polygon &p);
bool clipPolygonToEdge(Polygon &s, Edge &e, Polygon* &result);
bool clipPolygon(Polygon &s, Polygon &p, Polygon* &result);

#endif //_HELPERS_HPP
