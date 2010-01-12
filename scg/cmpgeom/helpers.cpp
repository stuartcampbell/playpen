#include "helpers.hpp"
#include "point.hpp"
#include "polygon.hpp"
#include "compare.hpp"
#include <iostream>
#include <cmath>

static const double EPSILON = std::numeric_limits<double>::epsilon();

bool pointInConvexPolygon(Point &s, Polygon &p)
{
  if (p.size() == 1)
    {
      return (s == p.point());
    }
  if (p.size() == 2)
    {
      eEdgeClass c = s.classify(*p.edge());
      return ((c == BETWEEN) || (c == ORIGIN) || (c == DESTINATION));
    }
  Vertex *org = p.v();
  for (int i = 0; i < p.size(); ++i, p.advance(CLOCKWISE))
    {
      if (s.classify(*p.edge()) == LEFT)
        {
          p.setV(org);
          return false;
        }
    }
  return true;
}

bool aimsAt(Edge &a, Edge &b, eEdgeClass aclass, eEdgeOrient crossType)
{
  Point va = a.dest - a.orig;
  Point vb = b.dest - b.orig;
  if (crossType != COLLINEAR)
    {
      double ca = va.x * vb.y;
      double cb = vb.x * va.y;
      if (compare(ca, cb) >= 0)
        {
          return (aclass != RIGHT);
        }
      else
        {
          return (aclass != LEFT);
        }
    }
  else
    {
      return (aclass != BEYOND);
    }
}

void advance(Polygon &A, Polygon &R, bool inside)
{
  A.advance(CLOCKWISE);
  if (inside && (R.point() != A.point()))
    {
      Point p = A.point();
#ifdef VERBOSE
      std::cout << "Advance adds cross pt: (" << p.x << "," << p.y << ")" << std::endl;
#endif
      R.insert(p);
    }
}

eEdgeOrient crossingPoint(Edge &e, Edge &f, Point &p)
{
  double s;
  double t;
  eEdgeOrient classe = e.intersect(f, s);
  if ((classe == COLLINEAR) || (classe == PARALLEL))
    {
      return classe;
    }
  double lene = (e.dest - e.orig).length();
  if ((s < -EPSILON*lene) || (s > 1.0+EPSILON*lene))
    {
#ifdef VERBOSE
      std::cout << "Help1" << std::endl;
#endif
      return SKEW_NO_CROSS;
    }
  f.intersect(e, t);
  double lenf = (f.orig - f.dest).length();
  if (compare(-EPSILON*lenf, t) <= 0 && compare(t, 1.0+EPSILON*lenf) <= 0)
    {
      if (compare(t, EPSILON*lenf) <= 0)
        {
          p = f.orig;
        }
      else if (compare(t, 1.0-EPSILON*lenf) >= 0)
        {
          p = f.dest;
        }
      else if (compare(s, EPSILON*lene) <= 0)
        {
          p = e.orig;
        }
      else if (compare(s, 1.0-EPSILON*lene) >= 0)
        {
          p = e.dest;
        }
      else
        {
          p = f.point(t);
        }
#ifdef VERBOSE
      std::cout << "Help2" << std::endl;
#endif
      return SKEW_CROSS;
    }
  else
    {
#ifdef VERBOSE
      std::cout << "Help3" << std::endl;
#endif
      return SKEW_NO_CROSS;
    }
}

Polygon *convexPolygonIntersect(Polygon &P, Polygon &Q)
{
  Polygon *R;
  Point iPnt;
  Point startPnt;
  eEdgeIn inflag = UNKNOWN;
  int phase = 1;
  int maxItns = 2 * (P.size() + Q.size());
#ifdef VERBOSE
  std::cout << "Max iterations = " << maxItns << std::endl;
#endif
  for (int i = 1; i <= maxItns; ++i)
    {
#ifdef VERBOSE
      std::cout << "Iteration " << i <<  " Phase = " << phase << std::endl;
#endif
      Edge *p = P.edge();
      Edge *q = Q.edge();
      eEdgeClass pclass = p->dest.classify(*q);
#ifdef VERBOSE
      std::cout << "Class P Pt" << std::endl;
      std::cout << "Class Pt: (" << p->dest.x << "," << p->dest.y << ")";
      std::cout << std::endl;
      std::cout << "Edge Orig Pt (" << q->orig.x << "," << q->orig.y << ")";
      std::cout << std::endl;
      std::cout << "Edge Dest Pt (" << q->dest.x << "," << q->dest.y << ")";
      std::cout << std::endl;
      std::cout << "P pt class: " << pclass << std::endl;      
#endif 
      eEdgeClass qclass = q->dest.classify(*p);
#ifdef VERBOSE
      std::cout << "Class Q Pt" << std::endl;
      std::cout << "Class Pt: (" << q->dest.x << "," << q->dest.y << ")";
      std::cout << std::endl;
      std::cout << "Edge Orig Pt (" << p->orig.x << "," << p->orig.y << ")";
      std::cout << std::endl;
      std::cout << "Edge Dest Pt (" << p->dest.x << "," << p->dest.y << ")";
      std::cout << std::endl;
      std::cout << "Q pt class: " << qclass << std::endl;      
#endif
      eEdgeOrient crossType = crossingPoint(*p, *q, iPnt);
#ifdef VERBOSE
      std::cout << "PQ Orient: " << crossType << std::endl;
#endif
      if (crossType == SKEW_CROSS)
        {
          if (1 == phase)
            {
              phase = 2;
#ifdef VERBOSE
              std::cout << "Found a crossing pt: (" << iPnt.x << ",";
              std::cout << iPnt.y << ")" << std::endl;
#endif
              R = new Polygon();
              R->insert(iPnt);
              startPnt = iPnt;
            }
          else if (iPnt != R->point())
            {
#ifdef VERBOSE
              std::cout << "Found a crossing pt: (" << iPnt.x << ",";
              std::cout << iPnt.y << ")" << std::endl;
#endif
              if (iPnt != startPnt)
                {
                  R->insert(iPnt);
                }
              else
                {
                  return R;
                }
            }
          if (pclass == RIGHT)
            {
              inflag = P_IS_INSIDE;
            }
          else if (pclass == RIGHT)
            {
              inflag = Q_IS_INSIDE;
            }
          else
            {
              inflag = UNKNOWN;
            }
        }
      else if ((crossType == COLLINEAR) && (pclass != BEHIND) && \
               (qclass != BEHIND))
        {
          inflag = UNKNOWN;
        }
#ifdef VERBOSE
      std::cout << "Current in flag: " << inflag << std::endl;
#endif
      bool pAIMSq = aimsAt(*p, *q, pclass, crossType);
      bool qAIMSp = aimsAt(*q, *p, qclass, crossType);
#ifdef VERBOSE
      std::cout << "P aims at Q:" << pAIMSq << std::endl;
      std::cout << "Q aims at P:" << qAIMSp << std::endl;
#endif
      if (pAIMSq && qAIMSp)
        {
          if ((inflag == Q_IS_INSIDE) || ((inflag == UNKNOWN) && \
                                          (pclass == LEFT)))
            {
#ifdef VERBOSE
              std::cout << "Move edge on P" << std::endl;
#endif
              advance(P, *R, false);
            }
          else 
            {
#ifdef VERBOSE
              std::cout << "Move edge on Q" << std::endl;
#endif
              advance(Q, *R, false);
            }
        }
      else if (pAIMSq)
        {
#ifdef VERBOSE
          std::cout << "Move edge on P" << std::endl;
#endif
          advance(P, *R, inflag==P_IS_INSIDE);
        }
      else if (qAIMSp)
        {
#ifdef VERBOSE
          std::cout << "Move edge on Q" << std::endl;
#endif
          advance(Q, *R, inflag==Q_IS_INSIDE);
        }
      else 
        {
          if ((inflag == Q_IS_INSIDE) || ((inflag == UNKNOWN) && \
                                          (pclass == LEFT)))
            {
#ifdef VERBOSE
              std::cout << "Move edge on P" << std::endl;
#endif
              advance(P, *R, false);
            }
          else 
            {
#ifdef VERBOSE
              std::cout << "Move edge on Q" << std::endl;
#endif
              advance(Q, *R, false);
            }
        }
    } // for
  Point pq = P.point();
  Point qp = Q.point();

  if (pointInConvexPolygon(pq, Q))
    {
      return new Polygon(P);
    }
  else if (pointInConvexPolygon(qp, P))
    {
      return new Polygon(Q);
    }
  return new Polygon();
}

void printOverlap(Polygon &p)
{
  std::cout << "Polygon Size = " << p.size() << std::endl;
  for (int i = 0; i < p.size(); ++i)
    {
      Vertex *v = p.cw();
      Point vp = v->point();
      std::cout << "Pt(" << i << "): " << vp.x << ", " << vp.y << std::endl;
      p.advance(CLOCKWISE);
    }
}

bool clipPolygonToEdge(Polygon &s, Edge &e, Polygon* &result)
{
  Polygon *p = new Polygon();
  Point crossingPt;
  for (int i = 0; i < s.size(); s.advance(CLOCKWISE), ++i)
    {
      Point orig = s.point();
      Point dest = s.cw()->point();
      bool origIsInside = (orig.classify(e) != LEFT);
      bool destIsInside = (dest.classify(e) != LEFT);
      if (origIsInside != destIsInside)
        {
          double t;
          e.intersect(*s.edge(), t);
          crossingPt = e.point(t);
        }
      if (origIsInside && destIsInside)
        {
          p->insert(dest);
        }
      else if(origIsInside && !destIsInside)
        {
          if (orig != crossingPt)
            {
              p->insert(crossingPt);
            }
        }
      else if (!origIsInside && destIsInside)
        {
          p->insert(crossingPt);
          if (dest != crossingPt)
            {
              p->insert(dest);
            }
        }
    }
  result = p;
  return (p->size() > 0);
}

bool clipPolygon(Polygon &s, Polygon &p, Polygon* &result)
{
  Polygon *q = new Polygon(s);
  Polygon *r;
  bool flag = true;
  for (int i = 0; i < p.size(); ++i, p.advance(CLOCKWISE))
    {
      Edge *e = p.edge();
      if (clipPolygonToEdge(*q, *e, r))
        {
          delete q;
          q = r;
        }
      else
        {
          delete q;
          flag = false;
          break;
        }
    }
  if (flag)
    {
      result = q;
      return true;
    }
  return false;
}
