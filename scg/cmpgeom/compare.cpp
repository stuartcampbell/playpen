#include "compare.hpp"
#include <limits>
#include <cmath>

int compare(const double & value1, const double & value2)
{
  if(!(std::fabs(value1 - value2) > std::numeric_limits<double>::epsilon()))
    {
      return 0;
    }
  
  if(value1 > value2)
    {
      return 1;
    }
  else
    {
      return -1;
    }
}
