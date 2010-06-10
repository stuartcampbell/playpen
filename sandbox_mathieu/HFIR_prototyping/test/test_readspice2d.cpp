/*
 * Test for the HFIR SPICE SANS reader
 */

#include <string>
#include <iostream>
#include <sstream>
#include "readspice2d.h"
using namespace std;


/*
 * Pretty print for development
 * TODO: trash after we're done, or make it usable
 */
ostream& operator<<(ostream& out, const HFIR::SANS::ReadSpice2D& x) {
  out << "HFIR/SPICE 2D data file: " << x.getFilePath() << endl;
  out << "  Instrument:      " << x.getInstrument() << endl;
  out << "  Expt title:      " << x.getExperimentTitle() << endl;
  out << "  Users:           " << x.getUsers() << endl;
  out << "  Expt number:     " << x.getExperimentNumber() << endl;
  out << "  Scan title:      " << x.getScanTitle() << endl;

  out << "  Detector pixels: " << x.getNPixelsX() << " " << x.getNPixelsY() << endl;
  out << "  Counting time:   " << x.getCountingTime() << endl;
  out << "  Detector counts: " << x.getDetectorCounts() << endl;

  // Get collimator positions
  vector<double> coll = x.getCollimators();
  for(unsigned int i=0; i<coll.size(); i++) {
    out << "  Collimator " << i << ":    " << coll[i] << endl;
  }
  return out;
}

int main() {
  HFIR::SANS::ReadSpice2D r( "BioSANS_exp61_scan0004_0001.xml" );
  r.read();
  cout << r << endl;
}
