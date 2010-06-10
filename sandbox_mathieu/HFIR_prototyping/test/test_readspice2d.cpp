/*
 * Test for the HFIR SPICE SANS reader
 */

#include <string>
#include <iostream>
#include <sstream>
#include <assert.h>
#include "sansspice2d.h"
using namespace std;

using HFIR::SANS::SANSSpice2D;


/*
 * Pretty print for development
 * TODO: trash after we're done, or make it usable
 */
ostream& operator<<(ostream& out, const HFIR::SANS::SANSSpice2D& x) {
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

/**
 * Test reading data file
 */
void checkDataRead() {
  SANSSpice2D r( "BioSANS_exp61_scan0004_0001.xml" );
  r.read();

  DetectorArray d = r.getData();
  double dchk[10] = {98, 123, 102, 100, 87, 102, 96, 95, 102, 91};
  for(unsigned int i=0; i<10; i++) {
    assert(d[1][i]==dchk[i]);
    cout << d[1][i] << " ";
  }
  cout << endl;
}

/**
 * Test scaling
 */
void checkScale(double scale) {
  SANSSpice2D r( "BioSANS_exp61_scan0004_0001.xml" );
  r.read();
  r.scaleBy(2.0);

  DetectorArray d = r.getData();
  double dchk[10] = {98, 123, 102, 100, 87, 102, 96, 95, 102, 91};
  for(unsigned int i=0; i<10; i++) {
    assert(d[1][i]==dchk[i]*scale);
    cout << d[1][i] << " ";
  }
  cout << endl;
}

/**
 * Test subtraction
 */
void checkSubtract() {
  SANSSpice2D r( "BioSANS_exp61_scan0004_0001.xml" );
  r.read();
  //TODO: make copy constructor...
  SANSSpice2D r2( "BioSANS_exp61_scan0004_0001.xml" );
  r2.read();

  r.scaleBy(2.0);
  r.subtractFrom(r2);

  DetectorArray d = r.getData();
  double dchk[10] = {98, 123, 102, 100, 87, 102, 96, 95, 102, 91};
  for(unsigned int i=0; i<10; i++) {
    assert(d[1][i]==dchk[i]);
    cout << d[1][i] << " ";
  }
  cout << endl;
}



int main() {
  checkDataRead();
  checkScale(2.0);
  checkSubtract();
}
