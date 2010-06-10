/*
 *
 *                  Spallation Neutron Source
 *          Oak Ridge National Laboratory, Oak Ridge TN.
 *
 *
 *                             NOTICE
 *
 * For this software and its associated documentation, permission is granted
 * to reproduce, prepare derivative works, and distribute copies to the public
 * for any purpose and without fee.
 *
 * This material was prepared as an account of work sponsored by an agency of
 * the United States Government.  Neither the United States Government nor the
 * United States Department of Energy, nor any of their employees, makes any
 * warranty, express or implied, or assumes any legal liability or
 * responsibility for the accuracy, completeness, or usefulness of any
 * information, apparatus, product, or process disclosed, or represents that
 * its use would not infringe privately owned rights.
 *
 */
#ifndef SANS_HFIR_READSPICE2D_H
#define SANS_HFIR_READSPICE2D_H

#include <iostream>
#include <vector>

namespace Poco {
  namespace XML {
    class Element;
  }
}

using Poco::XML::Element;
using namespace std;

/// Detector array definition
typedef vector< vector<double> > DetectorArray;

namespace HFIR
{
  namespace SANS
  {
    /*
     *  Standalone class to load HFIR/SPICE data for SANS
     */
    class SANSSpice2D
    {
      public:
        /// Constructor
        // TODO: make copy constructor and constructor without arguments
    	  SANSSpice2D( const std::string& );
    	  /// Destructor
    	  ~SANSSpice2D();
    	  /// Get file path
    	  const std::string& getFilePath() const { return filepath; };

    	  // Header section

    	  /// Get instrument name
        const std::string& getInstrument() const { return instrument; };
        /// Get reactor power
        double getReactorPower() const { return reactorPower; };
        /// Get experiment title
        const std::string& getExperimentTitle() const { return experimentTitle; };
        /// Get experiment number
        long getExperimentNumber() const { return experimentNumber; };

        const std::string& getCommand() const { return command; };
        const std::string& getBuiltinCommand() const { return builtinCommand; };
        const std::string& getUsers() const { return users; };
        const std::string& getLocalContact() const { return localContact; };
        const std::string& getScanTitle() const { return scanTitle; };
        const std::string& getSampleName() const { return sampleName; };
        double getScanNumber() const { return scanNumber; };
        const std::string& getSampleChanger() const { return sampleChanger; };
        double getSampleThickness() const { return sampleThickness; };
        double getSampleCountRate() const { return sampleCountRate; };
        bool isTransmission() const { return transmission; };
        long getScanTransmission() const { return scanTransmission; };
        long getSensitivityRunNumber() const { return sensitivityRunNumber; };
        long getBeamBlockedRunNumber() const { return beamBlockedRunNumber; };
        long getEmptyRunNumber() const { return emptyRunNumber; };
        long getNPixelsX() const { return numberXPixels; };
        long getNPixelsY() const { return numberYPixels; };
        double getPixelsSizeX() const { return mmPerPixelX; };
        double getPixelsSizeY() const { return mmPerPixelY; };
        double getBeamCenterX() const { return beamCenterPixelX; };
        double getBeamCenterY() const { return beamCenterPixelY; };
        double getAbsoluteIntensityConstant() const { return absIntensityConst; };
        double getSourceApertureSize() const { return sourceApertSize; };
        double getSampleApertureSize() const { return sampleApertSize; };
        double getBeamTrapDiameter() const { return beamTrapDiameter; };
        double getSourceDistance() const { return sourceDistance; };
        double getSampleToFlangeDistance() const { return sampleToFlange; };
        double getSampleApertureToFlangeDistance() const { return sampleApertToFlange; };
        double getTankInternalOffset() const { return tankInternalOffset; };
        double getWavelength() const { return wavelength; };
        double getWavelengthSpread() const { return wavelength_spread; };
        const std::string& getComment() const { return comment; };
        const std::string& getImagePath() const { return imagePath; };

        // Motor Position section
        double getAttenuation() const { return attenuation; };
        double getTemperature() const { return temperature; };
        double getLambda() const { return lambda; };
        double getLambdaSpread() const { return dlambda; };
        vector<double> getCollimators() const { return collimators; };
        double getNGuides() const { return nGuides; };
        double getApertureX() const { return apertureX; };
        double getAttenuatorPosition() const { return attenuatorPosition; };
        double getBeamTrapX() const { return beamTrapX; };
        double getDetectorTranslation() const { return detectorTranslation; };
        double getBeamTrapSize() const { return beamTrapSize; };
        double getSampleDetectorDistance() const { return sampleDetectorDistance; };
        double getSampleX() const { return sampleX; };
        double getBeamTrap200mmY() const { return beamTrap200mmY; };
        double getBeamTrap25mmY() const { return beamTrap25mmY; };
        double getBeamTrap40mmY() const { return beamTrap40mmY; };
        double getBeamTrap100Y() const { return beamTrap100mmY; };

        // Parameters section

        /// Sample Temperature on Lakeshore 340 [K]
        double getSampleTemperature() const { return sampleTemperature; };

        // Counters section

        /// Counting time [s]
        double getCountingTime() const { return countingTime; };
        /// Detector counts [counts]
        double getDetectorCounts() const { return detectorCounts; };
        /// Monitor counts [counts]
        double getMonitorCounts() const { return monitorCounts; };
        /// PSD counts [counts]
        double getPSDCounts() const { return psdCounts; };

        /// Detector data.
        // The Spice format stores the data as "INT32", so an integer would suffice.
        // We'll use that class for corrected data so we use doubles instead.
        DetectorArray data;
        DetectorArray dataError;

        /// Read the data file
    	  void read();
    	  /// Get data pointer
    	  const DetectorArray& getData() const { return data; };
    	  /// Scale the data by the given amount
    	  void scaleBy( double );
    	  /// Subtract another data set from the current one
    	  void subtractFrom( SANSSpice2D& );

      private:
    	  /// Location of the file to read
    	  const std::string filepath;

    	  // Header section

    	  /// Instrument name
    	  std::string instrument;
        /// Reactor power
        double reactorPower;
        /// Experiment title
        std::string experimentTitle;
        /// Experiment number
        long experimentNumber;
        /// Command string
        std::string command;
        /// Built-in command string
        std::string builtinCommand;
        /// User list
        std::string users;
        /// Local Contact
        std::string localContact;
        /// Scan title
        std::string scanTitle;
        /// Sample name
        std::string sampleName;
        /// Scan number
        double scanNumber;
        /// Sample changer
        std::string sampleChanger;
        /// Sample thickness [cm]
        double sampleThickness;
        /// Sample count rate
        double sampleCountRate;
        /// True is this is a transmission measurement [?]
        bool transmission;
        /// Transmission for scan [?]
        long scanTransmission;
        /// Detector sensitivity run number
        long sensitivityRunNumber;
        /// Blocked beam run number
        long beamBlockedRunNumber;
        /// Empty run number
        long emptyRunNumber;
        /// Number of detector pixels in X
        long numberXPixels;
        /// Number of detector pixels in Y
        long numberYPixels;
        /// Pixel size in X [mm]
        double mmPerPixelX;
        /// Pixel size in Y [mm]
        double mmPerPixelY;
        /// Beam center position in X [pixel]
        double beamCenterPixelX;
        /// Beam center position in Y [pixel]
        double beamCenterPixelY;
        /// Absolute intensity constant [?]
        double absIntensityConst;
        /// Source aperture size [units?]
        double sourceApertSize;
        /// Sample aperture size [units?]
        double sampleApertSize;
        /// Beam trap diameter [units?]
        double beamTrapDiameter;
        /// Source distance [units?]
        double sourceDistance;
        /// Sample to flange distance [units?]
        double sampleToFlange;
        /// Sample aperture to flange distance [units?]
        double sampleApertToFlange;
        /// Tank internal offset [units?]
        double tankInternalOffset;
        /// Neutron wavelength [Angstrom?]
        double wavelength;
        /// Neutron wavelenght spread [Angstrom?]
        double wavelength_spread;
        /// Comment
        std::string comment;
        /// File path to detector image
        std::string imagePath;

        // Motor positions section

        /// Attenuation [%]
        double attenuation;
        /// Lakeshore 340 Temperature Setpoint [K]
        double temperature;
        /// Incident wavelength [Angstrom]
        double lambda;
        /// Incident wavelength spread (FWHM) [Angstrom]
        double dlambda;
        /// Collimator positions [mm]
        vector<double> collimators;
        /// Number of guides
        double nGuides;
        /// Sample Changer X stage position [mm]
        double apertureX;
        /// Attenuator stage position [mm]
        double attenuatorPosition;
        /// Beamtrap X position [mm]
        double beamTrapX;
        /// Horizontal detector translation [mm]
        double detectorTranslation;
        /// Beam trap size [mm]
        double beamTrapSize;
        /// Control the distance from the sample to the detector [mm]
        double sampleDetectorDistance;
        /// Sample Changer X stage position [mm]
        double sampleX;
        /// 200mm Beamtrap Y position [mm]
        double beamTrap200mmY;
        /// 25mm Beamtrap Y position [mm]
        double beamTrap25mmY;
        /// 40mm Beamtrap Y position [mm]
        double beamTrap40mmY;
        /// 100mm Beamtrap Y position [mm]
        double beamTrap100mmY;

        // Parameters section

        /// Sample Temperature on Lakeshore 340 [K]
        double sampleTemperature;

        // Counters section

        /// Counting time [s]
        double countingTime;
        /// Detector counts [counts]
        double detectorCounts;
        /// Monitor counts [counts]
        double monitorCounts;
        /// PSD counts [counts]
        double psdCounts;

        /// True if the file was successfully read, False otherwise
    	  bool success;

    	  /// This method throws not found error if a element is not found in the xml file
    	  void throwException(Poco::XML::Element* elem,const std::string& name,const std::string& fileName);
        /// Get the value of an element as a double
        double _getDouble(Poco::XML::Element* entry, const std::string& element_name);
        /// Get the value of an element as a double
        long _getLong(Poco::XML::Element* entry, const std::string& element_name);
    	  /// Get the value of an element as a string
    	  const std::string _getString(Element* parent_element, const std::string& element_name);
        /// Get the value of an element as a bool
        bool _getBool(Element* parent_element, const std::string& element_name);
    };

  }
}

#endif
