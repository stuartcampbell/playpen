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
#include <string>
#include <iostream>
#include <sstream>
#include <math.h>
#include "Poco/Path.h"
#include "Poco/StringTokenizer.h"
#include "Poco/DOM/DOMParser.h"
#include "Poco/DOM/Document.h"
#include "Poco/DOM/Element.h"
#include "Poco/DOM/NodeList.h"
#include "Poco/DOM/Node.h"
#include "Poco/DOM/Text.h"

using namespace std;
using Poco::XML::Element;

#include "sansspice2d.h"

// Parse string and convert to numeric type
template <class T>
bool from_string(T& t, const std::string& s, std::ios_base& (*f)(std::ios_base&))
{
  std::istringstream iss(s);
  return !(iss >> f >> t).fail();
}


namespace HFIR
{
	namespace SANS
	{
		/// Constructor
		SANSSpice2D::SANSSpice2D( const std::string& filepath ) : filepath(filepath), success(false) {}
		/// Destructor
		SANSSpice2D::~SANSSpice2D() {}

    /*
     * Returns the value of an XML element as a double
     * @param parent_element Parent XML element
     * @param element_name Name of the child element to retrieve
     * @returns Double value
     */
    double SANSSpice2D::_getDouble(Element* parent_element, const std::string& element_name) {
      Element*element = parent_element->getChildElement(element_name);
      throwException(element, element_name, filepath);
      std::stringstream x(element->innerText());
      double d;
      x >> d;
      return d;
    }

    /*
     * Returns the value of an XML element as a long
     * @param parent_element Parent XML element
     * @param element_name Name of the child element to retrieve
     * @returns Long value
     */
    long SANSSpice2D::_getLong(Element* parent_element, const std::string& element_name) {
      Element*element = parent_element->getChildElement(element_name);
      throwException(element, element_name, filepath);
      std::stringstream x(element->innerText());
      long d;
      x >> d;
      return d;
    }

    /*
     * Returns the value of an XML element as a bool
     * @param parent_element Parent XML element
     * @param element_name Name of the child element to retrieve
     * @returns bool value
     */
    bool SANSSpice2D::_getBool(Element* parent_element, const std::string& element_name) {
      Element*element = parent_element->getChildElement(element_name);
      throwException(element, element_name, filepath);
      std::stringstream x(element->innerText());
      bool d;
      x >> d;
      return d;
    }

    /*
     * Returns the value of an XML element as a string
     * @param parent_element Parent XML element
     * @param element_name Name of the child element to retrieve
     * @returns String value
     */
		const std::string SANSSpice2D::_getString(Element* parent_element, const std::string& element_name) {
      Element*element = parent_element->getChildElement(element_name);
      throwException(element, element_name, filepath);
      return element->innerText();
		}

		/*
		 *
		 */
		void SANSSpice2D::read()
		{
			// Set up the DOM parser and parse xml file
			Poco::XML::DOMParser pParser;
			Poco::XML::Document* pDoc;
			pDoc = pParser.parse(filepath);

			// Get pointer to root element
			Element* pRootElem = pDoc->documentElement();
			if (!pRootElem->hasChildNodes())
			{
				throw "No root element in file " + filepath;
			}

			// Read header data
			// TODO: don't forget to read in the attributes for units and types

			Element* sasEntryElem = pRootElem->getChildElement("Header");
			throwException(sasEntryElem, "Header", filepath);

			instrument           = _getString(sasEntryElem, "Instrument");
			reactorPower         = _getDouble(sasEntryElem, "Reactor_Power");
			experimentTitle      = _getString(sasEntryElem, "Experiment_Title");
			experimentNumber     = _getLong(sasEntryElem,   "Experiment_number");
      command              = _getString(sasEntryElem, "Command");
      builtinCommand       = _getString(sasEntryElem, "Builtin_Command");
      users                = _getString(sasEntryElem, "Users");
      localContact         = _getString(sasEntryElem, "Local_Contact");
      scanTitle            = _getString(sasEntryElem, "Scan_Title");
      sampleName           = _getString(sasEntryElem, "Sample_Name");
      scanNumber           = _getDouble(sasEntryElem, "Scan_Number");
      sampleChanger        = _getString(sasEntryElem, "Sample_Changer");
      sampleThickness      = _getDouble(sasEntryElem, "Sample_Thickness");
      sampleCountRate      = _getDouble(sasEntryElem, "Sample_CountRate");
      transmission         = _getBool(sasEntryElem,   "Transmission");
      scanTransmission     = _getLong(sasEntryElem,   "Transmission_for_Scan");
      sensitivityRunNumber = _getLong(sasEntryElem,   "Detector_Sensitivity_Run_Number");
      beamBlockedRunNumber = _getLong(sasEntryElem,   "Beam_Blocked_Run_Number");
      emptyRunNumber       = _getLong(sasEntryElem,   "Empty_Run_Number");
      numberXPixels        = _getLong(sasEntryElem,   "Number_of_X_Pixels");
      numberYPixels        = _getLong(sasEntryElem,   "Number_of_Y_Pixels");
      mmPerPixelX          = _getDouble(sasEntryElem, "x_mm_per_pixel");
      mmPerPixelY          = _getDouble(sasEntryElem, "y_mm_per_pixel");
      beamCenterPixelX     = _getDouble(sasEntryElem, "beam_center_x_pixel");
      beamCenterPixelY     = _getDouble(sasEntryElem, "beam_center_y_pixel");
      absIntensityConst    = _getDouble(sasEntryElem, "absolute_intensity_constant");
      sourceApertSize      = _getDouble(sasEntryElem, "source_aperture_size");
      sampleApertSize      = _getDouble(sasEntryElem, "sample_aperture_size");
      beamTrapDiameter     = _getDouble(sasEntryElem, "beamtrap_diameter");
      sourceDistance       = _getDouble(sasEntryElem, "source_distance");
      sampleToFlange       = _getDouble(sasEntryElem, "sample_to_flange");
      sampleApertToFlange  = _getDouble(sasEntryElem, "sample_aperture_to_flange");
      tankInternalOffset   = _getDouble(sasEntryElem, "sample_aperture_to_flange");
      wavelength           = _getDouble(sasEntryElem, "wavelength");
      wavelength_spread    = _getDouble(sasEntryElem, "wavelength_spread");
      comment              = _getString(sasEntryElem, "Comment");
      imagePath            = _getString(sasEntryElem, "ImagePath");

      // Read "motor positions" section
      // TODO: read in "pos", "units", "description" attributes
      // TODO: define "standard" positions with "pos" values
      sasEntryElem = pRootElem->getChildElement("Motor_Positions");
      throwException(sasEntryElem, "Motor_Positions", filepath);

      attenuation          = _getDouble(sasEntryElem, "attenuation");
      temperature          = _getDouble(sasEntryElem, "temp");
      lambda               = _getDouble(sasEntryElem, "lambda");
      dlambda              = _getDouble(sasEntryElem, "dlambda_rel_fwhm");
      collimators          = vector<double>(8,0);
      collimators[0]       = _getDouble(sasEntryElem, "coll_1");
      collimators[1]       = _getDouble(sasEntryElem, "coll_2");
      collimators[2]       = _getDouble(sasEntryElem, "coll_3");
      collimators[3]       = _getDouble(sasEntryElem, "coll_4");
      collimators[4]       = _getDouble(sasEntryElem, "coll_5");
      collimators[5]       = _getDouble(sasEntryElem, "coll_6");
      collimators[6]       = _getDouble(sasEntryElem, "coll_7");
      collimators[7]       = _getDouble(sasEntryElem, "coll_8");
      nGuides              = _getDouble(sasEntryElem, "nguides");
      apertureX            = _getDouble(sasEntryElem, "aperture_x");
      attenuatorPosition   = _getDouble(sasEntryElem, "attenuator_pos");
      beamTrapX            = _getDouble(sasEntryElem, "beam_trap_x");
      detectorTranslation  = _getDouble(sasEntryElem, "detector_trans");
      beamTrapSize         = _getDouble(sasEntryElem, "beam_trap_size");
      sampleDetectorDistance = _getDouble(sasEntryElem, "sample_det_dist");
      sampleX              = _getDouble(sasEntryElem, "sample_x");
      beamTrap200mmY       = _getDouble(sasEntryElem, "trap_y_101mm");
      beamTrap25mmY        = _getDouble(sasEntryElem, "trap_y_25mm");
      beamTrap40mmY        = _getDouble(sasEntryElem, "trap_y_50mm");
      beamTrap100mmY       = _getDouble(sasEntryElem, "trap_y_76mm");


      // Read "Parameter Positions" section
      sasEntryElem = pRootElem->getChildElement("Parameter_Positions");
      throwException(sasEntryElem, "Parameter_Positions", filepath);
      sampleTemperature    = _getDouble(sasEntryElem, "tsample");

      // Read "Counters" section
      sasEntryElem = pRootElem->getChildElement("Counters");
      throwException(sasEntryElem, "Counters", filepath);
      countingTime         = _getDouble(sasEntryElem, "time");
      detectorCounts       = _getDouble(sasEntryElem, "detector");
      monitorCounts        = _getDouble(sasEntryElem, "monitor");
      psdCounts            = _getDouble(sasEntryElem, "psd");

      // Read in data section
      sasEntryElem = pRootElem->getChildElement("Data");
      throwException(sasEntryElem, "Data", filepath);

      // Read in the data buffer
      std::string data_str = _getString(sasEntryElem, "Detector");

      // Create empty data arrays
      data      = DetectorArray(getNPixelsX(), vector<double>(getNPixelsY(), 0));
      dataError = DetectorArray(getNPixelsX(), vector<double>(getNPixelsY(), 0));

      // Parse out each pixel. Pixels can be separated by white space, a tab, or an end-of-line character
      Poco::StringTokenizer pixels(data_str, " \n\t", Poco::StringTokenizer::TOK_TRIM | Poco::StringTokenizer::TOK_IGNORE_EMPTY);
      Poco::StringTokenizer::Iterator pixel = pixels.begin();

      int ipixel = 0;
      int npixelsx = getNPixelsX();

      while (pixel != pixels.end())
      {
        int ix = ipixel%npixelsx;
        int iy = (int)ipixel/npixelsx;

        // Get the count value and assign it to the right bin
        int count;
        from_string<int>(count, *pixel, std::dec);
        data[ix][iy] = (double)count;

        // Data uncertainties, computed according to the IGOR reduction code
        dataError[ix][iy] = sqrt( 0.5 + fabs( (double)count - 0.5 ));
        // The following is what I would suggest instead...
        //dataError[ix][iy] = count > 0 ? sqrt((double)count) : 0.0;

        ++pixel;
        ipixel++;
      }

      // Set flag that indicates proper completion
      success = true;

		}

		/*
		 * Scale the data by the given scaling factor
		 */
		void SANSSpice2D::scaleBy( double scale ) {
      for(unsigned int ix=0; ix<data.size(); ix++) {
        for(unsigned int iy=0; iy<data.size(); iy++) {
          data[ix][iy] *= scale;
          dataError[ix][iy] *= scale;
        }
      }
		}

		/*
		 * Substract another data array from the current data array
		 */
		void SANSSpice2D::subtractFrom( SANSSpice2D& to_subtract ) {
		  DetectorArray sub_data = to_subtract.getData();

      for(unsigned int ix=0; ix<data.size(); ix++) {
        for(unsigned int iy=0; iy<data.size(); iy++) {
          data[ix][iy] -= sub_data[ix][iy];
          //dataError[ix][iy] ...
        }
      }
		}


		/* This method throws not found error if a element is not found in the xml file
		 * @param elem pointer to  element
		 * @param name  element name
		 * @param fileName xml file name
		 */
		void SANSSpice2D::throwException(Poco::XML::Element* elem, const std::string & name,
		    const std::string& fileName)
		{
		  if (!elem)
		  {
		    throw name + " element not found in HFIR/SPICE file" + fileName;
		  }
		}

	}
}
