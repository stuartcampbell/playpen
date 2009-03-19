datev=`date +%Y_%m_%d_%H%M`
dirname=`echo "libisis_$datev"`
mkdir $dirname
cd $dirname
mkdir DLL
cp ../win32_distref/*.dll ./DLL
# cp ../win32_distref/*.lib ./DLL
cp ../win32_distref/libisis_start.m .
cp ../win32_distref/libisis_init.m .
cp ../win32_distref/libisis_off.m .
cp ../Release/libisisexc.dll ./DLL

mkdir License
cp ../License/*.* ./License

mkdir graphics
for item in `ls -d ../bindings/matlab/graphics/*/`
do
#  echo $item
#  echo  ${item#*/*/*/}
  mkdir  ${item#*/*/*/}
  cp $item/*.* ${item#*/*/*/}   
done
cp ../bindings/matlab/graphics/*.m ./graphics
cp ../bindings/matlab/graphics/gtkhelp.txt ./graphics
cp ../bindings/matlab/graphics/images/*.JPG ./graphics/images/

for item in `ls -d ../bindings/matlab/graphics/*/*/`
do
#  echo $item
#  echo  ${item#*/*/*/}
  mkdir  ${item#*/*/*/}
  cp $item/*.* ${item#*/*/*/}   
done

for item in `ls -d ../bindings/matlab/graphics/*/*/*/`
do
#  echo $item
#  echo  ${item#*/*/*/}
  mkdir  ${item#*/*/*/}
  cp $item/*.* ${item#*/*/*/}   
done

mkdir ./classes
for item in `ls -d ../bindings/matlab/classes/\@IXT*`
  do
#  echo $item
#  echo  ${item#*/*/*/}
  mkdir  ${item#*/*/*/}
  cp $item/*.* ${item#*/*/*/}   
done

for item in `ls -d ../bindings/matlab/classes/\no_class`
  do
#  echo $item
#  echo  ${item#*/*/*/}
  mkdir  ${item#*/*/*/}
  cp $item/*.* ${item#*/*/*/}   
done

mkdir ./matlab
mkdir ./matlab/example_scripts
cp ../tests/mari_tests/mari_homer.m ./matlab/example_scripts
cp ../tests/het_tests/het_homer.m ./matlab/example_scripts
cp ../tests/maps_tests/maps_homer.m ./matlab/example_scripts
cd ./matlab
mkdir HET homer homer_gui MAPS MARI MERLIN mgeniefuncs Instrument_files
cp ../../applications/matlab/HET/*.m HET
cp ../../applications/matlab/MARI/*.m MARI
cp ../../applications/matlab/MAPS/*.m MAPS
cp ../../applications/matlab/MERLIN/*.m MERLIN
cp ../../applications/matlab/homer/*.m homer
cp ../../applications/matlab/homer_gui/*.m homer_gui
cp ../../applications/matlab/homer_gui/*.fig homer_gui
cp ../../applications/matlab/mgeniefuncs/*.m mgeniefuncs

cd homer_gui
mkdir instrument_setup
cp ../../../applications/matlab/homer_gui/instrument_setup/*.txt instrument_setup

cd ..

cd Instrument_files
mkdir maps_files mari_files het_files merlin_files 
# each of these directories is specific and handcrafted
# if one of these files is removed from the svn then it should break
# MAPS
cp ../../../tests/maps_files/4to1.map ./maps_files
cp ../../../tests/maps_files/mid_tubes.map ./maps_files
cp ../../../tests/maps_files/4to1_022.msk ./maps_files
cp ../../../tests/maps_files/getei_m_index.dat ./maps_files
cp ../../../tests/maps_files/fermi_chopper.nxs ./maps_files
cp ../../../tests/maps_files/moderator.nxs ./maps_files
cp ../../../tests/maps_files/source.nxs ./maps_files
cp ../../../tests/maps_files/attenuator_1.nxs ./maps_files
# MARI
cp ../../../tests/mari_files/mari_res.map ./mari_files
cp ../../../tests/mari_files/getei_m_index.dat ./mari_files
cp ../../../tests/mari_files/moderator.nxs ./mari_files
cp ../../../tests/mari_files/source.nxs ./mari_files
cp ../../../tests/mari_files/fermi_chopper.nxs ./mari_files
cp ../../../tests/mari_files/apertures.nxs ./mari_files
cp ../../../tests/mari_files/aperture_1.nxs ./mari_files
#cp ../../../tests/mari_files/aperture_2.nxs ./mari_files
cp ../../../tests/mari_files/attenuator_1.nxs ./mari_files
# MERLIN
cp ../../../tests/merlin_files/1to1.map ./merlin_files
cp ../../../tests/merlin_files/4to1.msk ./merlin_files
cp ../../../tests/merlin_files/merlin_monitors.map ./merlin_files
cp ../../../tests/merlin_files/getei_m_index.dat ./merlin_files
cp ../../../tests/merlin_files/moderator.nxs ./merlin_files
cp ../../../tests/merlin_files/source.nxs ./merlin_files
cp ../../../tests/merlin_files/fermi_chopper.nxs ./merlin_files
cp ../../../tests/merlin_files/rings_map.map ./merlin_files
cp ../../../tests/merlin_files/diag_tubes1to1.map ./merlin_files
cp ../../../tests/merlin_files/diag_tubes4to1.map ./merlin_files
cp ../../../tests/merlin_files/attenuator_1.nxs ./merlin_files
# HET
cp ../../../tests/het_files/RINGS_HET.MAP ./het_files
cp ../../../tests/het_files/MONITORS_HET.MAP ./het_files
cp ../../../tests/het_files/*.map ./het_files
cp ../../../tests/het_files/getei_m_index.dat ./het_files
cp ../../../tests/het_files/het_991.msk ./het_files
cp ../../../tests/het_files/source.nxs ./het_files
cp ../../../tests/het_files/moderator.nxs ./het_files
cp ../../../tests/het_files/aperture_1.nxs ./het_files
cp ../../../tests/het_files/apertures.nxs ./het_files
cp ../../../tests/het_files/attenuator_1.nxs ./het_files
cp ../../../tests/het_files/fermi_chopper.nxs ./het_files
# leave Instrument files
cd ..

mkdir ./utilities
for item in `ls -d ../../bindings/matlab/utilities/*/`
do
#  echo $item
#  echo  ${item#*/*/*/*/}
  mkdir  ${item#*/*/*/*/}
  cp $item/*.m ${item#*/*/*/*/}   
done
cp ../../bindings/matlab/utilities/*.m ./utilities

#leave matlab
cd ..

#leave distdir
cd .. 

# make zip file

zip -rq libisis.zip $dirname


