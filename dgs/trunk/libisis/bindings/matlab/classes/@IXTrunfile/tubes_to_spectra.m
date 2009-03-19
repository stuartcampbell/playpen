function spectra = tubes_to_spectra(runfile,tubes)

spectra = libisisexc('IXTrunfile','tubes_to_spectra',IXTmask,runfile,tubes);
