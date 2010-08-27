add_library (MathFunctions mysqrt.cxx)

install (TARGETS MathFunctions DESTINATION lib)
install (FILES mysqrt.h DESTINATION include)
