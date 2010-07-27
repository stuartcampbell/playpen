from unit_convert import E2V,V2lambda

def  bandwidth_calc(Ei,T0nu,T0w=0.19,spec='SEQUOIA'):
    """
    Ei is the centeral energy in meV
    T0nu is the frequency of the T0 chopper in Hz
    spec can equal 'ARCS' or 'SEQUOIA' default is 'SEQUOIA'
    T0w is the effective width of the T0 chopper slot.  default is 0.19 m which is determined from the 1/2 width of
    the monitor on SEQUOIA for 70 meV and 30 Hz.
    calculate the bandwidth from the T0 chopper
    5.18.2010 GEG
    """
    pos_spec=set(['ARCS','SEQUOIA'])
    tmpset=set([spec])
    if not tmpset.issubset(pos_spec):
        #raise RuntimeError, 'spectrometer must be'+pos_spec
        raise RuntimeError,'spectrometer not defined'
    #define lengths for spectrometers
    if spec=='ARCS':
        L=array([8.77,11.6,13.6,3.4])
    else:
        L=array([9.78,18.0,20.0,6.3])
    dtT0=T0w/(0.2*T0nu*2.0*pi)/2.0
    vcen=E2V(Ei)
    tcen=L[0]/vcen
    tT0min=tcen-dtT0/2.0
    tT0max=tcen+dtT0/2.0
    return {"lambdamin":V2lambda(L[0]/tT0min),"lambdamax":V2lambda(L[0]/tT0max), "tmin":tT0min, "tmax":tT0max}
    
    
    
    
