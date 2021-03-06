from numpy import sqrt

def E2V(E):
    """
      Takes a neutron energy in meV and converts it to velocity in m/s 
    """
# for energy in mev returns velocity in m/s
    return sqrt(E/5.227e-6)

def V2E(V):
    """
      Takes a neutron velocity in m/s and converts it to energy in meV
    """
# for  v in  m/s returns energy in meV
    return 5.227e-6*V*V
    
def V2lambda(V):
    return sqrt(81.81/V2E(V))

def E2lambda(E):
   return sqrt(81.81/E)
   
def lambda2E(l):
   return 81.81/l/l
   
