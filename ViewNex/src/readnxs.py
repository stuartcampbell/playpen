import tables
h5_file = tables.openFile('REF_L_23554.nxs','r')
tab = h5_file.root.REF_L_23554.raw_data
for line in tab.iterrows():
    print line
