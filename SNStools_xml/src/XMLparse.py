from xml.dom import minidom
xmldoc = minidom.parse('SNStools.xml')

app = xmldoc.getElementsByTagName("Application")
##### Code to read attributes values #####
a = app[0].attributes.keys()
#print a  #[u,id']
value = app[0].attributes["description"]
#print value.name #id
#print value.value #hello

#ApplauncherNode = xmldoc.firstChild
#app = ApplauncherNode.attributes.keys()

exelist = xmldoc.getElementsByTagName('Exe')
exe = exelist[0]

##### Code to read the contain of a tag #######
#BatchTeleportation.append(exelist[0].firstChild.data)
#print BatchTeleportation[0]
##BatchTeleporation[0] -> /SNS/software/idltools/pyqt/BatchTeleporation/BatchTeleportation


#BatchTeleportation = []
#BatchTeleportation.append(descriptionlist[0].toxml())
#BatchTeleportation.append(imagelist[0].toxml())

#print exelist[0].firstChild





#<Applauncher>
#    <Application id="hello">
#                <BatchTeleportation>
#                                <Description>This application can be used to send by email a Batch File from one user to another user. The program takes care of attaching the right files with it and install them at the right place.</Description>
#                                <Image>BatchTeleportation</Image>
#                                <Exe>/SNS/software/idltools/pyqt/BatchTeleportation/BatchTeleportation</Exe>
#                </BatchTeleportation>
#                <BSSreduction>
#                                <Description>Data Reduction for the <i>Backscattering</i>instrument.</Description>
#                                <Image>BSSreduction</Image>
#                                <Exe>/SNS/software/idltools/BSSreduction</Exe>
#                </BSSreduction>
#                <CLoopES>
#                                <Description>Command Line lopper for elastic scans.</Description>
#                                <Image>CLoopES</Image>
#                                <Exe>/SNS/software/idltools/CLoopES</Exe>
#                </CLoopES>
#                <DAD>