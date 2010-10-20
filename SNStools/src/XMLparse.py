from xml.dom import minidom
xmldoc = minidom.parse ('/Users/t4v/Desktop/SNStools.xml')
ApplauncherNode = xmldoc.firstChild
ApplicationNode = ApplauncherNode.childNodes[1]
exelist = xmldoc.getElementsByTagName('Exe')
descriptionlist = xmldoc.getElementsByTagName('Description')
imagelist = xmldoc.getElementsByTagName('Image')
BatchTeleportation = []
BatchTeleportation.append(exelist[0].toxml())
BatchTeleportation.append(descriptionlist[0].toxml())
BatchTeleportation.append(imagelist[0].toxml())
print app1

