### Part 1.  Create the data frame from XML file

### Functions you'll want to use: xmlParse(), xmlRoot(), xpathSApply(), xmlGetAttr().
### It also might make it easier to use: xmlToList(), merge().

### (a) Load the data frame called LatLon from hw7.rda. 
LatLon = data.frame(LatLon)
str(LatLon)

### (b) Download the gzipped XML factbook document from
### http://jmatchparser.sourceforge.net/factbook/
### and create an XML "tree" in R 
install.packages("XML")
library(XML)
doc = xmlParse("factbook.xml")

### (c) Use XPath to extract the infant mortality and the CIA country codes from the XML tree
catalog = xmlRoot(doc)

Inf_Mor_Rate = xpathSApply(catalog, "//field[@name = 'Infant mortality rate']/rank/@number")
Inf_Mor_country = xpathSApply(catalog, "//field[@name = 'Infant mortality rate']/rank/@country")


### (d) Create a data frame called IM using this XML file.
### The data frame should have 2 columns: for Infant Mortality and CIA.Codes.
IM = data.frame('Infant Mortality' = Inf_Mor_Rate, "CIA.Codes" = Inf_Mor_country)
head(IM)

### (e) Extract the country populations from the same XML document
### Create a data frame called Pop using these data.
### This data frame should also have 2 columns, for Population and CIA.Codes.
Pop_country = xpathSApply(catalog, "//field['f2119'][@name='Population']/rank/@country")
Pop_population = xpathSApply(catalog, "//field['f2119'][@name='Population']/rank/@number")
Pop = data.frame("Population" = Pop_population, "CIA.Codes" = Pop_country)
head(Pop)

### (f) Merge the two data frames to create a data frame called IMPop with 3 columns:
### IM, Pop, and CIA.Codes
IMPop = merge(IM, Pop)
IMPop$CIA.Codes = toupper(IMPop$CIA.Codes)
head(IMPop)

### (g) Now merge IMPop with LatLon (from newLatLon.rda) to create a data frame called AllData that has 6 columns
### for Latitude, Longitude, CIA.Codes, Country Name, Population, and Infant Mortality
### (please check lat,long are not reversed in the file)
AllData = merge(IMPop, LatLon)
head(AllData)

### Part 2.  Create a KML document for google earth visualization.
### Make the KML document with stucture described in hw7_Intro.pdf.  
### You can use the addPlacemark function below to make
### the Placemark nodes, for which you need to complete the line for the Point node and
### figure out how to use the function.

makeBaseDocument = function(){
### This code creates the template for KML document 
### Your code here
template = newXMLDoc()
root = newXMLNode("kml", doc = template)
parent = newXMLNode("Document", parent = root)
child11 = newXMLNode("Name", "Country Facts", parent = parent)
child12 = newXMLNode("Description", "Infant Mortality", parent = parent)
child13 = newXMLNode("LookAt", parent = parent)
child131 = newXMLNode("longitude", -121, parent = child13)
child132 = newXMLNode("latitude", 43, parent = child13)
child133 = newXMLNode("altitude", 4100000, parent = child13)
child134 = newXMLNode("title", 0, parent = child13)
child135 = newXMLNode("heading", 0, parent = child13)
child136 = newXMLNode("altitudeMode", "absolute", parent = child13)

child14  = newXMLNode("Folder", parent = parent)
child141  = newXMLNode("Name", "CIA Fact Book", parent = child14)
template
}

addPlacemark = function(lat, lon, ctryCode, ctryName, pop, infM, parent, 
                        inf1, pop1, style = FALSE)
{
  pm = newXMLNode("Placemark", 
                  newXMLNode("name", ctryName), attrs = c(id = ctryCode), 
                  parent = parent)
  newXMLNode("description", paste(ctryName, "\n Population: ", pop, 
                                  "\n Infant Mortality: ", infM, sep =""),
             parent = pm)

  newXMLNode("Point", newXMLNode("coordinates", paste(lon, lat, 0, sep = ",")), parent = pm )
  
  
### You need to fill in the code for making the Point node above, including coordinates.
### The line below won't work until you've run the code for the next section to set up
### the styles.

  if(style) newXMLNode("styleUrl", paste("#YOR", inf1, "-", pop1, sep = ''), parent = pm)
}

### Use the two functions that you just implemented to created the KML document and save it 
### as 'Part2.kml'. open it in Google Earth. (You will need to install Google Earth.)  
### It should have pushpins for all the countries.  

### Your code here
part2 = makeBaseDocument()
root2 = xmlRoot(part2)
folder = root2[["Document"]][["Folder"]]
for(i in 1:length(AllData[,1])){
  addPlacemark(AllData[i,5], AllData[i,6], AllData[i,1], AllData[i,4], AllData[i,3],
               AllData[i,2], parent = folder)
}

part2

saveXML(part2, file = "Part2.kml")


### Part 3.  Add Style to your KML
### Now you are going to make the visualizatiion a bit fancier. To be more specific, instead of pushpins, we
### want different circle labels for countris with size representing population and the color representing  
### the infant motality rate.
### Pretty much all the code is given to you below to create style elements.
### Here, you just need to figure out what it all does.

### Start fresh with a new KML document, by calling makeBaseDocument()

doc2 = makeBaseDocument()

### The following code is an example of how to create cut points for 
### different categories of infant mortality and population size.
### Figure out what cut points you want to use and modify the code to create these 
### categories.
summary(AllData$Infant.Mortality)


infCut = cut(as.numeric(as.character(AllData$Infant.Mortality)), breaks = c(0, 10, 25, 50, 75, 200))
infCut = as.numeric(infCut)
popCut = cut(log(as.numeric(as.character(AllData$Population))), breaks = 5)
popCut = as.numeric(popCut)
popCut
### Now figure out how to add styles and placemarks to doc2
### You'll want to use the addPlacemark function with style = TRUE
### Below is code to make style nodes. 
### You should not need to do much to it.


### You do want to figure out what scales to use for the sizes of your circles. Try different 
### setting of scale here.
scale = c(1,2,3,4,5)
# scale = c(XX,XX,XX,XX,XX) Try your scale here for better visualization
colors = c("blue","green","yellow","orange","red")

addStyle = function(col1, pop1, parent, DirBase, scales = scale)
{
  st = newXMLNode("Style", attrs = c("id" = paste("YOR", col1, "-", pop1, sep="")), parent = parent)
  newXMLNode("IconStyle", 
             newXMLNode("scale", scales[pop1]), 
             newXMLNode("Icon", paste(DirBase, "color_label_circle_", colors[col1], ".png", sep ="")), parent = st)
}


root2 = xmlRoot(doc2)
DocNode = root2[["Document"]]
folder2 = DocNode[["Folder"]]

for (k in 1:5)
{
  for (j in 1:5)
  {
    addStyle(j, k, DocNode, 'color_label_circle/')
  }
}

### You will need to figure out what order to call addStyle() and addPlacemark()
### so that the tree is built properly. You may need to adjust the code to call the png files
### Your code here

for(i in 1:length(AllData[,1])){
  addPlacemark(AllData[i,5], AllData[i,6], AllData[i,1], AllData[i,4], AllData[i,3],
               AllData[i,2], parent = folder2, style = T, inf1 = infCut[i], pop1 = popCut[i])
}

saveXML(doc2, file = "doc2.kml")
### Finally, save your KML document, call it Part3.kml and open it in Google Earth to 
### verify that it works.  For this assignment, you only need to submit your code, 
### nothing else.  You can assume that the grader has already loaded hw7.rda.

