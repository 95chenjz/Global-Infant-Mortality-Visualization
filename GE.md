# Google Earth Visuallzation
95chenjz  
`r format(Sys.Date())`  
##Part 1.  Create the data frame from XML file
###Load the data frame called LatLon from hw7.rda.

```r
load("hw7.rda")
LatLon = data.frame(LatLon)
str(LatLon)
```

```
## 'data.frame':	219 obs. of  4 variables:
##  $ Country.Name: Factor w/ 277 levels "AFGHANISTAN",..: 1 2 3 4 5 6 7 8 9 10 ...
##  $ CIA.Codes   : Factor w/ 270 levels "-","AA","AC",..: 5 8 6 12 10 11 17 18 3 13 ...
##  $ Latitude    : num  33 41 28 -14.3 42.5 ...
##  $ Longitude   : num  65 20 3 -170 1.5 ...
```

###Download the gzipped XML factbook document from http://jmatchparser.sourceforge.net/factbook/ and create an XML "tree" in R 

```r
library(XML)
doc = xmlParse("factbook.xml")
```

###Use XPath to extract the infant mortality and the CIA country codes from the XML tree

```r
catalog = xmlRoot(doc)

Inf_Mor_Rate = xpathSApply(catalog, "//field[@name = 'Infant mortality rate']/rank/@number")
Inf_Mor_country = xpathSApply(catalog, "//field[@name = 'Infant mortality rate']/rank/@country")
```

###Create a data frame called IM using this XML file.
The data frame should have 2 columns: for Infant Mortality and CIA.Codes.

```r
IM = data.frame('Infant Mortality' = Inf_Mor_Rate, "CIA.Codes" = Inf_Mor_country)
head(IM)
```

```
##   Infant.Mortality CIA.Codes
## 1           117.23        af
## 2           104.34        ml
## 3           100.14        so
## 4            92.86        ct
## 5            90.92        pu
## 6            90.30        cd
```

###Extract the country populations from the same XML document
Create a data frame called Pop using these data.  
This data frame should also have 2 columns, for Population and CIA.Codes.

```r
Pop_country = xpathSApply(catalog, "//field['f2119'][@name='Population']/rank/@country")
Pop_population = xpathSApply(catalog, "//field['f2119'][@name='Population']/rank/@number")
Pop = data.frame("Population" = Pop_population, "CIA.Codes" = Pop_country)
head(Pop)
```

```
##   Population CIA.Codes
## 1 1355692576        ch
## 2 1236344631        in
## 3  511434812        ee
## 4  318892103        us
## 5  253609643        id
## 6  202656788        br
```

###Merge the two data frames to create a data frame called IMPop with 3 columns: IM, Pop, and CIA.Codes

```r
IMPop = merge(IM, Pop)
IMPop$CIA.Codes = toupper(IMPop$CIA.Codes)
head(IMPop)
```

```
##   CIA.Codes Infant.Mortality Population
## 1        AA            11.74     110663
## 2        AC            13.29      91295
## 3        AE            10.92    5628805
## 4        AF           117.23   31822848
## 5        AG            21.76   38813722
## 6        AJ            26.67    9686210
```

###Merge IMPop with LatLon (from newLatLon.rda) to create a data frame called AllData that has 6 columns for Latitude, Longitude, CIA.Codes, Country Name, Population, and Infant Mortality.

```r
AllData = merge(IMPop, LatLon)
head(AllData)
```

```
##   CIA.Codes Infant.Mortality Population         Country.Name Latitude
## 1        AA            11.74     110663                ARUBA    12.50
## 2        AC            13.29      91295  ANTIGUA AND BARBUDA    17.05
## 3        AE            10.92    5628805 UNITED ARAB EMIRATES    24.00
## 4        AF           117.23   31822848          AFGHANISTAN    33.00
## 5        AG            21.76   38813722              ALGERIA    28.00
## 6        AJ            26.67    9686210           AZERBAIJAN    40.50
##   Longitude
## 1  -69.9667
## 2  -61.8000
## 3   54.0000
## 4   65.0000
## 5    3.0000
## 6   47.5000
```

## Part 2.  Create a KML document for google earth visualization.


```r
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
```


```r
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
```


Your code here

```r
part2 = makeBaseDocument()
root2 = xmlRoot(part2)
folder = root2[["Document"]][["Folder"]]
for(i in 1:length(AllData[,1])){
  addPlacemark(AllData[i,5], AllData[i,6], AllData[i,1], AllData[i,4], AllData[i,3],
               AllData[i,2], parent = folder)
}

saveXML(part2, file = "Part2.kml")
```

```
## [1] "Part2.kml"
```

## Part 3.  Add Style to KML
Instead of pushpins, we want different circle labels for countris with size representing population and the color representing the infant motality rate. Pretty much all the code is given to you below to create style elements. 

### Start fresh with a new KML document, by calling makeBaseDocument()

```r
doc2 = makeBaseDocument()
```

The following code is an example of how to create cut points for different categories of infant mortality and population size. 

```r
infCut = cut(as.numeric(as.character(AllData$Infant.Mortality)), breaks = c(0, 10, 25, 50, 75, 200))
infCut = as.numeric(infCut)
popCut = cut(log(as.numeric(as.character(AllData$Population))), breaks = 5)
popCut = as.numeric(popCut)
```



```r
scale = c(0.2,0.5,1,2,5)

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
```



```r
for(i in 1:length(AllData[,1])){
  addPlacemark(AllData[i,5], AllData[i,6], AllData[i,1], AllData[i,4], AllData[i,3],
               AllData[i,2], parent = folder2, style = T, inf1 = infCut[i], pop1 = popCut[i])
}
```
###Save KML document, call it Part3.kml and open it in Google Earth to verify that it works.  For this assignment, you only need to submit your code, nothing else.  

```r
saveXML(doc2, file = "doc2.kml")
```

```
## [1] "doc2.kml"
```







