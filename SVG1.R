library("Cairo")
library("SVGAnnotation")
library("XML")
depth.col = gray.colors(100)[cut(quakes$depth, 100, label=FALSE)]
depth.ord = rev(order(quakes$depth))
doc = svgPlot(
 plot(lat ~ long, data = quakes[depth.ord, ],
 pch = 19, col = depth.col[depth.ord],
 xlab = "Longitude", ylab="Latitude",
 main = "Fiji Region Earthquakes") )
##
addToolTips(doc, 
apply(quakes[depth.ord, ], 1, function(x)
paste(names(quakes), x, sep = " = ", collapse = ", ")))
##
ax = getAxesLabelNodes(doc)
addToolTips(ax[c("xaxis", "yaxis")], c("Degrees east of the prime meridean",
"Degrees south of the equator"), addArea = TRUE)
usgs = "http://earthquake.usgs.gov/eqcenter/recenteqsww/"
region = "Maps/region/S_Pacific.php"
addAxesLinks(ax$title, paste(usgs, region, sep = ""))
saveXML(doc, "quakes.svg")
##
library("SVGAnnotation")
data("traffic")
Occupancy = unlist(traffic[ c("Occ1", "Occ2", "Occ3")])
Flow = unlist(traffic[c("Flow1", "Flow2", "Flow3")])
library("hexbin")
hbin = hexbin(Occupancy, Flow)
doc = svgPlot(
plot(hbin,
main = "Loop Detector #313111 on I-80E Sacramento"))
ptz = getPlotPoints(doc)
length(ptz)
[1] 276
length(hbin@count)
[1] 276
tips = paste("Count: ", hbin@count)
addToolTips(ptz, tips)
saveXML(doc, "hexbin.svg")
##
library("SVGAnnotation")
doc = svgPlot({
     par(mfrow = c(1, 2))
     plot(Murder ~ UrbanPop, USArrests, main = "", cex = 1.4)
     plot(Rape ~ UrbanPop, USArrests, main = "", cex = 1.4)
     }, width = 14, height = 7)
##
linkPlots(doc)
saveXML(doc, "USArrests_linked.SVG")
##
library("Cairo")
library("XML")
library("SVGAnnotation")
data("rat.diet", package="fields")
lambdas= 2:floor(0.6 * length(unique(rat.diet$t)))
xrange = range(rat.diet$t)
xinterps = seq(xrange[1], xrange[2], by = 0.5)
doc = svgPlot({
par(mfrow = c(1, 2))
plot(con ~ t, data = rat.diet, log = "", xlim = xrange, 
xlab = "Time (days)", ylab = "Median Food Intake", 
main = "Control group")
predicted = lapply(lambdas, function(lam) {
spl = smooth.spline(rat.diet$t, rat.diet$con, df = lam)
lines(predict(spl, xinterps), col="green", lwd = 2)
predict(spl, rat.diet$t)
     })
range.y = range(unlist(sapply(predicted, function(pred) {
range(rat.diet$con - pred$y)})))
plot(y = 0, x = 0, xlim = xrange,  ylim = range.y, type = "n",
main = paste("Residual plot"), ylab = "Residuals", 
xlab = "Time (days)")
abline(h = 0, col = "lightgray", lwd = 2, lty = 3)
sapply(predicted, function(p) points(p$x, rat.diet$con - p$y))
    })
##
plots = getPlotRegionNodes(doc)
lines = getNodeSet(doc, "//x:path[contains(@style, 'rgb(0%,100%,0%)')]", "x")
length(lines) == length(lambdas)
invisible(mapply(function(lam, node) {
xmlAttrs(node, append =TRUE) = c(id = lam, visibility = "hidden")
}, paste("curve-lambda-", lambdas, sep = ""), lines))
xmlAttrs(lines[[1]], append = TRUE) = c(visibility = "visible")
numPoints = nrow(rat.diet) 
points = xmlChildren(plots[[3]])[-1]
lambdaVal = rep(lambdas, each = numPoints)
index = matrix(1:length(points), , length(lambdas))
at = plots[[3]]
nodes = sapply(seq(along = lambdas),
function(i) {
g = newXMLNode("g", attrs = 
c(id = paste("residual-group", lambdas[i], sep = "-"),
visibility = "hidden"), parent = at, 
namespaceDefinitions = c(xlink = "http://www.w3.org/1999/xlink") )
removeNodes(points[index[,i]])
addChildren(g, points[index[,i]])
       })
xmlAttrs(nodes[[1]], TRUE) = c(visibility = "visible")
svgRoot = xmlRoot(doc)
enlargeSVGViewBox(doc, y = 100, svg = svgRoot)
onl = sprintf("init(evt, %d);", max(lambdas) )
jscript = list.files(path = system.file("examples", "Javascript", 
package = "SVGAnnotation"), full.names = TRUE, pattern = "linkedSmoother")
addSlider(doc, onload = onl, svg = svgRoot, javascript = jscript, id = "slider-lambda")
saveXML(doc, "linkedSmoother.svg")
##
 <body onload="loaded()">
 <center>
 <h3>Presidential Election 2008 </h3>
 <p>
 The map is interactive in that you can click on 
 anywhere within a state to see more detailed
 results about the voting in that state.
 </p>
 </center>
 <div id="main">
    <div id="summaryMap">
       <div id="stateSummary"></div>
       <div id="map">
           <object id="SVGMap" data="stateMap.SVG"
             type="image/SVG+XML" width="700" height="700">
         </object>
      </div>
  </div>
 <div id="countySummary">
    <a id="toggleCounty"
            onclick="toggleCountyView()"> + click to see county
               results</a>
 <div id="countySummaryContent" class="hidden">
            click on a state to see more detailed information 
               about the election results for that state.
      </div>
    </div>
 </div>
##
stateO = sapply(states, function(x) sum(x$Obama))
stateM = sapply(states, function(x) sum(x$McCain))
winner = 1 + (stateO > stateM)
##
stateInd = match.map("state", regions)
polyWinners = winner[stateInd]
stateColors = c("#E41A1C", "#377EB8")[polyWinners]
doc = SVGPlot({
   map("state", fill = TRUE, col = stateColors)
   title("Election 2008")
   })
##
polygonPaths = getPlotPoints(doc)
length(polygonPaths)
 [1] 63
length(stateInd)
 [1] 63
##
urls =
  paste("http://elections.nytimes.com/2008/results/states/",
  names(winner)[stateInd], ".HTML", sep = "")
##
stateColors = c("#E41A1C", "#377EB8")[polyWinners]
doc = SVGPlot({
   map("state", fill = TRUE, col = stateColors)
   title("Election 2008")
   })
polygonPaths = getPlotPoints(doc)
addLink(polygonPaths, urls, CSS = character())
##
 <script type="text/javascript">
  function loaded() {
           show("national");
           var doc;
           doc = document.getElementById("SVGMap");
           doc = doc.getElement();
           for(var i = 0; i < polyIds.length; i++){
                 var el = doc.getElementById(polygonIds[i]);
                 var txt = "parent.show(" + polyStates[i] + ")";
                 el.setAttribute("onclick", txt));
             }
  }
</script> 
##
<script type="text/javascript">
  function show(stateName) {
           var val;
           var div;
           val = stateSummaryTables[stateName];
           div = document.getElementById("stateSummary");
           div.innerHTML = val;
           val = countyTables[stateName];
           if (val) {
                   div = document.getElementById
   +                             ("countySummaryContent");
                   div.innerHTML = val;
             }
  }
</script>
##
 <script type="text/javascript"
     src="stateHTMLTables.js"></script>
##

