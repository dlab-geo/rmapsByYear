
ichoropleth2 <- function(x, data, pal = "Blues", ncuts = 5, animate = NULL, play = F, map = 'usa', legend = TRUE, labels = TRUE, map_title='', 
                         include_lowest=FALSE, map_breaks = NULL, nodata_color = NULL, nodata_label="NA", ...) {
  
  # create a new Datamaps map object
  d <- Datamaps$new()
  
  # map the formula to the data
  fml = lattice::latticeParseFormula(x, data = data)
  
  # create the data bins
  if (!is.null(map_breaks)) {
    # custom map key
    myfillkey <- cut(fml$left, breaks = map_breaks, ordered_result=TRUE, include.lowest=include_lowest)
  
  } else {
    # calculate quantiles
    myfillkey <- cut(
      fml$left,
      unique(quantile(fml$left, seq(0, 1, 1/ncuts),, na.rm=TRUE)),
      ordered_result = TRUE,
      include.lowest = include_lowest
    )
  }
  
  # associate the fillkey with the data
  data = transform (data, fillKey = myfillkey)
  
  # determine the fill colors for each data bin
  fillColors = brewer.pal(ncuts, pal)
  myfills<- as.list(setNames(fillColors, levels(data$fillKey)))
  
  
  if (!is.null(nodata_color)) {
    # add a fillkey for our nodata value
    levels(data$fillKey) <- c(levels(data$fillKey), nodata_label) #add it to the factor levels
    data <- within(data, fillKey[is.na(fillKey)] <- nodata_label) #update the dataframe replacing the fillkey value with the nodata label
 
    # add the nodata color to the end of the color pal
    fillColors <- c(brewer.pal(ncuts, pal), nodata_color)
  }
  
  # update the fill labels  
  myfills<- as.list(setNames(fillColors, levels(data$fillKey)))
  
  # update the map object
  d$set(
    scope = map, 
    fills = myfills,  
    legend = legend,
    labels = labels,
    ...
  )
  
  if (!is.null(animate)){
    
    range_ = summary(data[[animate]])
    data = dlply(data, animate, function(x){
      y = toJSONArray2(x, json = F)
      names(y) = lapply(y, '[[', fml$right.name)
      return(y)
    })
    
    d$set(
      bodyattrs = "ng-app ng-controller='rChartsCtrl'"  
    )
    
    d$addAssets(
      jshead = "http://cdnjs.cloudflare.com/ajax/libs/angular.js/1.2.1/angular.min.js"
    )
    
    if (play == T){
      # create the javascript template for a map with a play button
      d$setTemplate(chartDiv = sprintf("
                                       <div class='container'>
                                       <button ng-click='animateMap()'>Play</button>
                                       <span ng-bind='year'></span>
                                       <div id='{{chartId}}' class='rChart datamaps'></div>
                                       </div>
                                       <script>
                                       function rChartsCtrl($scope, $timeout){
                                       $scope.year = %s;
                                       $scope.animateMap = function(){
                                       if ($scope.year > %s){
                                       return;
                                       }
                                       map{{chartId}}.updateChoropleth(chartParams.newData[$scope.year]);
                                       $scope.year += 1
                                       $timeout($scope.animateMap, 1000)
                                       }
                                       }
                                       </script>", range_[1], range_[6])
      )
      
    } else {
      # Create the javascript template for a map with a year slider
      d$setTemplate(chartDiv = sprintf("
                                       <div class='container'>
                                       <h3>
                                       %s <span ng-bind='year'></span>
                                       </h3>
                                       <p>
                                       <button ng-click='previousMap()'> < </button>
                                       <input id='slider' style='width:150px;' type='range' min=%s max=%s ng-model='year' width=100>
                                       <button ng-click='nextMap()'> > </button>
                                       </p>
                                       
                                       <div id='{{chartId}}' class='rChart datamaps'></div>          
                                       </div>
                                       <script>
                                       function rChartsCtrl($scope){
                                       $scope.year = %s;
                                       $scope.$watch('year', function(newYear){
                                       map{{chartId}}.updateChoropleth(chartParams.newData[newYear]);
                                       })
                                       $scope.previousMap = function(){
                                       if ($scope.year != %s) {
                                       $scope.year = parseInt($scope.year) - 1;
                                       }
                                       }
                                       $scope.nextMap = function(){
                                       if ($scope.year != %s) {
                                       $scope.year = parseInt($scope.year) + 1;
                                       }
                                       }
                                       
                                       }
                                       </script>", map_title, range_[1], range_[6], range_[1], range_[1], range_[6])
      )
      
      
      
    }
    
    d$set(newData = data, data = data[[1]], labels=labels)
    
  } else {
    d$set(data = dlply(data, fml$right.name))
  }
  return(d)
}