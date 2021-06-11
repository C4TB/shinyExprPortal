$( document ).ready(function() {
  // !preview r2d3 data=data.frame(id=c(1,2,3,4), das1 = c(0,3,4,5), das2 = c(3,2,1,6), das3 = c(2,2,2,8), das4 = c(1,3,4,5), age = c(25,30,50,40)), d3_version = 5, dependencies = 'd3-legend.js', options = list (color = 'age', color_type='numeric', columns = c('das1','das2','das3','das4') ) 
//
// r2d3: https://rstudio.github.io/r2d3
//
//console = d3.window(svg.node()).console;/

svg = svg.attr("background-color", "unset");

if (typeof data === "undefined") {
  console.log("data undefined");
  return;  
}

var patients = [];
 data.forEach(function(e,i,a) {
  var patient = {};
  var cols = [];
  for (var key in options.columns) {
    cols.push(e[options.columns[key]]);
  }
  patient.values = cols;
  patient.color = e[options.color]; 
  patients.push(patient);
}); 

var max_col = Math.ceil(Math.sqrt(patients.length));
var cell_width = Math.ceil(0.75*width / max_col);
var cell_height = cell_width;

var color_scale;
var legend_scale;

if (options.color_type == "numeric")
{
  var min = d3.min(patients, function(d) { return d.color; });
  var max = d3.max(patients, function(d) { return d.color; });  
  
  //Symmetry switch
  if (Math.abs(min) < Math.abs(max))
  {
    if ((max > 0) && (min < 0))
      min = -max;
  }
  else if (Math.abs(min) > Math.abs(max)) {
    if (min < 0)
      max = -min;
  }
  
  color_scale = d3.scaleSequential()
                      .domain([min,max]) 
                      .interpolator(d3.interpolateViridis); 
  legend_scale = d3.scaleLinear()
                .domain([min,max]).range([0,200]);
}
else if (options.color_type == 'character')
{
  var unique_values = d3.map(patients, function(d){return d.color;}).keys();
  color_scale = d3.scaleOrdinal()
                  .domain(unique_values)
                  .range(d3.schemeCategory10);
}

//console.log(color_scale.domain());

var spacing = 1;

var scale_x = d3.scaleLinear()
                .domain([0, options.columns.length-1])
                .range([spacing,cell_width-spacing]);
                
var scale_y = d3.scaleLinear()
                .domain([0,d3.max(patients, function(d) {
                                return d3.max(d.values,function(v) {
                                    return v; 
                                      });
                                })])
                .range([cell_height-spacing, spacing]);
                
//console.log(patients[1]);
svg.append('rect')
  .attr('width', max_col*cell_width)
  .attr('height', max_col*cell_height)
  .style('fill', 'white')
  .style('stroke', 'silver')
  .style('stroke-width', 0.25);
  

svg.selectAll('g').remove();
g_patient = svg.selectAll('g.patient')
  //.data(patients.sort(function(a, b){ return d3.ascending(a.sort, b.sort); }))
  .data(patients)
  .enter().append('g')
  .attr("class","patient")
    //.attr('width',  cell_width)
    //.attr('height', cell_height)
    .attr('transform',function(d,i) { 
            return 'translate('+(cell_width*(i % max_col))+','
                               +(cell_height*Math.floor(i / max_col))+')';
    });
    
    g_patient.each(function(d,i) { 
      
   // console.log(d.value);
    
    //var color_b = (d[0] == true) ? "black" : "red";
   
     d3.select(this).append('rect')
    .attr('x',0)
    .attr('y',0)
    .style('stroke','red')
    .style('stroke-width', 1.5)
    .style('fill', 'none')
    .style('opacity', 0.0)
    .attr('rx',5)
    .attr('width',cell_width)
    .attr('height',cell_height);
    
    d3.select(this).append("path")
    .datum(d.values)
    .attr('stroke', color_scale(d.color) )
    .attr('stroke-width',1.5)
    .attr('fill', 'none')
    .attr("d", d3.line()
                .x(function(d,i) { return scale_x(i); })
                .y(function(v,i) { return scale_y(v); }));
                
    /* d3.select(this).selectAll("circle")
      .data(d[1])
      .enter()
      .append("circle")
      .style("fill","red")
      .attr("r",1.5)
      .attr("cx",function(d,i) { return scale_x(i); })
      .attr("cy",function(d,i) { return scale_y(d); })
     .on("mouseover", function(d) {		
          console.log(d);
            }); */
      } 
  ); 

var legend_clicked = true;

var legend = d3.legendColor()
            .labelFormat(d3.format(".2f"))
            .cells(6)
          //  .titleWidth(100)
          //  .title(options.color)
            .scale(color_scale)
            .on("cellclick", function(d) { 
              
              //var active = this.active ? false : true;
              //d3.select(this).select("text").style("fill", active? 'red' : 'black');
              //legend_clicked = !legend_clicked;
              Shiny.setInputValue(options.id + "-legend_filter", d, {priority: "event"});
            });
            
    

if (options.color_type == "numeric")
{

  var colorGradient = svg.append('defs')
      .append('linearGradient')
      .attr('id','color-grad');
      
  colorGradient
    .attr("x1", "0%")
    .attr("y1", "0%")
    .attr("y2", "100%")
    .attr("x2", "0%");
    
  //colorGradient.selectAll("stop")
  var stops = [];
  //var incr = (color_scale.domain()[1] - color_scale.domain()[0])/100;
  map = d3.scaleLinear()
          .domain([0,100])
          .range([color_scale.domain()[0],color_scale.domain()[1]]);
          
    for(var i=0; i<100;i++) {
      
      stops[i] = {offset: i + '%', color: color_scale(map(i)) };
    //  incr = incr + incr;
    
  }
  
  colorGradient.selectAll("stop")
    .data(stops)
    .enter().append("stop")
    .attr("offset", function(d) { 
      return d.offset; 
    })
    .attr("stop-color", function(d) { 
      return d.color; 
    });
  
  // draw the rectangle and fill with gradient
  var lQ = svg.append("g")
    .attr("class", "legendQuant")
     .attr("transform", "translate("+((max_col*cell_width)+30)+",10)");
    
    
  var label_start = lQ.append('text')
    .attr('class','brushLabel')
    .attr('id', 'label_start')
    .attr('text-anchor','end')
    .attr('dy','.25em')
    .style('fill','black')
    .attr('x', 0)
    .attr('y', 0);

  /* var tick_start = lQ.append('line')
    .attr('class','tickBrush')
    .style('stroke','#000')
    .attr('x1',-5)
    .attr('x2',5)
    .attr('y1',0)
    .attr('y2',0);
    
  var tick_end = lQ.append('line')
    .attr('class','tickBrush')
    .style('stroke','#000')
    .attr('x1',-5)
    .attr('x2',5)
    .attr('y1',0)
    .attr('y2',0); */
  
  var label_end = lQ.append('text')
    .attr('class','brushLabel')
    .attr('id', 'label_end')
    .attr('text-anchor','end')
    .attr('dy','.25em')
    .style('fill','black')
    .attr('x', 0)
    .attr('y', 0);
  
 
     
    lQ.append("rect")
    .attr("x", 5)
    .attr("y", 0)
    .attr("width", 20)
    .attr("height", 200)
    .style("fill", "url(#color-grad)");
    
    var ticks = d3.range(0,11)
    .map(function(d) { return map(d*10); });
    
    var axisLeg = d3.axisRight(legend_scale)
      .tickFormat(d3.format("4"))
      .tickValues(ticks); 
      
    var axis_g = lQ
      .append("g")
      .attr("class", "axis")
      .attr("transform", "translate(25, 0)")
      .call(axisLeg);
      
    lQ.selectAll(".axis path, .axis line")
        .style("stroke-width", "1.5px")
        .style('shape-rendering','geometricPrecision');  
    lQ.selectAll(".tick").style("font", "8.5pt sans-serif");

  lQ
    .call( d3.brushY()                 // Add the brush feature using the d3.brush function
      .extent( [ [5,0], [50, 200] ] ) // initialise the brush area: start at 0,0 and finishes at width,height: it means I select the whole graph area
      .on("end", function(e) {
        
        if (!d3.event.selection) {
          //console.log('nothing');
          //Shiny.setInputValue("extent_start",-999, {priority: "event"});
          //Shiny.setInputValue("extent_end", -999, {priority: "event"});
           Shiny.setInputValue(options.id + "-extent_event",null, {priority: "event"});
           label_start.style('display','none');
           label_end.style('display','none');
           
         g_patient
          .each(function(d) {
              d3.select(this)
              .select('rect')
              .style('opacity', 0);
          });
              
         } else {
         var extent = d3.event.selection;
         var invert_s = legend_scale.invert(extent[0]);
         var invert_e = legend_scale.invert(extent[1]);
         //console.log(extent);
         //console.log(legend_scale.invert(extent[0])+','+legend_scale.invert(extent[1]));
         label_start.attr('y', extent[0]).text(invert_s.toFixed(2)).style('display','inherit');
        // tick_start.attr('y1', extent[0]).attr('y2', extent[0]);
         label_end.attr('y', extent[1]).text(invert_e.toFixed(2)).style('display','inherit');
        // tick_end.attr('y1', extent[1]).attr('y2', extent[1]);
         //Shiny.setInputValue("extent_start",invert_s, {priority: "event"});
         //Shiny.setInputValue("extent_end", invert_e, {priority: "event"});
         Shiny.setInputValue(options.id + "-extent_event", {"start": invert_s, "end": invert_e},{priority: "event"});
         
         g_patient
       //  .filter(function(d) { console.log(d); return ((invert_s <= d.color ) && (d.color <= invert_e));  )
         .each(function(d) {
              d3.select(this)
              .select('rect')
              .style('opacity', function(d) { 
                  if  ((invert_s <= d.color ) && (d.color <= invert_e))
                    return 0.5;  
                  else return 0;
              });
         }
           );
         
        }
        }) // Each time the brush selection changes, trigger the 'updateChart' function
    /*  .on("end", function() {
        if (!d3.event.selection) {
         Shiny.setInputValue("extent_start", null, {priority: "event"});
         Shiny.setInputValue("extent_end", null, {priority: "event"});
        } 
      }) */
    ); 
    
}
else {
  var lQ = svg.append("g")
      .attr("class", "legendQuant")
      .attr("transform", "translate("+((max_col*cell_width)+20)+",10)")
      .style("cursor","pointer")
      .call(legend);
}
    

});
