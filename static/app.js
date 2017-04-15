var plotData = {};

var renderLive = function(limit, duration) {

  var width = window.innerWidth,
      height = window.innerHeight / 2 - 30,
      now = new Date(Date.now() - duration),
      margin = {top: 20, right: 80, bottom: 30, left: 50},
      chartWidth = width - margin.left - margin.right,
      chartHeight = height - margin.top - margin.bottom;

  var x = d3.scaleTime()
    .domain([now - (limit - 2), now - duration])
    .range([0, chartWidth])

  var y = d3.scaleLinear()
    .domain([0, 50])
    .range([chartHeight, 0])

  var line = d3.line()
    .curve(d3.curveBasis)
    .x(function(d, i) { return x(now - (limit + 1 - i) * duration) })
    .y(function(d) { return y(d) });

  var svg = d3.select('#chart-live').append('svg')
    .attr('width', width)
    .attr('height', height);

  var g = svg.append("g").attr("transform", "translate(" + margin.left + "," + margin.top + ")");

  var xaxis = g.append('g')
    .attr('class', 'x axis')
    .attr('transform', 'translate(0,' + chartHeight + ')')
    .call(d3.axisBottom(x));

  var yaxis = g.append("g")
    .attr("class", 'y axis')
    .call(d3.axisLeft(y))

  g.append("g")
    .attr("class", 'y axis')
    .attr("transform", "translate(" + chartWidth + ",0)")
    .call(d3.axisRight(y));

  var paths = g.append('g');

  function tick() {

    now = new Date();

    for (var seriesName in plotData) {
      var series = plotData[seriesName];
      if (series.path === undefined) {
        series.path = paths.append('path')
          .data([series.values])
          .style("stroke", "black")
          .style("fill", "none");
        series.path.attr('d', line);
      }
      series.path
        .transition()
        .duration(duration)
        .ease(d3.easeLinear)
        .attr('d', line);
    }

    x.domain([now - (limit - 1) * duration, now - duration])

    xaxis.transition()
      .duration(duration)
      .ease(d3.easeLinear)
      .call(d3.axisBottom(x));

  }

  setInterval(tick, duration);

}

var renderHistorical = function(historicalData) {

  d3.select("#chart-hist").selectAll("*").remove();

  var width = window.innerWidth,
    height = window.innerHeight / 2 - 30,
    margin = {top: 20, right: 80, bottom: 30, left: 50},
    chartWidth = width - margin.left - margin.right,
    chartHeight = height - margin.top - margin.bottom;

  // get full range of x date values from nested payload
  var extremes = historicalData.map(function(h) {
    return d3.extent(h.observations.map(function (ih) { return ih.ts; }))
  })
  .reduce(function(acc, val) {
    return [Math.min(acc[0], val[0]), Math.max(acc[1], val[1])];
  }, [1e20, 0])
  .map(function(d) { return new Date(d * 1000); });

  var x = d3.scaleTime()
    .domain(extremes)
    .range([0, chartWidth])

  var y = d3.scaleLinear()
    .domain([0, 50])
    .range([chartHeight, 0])

  var line = d3.line()
    .curve(d3.curveBasis)
    .x(function(d, i) { return x(new Date(d.ts * 1000)); })
    .y(function(d) { return y(d.c); });

  var svg = d3.select('#chart-hist').append('svg')
    .attr('width', width)
    .attr('height', height);

  var g = svg.append("g").attr("transform", "translate(" + margin.left + "," + margin.top + ")");

  var xaxis = g.append('g')
    .attr('class', 'x axis')
    .attr('transform', 'translate(0,' + chartHeight + ')')
    .call(d3.axisBottom(x));

  var yaxis = g.append("g")
    .attr("class", 'y axis')
    .call(d3.axisLeft(y))

  g.append("g")
    .attr("class", 'y axis')
    .attr("transform", "translate(" + chartWidth + ",0)")
    .call(d3.axisRight(y));

  var paths = g.append('g');

  // forEach?
  historicalData.forEach(function(h) {
    paths.append('path')
      .data([h.observations])
      .style("stroke", "black")
      .style("fill", "none")
      .attr('d', line);
  });

}

var initSocket = function(limit) {
  ws = new WebSocket(window.location.href.replace("http", "ws"));
  ws.onmessage = function(msg) {
    var data = JSON.parse(msg.data);
    if (!plotData[data.n]) {
      var values = [];
      for (var i = limit - 1; i >= 0; i--) {
        values.push(0);
      }
      plotData[data.n] = {n: data.n, s: data.s, values: values}
    }
    var values = plotData[data.n].values;
    values.push(data.t);
    if (values.length > limit) {
      values.shift();
    }
  }
}

var fetchAndDrawHistorical = function() {
  d3.json(window.location.href + '/history', function(error, response) {
    renderHistorical(response);
  });
}

var app = function() {
  timeToShow = 120;
  initSocket(timeToShow);
  renderLive(timeToShow, 1000);
  setInterval(fetchAndDrawHistorical, 1000 * 60)
  fetchAndDrawHistorical();
}

window.onload = app;
