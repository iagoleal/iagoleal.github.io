function plot(svg, fData, xScale, yScale) {
  const line = d3.line()
    .x(d => xScale(d.x))
    .y(d => yScale(d.y));

  return svg.append("path")
    .datum(fData)
    .attr("fill", "none")
    .attr("class", "graph")
    .attr("d", line);

}

function plotEpigraph(svg, fData, xScale, yScale) {
  const epigraphArea = d3.area()
    .x(d => xScale(d.x))
    .y0(0)                 // Top of the plot
    .y1(d => yScale(d.y))  // Function curve
    .curve(d3.curveLinear);

  return svg.append("path")
    .datum(fData)
    .attr("class", "epigraph")
    .attr("d", epigraphArea)
}

function mark(svg, x, y) {
  return svg.append("circle")
    .attr("class", "mark")
    .attr("cx", x)
    .attr("cy", y)
    .attr("r", 4);
}

function makeCut(f, df, x0, minX, maxX) {
    return d3.range(minX, maxX, 0.01).map(x => ({x: x, y: f(x0) + df(x0) * (x - x0)}));
}

function plotFunction(id, f, df, minX, maxX) {
  const svg    = d3.select(id);
  const width  = svg.attr("width");
  const height = svg.attr("height");
  const margin = {top: 0, right: 0, bottom: 0, left: 0};

  const xScale = d3.scaleLinear()
    .domain([minX, maxX])
    .range([margin.left, width - margin.right]);

  const yScale = d3.scaleLinear()
    .domain([0, 5])   // TODO: calculate range for y
    .range([height - margin.bottom, margin.top]);

  // Generate data points
  const fData = d3.range(minX, maxX, 0.1).map(x => ({x: x, y: f(x)}));

  // Create a line generator
  const line = d3.line()
    .x(d => xScale(d.x))
    .y(d => yScale(d.y));

  const functionColor = "steelblue"

  plot(svg, fData, xScale, yScale)
    .attr("stroke", "black");

  plotEpigraph(svg, fData, xScale, yScale)
    .attr("fill", functionColor)
    .attr("fill-opacity", 0.3); // Adjust opacity as needed

  // Display cut for mouse x position
  svg.on("mousemove", function(event, d) {
    const cutColor   = "#ff8000"
    const [mouseX, _] = d3.pointer(event);
    const x0      = xScale.invert(mouseX);
    const cutData = makeCut(f, df, x0, minX, maxX);

    // Clean-up is required
    svg.select(".mark").remove();
    svg.select(".tangent-line").remove();

    // Append the path for the tangent line
    plot(svg, cutData, xScale, yScale)
      .attr("class", "tangent-line")
      .attr("stroke", cutColor);

    // Append a dot at the mouse's x position
    mark(svg, mouseX, yScale(f(x0)))
      .attr("fill", cutColor);
  });
}

plotFunction("#function-supporting-cut", x => x*x+1, x => 2*x, -2, 2);
