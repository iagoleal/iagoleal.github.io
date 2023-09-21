/*
 Types
*/

function Hyperplane(props) {
  this.x = props.x
  this.y = props.y
}

/*
  General Geometry
*/

function norm2(v) {
  return v.x ** 2 + v.y ** 2;
}

function norm(v) {
  return Math.sqrt(norm2(v));
}

// Normalize a 2d vector
function normalize(v) {
  const n = norm(v);
  return {x: v.x / n, y: v.y / n};
}

function intersectionPointCircle(point, circle) {
  const v = {x: point.x - circle.x, y: point.y - circle.y};
  return norm(v) <= circle.r;
}

function circleCircleIntersection(a, b) {
  const v = {x: a.x - b.x, y: a.y - b.y};

  return norm(v) <= a.r + b.r
}

/*
  Drawing figures
*/

function updateHyperplanes(svg, pos) {
  const width     = svg.attr("width");
  const height    = svg.attr("height");
  const maxLength = Math.sqrt(width ** 2 + height ** 2);

  return svg.selectAll(".hyperplane")
    .data(pos)
    .join(
      "line",
      update => update,
      exit   =>
        exit.transition("movePlane")
        .duration(50)
        .style("opacity", 0)
        .remove(),
    )
    .attr("x1", d => d.x - d.normal.y * maxLength)
    .attr("y1", d => d.y + d.normal.x * maxLength)
    .attr("x2", d => d.x + d.normal.y * maxLength)
    .attr("y2", d => d.y - d.normal.x * maxLength)
    .attr("class", "hyperplane");
}

function plot(svg, fData, xScale, yScale) {
  const line = d3.line()
  .x(d => xScale(d.x))
  .y(d => yScale(d.y));

  return svg.append("path")
    .datum(fData)
    .join("path")
    .attr("class", "function-graph")
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
    .attr("d", epigraphArea);
}

function mark(svg, x, y) {
  return svg.append("circle")
    .attr("class", "mark")
    .attr("cx", x)
    .attr("cy", y)
    .attr("r", 4);
}


/*
  Page figures
*/

function figureSetPointHyperplane(id) {
  const svg = d3.select(id);

  const width  = svg.attr("width");
  const height = svg.attr("height");
  const circle = { x: width/2, y: height/2, r: 100 };

  let pos = {
    x: circle.x + circle.r/Math.sqrt(2),
    y: circle.y + circle.r/Math.sqrt(2),
    normal: {x: Math.sqrt(2), y: Math.sqrt(2)}
  };

  // Our convex set
  const convexSet = svg.selectAll(".convex-set")
  .data([circle])
  .join("circle")
  .attr("cx", d => d.x)
  .attr("cy", d => d.y)
  .attr("r",  d => d.r)
  .attr("class", "convex-set");

  // Draw hyperplane separating mouse and convex set
  svg.on("mousemove", function (event) {
    const [x, y] = d3.pointer(event);
    const isInside = intersectionPointCircle({x, y}, circle);

    // Update convex set
    convexSet.classed("not-good", isInside);

    // Update hyperplane
    const normal = normalize({x: circle.x - x, y: circle.y - y});
    pos = {x, y, normal};

    updateHyperplanes(svg, isInside? [] : [pos]);

    // Mouse marker
    svg.selectAll(".mark").remove();
    if (!isInside) {
      mark(svg, pos.x, pos.y).attr("fill", "orange");
    }
  });

  updateHyperplanes(svg, [pos]);
}

function figureSetSupportingHyperplane(id) {
  const svg = d3.select(id);

  const width  = svg.attr("width");
  const height = svg.attr("height");
  const circle = { x: width/2, y: height/2, r: 100 };

  let pos = {
    x: circle.x + circle.r/Math.sqrt(2),
    y: circle.y + circle.r/Math.sqrt(2),
    normal: {x: Math.sqrt(2), y: Math.sqrt(2)}
  };

  // Our convex set
  const convexSet = svg.selectAll(".convex-set")
  .data([circle])
  .join("circle")
  .attr("cx", d => d.x)
  .attr("cy", d => d.y)
  .attr("r",  d => d.r)
  .attr("class", "convex-set");

  updateHyperplanes(svg, [pos]);

  // Draw hyperplane separating mouse and convex set
  svg.on("mousemove", function (event) {
    const [x, y] = d3.pointer(event);
    const isInside = intersectionPointCircle({x, y}, circle);

    // Update convex set
    convexSet.classed("not-good", isInside);

    // Update hyperplane
    const normal = normalize({x: circle.x - x, y: circle.y - y});
    const border = {x: circle.x - normal.x*circle.r, y: circle.y - normal.y*circle.r};
    pos = {x: border.x, y: border.y, normal};

    updateHyperplanes(svg, isInside ? [] : [pos]);

    // Mouse marker
    svg.selectAll(".mark").remove();
    if (!isInside) {
      mark(svg, pos.x, pos.y).attr("fill", "orange");
    }
  });
}

function figureSetSeparatingHyperplane(id) {
  const svg    = d3.select(id);
  const width  = +svg.attr("width");
  const height = +svg.attr("height");

  // data for convex bodies
  const bodies = [
    { x: 150, y: 150, r: 80,},
    { x: 350, y: 300, r: 50, }
  ];

  const bodiesSVG = svg.selectAll(".convex-set")
  .data(bodies)
  .enter().append("circle")
  .attr("class", "convex-set")
  .attr("cx", d => d.x)
  .attr("cy", d => d.y)
  .attr("r",  d => d.r)
  .call(d3.drag()
    .on("start", function(event, d) {
      d3.select(this).raise().classed("active", true);
    })
    .on("drag", dragging)
    .on("end", function(event, d) {
      d3.select(this).classed("active", false);
    }));

  function dragging(event, d) {
    // Ensure the circle stays within the SVG boundaries
    const newX = Math.max(d.r, Math.min(width  - d.r, event.x));
    const newY = Math.max(d.r, Math.min(height - d.r, event.y));

    d3.select(this)
      .attr("cx", d.x = newX)
      .attr("cy", d.y = newY);
    updateScene();
  }

  function updateScene() {
    const c1 = bodies[0];
    const c2 = bodies[1];

    if (circleCircleIntersection(c1, c2)) {
      bodiesSVG.classed("not-good", true);
      updateHyperplanes(svg, []);
    } else {
      bodiesSVG.classed("not-good", false);

      // New coordinates for hyperplane
      const normal = normalize({x: c2.x - c1.x, y: c2.y - c1.y});
      const mid = {
        x: (c1.x + normal.x * c1.r + c2.x - normal.x * c2.r) / 2,
        y: (c1.y + normal.y * c1.r + c2.y - normal.y * c2.r) / 2,
      }

      const pos = {x: mid.x, y: mid.y, normal};

      updateHyperplanes(svg, [pos]).lower();
    }
  }

  updateScene(); // Initial hyperplane calculation
}


function figureFunctionEpigraph(id, f, minX, maxX) {
  const svg    = d3.select(id);
  const width  = svg.attr("width");
  const height = svg.attr("height");
  const margin = {top: 0, right: 50, bottom: 0, left: 50};

  const xScale = d3.scaleLinear()
    .domain([minX, maxX])
    .range([margin.left, width - margin.right]);

  // Generate data points
  const fData = xScale.ticks(40).map(x => ({x: x, y: f(x)}));

  const yScale = d3.scaleLinear()
    .domain(d3.extent(fData, d => d.y))
    .range([height - margin.bottom, margin.top]);

  plot(svg, fData, xScale, yScale)
    .attr("stroke", "black");

  const g = svg.append("g");
  const epigraph = plotEpigraph(g, fData, xScale, yScale);
}

function figureFunctionSupportingCut(id, f, df, minX, maxX) {
  const svg    = d3.select(id);
  const width  = svg.attr("width");
  const height = svg.attr("height");
  const margin = {top: 0, right: 10, bottom: 0, left: 10};

  const xScale = d3.scaleLinear()
  .domain([minX, maxX])
  .range([margin.left, width - margin.right]);

  const yScale = d3.scaleLinear()
  .domain([0, 5])   // TODO: calculate range for y
  .range([height - margin.bottom, margin.top]);

  // Generate data points
  const fData = xScale.ticks(100).map(x => ({x: x, y: f(x)}));

  plot(svg, fData, xScale, yScale)
    .attr("stroke", "black");

  plotEpigraph(svg, fData, xScale, yScale)
    .attr("fill", "steelblue")
    .attr("fill-opacity", 0.3); // Adjust opacity as needed

  // Display cut for mouse x position
  svg.on("mousemove", function(event) {
    const [mouseX, _] = d3.pointer(event);
    const x0      = xScale.invert(mouseX);
    const cut     = {x: x0, fx: f(x0), dual: df(x0)};
    const cutData = makeCut(cut, xScale);

    // Clean-up is required
    svg.select(".mark").remove();
    svg.select(".hyperplane").remove();

    // Append the path for the tangent line
    plot(svg, cutData, xScale, yScale)
      .attr("class", "hyperplane");

    // Append a dot at the mouse's x position
    mark(svg, mouseX, yScale(f(x0))).attr("fill", "orange");
  });
}

function figureFunctionCuts(id, f, df, minX, maxX) {
  const div    = d3.select(id);
  const width = 350;
  const height = 400;
  const margin = {top: 0, right: 10, bottom: 0, left: 10};
  const cuts   = [];


  const svgFunc= div.append("svg")
    .attr("viewBox", `0 0 ${width} ${height}`)
    .attr("width", width)
    .attr("height", height)
    .attr("class", "diagram");

  const svgPoly = div.append("svg")
    .attr("viewBox", `0 0 ${width} ${height}`)
    .attr("width", width)
    .attr("height", height)
    .attr("class", "diagram");

  const xScale = d3.scaleLinear()
  .domain([minX, maxX])
  .range([0, width]);

  const yScale = d3.scaleLinear()
  .domain([0, 5])   // TODO: calculate range for y
  .range([height - margin.bottom, margin.top]);

  // Generate data points
  const fData = xScale.ticks(100).map(x => ({x: x, y: f(x)}));

  plot(svgFunc, fData, xScale, yScale)
    .attr("stroke", "black");

  // Polyhedral approximation begins as empty graph
  const poly = svgPoly.append("path")
    .datum(cuts)
    .attr("class", "function-graph polyhedral")
    .raise();

  svgFunc.on("mousemove", function(event) {
    const [mouseX, _] = d3.pointer(event);
    const x0      = xScale.invert(mouseX);
    const cut     = {x: x0, fx: f(x0), dual: df(x0)};
    const cutData = makeCut(cut, xScale);

    // Clean-up is required
    svgFunc.selectAll(".mark").remove();
    svgFunc.selectAll(".hyperplane").remove();

    // Append the path for the tangent line
    plot(svgFunc, cutData, xScale, yScale)
      .attr("class", "hyperplane");

    updatePolyhedral(poly, cuts.concat(cut), xScale, yScale);

    // Append a dot at the mouse's x position
    mark(svgFunc, mouseX, yScale(f(x0))).attr("fill", "orange");
  });

  svgFunc.on("click", function(event) {
    const [mouseX, mouseY] = d3.pointer(event);
    const x0      = xScale.invert(mouseX);
    const y0      = f(x0);
    const cut     = {x: x0, fx: y0, dual: df(x0)};
    const cutData = makeCut(cut, xScale);
    cuts.push(cut);

    updatePolyhedral(poly, cuts, xScale, yScale);

    // Append the path for the tangent line
    plot(svgPoly, cutData, xScale, yScale)
      .attr("class", "hyperplane")
      .style("opacity", 0.2);

    // Append a dot at the mouse's x position
    mark(svgPoly, mouseX, yScale(y0)).attr("fill", "orange");
  });
}

function figureFunctionEpigraphCarving(id, f, df, minX, maxX) {
  const svg    = d3.select(id);
  const width  = svg.attr("width");
  const height = svg.attr("height");
  const margin = {top: 0, right: 0, bottom: 0, left: 0};
  const cuts   = [];

  const xScale = d3.scaleLinear()
  .domain([minX, maxX])
  .range([margin.left, width - margin.right]);

  const yScale = d3.scaleLinear()
  .domain([0, 5])   // TODO: calculate range for y
  .range([height - margin.bottom, margin.top]);

  // Generate data points
  const fData = xScale.ticks(100).map(x => ({x: x, y: f(x)}));

  // Polyhedral approximation begins as empty graph
  const poly = svg.append("path")
    .datum(cuts)
    .attr("class", "function-graph polyhedral")
    .raise();


  plot(svg, fData, xScale, yScale)
    .style("opacity", 0.1);

  plotEpigraph(svg, [{x: minX, y: 0}, {x: maxX, y: 0}], xScale, yScale);

  svg.on("click", function(event) {
    const [mouseX, mouseY] = d3.pointer(event);
    const x0      = xScale.invert(mouseX);
    const y0      = Math.min(f(x0), yScale.invert(mouseY));
    const cut     = {x: x0, fx: y0, dual: df(x0)};
    const cutData = makeCut(cut, xScale);
    cuts.push(cut);

    const ff = x => d3.max(cuts, cut => cut.fx + cut.dual*(x - cut.x));
    const ffData = xScale.ticks(400).map(x => ({x: x, y: ff(x)}));

    svg.select(".epigraph").remove();
    plotEpigraph(svg, ffData, xScale, yScale);

    // Append the path for the tangent line
    plot(svg, cutData, xScale, yScale)
      .attr("class", "hyperplane")
      .style("opacity", 0.2);

    updatePolyhedral(poly, cuts, xScale, yScale);

    // Append a dot at the mouse's x position
    mark(svg, mouseX, yScale(y0)).attr("fill", "orange");
  });
}

function plot(svg, fData, xScale, yScale) {
  const line = d3.line()
  .x(d => xScale(d.x))
  .y(d => yScale(d.y));

  return svg.append("path")
    .datum(fData)
    .join("path")
    .attr("class", "function-graph")
    .attr("d", line);
}

function updatePolyhedral(poly, cuts, xScale, yScale) {
  const ff = x => d3.max(cuts, cut => cut.fx + cut.dual*(x - cut.x));
  const ffData = xScale.ticks(400).map(x => ({x: x, y: ff(x)}));

  const line = d3.line()
    .curve(d3.curveLinear)
    .x(d => xScale(d.x))
    .y(d => yScale(d.y));

  return poly
    .datum(ffData)
    .transition()
    .duration(100)
    .ease(d3.easeCubic)
    .attr("d", line);
}

function makeCut(cut, xScale) {
  return xScale.ticks(2).map(x => ({x: x, y: cut.fx + cut.dual * (x - cut.x)}));
}

function cutToHyperplane(cut, xScale, yScale) {
  // Assuming cut.x represents 'a', cut.fx represents 'b', and cut.dual represents 'k'
  const x = xScale(cut.x); // x-coordinate of a point on the line
  const y = yScale(cut.fx); // y-coordinate of a point on the line

  // Normal vector for the line (taking into consideration the scales)
  const normalX = -cut.dual;
  const normalY = 1;

  let p =  {
    x,
    y,
    normal: normalize({x: xScale(normalX), y: yScale(normalY)}),
  }

  console.log({cut, p});
  return p
}
