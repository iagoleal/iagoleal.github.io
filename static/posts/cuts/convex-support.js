/*
 Types
*/

class Hyperplane {
  constructor(props) {
    this.x = props.x;
    this.y = props.y;
    this.normal = normalize({x: props.normal.x, y: props.normal.y});
  }

  get tangent() {
    return {x: this.normal.y, y: -this.normal.x}
  }
}

class Cut {
  constructor(x, intercept, inclination) {
    this.x    = x
    this.fx   = intercept
    this.dual = inclination
  }

  eval(p) {
    return this.fx + this.dual*(p - this.x);
  }
}

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

// Find the optimal hyperplane separating two circles
function circleDisjointSeparator(c1, c2) {
  const normal = normalize({x: c2.x - c1.x, y: c2.y - c1.y});
  // Midpoint between circle's boundaries
  const mid = {
    x: ((c1.x + normal.x * c1.r) + (c2.x - normal.x * c2.r)) / 2,
    y: ((c1.y + normal.y * c1.r) + (c2.y - normal.y * c2.r)) / 2,
  }

  return new Hyperplane({x: mid.x, y: mid.y, normal});
}

/*
  Drawing figures
*/

function updateConvexSets(selector, data) {
  return selector.selectAll(".convex-set")
    .data(data)
    .join("circle")
    .attr("cx", d => d.x)
    .attr("cy", d => d.y)
    .attr("r",  d => d.r)
    .attr("class", "convex-set");
}

function updateHyperplanes(selector, pos) {
  // TODO: Fix these numbers
  const width     = 750
  const height    = 400
  const maxLength = Math.sqrt(width ** 2 + height ** 2);

  return selector.selectAll(".hyperplane")
    .data(pos)
    .join(
      enter  => enter.append("line"),
      update => update,
      exit   =>
        exit.transition("movePlane")
        .duration(50)
        .style("opacity", 0)
        .remove(),
    )
    .attr("x1", d => d.x - d.tangent.x * maxLength)
    .attr("y1", d => d.y - d.tangent.y * maxLength)
    .attr("x2", d => d.x + d.tangent.x * maxLength)
    .attr("y2", d => d.y + d.tangent.y * maxLength)
    .attr("class", "hyperplane");
}

function updateMarks(selector, pos) {
  return selector.selectAll(".mark")
    .data(pos)
    .join("circle")
    .attr("cx", d => d.x)
    .attr("cy", d => d.y)
    .attr("r", 4)
    .attr("class", "mark");
}

function plot(elem, fs, xScale, yScale) {
  fs = Array.isArray(fs) ? fs : [fs]
  const fData = fs.map(f => xScale.ticks(40).map(x => ({x: x, y: f(x)})));

  return elem.selectAll(".function-graph")
    .data(fData)
    .join("path")
    .attr("class", "function-graph")
    .attr("d", d3.line()
      .x(d => xScale(d.x))
      .y(d => yScale(d.y))
    );
}

function plotEpigraph(elem, fs, xScale, yScale) {
  fs = Array.isArray(fs) ? fs : [fs]
  const fData = fs.map(f => xScale.ticks(400).map(x => ({x: x, y: f(x)})));

  return elem.selectAll(".epigraph")
    .data(fData)
    .join("path")
    .attr("class", "epigraph")
    .transition()
    .duration(200)
    .ease(d3.easeCubic)
    .attr("d", d3.area()
      .x(d => xScale(d.x))
      .y0(0)                 // Top of the plot
      .y1(d => yScale(d.y))  // Function curve
      .curve(d3.curveLinear)
    );

}

function updatePolyhedral(poly, cuts, xScale, yScale) {
  const f = cutApproximation(cuts);
  const fData = xScale.ticks(400).map(x => ({x: x, y: f(x)}));

  return poly.selectAll(".polyhedral")
    .data([fData])
    .join("path")
    .attr("class", "function-graph polyhedral")
    .transition()
    .duration(200)
    .ease(d3.easeCubic)
    .attr("d", d3.line()
      .x(d => xScale(d.x))
      .y(d => yScale(d.y))
    );
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

  let pos = new Hyperplane({
    x: circle.x + circle.r / Math.sqrt(2),
    y: circle.y + circle.r / Math.sqrt(2),
    normal: {x: 1, y: 1}
  });

  // Our convex set
  const convexSet   = svg.append("g");
  const hyperplanes = svg.append("g")
    .attr("width", width)
    .attr("height", width);

  updateConvexSets(convexSet, [circle]);
  updateHyperplanes(hyperplanes, [pos]);
  updateMarks(hyperplanes, [pos]);

  // Draw hyperplane separating mouse and convex set
  svg.on("mousemove", function (event) {
    const [x, y]   = d3.pointer(event);
    const isInside = intersectionPointCircle({x, y}, circle);

    // Update convex set
    convexSet.selectAll(".convex-set")
      .classed("not-good", isInside);

    // Update hyperplane
    pos = new Hyperplane({ x, y, normal: { x: circle.x - x, y: circle.y - y } });

    updateHyperplanes(hyperplanes, isInside ? [] : [pos]);
    updateMarks(hyperplanes, isInside ? [] : [pos]);
  });
}

function figureSetSupportingHyperplane(id) {
  const svg = d3.select(id);

  const width  = svg.attr("width");
  const height = svg.attr("height");
  const circle = { x: width/2, y: height/2, r: 100 };

  // Initial hyperplane
  let pos = new Hyperplane({
    x: circle.x + circle.r / Math.sqrt(2),
    y: circle.y + circle.r / Math.sqrt(2),
    normal: {x: Math.sqrt(2), y: Math.sqrt(2)}
  });

  // Our convex set
  const convexSet   = svg.append("g");
  const hyperplanes = svg.append("g")
    .attr("width", width)
    .attr("height", width);

  updateConvexSets(convexSet, [circle]);
  updateHyperplanes(hyperplanes, [pos]);
  updateMarks(hyperplanes, [pos]);

  // Draw hyperplane separating mouse and convex set
  svg.on("mousemove", function (event) {
    const [x, y] = d3.pointer(event);
    const isInside = intersectionPointCircle({x, y}, circle);

    // Update convex set
    convexSet.selectAll(".convex-set")
      .classed("not-good", isInside);

    // Update Hyperplane
    const normal = normalize({x: circle.x - x, y: circle.y - y});
    const border = {x: circle.x - normal.x*circle.r, y: circle.y - normal.y*circle.r};
    pos = new Hyperplane ({x: border.x, y: border.y, normal});

    updateHyperplanes(hyperplanes, isInside ? [] : [pos]);
    updateMarks(hyperplanes, isInside ? [] : [pos]);
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


  const planes = svg.append("g")
    .attr("width", width)
    .attr("height", width);

  const bodiesSVG = svg.append("g");

  updateConvexSets(bodiesSVG, bodies)
    .classed("draggable", true)
    .call(d3.drag()
      .on("drag", function(event, c) {
        // Ensure the circle stays within the SVG boundaries
        d3.select(this)
          .attr("cx", c.x = Math.max(0 + c.r, Math.min(width  - c.r, event.x)))
          .attr("cy", c.y = Math.max(0 + c.r, Math.min(height - c.r, event.y)));

        updateScene();
      }));

  function updateScene() {
    const c1 = bodies[0];
    const c2 = bodies[1];
    const inter = circleCircleIntersection(c1, c2)

    bodiesSVG.selectAll(".convex-set").classed("not-good", inter);
    updateHyperplanes(planes, inter ? [] : [circleDisjointSeparator(c1, c2)]);
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

  const yScale = d3.scaleLinear()
    .domain([-1, 2])
    .range([height - margin.bottom, margin.top]);

  const g = svg.append("g");

  plotEpigraph(g, f, xScale, yScale);
  plot(g, f, xScale, yScale);
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

  const fGraph = svg.append("g");
  const cutsGraph = svg.append("g");

  plotEpigraph(fGraph, f, xScale, yScale);
  plot(fGraph, f, xScale, yScale);

  // Display cut for mouse x position
  svg.on("mousemove", function(event) {
    const [mouseX, _] = d3.pointer(event);
    const x0  = xScale.invert(mouseX);
    const cut = new Cut(x0, f(x0), df(x0));
    const hyperplane = cutToHyperplane(cut, xScale, yScale);

    // Show where is the tangent line
    updateHyperplanes(cutsGraph, [hyperplane]);
    updateMarks(cutsGraph, [hyperplane]);
  });
}


function figureFunctionCuts(id, f, df, minX, maxX) {
  const div    = d3.select(id);
  const width = 350;
  const height = 400;
  const margin = {top: 0, right: 10, bottom: 0, left: 10};
  const cuts   = [];
  const hyperplanes = [];

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

  // Polyhedral approximation begins as empty graph
  const func = svgFunc.append("g");
  const poly = svgPoly.append("g");
  const planes = svgFunc.append("g");
  const marks = svgFunc.append("g");

  plot(func, f, xScale, yScale);
  updatePolyhedral(poly, cuts, xScale, yScale);

  svgFunc.on("mousemove", function(event) {
    const [mouseX, _] = d3.pointer(event);
    const x0      = xScale.invert(mouseX);
    const cut     = new Cut(x0,  f(x0), df(x0));
    const hyperplane = cutToHyperplane(cut, xScale, yScale);

    const tmpHyperplanes = hyperplanes.concat(hyperplane);

    updateHyperplanes(planes, tmpHyperplanes)
      .style("opacity", 0.2);
    updatePolyhedral(poly, cuts.concat(cut), xScale, yScale);
    updateMarks(marks, tmpHyperplanes);
  });

  svgFunc.on("click", function(event) {
    const [mouseX, mouseY] = d3.pointer(event);
    const x0      = xScale.invert(mouseX);
    const y0      = f(x0);
    const cut     = new Cut(x0, y0, df(x0));
    const hyperplane = cutToHyperplane(cut, xScale, yScale);
    cuts.push(cut);
    hyperplanes.push(hyperplane);

    updateHyperplanes(planes, hyperplanes)
      .style("opacity", 0.2);
    updatePolyhedral(poly, cuts, xScale, yScale);
    updateMarks(marks, hyperplanes);
  });
}

function figureFunctionEpigraphCarving(id, f, df, minX, maxX) {
  const svg    = d3.select(id);
  const width  = svg.attr("width");
  const height = svg.attr("height");
  const margin = {top: 0, right: 0, bottom: 0, left: 0};
  const cuts   = [];
  const hyperplanes = [];

  const xScale = d3.scaleLinear()
    .domain([minX, maxX])
    .range([margin.left, width - margin.right]);

  const yScale = d3.scaleLinear()
    .domain([0, 5])   // TODO: calculate range for y
    .range([height - margin.bottom, margin.top]);

  const graph  = svg.append("g");
  const planes = svg.append("g");
  const poly   = svg.append("g");
  const marks  = svg.append("g");

  updatePolyhedral(poly, cuts, xScale, yScale);

  plot(graph, f, xScale, yScale)
    .style("opacity", 0.1);

  plotEpigraph(poly, x => 0, xScale, yScale);

  svg.on("click", function(event) {
    const [mouseX, mouseY] = d3.pointer(event);
    const x0      = xScale.invert(mouseX);
    const y0      = Math.min(f(x0), yScale.invert(mouseY));
    const cut     = new Cut(x0, y0, df(x0));
    cuts.push(cut);
    hyperplanes.push(cutToHyperplane(cut, xScale, yScale));

    updatePolyhedral(poly, cuts, xScale, yScale);
    plotEpigraph(poly, cutApproximation(cuts), xScale, yScale);

    // Append the path for the tangent line
    updateHyperplanes(planes, hyperplanes)
      .style("opacity", 0.2);

    // Append a dot at the mouse's x position
    updateMarks(marks, hyperplanes);
  });
}

function makeCut(cut, xScale) {
  return xScale.ticks(2).map(x => ({x: x, y: cut.eval(x)}));
}

function cutToHyperplane(cut, xScale, yScale) {
  // Assuming cut.x represents 'a', cut.fx represents 'b', and cut.dual represents 'k'
  const x = xScale(cut.x); // x-coordinate of a point on the line
  const y = yScale(cut.fx); // y-coordinate of a point on the line

  // Tangent components taking the scale into consideration
  const tx = xScale(cut.x + 1) - xScale(cut.x)
  const ty = yScale(cut.fx + cut.dual) - yScale(cut.fx)

  return new Hyperplane({
    x,
    y,
    normal: normalize({x: -ty, y: tx}),
  });
}

function cutApproximation(cuts) {
   return x => d3.max(cuts, cut => cut.fx + cut.dual*(x - cut.x));
}
