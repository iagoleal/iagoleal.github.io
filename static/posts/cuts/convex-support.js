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

function updateHyperplane(svg, pos) {
  const width     = svg.attr("width");
  const height    = svg.attr("height");
  const maxLength = Math.sqrt(width ** 2 + height ** 2);

  return svg.selectAll(".hyperplane")
    .data(pos ? [pos] : [])
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

  return svg.selectAll(".function-graph")
    .data([fData])
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

  return svg.selectAll(".epigraph")
    .data([fData])
    .join("path")
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

  updateHyperplane(svg, pos);

  // Draw hyperplane separating mouse and convex set
  svg.on("mousemove", function (event) {
    const [x, y] = d3.pointer(event);
    const isInside = intersectionPointCircle({x, y}, circle);

    // Update convex set
    convexSet.classed("not-good", isInside);

    // Update hyperplane
    const normal = normalize({x: circle.x - x, y: circle.y - y});
    pos = {x, y, normal};

    updateHyperplane(svg, isInside? null : pos);

    // Mouse marker
    svg.selectAll(".mark").remove();
    if (!isInside) {
      mark(svg, pos.x, pos.y).attr("fill", "orange");
    }
  });
}

function figureSetSupportingHyperplane(id) {
  const svg = d3.select(id);

  const width     = svg.attr("width");
  const height    = svg.attr("height");
  const circle    = { x: width/2, y: height/2, r: 100 };

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

  updateHyperplane(svg, pos);

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

    updateHyperplane(svg, isInside ? null : pos);

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
      updateHyperplane(svg, null);
    } else {
      bodiesSVG.classed("not-good", false);

      // New coordinates for hyperplane
      const normal = normalize({x: c2.x - c1.x, y: c2.y - c1.y});
      const mid = {
        x: (c1.x + normal.x * c1.r + c2.x - normal.x * c2.r) / 2,
        y: (c1.y + normal.y * c1.r + c2.y - normal.y * c2.r) / 2,
      }

      let pos = {x: mid.x, y: mid.y, normal};

      updateHyperplane(svg, pos).lower();
    }
  }

  updateScene(); // Initial hyperplane calculation
}


function makeCut(f, df, x0, minX, maxX) {
  return d3.range(minX, maxX, 0.01).map(x => ({x: x, y: f(x0) + df(x0) * (x - x0)}));
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
  svg.on("mousemove", function(event, d) {
    const [mouseX, _] = d3.pointer(event);
    const x0      = xScale.invert(mouseX);
    const cutData = makeCut(f, df, x0, minX, maxX);

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
