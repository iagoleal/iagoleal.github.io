import * as d3 from "https://cdn.jsdelivr.net/npm/d3@7/+esm";

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

  toHyperplane(scale) {
    const x = scale.x(this.x);
    const y = scale.y(this.fx);

    // Tangent components taking the scale into consideration
    const tx = scale.x(this.x + 1) - scale.x(this.x)
    const ty = scale.y(this.fx + this.dual) - scale.y(this.fx)

    return new Hyperplane({ x, y, normal: normalize({ x: -ty, y: tx }) });
  }
}

// Turn a list of cuts into a polyhedral function.
function cutApproximation(cuts) {
  if (cuts !== undefined && cuts.length > 0) {
    return x => d3.max(cuts, cut => cut.fx + cut.dual*(x - cut.x));
  }
}

class Scale {
  constructor(svg, xDomain, yDomain) {
    const viewbox = svg.attr("viewBox").split(" ").map(x => +x);

    if (! Array.isArray(viewbox) || viewbox.length != 4) {
      console.error("Problem querying viewBox from SVG.");
    }

    this.x = d3.scaleLinear()
      .domain(xDomain)
      .range([viewbox[0], viewbox[0] + viewbox[2]]);

    this.y = d3.scaleLinear()
      .domain(yDomain)
      .range([viewbox[1] + viewbox[3], viewbox[1]]);  // Invert y-axis for plotting functions
  }
}


/*
  General Geometry
*/

function findMax(a, f) {
  return a.reduce((acc, x, i) => {
      const fx = f(x)
      if (fx > acc.value) {
        acc.value    = fx;
        acc.argument = x;
        acc.index    = i;
      }
      return acc;
    },
    { value: -Infinity, argument: undefined, index: -1 }
  );
}

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

function intersectionCircleCircle(a, b) {
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

function mouseUnscale(event, scale) {
  const [mx, my] = d3.pointer(event);
  return [scale.x.invert(mx), scale.y.invert(my)]
}

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
  const width     = 800;
  const height    = 400;
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

function plot(elem, fs, scale) {
  fs = Array.isArray(fs) ? fs : [fs]
  const fData = fs.map(f => scale.x.ticks(400).map(x => ({x: x, y: f(x)})));

  return elem.selectAll(".function-graph")
    .data(fData)
    .join("path")
    .attr("class", "function-graph")
    .attr("d", d3.line()
      .x(d => scale.x(d.x))
      .y(d => scale.y(d.y))
    );
}

function plotEpigraph(elem, fs, scale) {
  fs = Array.isArray(fs) ? fs : [fs]
  const ymin  = scale.y.domain()[0]
  const fData = fs.map(f => scale.x.ticks(400).map(x => ({
    x: x,
    y: (f === undefined) ? ymin : f(x),
  })));

  return elem.selectAll(".epigraph")
    .data(fData)
    .join("path")
    .attr("class", "epigraph")
    .transition()
    .duration(200)
    .ease(d3.easeCubic)
    .attr("d", d3.area()
      .x(d => scale.x(d.x))
      .y0(scale.y.range()[1])  // Top of the plot
      .y1(d => scale.y(d.y))   // Function curve
      .curve(d3.curveLinear)
    );

}

function updatePolyhedral(poly, cuts, scale) {
  const f = cutApproximation(cuts);
  const ymin = scale.y.domain()[0]
  const fData = scale.x.ticks(400).map(x => ({
    x: x,
    y: f === undefined ? ymin : f(x),
  }));

  return poly.selectAll(".polyhedral")
    .data([fData])
    .join("path")
    .attr("class", "function-graph polyhedral")
    .transition()
    .duration(200)
    .ease(d3.easeCubic)
    .attr("d", d3.line()
      .x(d => scale.x(d.x))
      .y(d => scale.y(d.y))
    );
}

/*
  Page figures
*/

export function figureSetPointHyperplane(id) {
  const svg = d3.select(id);
  const circle = { x: 0, y: 0, r: 100 };

  // Our convex set
  const gSet   = svg.append("g");
  const gPlane = svg.append("g");

  function updateScene(x, y) {
    const isInside = intersectionPointCircle({x, y}, circle);

    // Update convex set
    gSet.selectAll(".convex-set")
      .classed("not-good", isInside);

    // Update hyperplane
    const pos = new Hyperplane({x, y, normal: {x: circle.x - x, y: circle.y - y}});

    updateHyperplanes(gPlane, isInside ? [] : [pos]);
    updateMarks(gPlane, isInside ? [] : [pos]);
  }

  svg.on("mousemove", event => updateScene(...d3.pointer(event)));

  updateConvexSets(gSet, [circle]);
  updateScene(circle.r, circle.r);
}

export function figureSetSupportingHyperplane(id) {
  const svg = d3.select(id);
  const circle = { x: 0, y: 0, r: 100 };

  // Our convex set
  const gSet   = svg.append("g");
  const gPlane = svg.append("g");

  function updateScene(x, y) {
    const isInside = intersectionPointCircle({x, y}, circle);

    // Update convex set
    gSet.selectAll(".convex-set")
      .classed("not-good", isInside);

    // Update Hyperplane
    const normal = normalize({x: circle.x - x, y: circle.y - y});
    const border = {x: circle.x - normal.x * circle.r, y: circle.y - normal.y * circle.r};
    const pos    = new Hyperplane ({x: border.x, y: border.y, normal});

    updateHyperplanes(gPlane, isInside ? [] : [pos]);
    updateMarks(gPlane, isInside ? [] : [pos]);
  }

  svg.on("mousemove", event => updateScene(...d3.pointer(event)));

  updateConvexSets(gSet, [circle]);
  updateScene(circle.r, circle.r);
}

export function figureSetSeparatingHyperplane(id) {
  const svg  = d3.select(id);
  const [minX, minY, width, height] = svg.attr("viewBox").split(" ").map(x => +x);

  // data for convex bodies
  const bodies = [
    { x: minX + width*0.3, y: minY + height*0.3, r: 80,},
    { x: minX + width*0.6, y: minY + height*0.6, r: 50, }
  ];

  const gPlane  = svg.append("g");
  const gBodies = svg.append("g");

  function updateScene() {
    const c1 = bodies[0];
    const c2 = bodies[1];
    const inter = intersectionCircleCircle(c1, c2)

    gBodies.selectAll(".convex-set").classed("not-good", inter);
    updateHyperplanes(gPlane, inter ? [] : [circleDisjointSeparator(c1, c2)]);
  }

  updateConvexSets(gBodies, bodies)
    .classed("draggable", true)
    .call(d3.drag()
      .on("drag", function(event, c) {
        // Ensure the circle stays within the SVG boundaries
        d3.select(this)
          .attr("cx", c.x = Math.max(minX + c.r, Math.min(minX + width  - c.r, event.x)))
          .attr("cy", c.y = Math.max(minY + c.r, Math.min(minY + height - c.r, event.y)));

        updateScene();
      }));

  updateScene(); // Initial hyperplane calculation
}

export function figureFunctionEpigraph(id, f, minX, maxX) {
  const svg   = d3.select(id);
  const scale = new Scale(svg, [minX, maxX], [-1, 2]);

  const g = svg.append("g");

  plotEpigraph(g, f, scale);
  plot(g, f, scale);
}


export function figureFunctionSupportingCut(id, f, df, minX, maxX) {
  const svg   = d3.select(id);
  const scale = new Scale(svg, [minX, maxX], [0, 5]);

  const gFunc = svg.append("g");
  const gCuts = svg.append("g");

  function placeCut(x0) {
    const cut = new Cut(x0, f(x0), df(x0));
    const hyperplane = cut.toHyperplane(scale);

    // Show where is the tangent line
    updateHyperplanes(gCuts, [hyperplane]);
    updateMarks(gCuts, [hyperplane]);
  }

  // Display cut for mouse x position
  svg.on("mousemove", function(event) {
    const [x0] = mouseUnscale(event, scale);
    placeCut(x0);
  });

  plotEpigraph(gFunc, f, scale);
  plot(gFunc, f, scale);
  placeCut(1);
}


export function figureFunctionCuts(id, f, df, minX, maxX) {
  const width     = 350;
  const height    = 400;
  let cuts        = [];
  let hyperplanes = [];

  const svgFunc= d3.select(`${id} > div:first-child`).append("svg")
    .attr("viewBox", `0 0 ${width} ${height}`)
    .attr("width", "100%")
    .attr("height", "100%")
    .attr("class", "diagram");

  const svgPoly = d3.select(`${id} > div:last-child`).append("svg")
    .attr("viewBox", `0 0 ${width} ${height}`)
    .attr("width", "100%")
    .attr("height", "100%")
    .attr("class", "diagram");

  const scale = new Scale(svgFunc, [minX, maxX], [0, 5]);

  // Polyhedral approximation begins as empty graph
  const gFunc   = svgFunc.append("g");
  const gPlanes = svgFunc.append("g");
  const gMarks  = svgFunc.append("g");
  const gPoly   = svgPoly.append("g");

  function updateScene(cuts, hyperplanes) {
    updateHyperplanes(gPlanes, hyperplanes)
      .style("opacity", 0.2);
    updateMarks(gMarks, hyperplanes);
    updatePolyhedral(gPoly, cuts, scale);
  }

  svgFunc.on("mousemove", function(event) {
    const [x0]       = mouseUnscale(event, scale);
    const cut        = new Cut(x0, f(x0), df(x0));
    const hyperplane = cut.toHyperplane(scale);

    updateScene(cuts.concat(cut), hyperplanes.concat(hyperplane));
  });

  svgFunc.on("click", function(event) {
    const [x0]       = mouseUnscale(event, scale);
    const cut        = new Cut(x0, f(x0), df(x0));
    const hyperplane = cut.toHyperplane(scale);
    cuts.push(cut);
    hyperplanes.push(hyperplane);

    updateScene(cuts, hyperplanes);
  });

  d3.select("#reset-function-cuts").on("click", function(event) {
    cuts        = [];
    hyperplanes = [];

    updateScene(cuts, hyperplanes);
  });

  plot(gFunc, f, scale);
  updateScene(cuts, hyperplanes);
}

export function figureFunctionEpigraphCarving(id, f, df, minX, maxX) {
  const svg   = d3.select(id);
  const scale = new Scale(svg, [minX, maxX], [0, 5]);

  const graph  = svg.append("g");
  const planes = svg.append("g");
  const poly   = svg.append("g");
  const marks  = svg.append("g");

  let cuts        = [];
  let hyperplanes = [];

  function updateScene(x0, y0) {
    if (x0 && y0) {
      const cut = new Cut(x0, Math.min(f(x0), y0), df(x0));
      cuts.push(cut);
      hyperplanes.push(cut.toHyperplane(scale));
    }

    plotEpigraph(poly, cutApproximation(cuts), scale);
    updatePolyhedral(poly, cuts, scale);
    // Append the path for the tangent line
    updateHyperplanes(planes, hyperplanes)
      .style("opacity", 0.2);
    // Append a dot at the mouse's x position
    updateMarks(marks, hyperplanes);
  }

  svg.on("click", function(event) {
    updateScene(...mouseUnscale(event, scale));
  });

  d3.select("#reset-epigraph-carving").on("click", function(event) {
    cuts        = [];
    hyperplanes = [];

    updateScene()
  });

  plot(graph, f, scale)
    .style("opacity", 0.1);
  updateScene();
}

export function figureLagrangian(id, f, minX, maxX) {
  const svg   = d3.select(id);
  const scale = new Scale(svg, [minX, maxX], [-1.5, 2]);

  const gFunc       = svg.append("g");
  const gLagrangian = svg.append("g");

  function Lagrangian(f, lambda) {
    return x => d3.min(d3.range(minX, maxX, 0.1), u => f(u) - lambda*(u-x))
  }

  function updateLagrangian() {
    const lambda = +d3.select("#slider-lagrangian-lambda").property("valueAsNumber");

    const sliderLabel = document.getElementById("slider-lambda-value");
    katex.render(`\\lambda = ${lambda}`, sliderLabel);

    plot(gLagrangian, Lagrangian(f, lambda), scale)
      .classed("hyperplane", true);
  }

  // Allow choosing lambda on the slider
  d3.select("#slider-lagrangian-lambda").on("input", updateLagrangian);

  plot(gFunc, f, scale);
  plotEpigraph(gFunc, f, scale);
  updateLagrangian();
}

export function figureLagrangianDual(id, f, minX, maxX) {
  const svg   = d3.select(id);
  const scale = new Scale(svg, [minX, maxX], [-1.5, 2]);

  const gFunc       = svg.append("g");
  const gLagrangian = svg.append("g");
  const gCuts       = svg.append("g");

  function dual(x) {
    return findMax(d3.range(-12, 8.0, 0.1), lambda =>
      d3.min(d3.range(minX, maxX, 0.1), u => f(u) - lambda*(u - x))
    )
  }

  function placeCut(x0) {
    const d0  = dual(x0);
    const cut = new Cut(x0, d0.value, d0.argument);
    const hyperplane = cut.toHyperplane(scale);

    // Show where is the tangent line
    updateHyperplanes(gCuts, [hyperplane]);
    updateMarks(gCuts, [hyperplane]);
  }

  // Display cut for mouse x position
  svg.on("mousemove", function(event) {
    const [x0] = mouseUnscale(event, scale);
    placeCut(x0);
  });

  plot(gFunc, f, scale);
  plotEpigraph(gFunc, f, scale);
  plot(gLagrangian, x => dual(x).value, scale);
  placeCut(1.5);
}
