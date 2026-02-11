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

function intersectionPointConvex(point, path) {
  // pathElem: SVGPathElement
  // point: {x, y}
  // Returns true if point is inside the path's fill or stroke
  return path?.isPointInFill
    ? path.isPointInFill(new DOMPoint(point.x, point.y))
    : false;
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

function supportingHyperplane(pathElem, point) {
  // Find the closest point on the path to 'point'
  const totalLength = pathElem.getTotalLength();
  let minDist = Infinity;
  let best = 0;

  // Sample points along the path
  const steps = 200;
  for (let i = 0; i <= steps; ++i) {
    const len = (i / steps) * totalLength;
    const p = pathElem.getPointAtLength(len);
    const dx = p.x - point.x;
    const dy = p.y - point.y;
    const dist = dx * dx + dy * dy;
    if (dist < minDist) {
      minDist = dist;
      best = len;
    }
  }

  // Compute tangent at bestLen
  const delta = 1;
  const p1 = pathElem.getPointAtLength(Math.max(0, best - delta));
  const p2 = pathElem.getPointAtLength(Math.min(totalLength, best + delta));
  const tangent = {
    x: p2.x - p1.x,
    y: p2.y - p1.y,
  };

  // Closest point on path
  const normal  = { x: -tangent.y, y: tangent.x };
  const closest = pathElem.getPointAtLength(best);

  return new Hyperplane({x: closest.x, y: closest.y, normal});
}


/*
  Drawing figures
*/

function mouseUnscale(event, scale) {
  const [mx, my] = d3.pointer(event);
  return [scale.x.invert(mx), scale.y.invert(my)]
}

function updateConvexCircles(selector, data) {
  return selector.selectAll(".convex-set")
    .data(data)
    .join("circle")
    .attr("cx", d => d.x)
    .attr("cy", d => d.y)
    .attr("r",  d => d.r)
    .attr("class", "convex-set");
}

function updateConvexSets(selector, data) {
  return selector.selectAll(".convex-set")
    .data(data)
    .join("path")
    .attr("d", d => d.d)
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

function numdiff(f) {
  return x => {
    const eps = 0.01
    return (f(x+eps) - f(x-eps)) / (2*eps)
  }
}

function dualize(f, minX, maxX) {
  return x => findMax(d3.range(-12, 8.0, 0.1), lambda =>
    d3.min(d3.range(minX, maxX, 0.05), u => f(u) - lambda*(u - x))
  )
}

function dual(f, minX, maxX) {
  return x => dualize(f, minX, maxX)(x).value;
}

function Lagrangian(f, lambda, minX, maxX) {
  return x => d3.min(d3.range(minX, maxX, 0.1), u => f(u) - lambda*(u - x))
}

/*
 * Cut factories
*/

function cutExact(f, df) {
  return x => new Cut(x, f(x), df(x));
}

function cutExactAt(f, df) {
  return (x, y) => new Cut(x, Math.min(f(x), y), df(x));
}

function cutLagrangian(f, minX, maxX) {
  const dual = dualize(f, minX, maxX);

  return x => {
    const dx = dual(x);
    return new Cut(x, dx.value, dx.argument);
  }
}

/*
  Page figures
*/

class Diagram {
  constructor(id, minX, maxX, minY, maxY) {
    this.id    = id;
    this.svg   = d3.select(`${id} svg`);
    this.minX  = minX;
    this.maxX  = maxX;
    this.scale = new Scale(this.svg, [minX, maxX], [minY, maxY]);
  }


  #pool = {cuts: [], hyperplanes: []};
  #cutListeners = [];

  #pushCut(cut) {
    // Update pool
    this.#pool.cuts.push(cut);
    this.#pool.hyperplanes.push(cut.toHyperplane(this.scale));

    // Tell everybody about it (simulate reactivity)
    this.#cutListeners.forEach(l => l(this.#pool));

    return this;
  }

  #resetCuts() {
    // Clean up pool
    this.#pool.cuts        = [];
    this.#pool.hyperplanes = [];

    // Tell everybody about it (simulate reactivity)
    this.#cutListeners.forEach(l => l(this.#pool));

    return this;
  }

  // Plot a function's graph
  plot(f, ...classes) {
    const g = this.svg.append("g");

    const p = plot(g, f, this.scale);
    for (const c of classes) {
      p.classed(c, true);
    }

    return this;
  }

  // Plot a function and its epigraph
  epigraph(f) {
    const g = this.svg.append("g");

    plotEpigraph(g, f, this.scale);
    plot(g, f, this.scale);

    return this;
  }

  // Add a cut on hover
  cut(factory) {
    const g = this.svg.append("g");

    const placeCut = (x0) => {
      const cut = factory(x0).toHyperplane(this.scale);

      updateHyperplanes(g, [cut]);
      updateMarks(g, [cut]);
    };

    this.svg.node().addEventListener("mousemove", e => {
      placeCut(...mouseUnscale(e, this.scale));
    });

    placeCut(1);

    return this;
  }

  cuts(factory) {
    const g = this.svg.append("g");

    const placeCut = (x0, y0) => {
      if (x0 && y0) {
        const cut = factory(x0, y0);
        this.#pushCut(cut);
      }
    }

    this.svg.on("click", e => placeCut(...mouseUnscale(e, this.scale)));

    this.#cutListeners.push(() => {
      // Append the path for the tangent line
      updateHyperplanes(g, this.#pool.hyperplanes)
        .style("opacity", 0.2);
      // Append a dot at the mouse's x position
      updateMarks(g, this.#pool.hyperplanes);
    });

    return this;
  }

  polyhedral() {
    const g = this.svg.append("g");

    const update = () => {
      plotEpigraph(g, cutApproximation(this.#pool.cuts), this.scale);
      updatePolyhedral(g, this.#pool.cuts, this.scale);
    }

    this.#cutListeners.push(update);

    update();

    return this;
  }

  buttonReset(id) {
    document.querySelector(id).addEventListener("click", () => {
      this.#resetCuts();
    });

    return this;
  }
}

function figureConvexSetHyperplane(id, hyperplaneFn, initialPoint = {x: 200, y: 0}) {
  const svg = d3.select(`${id} svg`);
  const convexPath = {
    d: `
      M -25 -120
      C -100 -145 -200 -20 -175 80
      C -150 155 -50 130 0 105
      C 75 55 206 -40 125 -70
      Z`
  };

  // Draw convex set
  const gSet   = svg.append("g");
  const gPlane = svg.append("g");
  updateConvexSets(gSet, [convexPath]);
  const pathElem = gSet.select(".convex-set").node();

  function updateScene(x, y) {
    const isInside = intersectionPointConvex({x, y}, pathElem);

    // Update convex set
    gSet.selectAll(".convex-set")
      .classed("not-good", isInside);

    // Compute hyperplane
    const pos = hyperplaneFn(pathElem, {x, y});

    updateHyperplanes(gPlane, isInside ? [] : [pos]);
    updateMarks(gPlane, isInside ? [] : [pos]);
  }

  svg.on("mousemove", event => updateScene(...d3.pointer(event)));
  updateScene(initialPoint.x, initialPoint.y);
}

// Point hyperplane: normal from mouse to boundary (at mouse location)
function pointHyperplane(pathElem, point) {
  const hp = supportingHyperplane(pathElem, point);
  hp.x = point.x;
  hp.y = point.y;

  return hp;
}

// Usage:
export function figureSetPointHyperplane(id) {
  figureConvexSetHyperplane(id, pointHyperplane, {x: 200, y: 0});
}

export function figureSetSupportingHyperplane(id) {
  figureConvexSetHyperplane(id, supportingHyperplane, {x: 200, y: 0});
}


export function figureSetSeparatingHyperplane(id) {
  const svg = d3.select(`${id} svg`);
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

  updateConvexCircles(gBodies, bodies)
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
  return new Diagram(id, minX, maxX, -1, 2)
    .epigraph(f);
}

export function figureFunctionSupportingCut(id, f, df, minX, maxX) {
  return new Diagram(id, minX, maxX, 0, 5)
    .epigraph(f)
    .cut(cutExact(f, df));
}

export function figureLagrangianDual(id, f, minX, maxX) {
  return new Diagram(id, minX, maxX, -1.5, 2)
    .epigraph(f)
    .plot(dual(f, minX, maxX), "relaxation-dual")
    .cut(cutLagrangian(f, minX, maxX));
}

export function figureFunctionEpigraphCarving(id, f, df, minX, maxX) {
  return new Diagram(id, minX, maxX, 0, 5)
    .polyhedral()
    .cuts(cutExactAt(f, df))
    .buttonReset("#reset-epigraph-carving");
}

export function figureLagrangian(id, f, minX, maxX) {
  const svg   = d3.select(`${id} svg`);
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

    plot(gLagrangian, Lagrangian(f, lambda), scale);
  }

  // Allow choosing lambda on the slider
  d3.select("#slider-lagrangian-lambda").on("input", updateLagrangian);

  plotEpigraph(gFunc, f, scale);
  plot(gFunc, f, scale);
  updateLagrangian();
}

export function figureCutHeight(id, f, minX, maxX) {
  const svg   = d3.select(`${id} svg`);
  const scale = new Scale(svg, [minX, maxX], [-1.5, 2]);

  const gFunc       = svg.append("g");
  const gLagrangian = svg.append("g");

  const x_0    = 1.35;
  const lambda = numdiff(f)(x_0);

  function updateLagrangian() {
    const intercept = +d3.select("#slider-lagrangian-b").property("valueAsNumber");

    const sliderLabel = document.getElementById("slider-b-value");
    katex.render(`b = ${intercept}`, sliderLabel);

    const affine = x => intercept + lambda*(x-x_0);
    plot(gLagrangian, affine(x_0) <= f(x_0) ? affine : [], scale)

    gFunc.selectAll(".epigraph").classed("not-good", affine(x_0) > f(x_0));
  }
  // Allow choosing lambda on the slider
  d3.select("#slider-lagrangian-b").on("input", updateLagrangian);

  plotEpigraph(gFunc, f, scale);
  plot(gFunc, f, scale);

  updateLagrangian();
}



export function figureFunctionCuts(id, f, df, minX, maxX) {
  const width     = 400;
  const height    = 400;
  let cuts        = [];
  let hyperplanes = [];

  const svgs = d3.selectAll(`${id} > .diagram-container`).append("svg")
    .attr("viewBox", `${-width/2} ${-height/2} ${width} ${height}`)
    .attr("preserveAspectRatio", "none")
    .style("max-height", "50vw")
    .attr("class", "diagram");

  const svgFunc = d3.select(svgs.nodes()[0]);
  const svgPoly = d3.select(svgs.nodes()[1]);

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

  d3.select("#reset-function-cuts").on("click", function(_event) {
    cuts        = [];
    hyperplanes = [];

    updateScene(cuts, hyperplanes);
  });

  plot(gFunc, f, scale);
  updateScene(cuts, hyperplanes);
}
