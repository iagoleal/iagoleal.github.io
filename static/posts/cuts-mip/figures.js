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

function numdiff(f) {
  return function(x) {
    const eps = 0.01
    return (f(x+eps) - f(x-eps)) / (2*eps)
  }
}

/*
  Page figures
*/

export function figureContinuousRelaxation(id, mip, relax, minX, maxX) {
  const svg   = d3.select(id);
  const scale = new Scale(svg, [minX, maxX], [-0.5, 2]);

  const gMip     = svg.append("g");
  const gBenders = svg.append("g");

  plotEpigraph(gMip, mip, scale);
  plot(gMip, mip, scale);

  plot(gBenders, relax, scale)
    .attr("class", "relaxation-continuous");
}

export function figureCutBenders(id, mip, relax, minX, maxX) {
  const svg   = d3.select(id);
  const scale = new Scale(svg, [minX, maxX], [-0.5, 2]);

  const gBenders = svg.append("g");
  const gMip     = svg.append("g");
  const gCuts = svg.append("g");

  function placeCut(x0) {
    const cut = new Cut(x0, relax(x0), numdiff(relax)(x0));
    const hyperplane = cut.toHyperplane(scale);

    // Show where is the tangent line
    updateHyperplanes(gCuts, [hyperplane]);
    updateMarks(gCuts, [hyperplane]);
  }

  svg.on("mousemove", function(event) {
    const [x0] = mouseUnscale(event, scale);
    placeCut(x0);
  });

  // Show or not show sliders
  const box = document.getElementById("show-continuous-relaxation");

  d3.select("#show-continuous-relaxation").on("input", function() {
    gBenders.selectAll(".relaxation-continuous")
      .attr("opacity", box.checked ? 1 : 0);
  });

  // Initial plots
  plotEpigraph(gMip, mip, scale);
  plot(gMip, mip, scale);
  plot(gBenders, relax, scale)
    .attr("class", "relaxation-continuous")
    .attr("opacity", box.checked ? 1 : 0);
  placeCut(1);
}
