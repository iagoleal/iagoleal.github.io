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


/*
  Drawing figures
*/

function mouseUnscale(event, scale) {
  const [mx, my] = d3.pointer(event);
  return [scale.x.invert(mx), scale.y.invert(my)]
}

function updateHyperplanes(selector, pos) {
  // TODO: Fix these numbers
  const width     = 800;
  const height    = 400;
  const maxLength = Math.sqrt(width ** 2 + height ** 2);

  return selector.selectAll(".hyperplane")
    .data(pos)
    .join("line")
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
    .attr("d", d3.area()
      .x(d => scale.x(d.x))
      .y0(scale.y.range()[1])  // Top of the plot
      .y1(d => scale.y(d.y))   // Function curve
      .curve(d3.curveLinear)
    );

}

/*
 * Good ol' Math
*/

function numdiff(f) {
  return x => {
    const eps = 0.01
    return (f(x+eps) - f(x-eps)) / (2*eps)
  }
}

function dualize(f, minX, maxX) {
  return x => findMax(d3.range(-6, 1.0, 0.1), lambda =>
    d3.min(d3.range(minX, maxX, 0.05), u => f(u) - lambda*(u - x))
  )
}

function mean(xs) {
  return xs.reduce((a, b) => a + b, 0) / xs.length;
}


/*
 * Helpers
*/


function cutConvex(f) {
  const df = numdiff(f);

  return x => new Cut(x, f(x), df(x));
}


/*
  Page figures
*/

class Diagram {
  constructor(id) {
    this.id    = id;
    this.svg   = d3.select(`${id} svg`);
    this.scale = new Scale(this.svg, [-2, 2], [-0.5, 2]);
  }

  // Plot a function's graph
  plot(f, kind) {
    const g = this.svg.append("g");

    plot(g, f, this.scale)
      .classed(kind, true);

    return this;
  }

  samples(fs) {
    this.fs = fs;
    const g = this.svg.append("g");

    plot(g, fs, this.scale)
      .classed("stochastic", true);

    return this;
  }

  // Add a cut for each sample on hover
  withCuts(factory) {
    const g = this.svg.append("g");
    const fcuts = this.fs.map(f => factory(f));

    const placeCut = (x0) => {
      const cuts = fcuts.map(f => f(x0).toHyperplane(this.scale));

      updateHyperplanes(g, cuts)
        .attr("opacity", 0.2);
      // updateMarks(g, cuts)
      //   .classed("stochastic", true);
    };

    this.svg.on("mousemove.cut", e => {
      placeCut(...mouseUnscale(e, this.scale));
    });

    placeCut(1);

    return this;
  }

  withAverageCut(f, factory) {
    const g = this.svg.append("g");

    const placeCut = (x0) => {
      const cuts = factory(f)(x0).toHyperplane(this.scale);

      updateHyperplanes(g, [cuts])
        .classed("deterministic", true);
      updateMarks(g, [cuts]);
    };

    this.svg.on("mousemove.cutAvg", e => {
      placeCut(...mouseUnscale(e, this.scale));
    });

    placeCut(1);

    return this;
  }
}

export function fig_randomFunction(id) {
  const g =  x => 0.5*(x - 1.5)*(x - 1)*(x + 0.5)*(x + 1.5) + 0.4;
  const fs =
    [ x => g(x)
    , x => g(x - 0.5)
    , x => g(x + 0.5)
    , x => g(x - 0.1) + 0.5
    , x => g(x + 0.1) - 0.5
    , x => g(x) - 0.3
    , x => g(x) + 0.3
    ]

  const cvxs = [
    x => 0.2*(x+0.5)**2,
    x => 0.2*(x)**2,
    x => 0.4*(x-0.5)**2,
    x => 0.3*(x-0.3)**2,
  ];

  const cavg = x => mean(cvxs.map(f => f(x)));

  new Diagram("#figure-random-function")
    .samples(fs);

  new Diagram("#figure-random-average")
    .samples(cvxs)
    .plot(cavg, "deterministic");

  new Diagram("#figure-random-cuts")
    .samples(cvxs)
    .withCuts(cutConvex);

  new Diagram("#figure-average-cuts")
    .samples(cvxs)
    .withCuts(cutConvex)
    .plot(cavg, "deterministic")
    .withAverageCut(cavg, cutConvex);
}
