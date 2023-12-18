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

function lagrangianRelaxation(f, lambda, minX, maxX) {
  return x => d3.min(d3.range(minX, maxX, 0.1), u => f(u) - lambda*(u-x))
}

function cutConvex(f) {
  const df = numdiff(f);

  return x => new Cut(x, f(x), df(x));
}

function cutStrBenders(f, factory, minX, maxX) {
  return x => {
    const cut = factory(x);
    cut.fx = lagrangianRelaxation(f, cut.dual, minX, maxX)(x);

    return cut;
  }
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
  constructor(id, minX, maxX) {
    this.id    = id;
    this.svg   = d3.select(`${id} svg`);
    this.minX  = minX;
    this.maxX  = maxX;
    this.scale = new Scale(this.svg, [minX, maxX], [-0.5, 2]);
  }

  // Plot a function and its epigraph
  epigraph(f) {
    const g = this.svg.append("g");

    plotEpigraph(g, f, this.scale);
    plot(g, f, this.scale);

    return this;
  }

  // Plot a function's graph
  plot(f, kind) {
    const g = this.svg.append("g");

    plot(g, f, this.scale)
      .attr("class", kind);

    return this;
  }

  // Plot a function subject to a checkbox toggle
  plotToggleable(f, name, kind) {
    const box = d3.select(`${this.id} input[name="${name}"]`);
    const g   = this.svg.append("g");

    box.on("input.toggle", () => {
      g.selectAll(`.${kind}`)
        .attr("opacity", box.property("checked") ? 1 : 0);
    });

    // Initial plots
    plot(g, f, this.scale)
      .attr("class", kind)
      .attr("opacity", box.property("checked") ? 1 : 0);

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

    this.svg.on("mousemove.cut", e => {
      placeCut(...mouseUnscale(e, this.scale));
    });

    placeCut(1);

    return this;
  }

  // Allow the user to strengthen a cut by clicking on the diagram
  cutStrenghten(factory) {
    const scale = this.scale;

    // Stop cut lifting animation whenever the user chooses another position
    this.svg.on("mousemove.strenghtened", () => {
      this.svg.selectAll(".hyperplane, .mark").interrupt("strenghtened-cut");
    });

    // By clicking in the diagram, the user can change the current cut position.
    this.svg.on("click.strenghtened", e => {
      const [x0] = mouseUnscale(e, scale);

      const cut = factory(x0).toHyperplane(this.scale);

      const width     = 800;
      const height    = 400;
      const maxLength = Math.sqrt(width ** 2 + height ** 2);

      this.svg.selectAll(".hyperplane")
        .data([cut])
          .transition("strenghtened-cut")
            .duration(750)
            .attr("x1", d => d.x - d.tangent.x * maxLength)
            .attr("y1", d => d.y - d.tangent.y * maxLength)
            .attr("x2", d => d.x + d.tangent.x * maxLength)
            .attr("y2", d => d.y + d.tangent.y * maxLength)

      this.svg.selectAll(".mark")
        .data([cut])
          .transition("strenghtened-cut")
            .duration(750)
            .attr("cx", d => d.x)
            .attr("cy", d => d.y)
    });

    return this;
  }

  components(fs) {
    const g = this.svg.append("g");
    const cip = x => d3.min(fs.map(f => f(x)));

    const turnOn = (x0, y0) => {
      const smallest = d3.minIndex(fs, f => f(x0));

      plotEpigraph(g, y0 >= cip(x0) ? fs[smallest] : [], this.scale)
        .classed("epigraph-component", true);
    };

    this.svg.on("mousemove.components", e => {
      turnOn(...mouseUnscale(e, this.scale));
    });

    turnOn(this.maxX, 2);
  }
}

export function figureOVF(id, f, minX, maxX) {
  return new Diagram(id, minX, maxX)
    .epigraph(f);
}

export function figureContinuousRelaxation(id, mip, relax, minX, maxX) {
  return new Diagram(id, minX, maxX)
    .epigraph(mip)
    .plot(relax, "relaxation-continuous");
}

export function figureCutBenders(id, mip, relax, minX, maxX) {
  return new Diagram(id, minX, maxX)
    .epigraph(mip)
    .plotToggleable(relax, "show-relaxation-continuous", "relaxation-continuous")
    .cut(cutConvex(relax, minX, maxX));
}

export function figureCutLagrangian(id, mip, minX, maxX) {
  const dual = dualize(mip, minX, maxX);

  return new Diagram(id, minX, maxX)
    .epigraph(mip)
    .plotToggleable(x => dual(x).value, "show-relaxation-dual", "relaxation-dual")
    .cut(cutLagrangian(mip, minX, maxX));
}

export function figureCutStrenghtenedBenders(id, mip, relax, minX, maxX) {
  const dual    = dualize(mip, minX, maxX);
  const benders = cutConvex(relax);

  return new Diagram(id, minX, maxX)
    .epigraph(mip)
    .plotToggleable(relax, "show-relaxation-continuous", "relaxation-continuous")
    .plotToggleable(x => dual(x).value, "show-relaxation-dual", "relaxation-dual")
    .cut(benders)
    .cutStrenghten(cutStrBenders(mip, benders, minX, maxX));
}

export function figureMinOVF(id, fs, minX, maxX) {
  const cip = x => d3.min(fs.map(f => f(x)));

  return new Diagram(id, minX, maxX)
    .epigraph(cip)
    .components(fs)
}

export function figureSwitch(id, fs, minX, maxX) {
  const svg   = d3.select(`${id} svg`);
  const box   = d3.select(`${id} input[name="component"]`);
  const scale = new Scale(svg, [minX, maxX], [-0.5, 2]);

  const gComponents = svg.append("g");
  const gActive     = svg.append("g");

  box.on("input", function() {
    plotEpigraph(gActive, this.checked ? fs[1] : fs[0], scale)
      .classed("epigraph-component", true);
  });

  plotEpigraph(gComponents, fs, scale)
    .attr("opacity", 0.3);
  plotEpigraph(gActive, box.property("checked") ? fs[1] : fs[0], scale)
    .classed("epigraph-component", true);
}
