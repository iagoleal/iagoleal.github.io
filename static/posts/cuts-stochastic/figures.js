import * as d3 from "https://cdn.jsdelivr.net/npm/d3@7/+esm";

const minX = -2, maxX = 2;

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
    .attr("fill", "currentColor")
    .attr("class", "mark");
}

function plot(elem, fs, scale) {
  fs = Array.isArray(fs) ? fs : [fs];
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
  return x => findMax(d3.range(-12, 8.0, 0.1), lambda =>
    d3.min(d3.range(minX, maxX, 0.05), u => f(u) - lambda*(u - x))
  )
}

function mean(xs) {
  return xs.reduce((a, b) => a + b, 0) / xs.length;
}

function meanf(fs) {
  return x => mean(fs.map(f => f(x)));
}


/*
 * Helpers
*/

class StoFunc extends Array {
  constructor(fs) {
    if (Array.isArray(fs)) {
      super(...fs);
    } else {
      super(fs);
    }
  }

  get mean() {
    return x => mean(this.map(f => f(x)));
  }

  get linked() {
    return x => dualize(this.mean, -2, 2)(x).value;
  }

  get decomp() {
    return meanf(this.map(f => x => dualize(f, -2, 2)(x).value));
  }
}


function cutsConvex(fs, x) {
  function cutConvex(f, x) {
    const df = numdiff(f);

    return new Cut(x, f(x), df(x));
  }

  return fs.map(f => cutConvex(f, x));
}

function cutsLagrangian(fs, x) {
  function cutLagrangian(f, x) {
    const dx = dualize(f, minX, maxX)(x);

    return new Cut(x, dx.value, dx.argument);
  }

  return fs.map(f => cutLagrangian(f, x));
}

function cutAverage(fs, x) {
  const cuts = cutsConvex(fs, x);
  const fx   = mean(cuts.map(c => c.fx));
  const l    = mean(cuts.map(c => c.dual));

  return [new Cut(x, fx, l)];
}

function cutDecomposed(fs, x) {
  const cuts = cutsLagrangian(fs, x);
  const fx   = mean(cuts.map(c => c.fx));
  const l    = mean(cuts.map(c => c.dual));

  return [new Cut(x, fx, l)];
}

function svgObjectToInline(svg) {
  const inner = svg.contentDocument.getElementsByTagName("svg")[0];
  const parent = svg.parentNode;

  // Hello inline
  parent.appendChild(inner);
  // Goodbye external
  parent.removeChild(svg);
}

// Based on https://www.visualcinnamon.com/2016/06/glow-filter-d3-visualization/
function addGlowFilter(svg) {
  //Container for the gradients
  const defs = svg.append("defs");

  //Filter for the outside glow
  const filter = defs.append("filter")
    .attr("id","glow");

  filter.append("feGaussianBlur")
    .attr("stdDeviation","3.5")
    .attr("result","coloredBlur");

  const feMerge = filter.append("feMerge");
  feMerge.append("feMergeNode")
    .attr("in","coloredBlur");
  feMerge.append("feMergeNode")
    .attr("in","SourceGraphic");
}

/*
  Page figures
*/

class Diagram {
  constructor(id, f) {
    this.id    = id;
    this.svg   = d3.select(`${id} svg`);
    this.scale = new Scale(this.svg, [-2, 2], [-0.5, 2]);

    this.f = f;
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

  samples() {
    return this.plot(this.f, "stochastic");
  }

  // Add a cut for each sample on hover
  // factory gets a `StoFunc` and a point and returns an array of cuts for it
  cuts(fs, factory, ...classes) {
    const g = this.svg.append("g");

    const placeCut = (x0) => {
      const cuts = factory(fs, x0).map(c => c.toHyperplane(this.scale));

      const p = updateHyperplanes(g, cuts)
        .attr("opacity", 0.2);
      const m = updateMarks(g, cuts);

      for (const c of classes) {
        p.classed(c, true);
        m.classed(c, true);
      }
    };

    this.svg.node().addEventListener("mousemove", e => {
      placeCut(...mouseUnscale(e, this.scale));
    });

    placeCut(1);

    return this;
  }
}


/* Tree animation */

function randomR(lo, up) {
  return Math.random() * (up - lo) + lo;
}

// Make the stroke glow and then collapse to a point
function strokeZap(path, speed, color, reverse = false) {
  const sign = reverse ? 1 : -1;
  const nodes = Array.isArray(path) ? path : [path];

  return Promise.all(nodes.map(node => {
    const len = node.getTotalLength();
    const dur = len / speed;
    const kf  = {
      strokeDashoffset: [0, sign * len],
      strokeDasharray:  [len, len],
      stroke:           [color, color],
      strokeWidth:      ["2pt", "2pt"],
      strokeOpacity:    [0.5, 0.5],
    };

    return node.animate(kf, dur).finished;
  }));
}


function blink(path, time, color) {
  const nodes = Array.isArray(path) ? path : [path];
  const cAttention = "var(--color-attention, hsl(188 49% 55%))";
  const style = ({
    fill:        [color, cAttention],
    fillOpacity: [1, 0],
    easing:      "ease-in",
  });

  return Promise.all(nodes.map(node =>
    node.animate(style, time).finished)
  );
}


function sleep(ms) {
  return new Promise(r => setTimeout(r, ms));
}

function asyncForever(p) {
  (async () =>{
    while (true)
      await p();
  })();
}

function sequentially(as) {
  return as.reduce((prev, cur) => prev.then(cur), Promise.resolve());
}

class CutTree {
  constructor(id) {
    // Make illustration inline
    document.querySelectorAll(`${id} object`)
      .forEach(svgObjectToInline);

    // Storage
    this.id    = id;
    this.svg   = document.querySelector(`${id} svg`);

    // Alignment
    this.svg.classList.add("illustration");

    this.frame_time = 2000;
    this.speed      = 80 / (this.frame_time / 3);
    this.ncores     = 2;

    this.stage1     = this.svg.querySelector(".stage1 path");
    this.stage2s    = Array.from(this.svg.querySelectorAll(".stage2 path"));
    this.edgesState = Array.from(this.svg.querySelectorAll(".send-state path"));
    this.edgesCut   = Array.from(this.svg.querySelectorAll(".send-cut path"));
    this.cpool      = Array.from(this.svg.querySelectorAll(".cut-pool path"));

    this.futures    = this.stage2s.map((_, s) => () => this.animateSecondStage(s));
  }

  get nscen() {
    return this.stage2s.length;
  }

  // Animations for first stage solution + send
  async animateFirstStage() {
    const cOpposite  = "var(--color-opposite)";

    await blink(this.stage1, this.frame_time, cOpposite);
    await strokeZap(this.edgesState, this.speed, cOpposite);
    await strokeZap(this.edgesCut, this.speed, cOpposite);

    await sleep(this.frame_time * 0.3);
  }

  // Animations for 2nd stage node solution + send cut
  async animateSecondStage(s) {
    const solveTime = randomR(this.frame_time * 0.5, this.frame_time);
    const cAccent   = "var(--color-accent, hsl(147 42% 64%))";

    await blink(this.stage2s[s], solveTime, cAccent);
    await strokeZap(this.edgesCut[s], this.speed, cAccent, true);
    await blink(this.cpool[s], this.frame_time * 0.3, cAccent);

    await sleep(this.frame_time * 0.2);
  }

  async singleCut() {
    // Single cut only has one pool, so we repeat it
    const cpool = this.svg.querySelector(".cut-pool path");
    this.cpool  = Array(this.nscen).fill(cpool);

    while (true) {
      await this.animateFirstStage();

      await sequentially(this.futures);

      await sleep(this.frame_time * 0.3);
    }
  }

  async singleCutParallel() {
    // Single cut only has one pool, so we repeat it
    const cpool = this.svg.querySelector(".cut-pool path");
    this.cpool  = Array(this.nscen).fill(cpool);

    while (true) {
      // First Stage
      await this.animateFirstStage();

      // Second stage
      await Promise.all([0, 1].map(c => {
        const inUse = this.futures.slice(c * this.ncores, Math.min((c + 1) * this.ncores, this.nscen));
        return sequentially(inUse);
      }));

      await sleep(this.frame_time * 0.3);
    }
  }

  async multiCut() {
    while (true) {
      await this.animateFirstStage();

      await sequentially(this.futures);

      await sleep(this.frame_time * 0.3);
    }
  }

  async multiCutParallel() {
    // Run each processor asynchrously forever.
    // They do not interact in this case
    asyncForever(() => this.animateFirstStage());

    for (let c = 0; c < this.ncores; c++) {
      const inUse = this.futures.slice(c * this.ncores, Math.min((c + 1) * this.ncores, this.stage2s.length));

      asyncForever(() => sequentially(inUse));
    }
  }

  async linkedCut() {
    this.stage2s = [Array.from(this.svg.querySelectorAll(".stage2 g path"))];

    while (true) {
      await this.animateFirstStage();
      await this.animateSecondStage(0);

      await sleep(this.frame_time * 0.3);
    }
  }

  async mixedCut() {
    const cOpposite  = "var(--color-opposite)";
    const cAccent    = "var(--color-accent, hsl(147 42% 64%))";

    this.stage1      = this.svg.querySelector(".stage1 path");

    this.s2Decomp    = Array.from(this.svg.querySelectorAll(".stage2-decomposed path"));
    this.s2Linked    = Array.from(this.svg.querySelectorAll(".stage2-linked g path"));

    this.cpoolE      = Array.from(this.svg.querySelectorAll(".cut-pool-E path"));
    this.cpool       = Array.from(this.svg.querySelectorAll(".cut-pool path"));

    this.edgesState  = Array.from(this.svg.querySelectorAll(".send-state path"));
    this.edgesMCut   = Array.from(this.svg.querySelectorAll(".send-cut-mcut path"));
    this.edgesMid    = Array.from(this.svg.querySelectorAll(".send-mid path"));
    this.edgesLinked = Array.from(this.svg.querySelectorAll(".send-cut-linked path"));

    // First stage
    asyncForever(async () => {
      await blink(this.stage1, this.frame_time, cOpposite);
      await strokeZap(this.edgesState, this.speed, cOpposite);

      await Promise.all([
        strokeZap(this.edgesLinked, this.speed, cOpposite),
        strokeZap(this.edgesMid, this.speed, cOpposite)
          .then(() => strokeZap(this.edgesMCut, this.speed, cOpposite))
      ]);
    });

    // Decomposed
    asyncForever(() => sequentially(this.s2Decomp.map((_, s) => async () => {
      const solveTime = randomR(this.frame_time * 0.5, this.frame_time);

      await blink(this.s2Decomp[s], solveTime, cAccent);
      await strokeZap(this.edgesMCut[s], this.speed, cAccent, true);
      await blink(this.cpool[s], this.frame_time * 0.3, cAccent);

      await sleep(this.frame_time * 0.2);
    })));

    // Linked
    asyncForever(async () => {
      const solveTime = 4 * randomR(this.frame_time * 0.5, this.frame_time);

      await blink(this.s2Linked, solveTime, cAccent);
      await strokeZap(this.edgesLinked, this.speed, cAccent, true);
      await blink(this.cpoolE, this.frame_time * 0.3, cAccent);

      await sleep(this.frame_time * 0.3);
    });
  }

}




export function main() {
  const g =  x => 0.5*(x - 1.5)*(x - 1)*(x + 0.5)*(x + 1.5) + 0.4;
  const fs = new StoFunc(
    [ x => g(x)
    , x => g(x - 0.5)
    , x => g(x + 0.5)
    , x => g(x - 0.1) + 0.5
    , x => g(x + 0.1) - 0.5
    , x => g(x) - 0.3
    , x => g(x) + 0.3
  ]);

  const cvxs = new StoFunc([
    x => 0.2*(x+0.5)**2,
    x => 0.4*(x-0.5)**2,
    x => 0.3*(x-0.3)**2,
  ]);

  const W    = x => 0.3 + 0.5*(x - 1.5)*(x - 1)*(x + 0.5)*(x + 1.5);
  const Wsto = new StoFunc([-0.5, 0, 0.5].map(z => x => W(x - z)));

  // Stochastic OVF
  new Diagram("#figure-random-function", fs)
    .plot(fs, "stochastic")

  new Diagram("#figure-random-average", cvxs)
    .plot(cvxs, "stochastic")
    .plot(cvxs.mean, "deterministic");

  new Diagram("#figure-random-cuts", cvxs)
    .plot(cvxs, "stochastic")
    .cuts(cvxs, cutsConvex);

  // Convex SP
  new Diagram("#figure-average-cuts", cvxs)
    .plot(cvxs, "stochastic")
    .cuts(cvxs, cutsConvex, "stochastic")
    .plot(cvxs.mean, "deterministic")
    .cuts(cvxs, cutAverage, "deterministic");

  // Linked
  new Diagram("#figure-cuts-conv", Wsto)
    .plot(Wsto, "stochastic")
    .cuts(Wsto, cutsLagrangian, "stochastic")
    .plot(Wsto.mean, "deterministic")
    .cuts(Wsto, cutDecomposed, "deterministic", "decomposed");

  new Diagram("#figure-econv", Wsto)
    .plot(Wsto, "stochastic")
    .plot(Wsto.mean,   "deterministic")
    .plot(Wsto.decomp, "deterministic", "decomposed")
    .plot(Wsto.linked, "deterministic", "linked");


  /* ~*~ Tikz animations ~*~ */

  new CutTree("#figure-tree-singlecut")
    .singleCut();

  new CutTree("#figure-tree-singlecut-parallel")
    .singleCutParallel();

  new CutTree("#figure-tree-multicut")
    .multiCut();

  new CutTree("#figure-tree-multicut-parallel")
    .multiCutParallel();

  new CutTree("#figure-tree-linked")
    .linkedCut();

  new CutTree("#figure-tree-mixed")
    .mixedCut();
}
