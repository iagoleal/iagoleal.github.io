function draw(el, tag, attrs, styles) {
  const node = document.createElementNS('http://www.w3.org/2000/svg', tag);

  for (const prop in attrs) {
    node.setAttribute(prop, attrs[prop]);
  }

  for (const prop in styles) {
    node.style[prop] = styles[prop];
  }

  return el.appendChild(node);
}

function flip(a, i) {
  return (a[i] = !a[i]);
}

function spin(b) {
  return b ? +1 : -1;
}

function ArrayListener(xs) {
  const listeners = Array.from(Array(xs.length), () => []);
  xs.observe = function(ks, f) {
    for (const i of (Array.isArray(ks) ? ks : [ks])) {
      listeners[i].push(f)
    }
  }

  return new Proxy(xs, {
      set(target, k, v) {
        target[k] = v;

        for (const cb of listeners[k]) {
          cb(k, v);
        }

        return true;
      }
    }
  );
}

function zeros(rows, cols) {
  return Array.from(Array(rows), () => new Array(cols).fill(0));
}


function createPopup({ container, className = "popup" } = {}) {
  let popup = container.querySelector(`.${className}`);
  if (!popup) {
    popup = document.createElement("div");
    popup.className = className;
    popup.style.position = "absolute";
    container.style.position = "relative";
    container.appendChild(popup);
  }

  function show(x, y, text) {
    popup.textContent = text;
    popup.style.left = `${x}px`;
    popup.style.top = `${y}px`;
    popup.classList.add("visible");
  }

  function hide() {
    popup.classList.remove("visible");
  }
  return { show, hide, destroy: () => popup.remove(), element: popup };
}

function wavyPath(x1, y1, x2, y2, t, amplitude = 3, frequency = 2) {
  // Midpoint
  const mx = (x1 + x2) / 2;
  const my = (y1 + y2) / 2;
  // Perpendicular direction
  const dx = y2 - y1;
  const dy = x1 - x2;
  const len = Math.sqrt(dx*dx + dy*dy) || 1;
  // Animate the amplitude with time
  const phase = Math.sin(t * frequency) * amplitude;
  const cx = mx + phase * dx / len;
  const cy = my + phase * dy / len;
  // Quadratic Bezier from (x1,y1) to (x2,y2) with control (cx,cy)
  return `M${x1},${y1} Q${cx},${cy} ${x2},${y2}`;
}

function edgeTensionController(edge, a, b) {
  let animId = null;

  const setShape = (wavy, t = 0) =>
    edge.setAttribute(
      "d",
      wavy ? wavyPath(a.x, a.y, b.x, b.y, t) : `M${a.x},${a.y} ${b.x},${b.y}`
    );

  const animate = () => {
    setShape(true, performance.now() / 400);
    animId = requestAnimationFrame(animate);
  };

  return (align) => {
    if (align) {
      if (animId) cancelAnimationFrame(animId);
      animId = null;
      setShape(false);
      return;
    } else if (!animId) {
      animate();
    }
  };
}

function isingObj(states, J, h = null) {
  const N = states.length;
  let energy = 0;

  for (let i = 0; i < N; ++i) {
    // pairwise interactions
    for (let j = i + 1; j < N; ++j) {
      energy += J[i][j] * states[i] * states[j];
    }

    // External field
    if (h) {
      energy += h[i] * states[i];
    }
  }

  return energy;
}

function quboObj(states, Q) {
  const N = states.length;
  let energy = 0;

  for (let i = 0; i < N; i++) {
    for (let j = i ; j < N; j++) {
      energy += Q[i][j] * states[i] * states[j];
    }
  }

  return energy;
}

const Modes = {
  ising: {
    name: "ising",
    energy: isingObj,
    edgeAnim: (edge, p, q) => edgeTensionController(edge, p, q),
    alignment: (x, y, w) => w * spin(x) * spin(y) > 0,
  },
  qubo: {
    name: "qubo",
    energy: quboObj,
    edgeAnim: () => () => {},
    alignment: (x, y, _) => x && y,
  }
}

export class Diagram {
  constructor(id, mode, states, edges) {
    this.id     = id;
    this.svg    = document.querySelector(`${id} svg`);
    this.mode   = Modes[mode];
    this.edges  = edges;
    this.states = ArrayListener(states);

    this.pos = [
      {x: 100, y: 350},
      {x: 150, y: 150},
      {x: 300, y: 400},
      {x: 400, y: 250},
      {x: 700, y: 100},
      {x: 900, y: 350},
    ];

    this.J = zeros(states.length, states.length);
    for (const [[i, j], w] of edges) {
      this.J[i][j] = w;
      this.J[j][i] = w;
    }

  return this;
  }

  graph() {
    return this
      .#drawEdges()
      .#drawNodes();
  }

  externalField(h = null) {
    this.h = h;

    return this
      .#fieldValues()
      .#drawField();
  }

  #drawNodes() {
    const {states, pos} = this;
    const gs = draw(this.svg, "g");

    for (const [i, site] of pos.entries()) {
      const g = draw(gs, "g", {});

      const node = draw(g, "circle", {
        cx: site.x,
        cy: site.y,
        r: 20,
        class: 'site'
        },
      );

      node.classList.add(this.mode.name);
      node.classList.toggle("active", states[i]);

      states.observe(i, (_, v) => {
        node.classList.toggle("active", v);
      });

      node.addEventListener("click", () => flip(this.states, i));
    }

    return this;
  }


  #drawEdges() {
    const {states, pos} = this;
    const ge = draw(this.svg, "g");
    const maxAbsJ = Math.max(...this.edges.map(([[,], w]) => Math.abs(w)));

    for (const [[s, t], w] of this.edges) {
      const edge = draw(ge, "path", {
        class: `edge`,
        d: `M${pos[s].x},${pos[s].y} ${pos[t].x},${pos[t].y}`,
      }, {
        "stroke-width": 1 + Math.abs(w) / maxAbsJ,
        fill: "none",
      });
      edge.classList.add(this.mode.name);

      const anim = this.mode.edgeAnim(edge, pos[s], pos[t]);

      const redrawEdge = () => {
        const aligned = this.mode.alignment(states[s], states[t], w);
        edge.classList.toggle("aligned", aligned);
        anim(aligned);
      };

      states.observe([s, t], redrawEdge);
      redrawEdge();
    }

    return this;
  }

  weights() {
    const pos = this.pos;

    // Create a group for weights
    const gw = draw(this.svg, "g");

    for (const [[s, t], w] of this.edges) {
      const mx = (pos[s].x + pos[t].x) / 2;
      const my = (pos[s].y + pos[t].y) / 2;
      const dx = pos[t].y - pos[s].y;
      const dy = pos[s].x - pos[t].x;
      const len = Math.sqrt(dx*dx + dy*dy) || 1;
      const offset = 18;
      const ox = mx + offset * dx / len;
      const oy = my + offset * dy / len;

      draw(gw, "text", {
        x: ox,
        y: oy,
        class: "weight",
      }, {}).textContent = w;
    }

    return this;
  }

  #drawField() {
    const g = this.svg.insertBefore(
      document.createElementNS('http://www.w3.org/2000/svg', 'g'),
      this.svg.firstChild
    );
    g.classList.add("magnetic-field");

    // Get SVG dimensions
    const svgRect = this.svg.getBoundingClientRect();
    const width   = this.svg.viewBox.baseVal.width || svgRect.width;
    const height  = this.svg.viewBox.baseVal.height || svgRect.height;

    // Level curve parameters
    const nCurves   = 6;
    const amplitude = height * 0.10;
    const frequency = 2 * Math.PI / width * 1.5; // 1.5 full waves across width
    const color     = "#90caf9";
    const opacity   = 0.13;
    const blur      = 1.5;
    const step      = 14;

    // Create SVG filter for blur if not present
    let defs = this.svg.querySelector("defs");
    if (!defs) {
      defs = document.createElementNS('http://www.w3.org/2000/svg', 'defs');
      this.svg.insertBefore(defs, this.svg.firstChild);
    }
    if (!this.svg.querySelector("#level-curve-blur")) {
      const filter = document.createElementNS('http://www.w3.org/2000/svg', 'filter');
      filter.setAttribute("id", "level-curve-blur");
      filter.innerHTML = `<feGaussianBlur stdDeviation="${blur}" />`;
      defs.appendChild(filter);
    }

    // Draw the static level curves
    for (let i = 0; i < nCurves; ++i) {
      const y0 = height * (0.18 + 0.64 * i / (nCurves - 1));
      let d = "";
      for (let x = 0; x <= width; x += step) {
        const y = y0 + amplitude * Math.sin(frequency * x + i * 0.7);
        d += (x === 0 ? "M" : "L") + x + "," + y;
      }
      // Use draw() to create the path element and set attributes
      draw(g, 'path', {
        d,
        stroke: color,
        'stroke-width': '2.2',
        fill: 'none',
        opacity,
        filter: 'url(#level-curve-blur)'
      });
    }

    return this;
  }


  #fieldValues() {
      const { pos, h, svg, id } = this;
      const container = document.querySelector(id);
      const popup = createPopup({ container });

      Array.from(svg.querySelectorAll("circle.site")).forEach((node, i) => {
        node.addEventListener("mouseenter", () => {
          const pt = svg.createSVGPoint();
          pt.x = pos[i].x + 32;
          pt.y = pos[i].y - 24;
          const ctm = svg.getScreenCTM();
          if (ctm) {
            const { left, top } = container.getBoundingClientRect();
            const { x, y }      = pt.matrixTransform(ctm);

            popup.show(x - left, y - top, `h = ${h[i]}`);
          }
        });
        node.addEventListener("mouseleave", popup.hide);
      });

      return this;
    }

  isingEnergy() {
    const container = document.querySelector(this.id);
    const svg = this.svg;

    let label = container.querySelector(".ising-energy-label");
    if (!label) {
      label = document.createElement("div");
      label.className = "ising-energy-label";
      svg.insertAdjacentElement("afterend", label);
    }

    const redraw = () => {
      let formula;
      if (this.mode.name === "qubo") {
        formula = "f(x) = x^T Q x = ";
      } else if (Array.isArray(this.h)) {
        formula = "H(s) = s^T J s + h^T s = ";
      } else {
        formula = "H(s) = s^T J s = ";
      }
      const energy = this.mode.energy(this.states, this.J, this.h);

      // Animate if energy changed
      const valueSpan = document.createElement('span');
      valueSpan.className   = 'energy-value';
      valueSpan.textContent = energy;
      valueSpan.classList.add("energy-changed");
      setTimeout(() => valueSpan.classList.remove("energy-changed"), 180);

      // Render formula with KaTeX, then append the value span
      if (window.katex) {
        window.katex.render(formula, label, { displayMode: false });
      } else {
        label.innerHTML = formula;
      }
      label.appendChild(valueSpan);
    };

    this.states.observe([...Array(this.states.length).keys()], redraw);
    redraw();

    return this;
  }
}
