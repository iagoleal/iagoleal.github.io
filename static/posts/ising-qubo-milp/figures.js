function renderMath(el, expr) {
  if (window.katex) {
    window.katex.render(expr, el, { displayMode: false });
  } else {
    el.textContent = expr;
  }
}

function draw(el, tag, attrs, styles) {
  const node = document.createElementNS("http://www.w3.org/2000/svg", tag);

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

  xs.observe = function(arg1, arg2 = null) {
    let ks, f;
    if (arg2 === null) {
      ks = xs.map((_, i) => i);
      f = arg1;
    } else {
      ks = Array.isArray(arg1) ? arg1 : [arg1];
      f = arg2;
    }

    ks.forEach((i) => listeners[i].push(f));
  };

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

class Popup {
  constructor({ container, className = "popup" } = {}) {
    this.container = container;
    this.className = className;

    this.popup     = this.#createPopup();
  }

  #createPopup() {
    const popup = document.createElement("div");
    popup.className = this.className;
    popup.style.position = "absolute";

    this.container.style.position = "relative";
    this.container.appendChild(popup);
    return popup;
  }

  show(x, y, text) {
    this.popup.textContent = text;
    this.popup.style.left = `${x}px`;
    this.popup.style.top  = `${y}px`;
    this.popup.classList.add("visible");
  }

  hide() {
    this.popup.classList.remove("visible");
  }

  destroy() {
    this.popup.remove();
  }

  get element() {
    return this.popup;
  }
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
        class: "site"
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
      document.createElementNS("http://www.w3.org/2000/svg", "g"),
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
      defs = document.createElementNS("http://www.w3.org/2000/svg", "defs");
      this.svg.insertBefore(defs, this.svg.firstChild);
    }
    if (!this.svg.querySelector("#level-curve-blur")) {
      const filter = document.createElementNS("http://www.w3.org/2000/svg", "filter");
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
      draw(g, "path", {
        d,
        stroke: color,
        "stroke-width": "2.2",
        fill: "none",
        opacity,
        filter: "url(#level-curve-blur)"
      });
    }

    return this;
  }


  #fieldValues() {
      const { pos, h, svg, id } = this;
      const container = document.querySelector(id);
      const popup = new Popup({ container });

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

        node.addEventListener("mouseleave", () => popup.hide());
      });

      return this;
    }

  isingEnergy(formula) {
    const container = document.querySelector(this.id);
    const {svg, states, J, h} = this;

    const mathLabel = new MathLabel(container, `${formula} =`);
    svg.insertAdjacentElement("afterend", mathLabel.element);

    const valueSpan = document.createElement("span");
    valueSpan.className = "energy-value";
    mathLabel.element.appendChild(valueSpan);

    const redraw = () => {
      const energy = this.mode.energy(states, J, h);

      renderMath(valueSpan, energy.toString());

      valueSpan.classList.add("energy-changed");
      setTimeout(() => {
        return valueSpan.classList.remove("energy-changed")
      }, 180);
    };

    states.observe(redraw);
    redraw();

    return this;
  }
}

class MathLabel {
  constructor(container, formula = "") {
    this.element = document.createElement("div");
    this.element.className = "math-label";
    container.appendChild(this.element);

    if (formula) {
      this.set(formula);
    }
  }

  set(formula) {
    if (window.katex) {
      window.katex.render(formula, this.element, { displayMode: false });
    } else {
      this.element.textContent = formula;
    }
  }
}


function binaryCoeffs(lb, ub) {
  const bits = Math.ceil(Math.log2(ub - lb + 1));
  const coeffs = Array.from({ length: bits }, (_, j) => {
    return j < bits - 1 ? 2 ** j : (ub - lb - (2 ** (bits - 1)) + 1)
  });

  return coeffs;
}

const EncodingModes = {
  onehot: {
    init(lb, ub, initial) {
      const K = ub - lb + 1;
      const state = Array(K).fill(false);
      state[initial] = true;

      return state;
    },

    toggle(state, j) {
      state.fill(false);
      state[j] = true;
    },

    value(state, lb) {
      return lb + state.findIndex(Boolean);
    },
  },
  binary: {
    init(lb, ub, initial) {
      const coeffs = binaryCoeffs(lb, ub);
      const bits   = coeffs.length;
      const state  = Array(bits).fill(false);

      let value = initial;
      for (let j = bits - 1; j >= 0; j--) {
        if (value >= coeffs[j]) {
          state[j] = true;
          value -= coeffs[j];
        }
      }
      return state;
    },

    toggle(state, j) {
      state[j] = !state[j];
    },

    value(state, lb, ub) {
      const coeffs = binaryCoeffs(lb, ub);
      return lb + state.reduce((sum, bit, i) => sum + (bit ? coeffs[i] : 0), 0);
    }
  }
};

export class EncodingElement {
  #container;
  #states;
  #mode;
  #lb;
  #ub;

  constructor(id, lower, upper, initial = 0, mode = "onehot") {
    this.#container = document.querySelector(id);
    this.#mode = EncodingModes[mode];
    this.#lb   = lower;
    this.#ub   = upper;
    this.#states = ArrayListener(this.#mode.init(lower, upper, initial));
  }

  buttons() {
    const row = document.createElement("div");
    row.className = "encoding-diagram";

    const buttons = this.#states.map((state, j) => {
      const button = document.createElement("div");
      button.className   = "encoding-button";
      renderMath(button, `z_{${j+1}}`);
      button.classList.toggle("active", state);

      button.addEventListener("click", () => {
        this.#mode.toggle(this.#states, j);
        buttons.forEach((button, i) => button.classList.toggle("active", this.#states[i]));
      });

      row.appendChild(button);

      this.#states.observe(j, () => button.classList.toggle("active", this.#states[j]));

      return button;
    });

    this.#container.appendChild(row);

    return this;
  }

  #addLabel(writer) {
    // Container for math labels (x= ... , K = ...)
    let div = this.#container.querySelector('.math-label-container');
    if (!div) {
      div = document.createElement('div');
      div.className = 'math-label-container';
      this.#container.appendChild(div);
    }

    const label = new MathLabel(div, "");
    const update = () => label.set(writer());
    update();
    this.#states.observe(update);
    return label;
  }

  label() {
    this.#addLabel(() =>
      `x = ${this.#mode.value(this.#states, this.#lb, this.#ub)}`
    );
    return this;
  }

  labelNvar() {
    this.#addLabel(() =>
      `K = ${this.#states.length}`
    );

    return this;
  }
}
