//
// General Helpers =========================================
//

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

function blink(el, c, duration=180) {
  el.classList.remove(c);
  el.classList.add(c);
  setTimeout(() => { el.classList.remove(c); }, duration);

  return el;
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

//
// Geometry =========================================
//

function getSvgBBoxAnchor(el) {
  const svg  = el.ownerSVGElement;
  const ctm  = svg.getScreenCTM();
  const rect = el.getBBox();

  const point = svg.createSVGPoint();
  point.x = rect.x + rect.width / 2;
  point.y = rect.y + rect.height / 2;

  const screenPoint = point.matrixTransform(ctm);
  return {
    x: screenPoint.x,
    y: screenPoint.y,
  };
}

function edgeGeometry(p, q) {
  const mx = (p.x + q.x) / 2;
  const my = (p.y + q.y) / 2;
  const dx = q.y - p.y;
  const dy = p.x - q.x;
  const norm = Math.sqrt(dx*dx + dy*dy) || 1;
  return {
    mid:    { x: mx, y: my },
    normal: {x: dx / norm, y: dy / norm},
    norm,
  };
}

//
// UI Classes =========================================
//

class MathLabel {
  #label;
  #value;

  constructor(container, label = "", value = "") {
    this.element = document.createElement("div");
    this.element.className = "math-label";
    container.appendChild(this.element);

    // Create label and value spans
    this.labelSpan = document.createElement("span");
    this.valueSpan = document.createElement("span");
    this.valueSpan.className = "energy-value";

    this.element.appendChild(this.labelSpan);
    this.element.appendChild(document.createTextNode(" = "));
    this.element.appendChild(this.valueSpan);

    this.#value = value;
    this.#label = label;
    this.render();
  }

  set value(val) {
    if (val !== this.#value) {
      this.#value = val;
      this.render();

      blink(this.valueSpan, "energy-changed");

      return val;
    }
  }

  render() {
    renderMath(this.labelSpan, this.#label);
    renderMath(this.valueSpan, this.#value.toString());
  }
}

function binaryCoeffs(lb, ub) {
  const bits = Math.ceil(Math.log2(ub - lb + 1));
  const coeffs = Array.from({ length: bits }, (_, j) => {
    return j < bits - 1 ? 2 ** j : (ub - lb - (2 ** (bits - 1)) + 1)
  });

  return coeffs;
}


class Popup {
  constructor(container, className = "popup") {
    this.container = container;
    this.className = className;

    this.element = this.#createPopup();
  }

  #createPopup() {
    const el = document.createElement("div");
    el.className = this.className;
    el.style.position = "absolute";

    el.setAttribute("role", "tooltip");
    el.setAttribute("aria-live", "polite");

    this.container.style.position = "relative";
    this.container.appendChild(el);
    return el;
  }

  show(text, x, y) {
    renderMath(this.element, text);
    if (x) {
      this.element.style.left = `${x}px`;
    }
    if (y) {
      this.element.style.top  = `${y}px`;
    }
    this.element.classList.add("visible");
  }

  hide() {
    this.element.classList.remove("visible");
  }

  destroy() {
    this.element.remove();
  }

  get visible() {
    return this.element.classList.contains("visible");
  }
}

function wavyPath(p, q, t, amplitude = 3, frequency = 2) {
  const { mid, normal} = edgeGeometry(p, q);
  const phase = Math.sin(t * frequency) * amplitude;

  const cx = mid.x + phase * normal.x;
  const cy = mid.y + phase * normal.y;

  return `M${p.x},${p.y} Q${cx},${cy} ${q.x},${q.y}`;
}

function edgeTensionController(edge, a, b) {
  let animId = null;

  const setShape = (wavy, t = 0) =>
    edge.setAttribute(
      "d",
      wavy ? wavyPath(a, b, t) : `M${a.x},${a.y} ${b.x},${b.y}`
    );

  const animate = () => {
    setShape(true, performance.now() / 400);
    animId = requestAnimationFrame(animate);
  };

  return (interaction) => {
    if (interaction < 0) { // Negative spin
      if (animId) {
        cancelAnimationFrame(animId);
      }
      animId = null;
      setShape(false);
      return;
    } else if (!animId) {
      animate();
    }
  };
}

//
// State Management ==============================================
//

class StateVector {
  constructor(states) {
    this.d = ArrayListener(states);
  }

  observe(...xs) {
    return this.d.observe(...xs);
  }

  at(i) {
    return this.d[i];
  }

  flip(i) {
    this.d[i] = !this.d[i];
    return this.spin(i);
  }

  prod(...is) {
    return is.reduce((acc, j) => acc * this.spin(j), 1);
  }

  energy(edges) {
    let energy = 0;
    for (const [[s, t], w] of edges) {
      energy += w * this.spin(s) * this.spin(t);
    }

    if (edges.h) {
      for (let i = 0; i < edges.h.length; i++) {
        energy += edges.h[i] * this.spin(i);
      }
    }

    return energy;
  }

  static edgeAnim() {
    return () => {};
  }
}

class IsingStateVector extends StateVector {
  static mode = "ising";
  static varName(i) {
    return `s_{${i+1}}`;
  }
  static matName(i, j) {
    return `J_{${i+1}${j+1}}`;
  }

  static edgeAnim(edge, p, q) {
    return edgeTensionController(edge, p, q);
  }

  alignment(w, ...indices) {
    return Math.sign(w * this.prod(...indices));
  }

  show(i) {
    return this.at(i) ? "+1" : "-1";
  }
  spin(i) {
    return this.at(i) ? 1 : -1;
  }
}

class QuboStateVector extends StateVector {
  static mode = "qubo";
  static varName(i) {
    return `z_{${i+1}}`;
  }
  static matName(i, j) {
    return `Q_{${i+1}${j+1}}`;
  }

  alignment(_, ...indices) {
    return this.prod(...indices);
  }

  show(i) {
    return this.at(i) ? "1" : "0";
  }
  spin(i) {
    return this.at(i) ? 1 : 0;
  }
}


export function isingToQubo(adjacency, h) {
  const N = h.length;

  // Build full J matrix (symmetric)
  const J = Array.from({ length: N }, () => Array(N).fill(0));
  for (const [[i, j], w] of adjacency) {
    J[i][j] = w;
    J[j][i] = w;
  }

  // Q = 4J - 2*Diag((J + J^T)1 + h)
  // (J + J^T)1 = 2 * row sums (since J is symmetric)
  const diag = [];
  for (let i = 0; i < N; ++i) {
    let rowSum = 0;
    for (let j = 0; j < N; ++j)
      rowSum += J[i][j];
    diag[i] = -2 * (2 * rowSum + h[i]);
  }

  // QUBO adjacency list
  const qubo = [];
  for (let i = 0; i < N; ++i) {
    for (let j = i; j < N; ++j) {
      let val = 4 * J[i][j];
      if (i === j)
        val += diag[i];
      if (val !== 0)
        qubo.push([[i, j], val]);
    }
  }
  return qubo;
}

//
//  Graph Diagrams ==============================================
//

function weight(edges, i, j = null) {
  if (j === null) {
    return edges.h?.[i] ?? 0;
  }

  const se = edges.find(([[s, t], _]) => s === i && t === j);
  return se?.[1] ?? 0;
}

export class Diagram {
  #messages;

  constructor(id, states, edges, mode="ising") {
    this.id     = id;
    this.svg    = document.querySelector(`${id} svg`);
    this.edges  = edges;
    this.states = (mode === "ising")
      ? new IsingStateVector([...states])
      : new QuboStateVector([...states]);

    this.#messages = { nodes: [], edges: [] };

    this.pos = [
      {x: 100, y: 350},
      {x: 150, y: 150},
      {x: 300, y: 400},
      {x: 400, y: 250},
      {x: 700, y: 100},
      {x: 900, y: 350},
    ];

  this.svg.classList.add(this.mode.mode);

  return this;
  }

  get mode() {
    return this.states.constructor;
  }

  graph() {
    return this
      .edgesSketch()
      .#drawNodes();
  }

  #drawNodes() {
    const {states, pos} = this;
    const gs = draw(this.svg, "g");

    for (const [i, site] of pos.entries()) {
      const g = draw(gs, "g", {class: "site-wrapper"});

      const node = draw(g, "circle", {
        cx: site.x,
        cy: site.y,
        class: "site",
        },
      );

      node.setAttribute("state", states.spin(i))

      node.addEventListener("click", () => this.states.flip(i));

      states.observe(i, (_, v) => {
        node.setAttribute("state", this.states.spin(i)) // TODO: adjust status such that v = +/- 1
      });

    }

    this.#messages.nodes.push(
      (i) => `${this.mode.varName(i)} = ${this.states.show(i)}`,
    );

    return this;
  }

  edgesSketch() {
    const { pos, edges, svg } = this;
    const ge = draw(svg, "g");
    const gh = draw(svg, "g");

    for (const [[s, t], _] of edges) {
      draw(ge, "path", {
        class: "edge",
        d: `M${pos[s].x},${pos[s].y} ${pos[t].x},${pos[t].y}`,
      }, {
      });

      // Hack for larger hover zones
      draw(gh, "path", {
        class: "edge-hover",
        d: `M${pos[s].x},${pos[s].y} ${pos[t].x},${pos[t].y}`,
      }, {
        "stroke-width": 18,
        fill: "none",
        stroke: "#000",
        opacity: 0,
        "pointer-events": "stroke"
      });
    }

    this.#messages.edges.push(
      (i, j) => `${this.mode.varName(i)} ${this.mode.varName(j)} = ${this.states.prod(i, j)}`,
    );

    return this;
  }

  edgesActivate() {
    const { states, pos, edges } = this;
    const maxAbsJ = Math.max(...edges.map(([_, w]) => Math.abs(w)));

    // Update all visible edges
    this.svg.querySelectorAll('.edge').forEach((edge, i) => {
      const [[s, t], w] = edges[i];

      edge.classList.add("styled");

      edge.setAttribute("stroke-width", 2 + Math.abs(w) / maxAbsJ);

      const anim = this.mode.edgeAnim(edge, pos[s], pos[t]);
      const redrawEdge = () => {
        const aligned = this.states.alignment(w, s, t);
        edge.setAttribute("product", aligned);
        anim(aligned);
      };

      states.observe([s, t], redrawEdge);
      redrawEdge();
    });

    this.#messages.edges.push(
      (i, j) => {
        const w = this.states.prod(i, j) * weight(this.edges, i, j);
        return `${this.mode.matName(i, j)} ${this.mode.varName(i)} ${this.mode.varName(j)} = ${w}`
      }
    );

    if (this.edges.some(([[s, t], _]) => s === t)) {
      this.#messages.nodes.push(
        (i) => {
          const w = this.states.spin(i) * weight(this.edges, i, i);
          return `${this.mode.matName(i,i)} ${this.mode.varName(i)} = ${w}`;
        }
      );
    }

    return this;
  }

  weights() {
    const pos = this.pos;
    const gw = draw(this.svg, "g");

    for (const [[s, t], w] of this.edges) {
      let x, y;
      if (s === t) { // Node center
        x = pos[s].x;
        y = pos[s].y;
      } else { // Edge midpoint
        const { mid, normal} = edgeGeometry(pos[s], pos[t]);
        const offset = 28;
        x = mid.x + offset * normal.x;
        y = mid.y + offset * normal.y;
      }

      draw(gw, "text", {
        x: x,
        y: y,
        class: "weight"
      }, {}).textContent = w;
    }

    // Draw magnetic field weights if present (as node weights)
    if (this.edges.h) {
      for (let i = 0; i < pos.length; ++i) {
        draw(gw, "text", {
          x: pos[i].x,
          y: pos[i].y,
          class: "weight"
        }, {}).textContent = this.edges.h[i];
      }
    }

    return this.edgesActivate();
  }

  externalField() {
    this.#messages.nodes.push(
      (i) => {
        const w = this.states.spin(i) * this.edges.h[i];
        return `h_{${i+1}} ${this.mode.varName(i)} = ${w}`
      }
    );

    return this.#drawField();
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

  energyLabel(formula) {
    const container = document.querySelector(this.id);
    const {svg, states, edges} = this;

    const mathLabel = new MathLabel(container, formula, states.energy(edges));
    svg.insertAdjacentElement("afterend", mathLabel.element);

    const redraw = () => {
      const energy = states.energy(edges);
      mathLabel.value = energy;
    };

    states.observe(redraw);

    return this;
  }

  popup() {
    const popup = new Popup(document.querySelector(this.id));

    this.#popupFor(
      popup,
      '.site',
      this.#messages.nodes,
      (i) => [i]
    );

    this.#popupFor(
      popup,
      '.edge-hover',
      this.#messages.edges,
      (i) => {
        const [[s, t], w] = this.edges[i];
        return [s, t, w];
      },
    );

    return this;
  }

  #popupFor(popup, selector, messages, getVars) {
    const { svg, states } = this;

    svg.querySelectorAll(selector).forEach((el, i) => {
      const processMessages = () => {
        const lines = messages
          .map(f => f(...getVars(i)))
          .filter(Boolean)
          .map(line => line.replace(/=/, '&='));

        return `\\begin{aligned} ${lines.join(' \\\\[1ex] ')} \\end{aligned}`;
      };

      el.addEventListener('pointerenter', (e) => {
        const containerRect = popup.container.getBoundingClientRect();
        const anchor = getSvgBBoxAnchor(el);

        popup.show(
          processMessages(),
          anchor.x - containerRect.left,
          anchor.y - containerRect.top,
        );
      });
      el.addEventListener('pointerleave', () => popup.hide());

      states.observe(() => {
        if (popup.visible && el.matches(':hover')) {
          popup.show(processMessages());
        }
      });
    });

    return this;
  }
}


//
//  Encoding Diagrams ==============================================
//

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

    inputtype: "radio",
    title: "One-hot"
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
    },

    inputtype: "checkbox",
    title: "Binary",
  }
};

export class EncodingElement {
  #container;
  #states;
  #mode;
  #lb;
  #ub;
  #uuid;
  #onChange;

  constructor(id, lower, upper, initial = 0, mode = "onehot", options = {}) {
    this.#container = document.querySelector(id);
    this.#mode      = EncodingModes[mode];
    this.#lb        = lower;
    this.#ub        = upper;
    this.#states    = ArrayListener(this.#mode.init(lower, upper, initial));
    this.#uuid      = `encoding-group-${crypto.randomUUID()}`;

    this.render();
  }

  render() {
    this.#container.classList.add("encoding-diagram");
    this.#container.innerHTML = "";

    const fieldset = document.createElement("fieldset");

    const legend = document.createElement("legend");
    legend.textContent = `${this.#mode.title} Encoding`;
    fieldset.append(legend);

    this.#states.forEach((state, j) => {
      const label = document.createElement("label");

      const mathSpan = document.createElement("span");
      renderMath(mathSpan, `z_{${j+1}}`);

      const input   = document.createElement("input");
      input.type    = this.#mode.inputtype;
      input.id      = `${this.#uuid}-z${j+1}`;
      input.name    = this.#uuid;
      input.value   = j;
      input.checked = state;

      input.addEventListener("change", () => {
        this.#mode.toggle(this.#states, j);
      });

      this.#states.observe(j, () => {
        input.checked = this.#states[j];
      });

      label.append(mathSpan, input);
      fieldset.append(label);
    });


    this.#container.append(fieldset);

    this.#addLabel(() => {
      return ["K", () => this.#states.length]
    });

    this.#addLabel(() => {
      return ["x", () => this.#mode.value(this.#states, this.#lb, this.#ub)]
    });

    return this;
  }

  setBounds(lb, ub, initial = 0) {
    this.#lb = lb;
    this.#ub = ub;
    this.#states = ArrayListener(this.#mode.init(lb, ub, initial));
    this.render();
    return this;
  }

  #addLabel(writer) {
    let div = this.#container.querySelector('.math-label-container');
    if (!div) {
      div = document.createElement('div');
      div.className = 'math-label-container';
      this.#container.append(div);
    }

    const [formula, cb] = writer();
    const label = new MathLabel(div, formula, cb());
    this.#states.observe(() => label.value = cb());

    return label;
  }
}
