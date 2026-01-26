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
  return (a[i] *= -1);
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

function wavyPath(x1, y1, x2, y2, t, amplitude = 6, frequency = 2) {
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

export class Diagram {
  constructor(id, edges, states) {
    this.id     = id;
    this.svg    = document.querySelector(`${id} svg`);
    this.mode   = "ising";
    this.edges  = [];
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

      this.edges.push([i, j]);
    }

    this.h = [-5, 3, 2, 1, -2, 4];

  return this;
  }

  #drawNodes() {
    const spins = this.states;
    const pos = this.pos;
    const gs = draw(this.svg, "g");

    for (const [i, site] of pos.entries()) {
      // Create a group for each node
      const group = draw(gs, "g", {
        class: "spin-group",
        "data-spin": spins[i],
      });

      // Draw the circle inside the group
      const node = draw(group, "circle", {
        cx: site.x,
        cy: site.y,
        r: 20,
        class: "site",
      });

      spins.observe(i, (_, v) => {
        group.setAttribute("data-spin", v);
      });

      node.addEventListener("click", () => flip(this.states, i));
    }
  }


  #drawEdges() {
    const spins = this.states;
    const pos = this.pos;
    const ge = draw(this.svg, "g");
    const maxAbsJ = Math.max(...Object.values(this.J).flatMap(row => Object.values(row).map(Math.abs)));

    this.edges.forEach(([s, t]) => {
      const w = this.J[s][t];
      const edge = draw(ge, "path", {
        class: "interaction",
        d: `M${pos[s].x},${pos[s].y} ${pos[t].x},${pos[t].y}`,
      }, {
        "stroke-width": 1 + Math.abs(w) / maxAbsJ,
        fill: "none"
      });

      const tension = edgeTensionController(edge, pos[s], pos[t]);

      const redrawEdge = () => {
        const align = w * spins[s] * spins[t] > 0;
        edge.classList.toggle("harmony", align);
        edge.classList.toggle("tense", !align);

        tension(align);
      };

      spins.observe([s, t], redrawEdge);
      redrawEdge();
    });
  }

  graph() {
    this.#drawEdges();
    this.#drawNodes();
    return this;
  }

  weights() {
    const pos = this.pos;

    // Create a group for weights
    const gw = draw(this.svg, "g");

    for (const [s, t] of this.edges) {
      const w = this.J[s][t];
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
        "text-anchor": "middle",
        "alignment-baseline": "middle",
        "font-size": 22,
        "fill": "var(--color-background, white)",
        "pointer-events": "none",
      }, {}).textContent = w;
    }

    return this;
  }

  externalField() {
    const svgRect = this.svg.getBoundingClientRect();
    const width   = this.svg.viewBox.baseVal.width || svgRect.width;
    const height  = this.svg.viewBox.baseVal.height || svgRect.height;

    const g = this.svg.insertBefore(
      document.createElementNS('http://www.w3.org/2000/svg', 'g'),
      this.svg.firstChild
    );

    // Arrow grid parameters
    const arrowLength  = 18;
    const arrowHead    = 5;
    const arrowStroke  = "#3399ff";
    const arrowOpacity = 0.25;
    const arrowWidth   = 2;
    const xStep        = 32;
    const yStep        = 32;
    const baseSpeed    = 34; // px/sec along diagonal
    const speedJitter  = 8;  // px/sec, random per arrow
    const phaseJitter  = 2000; // ms, random per arrow

    // Diagonal unit vector
    const diagNorm = Math.sqrt(width * width + height * height);
    const ux = width / diagNorm;
    const uy = height / diagNorm;
    const moveDist = diagNorm + Math.max(width, height);

    // Grid: cover the SVG with arrows, offset so they fill the area as they move
    const nX = Math.ceil((width + moveDist) / xStep);
    const nY = Math.ceil((height + moveDist) / yStep);

    const arrows = Array.from({length: nY}, (_, j) => j * yStep - moveDist / 2)
      .flatMap(y =>
        Array.from({length: nX}, (_, i) => i * xStep - moveDist / 2)
          .map(x => {
            const speed = baseSpeed;
            const phase = Math.random() * phaseJitter;
            const arrowGroup = draw(g, "g", {}, {});
            arrowGroup.setAttribute("transform", `rotate(45)`);
            draw(arrowGroup, "line", {
              x1: 0,
              y1: 0,
              x2: arrowLength,
              y2: 0,
              stroke: arrowStroke,
              "stroke-width": arrowWidth,
              "stroke-linecap": "round",
              opacity: arrowOpacity,
            }, {});
            draw(arrowGroup, "polygon", {
              points: `${arrowLength},0 ${arrowLength-arrowHead},${-arrowHead/2} ${arrowLength-arrowHead},${arrowHead/2}`,
              fill: arrowStroke,
              opacity: arrowOpacity,
            }, {});
            return {g: arrowGroup, baseX: x, baseY: y, speed, phase};
          })
      );

    // Animate all arrows along the diagonal
    (function animate() {
      const t = performance.now();
      arrows.forEach(({g, baseX, baseY, speed, phase}) => {
        // Move along the diagonal, wrap around
        const dist = ((t + phase) / 1000 * speed) % moveDist;
        const x = baseX + ux * dist;
        const y = baseY + uy * dist;
        g.setAttribute("transform", `translate(${x},${y}) rotate(45)`);
      });
      requestAnimationFrame(animate);
    })();

    this.#fieldValues();

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
          const { x, y } = pt.matrixTransform(ctm);
          popup.show(x - left, y - top, `h = ${h[i]}`);
        }
      });
      node.addEventListener("mouseleave", popup.hide);
    });
  }
}
