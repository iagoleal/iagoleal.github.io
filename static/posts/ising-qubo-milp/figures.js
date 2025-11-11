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

export class Diagram {
  constructor(id, edges, states) {
    this.id     = id;
    this.svg    = document.querySelector(`${id} svg`);
    this.mode   = "ising";
    this.edges  = [];
    this.states = ArrayListener(states);

    this.J = zeros(states.length, states.length);
    for (const [[i, j], w] of edges) {
      this.J[i][j] = w;
      this.J[j][i] = w;

      this.edges.push([i, j]);
    }

  return this;
  }

  graph() {
    const spins = this.states;
    const pos = [
      {x: 100, y: 350},
      {x: 150, y: 150},
      {x: 300, y: 400},
      {x: 400, y: 250},
      {x: 700, y: 100},
      {x: 900, y: 350},
    ];

    const ge = draw(this.svg, "g");
    const gs = draw(this.svg, "g");

    // Draw Nodes
    for (const [i, site] of pos.entries()) {
      const node = draw(gs, "circle", {
        cx: site.x,
        cy: site.y,
        r: 20,
        fill: spins[i] == 1 ? "green" : "red",
        class: "site",
      });

      spins.observe(i, (_, v) => {
        node.setAttribute("fill", v == 1 ? "green" : "red");
      })

      node.addEventListener("click", () => flip(this.states, i));
    }

    // Draw edges
    for (const [s, t] of this.edges) {
      const w = this.J[s][t]
      const edge = draw(ge, "line",
        {
          class: "interaction",
          x1: pos[s].x,
          y1: pos[s].y,
          x2: pos[t].x,
          y2: pos[t].y,
        },
        {
          "stroke-width": Math.abs(w),
        });

      const redrawEdge = function() {
        const align = w * spins[s] * spins[t] > 0;
        edge.style["stroke"] =  align ? "green" : "red";
        edge.classList.toggle("tense", !align);
      }

      spins.observe([s, t], redrawEdge)
      redrawEdge()
    }

    return this;
  }

  weights() {
    return this;
  }

  externalField() {
    return this;
  }
}
