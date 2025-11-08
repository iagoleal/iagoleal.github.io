import * as d3 from "https://cdn.jsdelivr.net/npm/d3@7/+esm";

function addLine (el, x1, y1, x2, y2) {
  const line = document.createElementNS('http://www.w3.org/2000/svg','line');

  line.setAttribute("x1", x1);
  line.setAttribute("y1", y1);
  line.setAttribute("x2", x2);
  line.setAttribute("y2", y2);
  line.setAttribute("stroke", "black");

  return el.appendChild(line);
}

function draw(el, tag, attrs) {
  const node = document.createElementNS('http://www.w3.org/2000/svg', tag);

  for (const prop in attrs) {
    node.setAttribute(prop, attrs[prop]);
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
    // this.svg    = d3.select(`${id} svg`);
    this.svg    = document.querySelector(`${id} svg`);
    this.mode   = "ising";
    this.edges  = edges;
    this.states = ArrayListener(states);

    this.adjacency = zeros(states.length, states.length);
    for (const [i, j] of edges) {
      this.adjacency[i][j] = 1;
      this.adjacency[j][i] = 1;
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

    // Draw edges
    for (const [s, t] of this.edges) {
      const edge = draw(this.svg, "line", {
        class: "interaction",
        x1: pos[s].x,
        y1: pos[s].y,
        x2: pos[t].x,
        y2: pos[t].y,
      });

      spins.observe([s, t], () => {
        edge.setAttribute("stroke", spins[s] == spins[t] ? "green" : "red");
      })
    }

    // Draw Nodes
    for (const [i, site] of pos.entries()) {
      const node = draw(this.svg, "circle", {
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

    return this;
  }
}
