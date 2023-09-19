// Normalize a 2d vector
function normalize(p) {
  const norm = Math.sqrt(p.x ** 2 + p.y ** 2);
  return {x: p.x / norm, y: p.y / norm};
}

function pointCircleIntersection(point, circle) {
  return Math.sqrt((point.x - circle.x) ** 2 + (point.y - circle.y) ** 2) <= circle.r;
}

function figureSetPointHyperplane(id) {
  const svg = d3.select(id);

  const width     = svg.attr("width");
  const height    = svg.attr("height");
  const maxLength = Math.sqrt(width ** 2 + height ** 2);
  const circle    = {
    x: width/2,
    y: height/2,
    r: 100,
  };

  let pos = {
    x: circle.x + circle.r/Math.sqrt(2),
    y: circle.y + circle.r/Math.sqrt(2),
    normal: {x: Math.sqrt(2), y: Math.sqrt(2)}
  };

  function updateHyperplane(svg, pos) {
    return svg.selectAll(".hyperplane")
      .data(pos ? [pos] : [])
      .join(
        "line",
        update => update,
        exit   =>
          exit.transition("movePlane")
            .duration(100)
            .style("opacity", 0)
            .remove(),
      )
      .attr("x1", d => d.x - d.normal.y * maxLength)
      .attr("y1", d => d.y + d.normal.x * maxLength)
      .attr("x2", d => d.x + d.normal.y * maxLength)
      .attr("y2", d => d.y - d.normal.x * maxLength)
      .attr("class", "hyperplane");
  }


  // Our convex set
  const convexSet = svg.selectAll(".convex-set")
    .data([circle])
    .join("circle")
    .attr("cx", d => d.x)
    .attr("cy", d => d.y)
    .attr("r",  d => d.r)
    .attr("class", "convex-set");

  const hyperplane = updateHyperplane(svg, pos);

  // Draw hyperplane separating mouse and convex set
  svg.on("mousemove", function (event) {
    const [x, y] = d3.pointer(event);
    const isInside = pointCircleIntersection({x, y}, circle);

    // Update convex set
    convexSet.attr("class", isInside ? "red-circle" : "convex-set");

    // Update hyperplane
    const normal = normalize({x: circle.x - x, y: circle.y - y});
    pos = {x, y, normal};

    updateHyperplane(svg, isInside? null : pos);

    // Mouse marker
    svg.selectAll(".mark").remove();
    if (!isInside) {
      mark(svg, pos.x, pos.y).attr("fill", "orange");
    }
  });
}

function figureSetSupportingHyperplane(id) {
  const svg = d3.select(id);

  const width     = svg.attr("width");
  const height    = svg.attr("height");
  const maxLength = Math.sqrt(width ** 2 + height ** 2);
  const circle    = {
    x: width/2,
    y: height/2,
    r: 100,
  };

  let pos = {
    x: circle.x + circle.r/Math.sqrt(2),
    y: circle.y + circle.r/Math.sqrt(2),
    normal: {x: Math.sqrt(2), y: Math.sqrt(2)}
  };

  function updateHyperplane(svg, pos) {
    return svg.selectAll(".hyperplane")
      .data(pos ? [pos] : [])
      .join(
        "line",
        update => update,
        exit   =>
          exit.transition("movePlane")
            .duration(100)
            .style("opacity", 0)
            .remove(),
      )
      .attr("x1", d => d.x - d.normal.y * maxLength)
      .attr("y1", d => d.y + d.normal.x * maxLength)
      .attr("x2", d => d.x + d.normal.y * maxLength)
      .attr("y2", d => d.y - d.normal.x * maxLength)
      .attr("class", "hyperplane");
  }


  // Our convex set
  const convexSet = svg.selectAll(".convex-set")
    .data([circle])
    .join("circle")
    .attr("cx", d => d.x)
    .attr("cy", d => d.y)
    .attr("r",  d => d.r)
    .attr("class", "convex-set");

  const hyperplane = updateHyperplane(svg, pos);

  // Draw hyperplane separating mouse and convex set
  svg.on("mousemove", function (event) {
    const [x, y] = d3.pointer(event);
    const isInside = pointCircleIntersection({x, y}, circle);

    // Update convex set
    convexSet.attr("class", isInside ? "red-circle" : "convex-set");

    // Update hyperplane
    const normal = normalize({x: circle.x - x, y: circle.y - y});
    border = {x: circle.x + normal.x*circle.r, y: circle.y + normal.y*circle.r};
    pos = {x: border.x, y: border.y, normal};

    updateHyperplane(svg, isInside? null : pos);

    // Mouse marker
    svg.selectAll(".mark").remove();
    if (!isInside) {
      mark(svg, pos.x, pos.y).attr("fill", "orange");
    }
  });
}
