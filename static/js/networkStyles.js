var STYLES = [ {
  "format_version" : "1.0",
  "generated_by" : "cytoscape-3.5.1",
  "target_cytoscapejs_version" : "~2.1",
  "title" : "default",
  "style" : [ {
    "selector" : "node",
    "css" : {
      "text-opacity" : 1.0,
      "height" : 100.0,
      "text-valign" : "center",
      "text-halign" : "center",
      "font-family" : "SansSerif",
      "font-weight" : "normal",
      "color" : "rgb(0,0,0)",
      "font-size" : 22,
      "border-opacity" : 1.0,
      "shape" : "ellipse",
      "border-color" : "rgb(136,136,136)",
      "border-width" : 4.0,
      "background-opacity" : 1.0,
      "background-color" : "rgb(240,255,255)",
      "width" : 100.0,
      "content" : "data(name)",
      "text-outline-color": "white",
      "text-outline-width": 3,
      "font-family": "Arial, sans-serif"
    }
  }, {
    "selector": "node:selected",
    "css" : {
      "background-color": "pink",
    }
  }, {
    "selector": "node[nodeType='transcription factor']",
    "css" : {
      "shape": "polygon",
      "shape-polygon-points": "-.5, -1, 1, -1, .5, 1, -1, 1"
    }
  }, {
    "selector": "node[nodeType='SH2 protein']",
    "css" : {
      "shape": "vee"
    }
  }, {
    "selector": "node[nodeType='SH2-SH3 protein']",
    "css" : {
      "shape": "vee"
    }
  },  {
    "selector" : "node[Total > 16,387.79728649]",
    "css" : {
      "background-color" : "rgb(255,204,0)"
    }
  }, {
    "selector" : "node[Total = 16,387.79728649]",
    "css" : {
      "background-color" : "rgb(255,215,0)"
    }
  }, {
    "selector" : "node[Total > 10][Total < 16,387.79728649]",
    "css" : {
      "background-color" : "mapData(Total,10,16,387.79728649,rgb(255,230,0),rgb(255,215,0))"
    }
  }, {
    "selector" : "node[Total > 5][Total < 10]",
    "css" : {
      "background-color" : "mapData(Total,5,10,rgb(255,255,0),rgb(255,230,0))"
    }
  }, {
    "selector" : "node[Total > 2.25][Total < 5]",
    "css" : {
      "background-color" : "mapData(Total,2.25,5,rgb(255,255,126),rgb(255,255,0))"
    }
  }, {
    "selector" : "node[Total > 0][Total < 2.25]",
    "css" : {
      "background-color" : "mapData(Total,0,2.25,rgb(0,238,0),rgb(255,255,126))"
    }
  }, {
    "selector" : "node[Total > -2.25][Total < 0]",
    "css" : {
      "background-color" : "mapData(Total,-2.25,0,rgb(0,255,255),rgb(0,238,0))"
    }
  }, {
    "selector" : "node[Total > -5][Total < -2.25]",
    "css" : {
      "background-color" : "mapData(Total,-5,-2.25,rgb(0,204,255),rgb(0,255,255))"
    }
  }, {
    "selector" : "node[Total > -10][Total < -5]",
    "css" : {
      "background-color" : "mapData(Total,-10,-5,rgb(0,191,255),rgb(0,204,255))"
    }
  }, {
    "selector" : "node[Total > -20,753.45093737][Total < -10]",
    "css" : {
      "background-color" : "mapData(Total,-20,753.45093737,-10,rgb(0,127,255),rgb(0,191,255))"
    }
  }, {
    "selector" : "node[Total = -20,753.45093737]",
    "css" : {
      "background-color" : "rgb(0,127,255)"
    }
  }, {
    "selector" : "node[Total < -20,753.45093737]",
    "css" : {
      "background-color" : "rgb(0,153,255)"
    }
  }, {
    "selector" : "node[nodeType = 'transcription factor']",
    "css" : {
      "shape" : "parallelogram"
    }
  }, {
    "selector" : "node[nodeType = 'kinase']",
    "css" : {
      "shape" : "octagon"
    }
  }, {
    "selector" : "node[nodeType = 'SH2 protein']",
    "css" : {
      "shape" : "v"
    }
  }, {
    "selector" : "node[nodeType = 'tyrosine kinase']",
    "css" : {
      "shape" : "hexagon"
    }
  }, {
    "selector" : "node[nodeType = 'RNA binding protein']",
    "css" : {
      "shape" : "rectangle"
    }
  }, {
    "selector" : "node[nodeType = 'SRC-family kinase']",
    "css" : {
      "shape" : "diamond"
    }
  }, {
    "selector" : "node[nodeType = 'receptor tyrosine kinase']",
    "css" : {
      "shape" : "roundrectangle"
    }
  }, {
    "selector" : "node[nodeType = 'phosphatase']",
    "css" : {
      "shape" : "octagon"
    }
  }, {
    "selector" : "node[nodeType = 'SH3 protein']",
    "css" : {
      "shape" : "triangle"
    }
  }, {
    "selector" : "node[nodeType = 'SH2-SH3 protein']",
    "css" : {
      "shape" : "v"
    }
  }, {
    "selector" : "node[nodeType = 'unknown']",
    "css" : {
      "shape" : "ellipse"
    }
  }, {
    "selector" : "node[nodeType = 'G protein-coupled receptor']",
    "css" : {
      "border-width" : 16.0
    }
  }, {
    "selector" : "node[nodeType = 'kinase']",
    "css" : {
      "border-width" : 12.0
    }
  }, {
    "selector" : "node[nodeType = 'methyltransferase']",
    "css" : {
      "border-width" : 12.0
    }
  }, {
    "selector" : "node[nodeType = 'tyrosine kinase']",
    "css" : {
      "border-width" : 12.0
    }
  }, {
    "selector" : "node[nodeType = 'membrane protein']",
    "css" : {
      "border-width" : 8.0
    }
  }, {
    "selector" : "node[nodeType = 'SRC-family kinase']",
    "css" : {
      "border-width" : 12.0
    }
  }, {
    "selector" : "node[nodeType = 'receptor tyrosine kinase']",
    "css" : {
      "border-width" : 16.0
    }
  }, {
    "selector" : "node[nodeType = 'phosphatase']",
    "css" : {
      "border-width" : 14.0
    }
  }, {
    "selector" : "node[nodeType = 'deacetylase']",
    "css" : {
      "border-width" : 4.0
    }
  }, {
    "selector" : "node[nodeType = 'demethylase']",
    "css" : {
      "border-width" : 4.0
    }
  }, {
    "selector" : "node[nodeType = 'acetyltransferase']",
    "css" : {
      "border-width" : 12.0
    }
  }, {
    "selector" : "node:selected",
    "css" : {
      "background-color" : "rgb(255,51,136)"
    }
  }, {
    "selector" : "edge",
    "css" : {
      "opacity" : 1.0,
      "line-color" : "rgb(255,255,255)",
      "color" : "rgb(0,0,0)",
      "font-family" : "Dialog",
      "font-weight" : "normal",
      "width" : 3.0,
      "target-arrow-shape" : "none",
      "font-size" : 10,
      "content" : "",
      "source-arrow-shape" : "none",
      "target-arrow-color" : "rgb(230,230,250)",
      "text-opacity" : 1.0,
      "line-style" : "solid",
      "source-arrow-color" : "rgb(230,230,250)",
      "curve-style": "bezier"
    }
  }, {
    "selector" : "edge[edgeType = 'pp']",
    "css" : {
      "line-color" : "rgb(255,0,0)"
    }
  }, {
    "selector" : "edge[edgeType = 'Predicted']",
    "css" : {
      "line-color" : "rgb(0,238,238)"
    }
  }, {
    "selector" : "edge[edgeType = 'peptide']",
    "css" : {
      "line-color" : "rgb(0,0,0)"
    }
  }, {
    "selector" : "edge[edgeType = 'homology']",
    "css" : {
      "line-color" : "rgb(255,165,0)"
    }
  }, {
    "selector" : "edge[edgeType = 'Shared protein domains']",
    "css" : {
      "line-color" : "rgb(238,154,0)"
    }
  }, {
    "selector" : "edge[edgeType = 'controls-state-change-of']",
    "css" : {
      "line-color" : "rgb(160,32,240)"
    }
  }, {
    "selector" : "edge[edgeType = 'intersect']",
    "css" : {
      "line-color" : "rgb(190,190,190)"
    }
  }, {
    "selector" : "edge[edgeType = 'Pathway']",
    "css" : {
      "line-color" : "rgb(0,229,238)"
    }
  }, {
    "selector" : "edge[edgeType = 'experiments']",
    "css" : {
      "line-color" : "rgb(118,238,198)"
    }
  }, {
    "selector" : "edge[edgeType = 'controls-transport-of']",
    "css" : {
      "line-color" : "rgb(238,130,238)"
    }
  }, {
    "selector" : "edge[edgeType = 'merged']",
    "css" : {
      "line-color" : "rgb(47,79,79)"
    }
  }, {
    "selector" : "edge[edgeType = 'combined_score']",
    "css" : {
      "line-color" : "rgb(112,128,144)"
    }
  }, {
    "selector" : "edge[edgeType = 'positive correlation']",
    "css" : {
      "line-color" : "rgb(255,255,0)"
    }
  }, {
    "selector" : "edge[edgeType = 'controls-expression-of']",
    "css" : {
      "line-color" : "rgb(255,0,255)"
    }
  }, {
    "selector" : "edge[edgeType = 'database']",
    "css" : {
      "line-color" : "rgb(0,255,255)"
    }
  }, {
    "selector" : "edge[edgeType = 'Genetic interactions']",
    "css" : {
      "line-color" : "rgb(32,178,170)"
    }
  }, {
    "selector" : "edge[edgeType = 'correlation']",
    "css" : {
      "line-color" : "rgb(255,215,0)"
    }
  }, {
    "selector" : "edge[edgeType = 'controls-phosphorylation-of']",
    "css" : {
      "line-color" : "rgb(255,0,0)"
    }
  }, {
    "selector" : "edge[edgeType = 'negative correlation']",
    "css" : {
      "line-color" : "rgb(0,0,255)"
    }
  }, {
    "selector" : "edge[edgeType = 'in-complex-with']",
    "css" : {
      "line-color" : "rgb(0,205,0)"
    }
  }, {
    "selector" : "edge[edgeType = 'BioPlex']",
    "css" : {
      "line-color" : "rgb(0,238,0)"
    }
  }, {
    "selector" : "edge[edgeType = 'Physical interactions']",
    "css" : {
      "line-color" : "rgb(0,255,0)"
    }
  }, {
    "selector" : "edge[edgeType = 'pp']",
    "css" : {
      "target-arrow-shape" : "triangle"
    }
  }, {
    "selector" : "edge[edgeType = 'Predicted']",
    "css" : {
      "target-arrow-shape" : null
    }
  }, {
    "selector" : "edge[edgeType = 'peptide']",
    "css" : {
      "target-arrow-shape" : null
    }
  }, {
    "selector" : "edge[edgeType = 'homology']",
    "css" : {
      "target-arrow-shape" : null
    }
  }, {
    "selector" : "edge[edgeType = 'Shared protein domains']",
    "css" : {
      "target-arrow-shape" : null
    }
  }, {
    "selector" : "edge[edgeType = 'controls-state-change-of']",
    "css" : {
      "target-arrow-shape" : "triangle"
    }
  }, {
    "selector" : "edge[edgeType = 'intersect']",
    "css" : {
      "target-arrow-shape" : null
    }
  }, {
    "selector" : "edge[edgeType = 'Pathway']",
    "css" : {
      "target-arrow-shape" : null
    }
  }, {
    "selector" : "edge[edgeType = 'experiments']",
    "css" : {
      "target-arrow-shape" : null
    }
  }, {
    "selector" : "edge[edgeType = 'controls-transport-of']",
    "css" : {
      "target-arrow-shape" : "triangle"
    }
  }, {
    "selector" : "edge[edgeType = 'merged']",
    "css" : {
      "target-arrow-shape" : null
    }
  }, {
    "selector" : "edge[edgeType = 'combined_score']",
    "css" : {
      "target-arrow-shape" : null
    }
  }, {
    "selector" : "edge[edgeType = 'positive correlation']",
    "css" : {
      "target-arrow-shape" : null
    }
  }, {
    "selector" : "edge[edgeType = 'controls-expression-of']",
    "css" : {
      "target-arrow-shape" : "triangle"
    }
  }, {
    "selector" : "edge[edgeType = 'database']",
    "css" : {
      "target-arrow-shape" : null
    }
  }, {
    "selector" : "edge[edgeType = 'Genetic interactions']",
    "css" : {
      "target-arrow-shape" : null
    }
  }, {
    "selector" : "edge[edgeType = 'correlation']",
    "css" : {
      "target-arrow-shape" : null
    }
  }, {
    "selector" : "edge[edgeType = 'controls-phosphorylation-of']",
    "css" : {
      "target-arrow-shape" : "triangle"
    }
  }, {
    "selector" : "edge[edgeType = 'negative correlation']",
    "css" : {
      "target-arrow-shape" : null
    }
  }, {
    "selector" : "edge[edgeType = 'in-complex-with']",
    "css" : {
      "target-arrow-shape" : null
    }
  }, {
    "selector" : "edge[edgeType = 'BioPlex']",
    "css" : {
      "target-arrow-shape" : null
    }
  }, {
    "selector" : "edge[edgeType = 'Physical interactions']",
    "css" : {
      "target-arrow-shape" : null
    }
  }, {
    "selector" : "edge[edgeType = 'pp']",
    "css" : {
      "source-arrow-color" : "rgb(255,0,0)"
    }
  }, {
    "selector" : "edge[edgeType = 'Predicted']",
    "css" : {
      "source-arrow-color" : "rgb(0,238,238)"
    }
  }, {
    "selector" : "edge[edgeType = 'peptide']",
    "css" : {
      "source-arrow-color" : "rgb(0,0,0)"
    }
  }, {
    "selector" : "edge[edgeType = 'homology']",
    "css" : {
      "source-arrow-color" : "rgb(255,165,0)"
    }
  }, {
    "selector" : "edge[edgeType = 'Shared protein domains']",
    "css" : {
      "source-arrow-color" : "rgb(238,154,0)"
    }
  }, {
    "selector" : "edge[edgeType = 'controls-state-change-of']",
    "css" : {
      "source-arrow-color" : "rgb(160,32,240)"
    }
  }, {
    "selector" : "edge[edgeType = 'intersect']",
    "css" : {
      "source-arrow-color" : "rgb(190,190,190)"
    }
  }, {
    "selector" : "edge[edgeType = 'Pathway']",
    "css" : {
      "source-arrow-color" : "rgb(0,229,238)"
    }
  }, {
    "selector" : "edge[edgeType = 'experiments']",
    "css" : {
      "source-arrow-color" : "rgb(118,238,198)"
    }
  }, {
    "selector" : "edge[edgeType = 'controls-transport-of']",
    "css" : {
      "source-arrow-color" : "rgb(238,130,238)"
    }
  }, {
    "selector" : "edge[edgeType = 'merged']",
    "css" : {
      "source-arrow-color" : "rgb(47,79,79)"
    }
  }, {
    "selector" : "edge[edgeType = 'combined_score']",
    "css" : {
      "source-arrow-color" : "rgb(112,128,144)"
    }
  }, {
    "selector" : "edge[edgeType = 'positive correlation']",
    "css" : {
      "source-arrow-color" : "rgb(255,255,0)"
    }
  }, {
    "selector" : "edge[edgeType = 'controls-expression-of']",
    "css" : {
      "source-arrow-color" : "rgb(255,0,255)"
    }
  }, {
    "selector" : "edge[edgeType = 'database']",
    "css" : {
      "source-arrow-color" : "rgb(0,255,255)"
    }
  }, {
    "selector" : "edge[edgeType = 'Genetic interactions']",
    "css" : {
      "source-arrow-color" : "rgb(32,178,170)"
    }
  }, {
    "selector" : "edge[edgeType = 'correlation']",
    "css" : {
      "source-arrow-color" : "rgb(255,215,0)"
    }
  }, {
    "selector" : "edge[edgeType = 'controls-phosphorylation-of']",
    "css" : {
      "source-arrow-color" : "rgb(255,0,0)"
    }
  }, {
    "selector" : "edge[edgeType = 'negative correlation']",
    "css" : {
      "source-arrow-color" : "rgb(0,0,255)"
    }
  }, {
    "selector" : "edge[edgeType = 'in-complex-with']",
    "css" : {
      "source-arrow-color" : "rgb(0,205,0)"
    }
  }, {
    "selector" : "edge[edgeType = 'BioPlex']",
    "css" : {
      "source-arrow-color" : "rgb(0,238,0)"
    }
  }, {
    "selector" : "edge[edgeType = 'Physical interactions']",
    "css" : {
      "source-arrow-color" : "rgb(0,255,0)"
    }
  }, {
    "selector" : "edge[edgeType = 'pp']",
    "css" : {
      "target-arrow-color" : "rgb(255,0,0)"
    }
  }, {
    "selector" : "edge[edgeType = 'Predicted']",
    "css" : {
      "target-arrow-color" : "rgb(0,238,238)"
    }
  }, {
    "selector" : "edge[edgeType = 'peptide']",
    "css" : {
      "target-arrow-color" : "rgb(0,0,0)"
    }
  }, {
    "selector" : "edge[edgeType = 'homology']",
    "css" : {
      "target-arrow-color" : "rgb(255,165,0)"
    }
  }, {
    "selector" : "edge[edgeType = 'Shared protein domains']",
    "css" : {
      "target-arrow-color" : "rgb(238,154,0)"
    }
  }, {
    "selector" : "edge[edgeType = 'controls-state-change-of']",
    "css" : {
      "target-arrow-color" : "rgb(160,32,240)"
    }
  }, {
    "selector" : "edge[edgeType = 'intersect']",
    "css" : {
      "target-arrow-color" : "rgb(190,190,190)"
    }
  }, {
    "selector" : "edge[edgeType = 'Pathway']",
    "css" : {
      "target-arrow-color" : "rgb(0,229,238)"
    }
  }, {
    "selector" : "edge[edgeType = 'experiments']",
    "css" : {
      "target-arrow-color" : "rgb(118,238,198)"
    }
  }, {
    "selector" : "edge[edgeType = 'controls-transport-of']",
    "css" : {
      "target-arrow-color" : "rgb(238,130,238)"
    }
  }, {
    "selector" : "edge[edgeType = 'merged']",
    "css" : {
      "target-arrow-color" : "rgb(47,79,79)"
    }
  }, {
    "selector" : "edge[edgeType = 'combined_score']",
    "css" : {
      "target-arrow-color" : "rgb(112,128,144)"
    }
  }, {
    "selector" : "edge[edgeType = 'positive correlation']",
    "css" : {
      "target-arrow-color" : "rgb(255,255,0)"
    }
  }, {
    "selector" : "edge[edgeType = 'controls-expression-of']",
    "css" : {
      "target-arrow-color" : "rgb(255,0,255)"
    }
  }, {
    "selector" : "edge[edgeType = 'database']",
    "css" : {
      "target-arrow-color" : "rgb(0,255,255)"
    }
  }, {
    "selector" : "edge[edgeType = 'Genetic interactions']",
    "css" : {
      "target-arrow-color" : "rgb(32,178,170)"
    }
  }, {
    "selector" : "edge[edgeType = 'correlation']",
    "css" : {
      "target-arrow-color" : "rgb(255,215,0)"
    }
  }, {
    "selector" : "edge[edgeType = 'controls-phosphorylation-of']",
    "css" : {
      "target-arrow-color" : "rgb(255,0,0)"
    }
  }, {
    "selector" : "edge[edgeType = 'negative correlation']",
    "css" : {
      "target-arrow-color" : "rgb(0,0,255)"
    }
  }, {
    "selector" : "edge[edgeType = 'in-complex-with']",
    "css" : {
      "target-arrow-color" : "rgb(0,205,0)"
    }
  }, {
    "selector" : "edge[edgeType = 'BioPlex']",
    "css" : {
      "target-arrow-color" : "rgb(0,238,0)"
    }
  }, {
    "selector" : "edge[edgeType = 'Physical interactions']",
    "css" : {
      "target-arrow-color" : "rgb(0,255,0)"
    }
  }, {
    "selector" : "edge:selected",
    "css" : {
      "line-color" : "rgb(255,0,0)"
    }
  } ]
} ]

var finalStyle = STYLES[0].style

for(var i in finalStyle) {
	finalStyle[i].selector = finalStyle[i].selector.replace(/(Total\s[\<|\>|\=]\D*\d*)(\,)(\d)/g, "$1$3");
}

module.exports = {
	NETWORK_STYLES: finalStyle
}