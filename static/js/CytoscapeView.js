import React from 'react';
import cytoscape from './cytoscape.min'
import NodeStyles from './NodeStyles.js'

class CytoscapeView extends React.Component {
  constructor(props) {
    super(props);
    this.SUID = null;
    this.borderColorKey = {
        acetyl: "#FF8C00",
        methyl: "#005CE6",
        membrane: "#6600CC",
        kinase: "#EE0000",
        phosphatase: "#FFEC8B",
        receptor: "#BF3EFF",
        TM: "#6600CC",
      }
    this.edgeArrowList = [
        "pp",
        "controls-phosphorylation-of",
        "controls-expression-of",
        "controls-transport-of",
        "controls-state-change-of"
      ];
  }
  
  shouldComponentUpdate(nextProps) {
    if (nextProps.networkData.length < 1) return false;
  
    const nextNetwork = nextProps.networkData[nextProps.stateIndex].data;
    
    if (nextNetwork.data.SUID == this.SUID) return false;
    
    return true;
  }

  setEventListeners() {
    this.cy.$('node, edge').on('mouseover', (event) => {
      this.props.setInfoBoxData(event.target._private.data)
    })

    this.cy.$('node, edge').on('mouseout', (event) => {
      this.props.setInfoBoxData({})
    })

    this.cy.on('select',   this.updateSelectedElems.bind(this));
    this.cy.on('unselect', this.updateSelectedElems.bind(this))
  }

  setNodeBorderColors(node) {
    for(var key in this.borderColorKey){
      if(node.nodeType.includes(key)){
        this.cy.style()
          .selector('node[id="' + node.id + '"]')
          .style({ 'border-color':  this.borderColorKey[key] });
      }
    } 
  }

  setEdgeProps() {
    var edgeCollection = this.cy.edges();

    for (var i = 0; i < edgeCollection.length; i++) {
      let edge = edgeCollection[i]._private.data
      let calculatedWidth = ( edge.Weight < 0 ) ? 1 : Math.ceil(edge.Weight * 10)
      if(calculatedWidth > 10){ calculatedWidth = 10 };

      this.cy.style()
        .selector('edge[id="' + edge.id + '"]')
        .style({'width': calculatedWidth });

      if(this.edgeArrowList.indexOf(edge.edgeType) != -1 ){
        this.cy.style()
          .selector('edge[id="' + edge.id + '"]')
          .style({'target-arrow-shape': 'triangle'});
      }
    }

    this.cy.style().update()
  }
  
  getNodeMappings(node){
    var propertyValues = {},
        propertyMap, 
        nodeValue,
        smallValue,
        largeValue

    for (var property in NodeStyles) {
        propertyMap = NodeStyles[property];
        nodeValue = node[propertyMap.mappingColumn];

      if (nodeValue <= propertyMap.points[0].value) {
        propertyValues[property] = propertyMap.points[0].value.lesser;
        continue;
      }

      if (nodeValue >= propertyMap.points[propertyMap.points.length - 1].value){
        propertyValues[property] = propertyMap.points[0].value.greater;
        continue;
      }

      for (var i in propertyMap.points) {
        if( nodeValue == propertyMap.points[i].value ){
          propertyValues[property] =  propertyMap.points[i].equal;
          break;
        }

        if(nodeValue < propertyMap.points[i].value){
          if(property == "NODE_SIZE"){
            propertyValues[property] = ((nodeValue - propertyMap.points[i-1].value) /
                                       (propertyMap.points[i].value - propertyMap.points[i-1].value)) *
                                       (parseFloat(propertyMap.points[i].equal) - parseFloat(propertyMap.points[i-1].equal)) +
                                        parseFloat(propertyMap.points[i-1].equal);
          }

          //if(property == "NODE_FILL_COLOR"){
          //  propertyValues[property] = propertyMap.points[i-1].equal
          //}

          // New color code
          if(property == "NODE_FILL_COLOR"){
            // Figure out which number the value is closer too
            smallValue =  propertyMap.points[i-1].value;
            largeValue = propertyMap.points[i].value;
            if ((nodeValue - smallValue) / (largeValue - smallValue) <= .5) {
              propertyValues[property] = propertyMap.points[i-1].equal; // Closer to smaller value
            }
            else {
              propertyValues[property] = propertyMap.points[i].equal; // Closer to large value
            }
          }

          break;
        }
      }
    }

    return propertyValues;
  }

  setNodeSize(node) {
    var style = this.getNodeMappings(node)
    if('NODE_SIZE' in style){
      var widthHeight = style['NODE_SIZE'];

      this.cy.style()
      .selector('node[id="' + node.id + '"]')
      .style({'width': widthHeight,
              'height': widthHeight });
    }
  }

  setNodeBgColor(node) {
    var style = this.getNodeMappings(node)

    if('NODE_FILL_COLOR' in style){
      var nodeColor = style['NODE_FILL_COLOR'];

      this.cy.style()
        .selector('node[id="' + node.id + '"]')
        .style({ 'background-color':  nodeColor });
    }

    this.cy.style()
      .selector('node:selected')
      .style('background-color', 'pink');
  }

  centerZoom() {
    this.cy.zoom(this.cy.getFitViewport().zoom);
    this.cy.center();
  } 

  setNodeProps() {
    var nodes = this.cy.nodes()

    for(var i = 0; i < nodes.length; i++) {
      let node = nodes[i]._private.data
      this.setNodeBorderColors(node)
      this.setNodeSize(node)
      this.setNodeBgColor(node)
    }

    this.cy.style().update()
  }
  
  componentDidUpdate(){
    this.SUID = this.props.networkData[this.props.stateIndex].data.data.SUID;
    
    if(this.props.networkData.length == 0) return;

    this.cy.json( this.props.networkData[this.props.stateIndex].data);

    this.setEventListeners();
    this.setEdgeProps();
    this.setNodeProps();
    this.centerZoom();
  }

  componentDidMount() {
    this.cy = cytoscape({ 
      container: this.cyView,
      style: this.props.networkStyles
    });
  }

  updateSelectedElems() {
    this.props.setSelectedNodes(this.cy.nodes(':selected'))
  }

  render() {
    if(!this.props.networkStyles.length) return null
    return <div ref={(cyView) => this.cyView = cyView} id='cyView'></div>
  }
}

export default CytoscapeView;
