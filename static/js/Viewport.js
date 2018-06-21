import React from 'react';
import CytoscapeView from './CytoscapeView'

function HelpButton(props) {
	return <div id='HelpButton'>Help Button</div>
}

class ZoomControlPanel extends React.Component {
  render() {
    return(
      <div id='ZoomControlPanel'>Zoom Control Panel</div>
    )
  }
}

class SelectedNodes extends React.Component {
  
  render() {
    if(!this.props.selectedNodes.length) return null

    var items = []
    var nodes = this.props.selectedNodes;

    for(var i = 0; i < nodes.length; i++) {
      items.push(nodes[i]._private.data)
    }

    items.sort((a, b) => {
      if (a.label < b.label)
        return -1;
      if (a.label > b.label)
        return 1;
      return 0;
    })

    items = items.map((item) => {
      return (<div className="nodeBox" 
                key={item.id}>
                {item.label }
              </div>) 
    })

    return(
      <div className="selectedNodesContainer">
        <span className="selectedNodesHeader">Selected Nodes</span>
        <div className="nodeBoxContainer">
          {items}
        </div>
      </div> 
    )
  }
}

class InfoBox extends React.Component {
  render() {
    if(Object.keys(this.props.data).length === 0) return null;
    
    var data = this.props.data

    var type = data.hasOwnProperty('edgeType') ? "Edge" : "Protein"
    var label = data.hasOwnProperty('edgeType') ? data.name : data.label

    return (
      <div className='infoBox'>
        <h3>{label}</h3>
        <h4>{type}</h4>
        <strong>Node Type:</strong>{data.nodeType}<br></br>
        <strong>Approved Name:</strong>{data.name}<br></br>
        <strong>Compartment:</strong>{data.compartment}<br></br>
        <strong>Domains:</strong>{data.domains}<br></br>
      </div>
    )
  }
}

class Viewport extends React.Component {
  render(){
    return(
      <div id='Viewport'>
      	<CytoscapeView {...this.props} />
        <SelectedNodes selectedNodes={this.props.selectedNodes} />
      	<ZoomControlPanel />
      	<HelpButton />
        <InfoBox data={this.props.infoBoxData} />
      </div>
    )
  }
}

export default Viewport;
