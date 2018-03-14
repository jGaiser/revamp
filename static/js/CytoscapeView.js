import React from 'react';
import cytoscape from './cytoscape.min'

class CytoscapeView extends React.Component {
  constructor(props) {
    super(props);
    this.SUID = null;
  }
  
  shouldComponentUpdate(nextProps) {
    if (nextProps.networkData.length < 1) return false;
  
    const nextNetwork = nextProps.networkData[nextProps.stateIndex].data;
    
    if (nextNetwork.data.SUID == this.SUID) return false;
    
    return true;
  }
  
  componentDidUpdate(){
    this.SUID = this.props.networkData[this.props.stateIndex].data.data.SUID;
    
    if(this.props.networkData.length > 0){
      this.cy.json( this.props.networkData[this.props.stateIndex].data);
    }
    
  }

  componentDidMount() {
    this.cy = cytoscape({container: this.cyView});
  }

  render() {
    return <div ref={(cyView) => this.cyView = cyView} id='cyView'></div>
  }
}

export default CytoscapeView;
