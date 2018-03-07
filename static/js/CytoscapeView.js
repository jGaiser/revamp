import React from 'react';
import cytoscape from './cytoscape.min'

class CytoscapeView extends React.Component {
  constructor(props) {
    super(props)
  }

  componentDidMount() {
    this.cy = cytoscape({container: this.cyView});
  }

  render() {
    return <div ref={(cyView) => this.cyView = cyView} id='cyView'></div>
  }
}

export default CytoscapeView;
