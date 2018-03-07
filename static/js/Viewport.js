import React from 'react';
import CytoscapeView from './CytoscapeView'

function HelpButton(props) {
	return <div id='HelpButton'>Help Button</div>
}

class ZoomControlPanel extends React.Component {
  render() {
    return(
      <div id='ZoomControlPanel' >Zoom Control Panel</div>
    )
  }
}

class Viewport extends React.Component {
  render(){
    return(
      <div id='Viewport'>
      	<CytoscapeView />
      	<ZoomControlPanel />
      	<HelpButton />
      </div>
    )
  }
}

export default Viewport;
