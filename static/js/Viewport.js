import React from 'react';

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
      	<canvas id='cyView'>Canvas</canvas>
      	<ZoomControlPanel />
      	<HelpButton />
      </div>
    )
  }
}

export default Viewport; 