import React from 'react';
import CyButton from './CyButton';
import ControlPanel from './ControlPanel'

class Sidebar extends React.Component {
  render(){
    return(
      <div id='Sidebar'>
      	<CyButton onClick={() => this.props.togglePanel(true)} >
      		Generate New Network
      	</CyButton>

        <ControlPanel {...this.props} />
      </div>
    )
  }
}

export default Sidebar;
