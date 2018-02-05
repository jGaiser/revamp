import React from 'react';
import CyButton from './CyButton';

class Sidebar extends React.Component {
  render(){
    return(
      <div id='Sidebar'>
      	<CyButton onClick={() => this.props.togglePanel(true)} >
      		Generate New Network
      	</CyButton>
      </div>
    )
  }
}

export default Sidebar;