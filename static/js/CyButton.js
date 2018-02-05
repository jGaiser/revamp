import React from 'react';

class CyButton extends React.Component {
	render() {
		return <button className='cyButton' onClick={this.props.onClick}>{this.props.children}</button>
	}
}

export default CyButton;