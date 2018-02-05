import React from 'react';

class InfoBox extends React.Component {
  render(){
  	if(!this.props.show) return null;

    return(
      <div id='InfoBox'>Info Box</div>
    )
  }
}

export default InfoBox;