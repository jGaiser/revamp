import React from 'react';

const dataFilters = ['Crizantonib', 'Iressa', 'Gleevec', 'Geldanamycin', 'Lung Cancer Cells vs Normal Tissue'];
const layouts = ["Genemania Force Directed", "Attribute Circle", "Stacked Node-Layout", "Degree Circle", "Circular", "Attributes Layout", "Kamada Kawai", "Force Directed", "Cose", "Grid", "Hierarchical", "Fruchterman Rheingold", "Isom"]

class ControlPanelSection extends React.Component {
  constructor(props){
    super(props);
    this.state = {show: false}
  }
  render() {
    const childComponents = React.Children.map(this.props.children, (child) => {
      return React.cloneElement(child, {show: this.state.show});
    });
    return(
      <div className='cpSection'>
        <div className='cpSectionHeader' onClick={()=>this.setState({show: !this.state.show}) } >
          <span>▲</span>
          { this.props.sectionName }
        </div>

        <div className='cpItemsContainer'>
          { childComponents }
        </div>
      </div>
    )
  }
}

class SimpleListSection extends React.Component {
  render() {
    if(this.props.show == false) return null;
    var items = this.props.items.map((item) => {
      return <div key={item} className='controlBox'>{item}</div>
    })

    return(
      <div>
        {items}
      </div>
    )
  }
}

class NodeListSection extends React.Component {
  render(){
    if(this.props.show == false) return null;
    return <div>"No nodes here!"</div>
  }
}

class EdgeListSection extends React.Component {
  render(){
    if(this.props.show == false) return null;
    return <div>"No edges just yet."</div>
  }
}

class ControlPanel extends React.Component {
  render() {
    return(
      <div className='controlPanelContainer'>
      <ControlPanelSection sectionName="Node List">
        <NodeListSection />
      </ControlPanelSection>
      
      <ControlPanelSection sectionName="Edge Properties">
        <EdgeListSection />
      </ControlPanelSection>
      
        <ControlPanelSection sectionName="Network Layouts">
          <SimpleListSection items={layouts}/>
        </ControlPanelSection>
        
        <ControlPanelSection sectionName="Data Filters">
          <SimpleListSection items={dataFilters}/>
        </ControlPanelSection>
      </div>
    )
  }
}

export default ControlPanel
