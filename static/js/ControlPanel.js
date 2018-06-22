import React from 'react';

import {SlideDown} from 'react-slidedown'
import 'react-slidedown/lib/slidedown.css'

const dataFilters = ['Crizantonib', 'Iressa', 'Gleevec', 'Geldanamycin', 'Lung Cancer Cells vs Normal Tissue'];
const layouts = {"genemania-force-directed": "Genemania Force Directed", 
                 "attribute-circle": "Attribute Circle", 
                 "stacked-node-layout": "Stacked Node-Layout",
                 "degree-circle": "Degree Circle", 
                 "circular": "Circular", 
                 "attributes-layout": "Attributes Layout", 
                 "kamada-kawai": "Kamada Kawai", 
                 "force-directed": "Force Directed", 
                 "cose": "Cose", 
                 "grid": "Grid", 
                 "hierarchical": "Hierarchical", 
                 "fruchterman-rheingold": "Fruchterman Rheingold", 
                 "isom": "Isom"}

class ControlPanelSection extends React.Component {
  constructor(props){
    super(props);
    this.state = {show: false}
  }

  render() {
    const childComponents = React.Children.map(this.props.children, (child) => {
      return React.cloneElement(child);
    });

    return(
      <div className='cpSection'>
        <div className="cpSectionHeader" onClick={()=>this.setState({show: !this.state.show}) } >
          <span className={this.state.show ? "rotated" : ""}>▲</span>
          { this.props.sectionName }
        </div>

        <SlideDown className={'my-dropdown-slidedown'}>
          {this.state.show ? <div className='cpItemsContainer'>{ childComponents }</div> : null}
        </SlideDown>

      </div>
    )
  }
}

class SimpleListSection extends React.Component {
  render() {

    var items = this.props.items.map((item) => {
      return <div key={item} className='controlBox' onClick={this.props.clickHandler} >{item}</div>
    })

    return(
      <div>
        {items}
      </div>
    )
  }
}

class LayoutListSection extends React.Component {
  render() {

    var items = this.props.items.map((item) => {
      return <div key={item} className='controlBox' onClick={() => this.props.clickHandler(item)} >{layouts[item]}</div>
    })

    return(
      <div>
        {items}
      </div>
    )
  }
}

class NodeListSection extends React.Component {
  constructor(props) {
    super(props);
    this.state = {
      searchText: ""
    }
  }

  render(){
    if(this.props.show == false) return null;
    if(this.props.stateIndex == -1) return <div>"No nodes here!"</div>

    var nodes = this.props.networkData[this.props.stateIndex].data.elements.nodes;

    nodes = nodes.filter((item) => {
      if(this.state.searchText != "") {
        let name = item.data.name.toLowerCase(); 
        let searchText = this.state.searchText.toLowerCase();

        if(name.slice(0, searchText.length) != searchText) return false;
        return true;
      }

      return true;
    })
      
    var items = nodes.map((item) => {
      return <div key={item.data.id} className='controlBox'>{item.data.name}</div>
    })

    if(items.length == 0){
      items = [<div key='noResults'>No proteins matched search input.</div>];
    }

    return (
      <div>
        <div className="controlBox nodeSearchBox">
          <i className="fa fa-search" aria-hidden="true"></i>
          <input type="text" 
                 placeholder="Search Proteins" 
                 className="nodeSearchInput" 
                 value={ this.state.searchText } 
                 onChange={ (event) => { 
                   this.setState({searchText: event.target.value})
                 }}
          >
          </input>
        </div>

        <div>
          {items}
        </div>
      </div>
    )
  }
}

class EdgeListSection extends React.Component {
  render(){
    var i = 0;

    var items = this.props.networkStyles.map((item) => {
      if (item.css.hasOwnProperty('line-color') && item.selector.indexOf("edge[edgeType") > -1) {
        var edgeType = item.selector.split("'")[1]
        console.log(item);
        return(
          <div key={i++} className='controlBox'>
            {edgeType}
            <div className='edgeColorBox' style={{background: item.css['line-color']}}></div>
          </div>
        )
      }
    })

    return <div>{items}</div>
  }
}

class ControlPanel extends React.Component {
  render() {
    
    return(
      <div className='controlPanelContainer'>
        <ControlPanelSection sectionName="Node List">
          <NodeListSection {...this.props}/>
        </ControlPanelSection>
        
        <ControlPanelSection sectionName="Edge Properties">
          <EdgeListSection networkStyles={this.props.networkStyles} />
        </ControlPanelSection>
      
        <ControlPanelSection sectionName="Network Layouts">
          <LayoutListSection items={Object.keys(layouts)} clickHandler={this.props.updateLayout}/>
        </ControlPanelSection>
        
        <ControlPanelSection sectionName="Data Filters">
          <SimpleListSection items={dataFilters}/>
        </ControlPanelSection>
      </div>
    )
  }
}

export default ControlPanel
