import React, { Component } from 'react';
import GeneData from './geneData.js';
import '../css/App.css';
import '../css/Viewport.css';
import '../css/GenerateNetworkPanel.css';
import '../css/CyButton.css';
import '../css/ControlPanel.css'
import CyButton             from './CyButton'
import InfoBox              from './InfoBox';
import Sidebar              from './Sidebar';
import Viewport             from './Viewport';
import CytoscapeView        from './CytoscapeView';
import GenerateNetworkPanel from './GenerateNetworkPanel';
import Server from './Server';

class App extends Component {

  constructor(props) {
    super(props);
    
    this.state = {
      width: 0,
      height: 0,
      showInfoBox: false,
      showGenerateNetworkPanel: true,
      selectedFamilies: [],
      selectedGenes: [],
      networkData: [],
      stateIndex: -1,
    }
    
    this.updateDimensions = this.updateDimensions.bind(this);
    this.toggleGenerateNetworkPanel = this.toggleGenerateNetworkPanel.bind(this);
    this.updateSelectedFamilies = this.updateSelectedFamilies.bind(this);
    this.updateSelectedGenes = this.updateSelectedGenes.bind(this);
    this.updateNetworkData = this.updateNetworkData.bind(this);
    this.updateLayout = this.updateLayout.bind(this);
  }
  
  updateLayout(newLayout) {
    Server.buildNetwork(this.currentNetwork(), (response) => {
      const newNetworkID = response.data.networkSUID
      Server.updateNetworkLayout(newNetworkID, newLayout, (response) => {
        this.updateNetworkData(newNetworkID); 
        Server.deleteNetwork(newNetworkID, (response) => {
        });
      });
    })
  }
  
  updateNetworkData(templateID){
    Server.getNetworkData(templateID, (response) => {
      console.log(response);
      const newIndex    = this.state.stateIndex + 1;
      const networkData = this.state.networkData.slice(0, newIndex);
      
      networkData[newIndex] = response;
      
      this.setState({
        networkData: networkData,
        stateIndex: newIndex
      })
    })
  }
  
  currentNetwork(){
    return this.state.networkData[this.state.stateIndex];
  }

  updateDimensions() {
    this.setState({
      width: window.innerWidth,
      height: window.innerHeight,
    });
  }

  updateSelectedFamilies(family, add){
    let selectedFamilies = this.state.selectedFamilies.slice();

    if(add){
      selectedFamilies.push(family);
    } else {
      selectedFamilies.splice(selectedFamilies.indexOf(family), 1);
    }

    this.setState({ selectedFamilies });
  }

  updateSelectedGenes(gene, add){
    let selectedGenes = this.state.selectedGenes.slice();

    if (add) {
      selectedGenes.push(gene);
    } else {
      selectedGenes.splice(selectedGenes.indexOf(gene), 1);
    }

    this.setState({ selectedGenes });
  }

  toggleGenerateNetworkPanel(show){
    this.setState({
      showGenerateNetworkPanel: show
    })
  }

  componentDidMount() {
    this.updateDimensions();
    window.addEventListener('resize', this.updateDimensions);
  }
  
  render() {
    const appStyle = {
      width: this.state.width,
      height: this.state.height
    }

    const generateNetworkPanelProps = {
      togglePanel: this.toggleGenerateNetworkPanel,
      show: this.state.showGenerateNetworkPanel,
      geneFamilies: GeneData.GENE_FAMILIES,
      allGenes: GeneData.ALL_GENES,
      selectedFamilies: this.state.selectedFamilies,
      updateSelectedFamilies: this.updateSelectedFamilies,
      updateSelectedGenes: this.updateSelectedGenes,
      updateNetworkData: this.updateNetworkData
    }

    const sidebarProps = {
      updateLayout: this.updateLayout,
      togglePanel: this.toggleGenerateNetworkPanel,
      networkData: this.state.networkData,
      stateIndex: this.state.stateIndex,
    }
    
    const viewportProps = {
      networkData: this.state.networkData,
      stateIndex: this.state.stateIndex
    }
    
    return (
      <div id="CyBrowserApp" style={appStyle} >
        <Sidebar {...sidebarProps} />
        <Viewport {...viewportProps} />
        <GenerateNetworkPanel {...generateNetworkPanelProps}/>
        <InfoBox show={this.state.showInfoBox} />
      </div>
    );
  }
}

export default App;
