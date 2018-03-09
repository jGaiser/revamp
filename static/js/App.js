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
import Network from './Network';

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
    }
    
    this.updateDimensions = this.updateDimensions.bind(this);
    this.toggleGenerateNetworkPanel = this.toggleGenerateNetworkPanel.bind(this);
    this.updateSelectedFamilies = this.updateSelectedFamilies.bind(this);
    this.updateSelectedGenes = this.updateSelectedGenes.bind(this);
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
    
    // PROMISE something something.
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
    }

    const sidebarProps = {
      togglePanel: this.toggleGenerateNetworkPanel,
    }

    return (
      <div id="CyBrowserApp" style={appStyle} >
        <Sidebar {...sidebarProps} />
        <Viewport />
        <GenerateNetworkPanel {...generateNetworkPanelProps}/>
        <InfoBox show={this.state.showInfoBox} />
      </div>
    );
  }
}

export default App;
