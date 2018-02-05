import React from 'react';

const lastWordRegex = /\b(\w+)\W*$/;

class ExitButton extends React.Component {

    render() {
        return(
            <div className='exitButton' onClick={()=>this.props.onClick(false)}>
                <span>X</span>
            </div>
        )
    }
}

class AvailableFamilies extends React.Component {
    constructor(props) {
        super(props);
    }

    render() {
        const familyList = this.props.geneFamilies.map(item => {
            let selected = this.props.selectedFamilies.indexOf(item.value) > -1;
            let clickFunc = selected ? () => {} : this.props.updateSelectedFamilies;
            return(
              <li key={item.value}
                  onClick={() => clickFunc(item.value, true)}
                  className={selected ? 'selected' : ''}>
                  {item.label}
              </li>
            );
        });

        return(
            <div className='familyContainer'>
                <ul className='familyList'>
                    {familyList}
                </ul>
            </div>
        )
    }
}

class SelectedFamilies extends React.Component {
    constructor(props) {
        super(props);
    }

    render() {
        let familyList = this.props.geneFamilies.map(item => {
            if(this.props.selectedFamilies.indexOf(item.value) == -1) return;
            return(
                <li key={item.value}
                    onClick={() => this.props.updateSelectedFamilies(item.value, false)}>
                    {item.label}
                </li>
            )
        })

        return(
            <div className='familyContainer'>
                 <ul className='familyList'>
                     {familyList}
                 </ul>
            </div>
        )
    }
}

class GeneFamilySelect extends React.Component {
    constructor(props){
        super(props);
    }

    render() {
        return(
            <div className='GeneFamilySelect'>
                <AvailableFamilies {...this.props} />
                <SelectedFamilies {...this.props} />
            </div>
        )
    }
}

class GeneSuggestionPanel extends React.Component {

    componentDidUpdate(){
        this.updateUlScroll();
    }

    updateUlScroll(){
      if(!this.selectedLi || !this.ulContainer) return;

      let liHeight = this.selectedLi.clientHeight,
          liTop = this.selectedLi.offsetTop,
          liBottom = liTop  + liHeight,

          containerHeight = this.ulContainer.clientHeight,
          containerTop = this.ulContainer.scrollTop,
          containerBottom = containerTop + containerHeight;

      if(liBottom > containerBottom){
          this.ulContainer.scrollTo(0, containerBottom - (containerHeight - liHeight));
      }

      if(liTop < containerTop){
          this.ulContainer.scrollTo(0, containerTop - liHeight);
      }
    }

    render() {
        const suggestions = this.props.geneSuggestions;
        if(suggestions.length < 1) return null;

        var suggestionList = suggestions.map((item) => {
          return <li key={item}
                     ref={(li)=>{if(this.props.selectedGeneSuggestion.toLowerCase() == item.toLowerCase()) this.selectedLi=li}}
                     className={(this.props.selectedGeneSuggestion.toLowerCase() == item.toLowerCase()) ? 'selected' : ''}
                     onMouseEnter={()=>this.props.updateSelectedGeneSuggestion(item)}
                     onClick={this.props.addSuggestion}>
                     {item}
                 </li>;
        })

        return (
            <div ref={(container) => this.ulContainer = container} className='geneSuggestionBox'>
                <ul ref={(geneUL) => this.geneUL = geneUL} className='geneSuggestionList'>
                    {suggestionList}
                </ul>
            </div>
        )
    }
}

class IndividualSelect extends React.Component {
    constructor(props){
        super(props);

        this.state = {
            geneSearchText: '',
            geneSuggestions: [],
            selectedGeneSuggestion: ''
        }

        this.addSuggestion = this.addSuggestion.bind(this);
        this.getGeneSuggestions = this.getGeneSuggestions.bind(this);
        this.handleKeyDown = this.handleKeyDown.bind(this);
        this.handleChange = this.handleChange.bind(this);
        this.updateSelectedGeneSuggestion = this.updateSelectedGeneSuggestion.bind(this);
    }

    componentDidMount() {
        console.log()
    }

    handleChange(event) {
        this.setState({
            geneSearchText: event.target.value,
            geneSuggestions: this.getGeneSuggestions(event.target.value),
            selectedGeneSuggestion: ''
        });
    }

    getGeneSuggestions(textInput) {
        const searchGene = textInput.slice(
            textInput.search(lastWordRegex),
            textInput.length
        );

        console.log(searchGene);

        if(searchGene.length < 1) {
            var suggestions = [];
        } else {
            var suggestions = this.props.allGenes.filter((gene) => {
                if(gene.toLowerCase().indexOf(searchGene.toLowerCase()) == 0){
                  return true;
                }
                return false;
            })
        }

        return suggestions;
    }

    cycleSelectedSuggestion(dir){
        const suggestions = this.state.geneSuggestions;
        let index = suggestions.indexOf(this.state.selectedGeneSuggestion);


        if(dir == 'down') {
            if(index == suggestions.length-1) return;
            this.setState({selectedGeneSuggestion: suggestions[index + 1]})
        }

        if(dir == 'up') {
            if(index <= 0) return;
            this.setState({selectedGeneSuggestion: suggestions[index - 1]})
        }
    }

    handleKeyDown(event){
        const key = event.key;

        if(key == 'ArrowDown') {
            this.cycleSelectedSuggestion('down');
        }

        if(key == 'ArrowUp') {
            event.preventDefault();
            this.cycleSelectedSuggestion('up')
        }

        if(key == 'Enter') {
            this.addSuggestion();
        }
    }

    updateSelectedGeneSuggestion(gene){
        this.setState({selectedGeneSuggestion: gene});
    }

    addSuggestion() {
        if(this.state.selectedGeneSuggestion == "") return;
        let searchInput = this.state.geneSearchText.replace(lastWordRegex, this.state.selectedGeneSuggestion) + ", ";

        this.setState({
            geneSearchText: searchInput,
            geneSuggestions: [],
            selectedGeneSuggestion: ''
        });
    }

    render() {
        return (
          <div className='geneInputContainer'>
              <input type='text'
                     value={this.state.geneSearchText}
                     onChange={this.handleChange}
                     onKeyDown={this.handleKeyDown}>
              </input>
              <GeneSuggestionPanel
                  addSuggestion={this.addSuggestion}
                  geneSuggestions={this.state.geneSuggestions}
                  selectedGeneSuggestion={this.state.selectedGeneSuggestion}
                  updateSelectedGeneSuggestion={this.updateSelectedGeneSuggestion}/>
          </div>
        )
    }
}

class GenerateNetworkPanel extends React.Component {

    constructor(props) {
        super(props);

        this.state = {
            marginTop: 0,
        }

        this.updateStyle = this.updateStyle.bind(this);
    }

    updateStyle() {
        if(!this.panel) return;

        const margin = (window.innerHeight - this.panel.clientHeight) / 2;
        this.setState({
            marginTop: margin
        })
    }

    componentDidMount() {
        this.updateStyle();
        window.addEventListener('resize', this.updateStyle);
    }

    render() {
        if (!this.props.show) return null;

        const elemStyle = {
            marginTop: this.state.marginTop
        }

        return (
            <div
                id='GenerateNetworkPanel'
                ref={(panel) => {this.panel = panel; }}
                style={elemStyle}
            >
                <ExitButton onClick={this.props.togglePanel} />
                <GeneFamilySelect {...this.props} />
                <div className='clear'></div>
                <IndividualSelect allGenes={this.props.allGenes}/>
            </div>
        );
    }
}

export default GenerateNetworkPanel;
