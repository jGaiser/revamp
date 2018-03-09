import axios from 'axios';

class Network {
  constructor(){
    this.sayIt = "Spray it!"
  }

  static getNetworkData() {
    axios.get("http://localhost:1234/v1/networks/" + bromoTemplateID)
      .then(function(response){
        return response;
      })
      .catch(function(error){
        console.log(error);
      })
  }

}

export default Network;
