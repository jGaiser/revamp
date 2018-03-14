import axios from 'axios';

class Network {

  static getNetworkData(networkID, callBack) {
    axios.get("http://localhost:1234/v1/networks/" + networkID)
      .then(function(response){
        return callBack(response);
      })
      .catch(function(error){
        console.log(error);
      })
  }
}

export default Network;
