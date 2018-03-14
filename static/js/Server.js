import axios from 'axios';

class Server {

  static getNetworkData(networkID, callback) {
    // Have to retrieve view ID first. Ugly.
    axios.get("http://localhost:1234/v1/networks/" + networkID + "/views")
      .then(function(response){
        axios.get("http://localhost:1234/v1/networks/" + networkID + "/views/" + response.data[0])
          .then(function(response){
            return callback(response);
          })
          .catch(function(error){
            console.log(error);
          })
      })
      .catch(function(error){
        console.log(error);
      })
  }

  static buildNetwork(network, callback) {
    axios.post("http://localhost:1234/v1/networks/?title=tempnet", {}, network)
      .then(function(response){
        callback(response);
      })
      .catch(function(error){
        console.log(error);
      })
  }

  static updateNetworkLayout(networkID, layout, callback) {
    axios.post("http://localhost:1234/v1/apply/layouts/" + layout + "/" + networkID)
      .then(function(response){
        callback(response)
      })
  }
}

export default Server;
