var createSha  = require("sha.js");
var createHash = function(){ return createSha("sha512"); };
var createHmac = require("hmac").bind(null, createHash, 128);

exports.sha512 = function(bits){
  return createHash().update(bits).digest("hex");
};

exports.hmacSha512 = function(key){ 
  return function(bits){
    return createHmac(key).update(bits, "utf8").digest("hex");
  };
};
