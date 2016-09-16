// JS wrappers
module.exports = (function(){
  var lrs  = require("./output/Crypto.LRS.SimpleAPI/index.js");
  var lrs_ = require("./output/Crypto.LRS/index.js");
  var big  = require("big-integer");
  function jsNum(pursNum){
    return pursNum.toString(16);
  };
  function pursNum(jsNum){
    return big(jsNum, 16);
  };
  function jsParty(pursParty){
    return {
      publicKey: jsNum(lrs_.publicKey(pursParty)),
      privateKey: jsNum(lrs_.privateKey(pursParty))};
  };
  function pursParty(jsParty){
    return lrs_.Party.create(pursNum(jsParty.publicKey))(pursNum(jsParty.privateKey));
  };
  function jsSignature(pursSignature){
    return [
      jsNum(pursSignature.value0),
      jsNum(pursSignature.value1)]
      .concat(pursSignature.value2.map(jsNum))
      .join("_");
  };
  function pursSignature(jsSignature){
    var nums = jsSignature.split("_");
    return lrs_.Signature.create
      (pursNum(nums[0]))
      (pursNum(nums[1]))
      (nums.slice(2).map(pursNum));
  };
  function gen(){
    return jsParty(lrs.gen());
  };
  function sign(ring, party, message){
    return jsSignature(lrs.sign(ring.map(pursNum))(pursParty(party))(message)());
  };
  function verify(ring, signature, message){
    return lrs.verify(ring.map(pursNum))(pursSignature(signature))(message);
  };
  function link(signatureA, signatureB){
    return lrs.link(pursSignature(signatureA))(pursSignature(signatureB));
  };
  return {
    gen: gen,
    sign: sign,
    verify: verify,
    link: link};
})();
