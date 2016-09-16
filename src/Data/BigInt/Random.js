var big = require("big-integer");
var rng = require("secure-random");

exports.random = function(byteCount){
  return function(){
    var bytes = rng.randomArray(byteCount);
    var num = big(0);
    var mul = big(1);
    var base = big(256);
    for (var i=0; i<byteCount; ++i){
      num = num.add(big(bytes[i]).multiply(mul));
      mul = mul.multiply(base);
    }
    return num;
  };
};
