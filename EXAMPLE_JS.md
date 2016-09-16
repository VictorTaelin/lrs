#### Installing

    npm install lrs

#### Example

```javascript
var lrs = require("./index.js");

// 3 parties generate their public/private key pairs
var alice = lrs.gen();
var bob = lrs.gen();
var eve = lrs.gen();

// The list of public key is known and distributed
var group = [alice, bob, eve].map((m) => m.publicKey);

// Alice signs a message in behalf of one of the 3
var signed = lrs.sign(group, alice, "The body is buried on the backyard.");

// Anyone is able to verify *some* of them signed that message
console.log(lrs.verify(group, signed, "The body is buried on the backyard."));
  
// If that same person signs another message...
var signed2 = lrs.sign(group, alice, "Just kidding, he is alive.");

// We are able to tell the signature came from the same person
console.log(lrs.link(signed, signed2));
```
