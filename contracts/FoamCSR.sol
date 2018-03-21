pragma solidity ^0.4.13;

import "./CSR.sol";
import "./CSC.sol";
import "./SpatialUtils.sol";

contract FoamCSR {
  mapping(bytes12 => address) public registry;

  function register(bytes12 csc) public {
    if (registry[csc] == 0) {
      CSC caller = CSC(msg.sender);
      require(SpatialUtils.computeCSC(caller.geohash(), msg.sender) == csc);

      registry[csc] = msg.sender;
      RegisterCSC(csc, msg.sender, caller.geohash());
    }
  }

  event RegisterCSC(bytes12 csc, address addr, bytes8 geohash);
}