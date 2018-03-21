pragma solidity ^0.4.13;

import "./SpatialUtils.sol";
import "./CSR.sol";

/*

  CSC is an abstract class for anything that inherits a spatial coordinate, i.e.
  a geohash. It has the ability to register its location in any Crypto Spatial
  Registery (CSR).

*/

contract CSC {
    bytes8 public geohash;
    bytes12 public csc;

    function CSC(bytes8 _geohash) public {
      // set geohash
      geohash = _geohash;

      // compute CSC
      csc = SpatialUtils.computeCSC(geohash, address(this));
    }

    function register(address csrAddress) public {
      CSR csr = CSR(csrAddress);
      csr.register(csc);
    }
}