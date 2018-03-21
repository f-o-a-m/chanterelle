pragma solidity ^0.4.13;

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
      geohash = _geohash;
      csc = computeCSC(geohash, address(this));
    }

    function register(address csrAddress) public {
      CSR csr = CSR(csrAddress);
      csr.register(csc);
    }

    function computeCSC(bytes8 geohash_arg, address addr) internal pure returns(bytes12) {
      return bytes12(keccak256(geohash_arg, addr));
    }
}
